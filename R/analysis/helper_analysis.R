#' ---
#' title: "helper_analysis.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **helper_analysis**: general helper functions for scripts in /R/analysis
#' 



#'
#'  compute Nash–Sutcliffe model efficiency coefficient (NSE) 
my_nse = function(qobs, qsim, L=2, normalized=FALSE)
{
  # compute the standard NSE coefficient
  nse = 1 - drop_units( sum( abs(qsim - qobs)^L ) / sum( abs(qobs - mean(qobs))^L ) )
  
  # normalize, if requested
  if(normalized)
  {
    nse = 1 / (2 - nse)
  }
  
  return(nse)
}

#' identify contiguous time series within a longer record
my_split_series = function(dates, template=NULL)
{
  
  # handle bad input
  dates = as.Date(dates)
  
  # default is no mask, so copy `dates` to get full overlap
  if( is.null(template) ) { template = dates } else { template = as.Date(template) }
  
  # crop to time periods of overlap
  dates = dates[ dates %in% template ]
  
  # scan what's left for breaks in the time series
  breaks = c(0, which(diff(dates) != 1), length(dates))
  dates.break = lapply(seq(length(breaks) - 1), function(b) dates[(breaks[b] + 1):breaks[b+1]] )
  dates.nm = sapply(dates.break, function(x) paste(format(min(x), '%Y'), format(max(x), '%Y'), sep='-') )  
  return(setNames(dates.break, dates.nm))
}

#' write weather input text files for QSWAT and SWAT2012
my_swat_wmeteo = function(wdat, exdir, form='qswat', include=logical(0), suffix='', quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `wdat`: list of time series data: 'coords_sf', 'dates', 'tables', 'elevation' (see DETAILS) 
  # `exdir`: character string, path to directory to write output files
  # `form` : character string specifying output structure, either 'qswat' (the default) or 'swat'
  # `include`: (optional) boolean vector of same length as `wdat$coords_sf`, for writing subsets
  # `suffix`: (optional) suffix to append to filenames
  # `quiet`: logical, suppresses console messages
  #
  # RETURN VALUE:
  #
  # A list of all filenames written and their directory (`exdir`)
  #
  #
  # DETAILS: 
  #
  # Writes one of two different file structures, depending on `form`:
  #
  # For `swat`, we write one file per SWAT variable found in `wdat`. This contains (lat/long)
  # coordinates and the full time series of all point data sources (eg. 'pcp1.pcp' contains
  # precipitation data from all locations, as a numeric matrix). Elevation is also written to
  # these files (as integer) whenever it is supplied in `wdat`. Note, however, that the elevations
  # stored in these files are ignored by SWAT, which bases any elevation-related calculations on
  # the subbasin elevations listed in the *.sub files. Similarly, SWAT maps point weather data
  # sources to subbasins based on the integer IDs listed in the *.sub files, and not the lat/long
  # values in the weather input files (eg. integer ID "23" refers to the 23rd column of 'pcp1.pcp')
  #
  # With 'qswat', the point data location info DOES matter (it is used by QSWAT to construct the
  # *.sub files), and is stored separately from the actual time series data: Each weather variable
  # has a single text file (eg. 'pcp.txt') that lists the point coordinates and their elevations;
  # whereas the time series data go into location-specific files (eg. 'tmp_grid23.txt'). It's not
  # clear to me whether the elevation data are important in the 'qswat' case, but you should
  # probably include them to be on the safe side.
  #
  # The required entries of `wdat` are:
  #
  #   coords_sf: sfc POINT object with `name` attribute (unique names for each point data source)
  #   dates: vector of Date objects, in same order as the rows of the weather tables
  #   tables: named list of dataframes, one per variable, where column names match coords_sf$name
  #   elevation: (optional) named list of numeric elevations (in metres)
  # 
  # Supported SWAT variable names are: 
  # 
  # 'pcp' (precip, in mm/day)
  # 'tmin' and 'tmax' (temperature in degrees Celsius, where both min and max must be specified)
  # 'slr' (daily solar radiation, in MJ/m2)
  # 'wnd' (average speed, in m/s)
  # 'hmd' (relative humidity, expressed as fraction)
  # 
  # Entries of `wdat$tables` that do not match these names are ignored (nothing written). Note
  # that since 'tmin' and 'tmax' appear in the same SWAT input file ('.tmp'), they must both be
  # supplied or the function will write neither.
  #
  # Output filenames in 'swat' mode have the form '<varname><suffix>.<varname>' (eg. with
  # `suffix=1`, the precip data is written to the file 'pcp1.pcp'). In 'qswat' mode, they have the
  # form '<varname>.txt' for location data, and '<varname><suffix>_<pointname>.txt' for time series
  # data (eg. 'pcp1.txt' contains the location info for time series files 'pcp1_grid1.txt',
  # 'pcp1_grid2.txt', etc). The only exception is for temperature, where there is a single '.tmp'
  # file instead of separate tmin and tmax files.
  #
  # The optional `include` vector specifies (as boolean index) a subset of `wdat$coords_sf` to
  # include in the output. Note that for 'swat' mode, this controls the number of columns in the
  # output, and therefore the mapping (of columns to subbasins) in *.sub files should be adjusted
  # accordingly. By default all the data are written.
  #
  
  # missing data field (NA) is coded as "-99.0"
  na.value = -99
  
  # set EPSG code for latitude/longitude
  epsg.geo = 4326
  
  # define SWAT variable names, and expected input variable names
  vn.list = list(pcp='pcp', slr='slr', wnd='wnd', hmd='hmd', tmp=c('tmax', 'tmin'))
  
  # first date in the time series
  origin.date = wdat$dates[1]
  
  # check for invalid `form` argument
  if(!(form %in% c('qswat', 'swat')))
  {
    stop('`form` must either be "qswat" or "swat"') 
  }
  
  # if elevations not supplied, use missing data value
  if(is.null(wdat$elevation))
  {
    wdat$elevation =  setNames(rep(na.value, nrow(wdat$coords_sf)), wdat$coords_sf$name)
  }
  
  # handle unassigned `include` argument
  if(length(include)==0)
  {
    # default behaviour is to write all points
    include = setNames(rep(TRUE, nrow(wdat$coords_sf)), wdat$coords_sf$name)
  }
  
  # build index of `wdat` to write, extract geographic coordinates
  coords = wdat$coords_sf[include,]
  coords.geo = st_coordinates(st_transform(coords, epsg.geo))
  n.coords = nrow(coords)
  
  # contruct table of geographic (lat/long) coordinates, append elevations
  coords.tab = setNames(data.frame(coords.geo, row.names=coords$name), c('long', 'lat'))
  coords.tab$elevation = wdat$elevation[include]
  
  # it's unclear if there is a limit on precision - 3 decimal places should be plenty
  coords.tab = round(coords.tab, 3)
  
  # check for input matching SWAT variable names
  vn.idx = sapply(vn.list, function(nm) all(nm %in% names(wdat$tables)))
  vn.in = names(vn.idx)[vn.idx]
  
  # warn of unpaired temperature input (one but not both of 'tmin' or 'tmax' supplied)
  if(!vn.idx['tmp'] & any(c('tmin', 'tmax') %in% names(wdat$tables)))
  {
    warning('one of tmin/tmax not supplied. Temperature data will not be written')
  }
  
  # construct a list of strings to use as station-variable names (filenames, in 'qswat' mode)
  svn = setNames(lapply(vn.in, function(vn) paste(paste0(vn, suffix), coords$name, sep='_')), vn.in)
  
  # set up output directory (creates it if it doesn't exist)
  my_dir(exdir)
  
  # replace NA fields with numeric flag for missing data
  wdat$tables = lapply(wdat$tables, function(xx) xx %>% replace(is.na(.), na.value))
  
  # generate SWAT-readable text input files
  if(tolower(form)=='swat')
  {
    # Note that the first 4 lines of these files are treated as comments by SWAT
    l2.string = paste(c('Lati  ', substring(as.character(coords.tab[['lat']]), 1, 4)), collapse=' ')
    l3.string = paste(c('Long  ', substring(as.character(coords.tab[['long']]), 1, 4)), collapse=' ')
    l4.string = paste(c('Elev  ', substring(as.character(coords.tab[['elev']]), 1, 4)), collapse=' ')
    
    # define paths to the output files (one per variable)
    wstn.path = sapply(vn.in, function(vn) file.path(exdir, paste0(vn, suffix, '.txt')))
    
    # prepare Julian date-year strings that start each line
    dates.string = format(wdat$dates[include], '%Y%j')
    
    # write the variables one at a time in a loop
    if(!quiet) pb = txtProgressBar(max=length(vn.in), style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      l1.string = paste('Station ', paste(svn[[vn]], collapse=','))
      if(!quiet) print(paste('writing', basename(wstn.path[vn]), 'to directory', exdir))
      
      # handle temperature data, which requires concatentation of two variables
      if(vn=='tmp')
      {
        # we get 10 characters total to write the minmax vals (with no delimiters), or 5 per extremum
        tsmin.matrix = sapply(wdat$tables[['tmin']][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        tsmax.matrix = sapply(wdat$tables[['tmax']][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        
        # concatenate the max and min (entrywise), then concatenate lines
        ts.matrix = t(sapply(1:nrow(tsmin.matrix), function(idx) paste0(tsmax.matrix[idx,], tsmin.matrix[idx,])))
        ts.out = t(apply(ts.matrix, 1, paste0))
        
      } else {
        
        # handle standard variables, creating matrix of values, then concatenating by line
        ts.matrix = sapply(wdat$tables[[vn]][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        ts.out = t(apply(ts.matrix, 1, paste0))
        
      }
      
      # append dates to beginning of each line of numeric data 
      ts.lines = sapply(1:nrow(ts.out), function(idx) paste(c(dates.string[idx], ts.out[idx,]), collapse=''))
      
      # append comment lines and write to disk
      writeLines(c(l1.string, l2.string, l3.string, l4.string, ts.lines), con=wstn.path[vn])
      if(!quiet) setTxtProgressBar(pb, idx.vn)
      
    }
    if(!quiet) close(pb) 
    
    # finish and return filenames in list
    return(list(exdir=exdir,
                stations=setNames(basename(wstn.path), vn.in)))
    
  }
  
  # generate QSWAT-readable text input files
  if(tolower(form)=='qswat')
  {
    # build wdat station file tables for QSWAT as matrices of text
    wdat.wstn = lapply(setNames(nm=vn.in), function(vn) { 
      cbind(ID=1:n.coords, 
            NAME=svn[[vn]], 
            LAT=unname(coords.tab['lat']),
            LONG=unname(coords.tab['long']),
            ELEVATION=unname(coords.tab['elevation']))
    })
    
    # define paths to the output files (with time series stored separately from station location info)
    wstn.path = sapply(names(svn), function(fn) file.path(exdir, paste0(fn, suffix, '.txt')))
    wstn.ts.path = lapply(svn, function(fn) setNames(file.path(exdir, paste0(fn, '.txt')), nm=coords$name))
    
    # write the station location data
    sapply(names(svn), function(vn) write.csv(wdat.wstn[[vn]], file=wstn.path[vn], quote=F, row.names=F))
    
    # the first line of each time series data file is the origin date (without spaces) 
    origin.string = paste0(gsub('-', '', origin.date))
    
    # write the station time series data in a loop
    if(!quiet) pb = txtProgressBar(max=length(vn.in)*n.coords, style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      if(!quiet) print(paste('writing', n.coords, vn, 'files to directory', exdir))
      
      # loop over grid point locations
      for(idx.coords in 1:n.coords)
      {
        # identify point name and output path for the text file
        stn.name = coords$name[idx.coords]
        out.path = wstn.ts.path[[vn]][stn.name]
        if(!quiet) setTxtProgressBar(pb, idx.coords + (idx.vn-1)*n.coords)
        
        # handle temperature data, which requires concatentation of two variables
        if(vn=='tmp')
        {
          # write txt containing comma-separated concatenation of variables by row
          ts.out = sapply(wdat$tables[vn.list[['tmp']]], function(dat) dat[[stn.name]])
          writeLines(c(origin.string, apply(ts.out, 1, paste, collapse=',')), con=out.path)
          
        } else {
          
          # write txt for usual case of a single variable in each file (eg. prec, wind)
          ts.out = wdat$tables[[vn.list[[vn]]]][[stn.name]]
          writeLines(c(origin.string, ts.out), con=out.path)
          
        }
        
      }
      
    }
    if(!quiet) close(pb) 
    
    # finish and return filenames in list
    return(list(exdir=exdir,
                stations=setNames(basename(wstn.path), vn.in), 
                data=lapply(wstn.ts.path, basename)))
  }
  
}

#' run the TauDEM workflow to compute watershed geometry given a DEM
my_taudem = function(dem, odir, nstream=0, nproc=8, outlet.sf=NULL, bsf=NULL, bdepth=10)
{
  # ARGUMENTS:
  #
  # `dem`: raster object, a digital elevation model (units of meters)
  # `odir`: relative path to the output directory
  # `nstream`: integer, stream delineation threshold
  # `nproc`: integer, the number of cores to use with MSMPI
  # `outlet.sf`: sf object, (optional) point geometry for outlets, with integer attribute `id`
  # `bsf`: sf object, (optional) line geometry for drainage reinforcement ('burn-in')
  # `bdepth`: numeric (in meters), the vertical height to subtract from the DEM for burn-in
  #
  # RETURN VALUE:
  #
  # A named vector of character strings supplying paths to the TauDEM output files
  # 
  # DETAILS:
  #
  # This attempts to replicate the behaviour of TauDEM in QSWAT+, but with the option of 
  # using drop analysis to determine the stream threshold.
  #
  # based on: https://hydrology.usu.edu/taudem/taudem5/TauDEMRScript.txt and QSWAT+
  # Requires TauDEM binaries to be installed to the path hardcoded below as `exe.dir`.
  # get TauDEM installer from: http://hydrology.usu.edu/taudem/taudem5.0/downloads.html,
  # read about it here: https://hydrology.usu.edu/taudem/taudem2.0/taudem.html#Overview
  #
  # `nstream` is the contributing area threshold (in # of cells) above which a cell is
  # included in the stream (resp., channel) network. If `nstream` is zero or negative, 
  # TauDEM will attempt to find the smallest feasible value (producing the most detailed
  # feasible network) by drop analysis, based on the constant drop law of Broscoe (1959).
  #
  # Note that any non-line geometry in `bsf` will be ignored, and any assigned value for
  # `bdepth` will be ignored if `bsf` is not supplied (since there is nothing to burn).
  # Non-point geometries in `outlet.sf` will be ignored, and if `outlet.sf` is unassigned, the
  # main outlet of the watershed (in terms of area drained) is computed and set.
  
  
  # path to TauDEM binaries
  exe.dir = normalizePath('C:/SWAT/SWATEditor/TauDEM5Bin')
  
  # prefix for shell commands pointing to TauDEM directory and MS MPI
  sps = paste('pushd', exe.dir, '&&', 'mpiexec -n', nproc)
  
  # define the files written by the TauDEM workflow
  files.towrite = list(
    
    # DEM (source)
    c(name='dem',
      file=file.path(odir, 'dem_in.tif'), 
      type='GeoTIFF',
      description='input DEM (after burn-in, if `bsf` present)'),
    
    # outlet
    c(name='outlet',
      file=file.path(odir, 'outlet.shp'), 
      type='ESRI shapefile',
      description='points representing outlets of interest'),
    
    # snapped outlet
    c(name='outlet_snap',
      file=file.path(odir, 'outlet_snap.shp'), 
      type='ESRI shapefile',
      description='outlets after snap to stream network derived from `ad8`'),
    
    # pit-removed DEM
    c(name='fel',
      file=file.path(odir, 'taudem_fel.tif'), 
      type='raster',
      description='`dem` after pits (artifacts) raised to their pour point'),
    
    # D8 descent direction 
    c(name='p',
      file=file.path(odir, 'taudem_p.tif'), 
      type='GeoTIFF',
      description='steepest 8-point descent direction (discrete; 1=E, 2=NE, 3=N, etc)'),
    
    # D8 slope
    c(name='sd8',
      file=file.path(odir, 'taudem_sd8.tif'), 
      type='GeoTIFF',
      description='slope grid corresponding to `p`, reported as tan(angle)'),
    
    # D-infinity descent direction
    c(name='ang',
      file=file.path(odir, 'taudem_ang.tif'), 
      type='GeoTIFF',
      description='counter-clockwise angle (rad) from east (continuous)'),
    
    # D-infinity slope
    c(name='slp',
      file=file.path(odir, 'taudem_slp.tif'), 
      type='GeoTIFF',
      description='slope grid corresponding to `ang`, reported as tan(angle)'),
    
    # D8 contributing area
    c(name='ad8',
      file=file.path(odir, 'taudem_ad8.tif'), 
      type='GeoTIFF',
      description='contributing area of upslope neighbours, from `p` (discrete)'),
    
    # D-infinity contributing area
    c(name='sca',
      file=file.path(odir, 'taudem_sca.tif'), 
      type='GeoTIFF',
      description='contributing area of upslope neighbours, from `ang` (continuous)'),
    
    # streams from threshold
    c(name='sst',
      file=file.path(odir, 'taudem_sst.tif'), 
      type='GeoTIFF',
      description='stream network, delineated from `ad8` with threshold `nstream`'),
    
    # stream network ordering
    c(name='gord',
      file=file.path(odir, 'taudem_gord.tif'), 
      type='GeoTIFF',
      description='Strahler network order for flow network derived from `p`'),
    
    # longest upslope length
    c(name='plen',
      file=file.path(odir, 'taudem_plen.tif'), 
      type='GeoTIFF',
      description='path length from furthest cell draining into each cell'),
    
    # sum total upslope length
    c(name='tlen',
      file=file.path(odir, 'taudem_tlen.tif'), 
      type='GeoTIFF',
      description='total length of all paths draining into each cell'),
    
    # list of links in channel network tree
    c(name='tree',
      file=file.path(odir, 'taudem_tree.dat'), 
      type='plaintext file',
      description=' text file with list of links in channel network tree'),
    
    # list of coordinates in channel network tree
    c(name='coord',
      file=file.path(odir, 'taudem_coord.dat'), 
      type='plaintext file',
      description='text file with list of coordinates in channel network tree'),
    
    # watershed stream network shapefile
    c(name='demnet',
      file=file.path(odir, 'taudem_demnet.shp'), 
      type='ESRI shapefile',
      description='channel network shapefile resulting from StreamNet'),
    
    # stream network ordering
    c(name='ord',
      file=file.path(odir, 'taudem_ord.tif'), 
      type='GeoTIFF',
      description='Strahler network order raster from StreamNet'),
    
    # subbasin membership raster
    c(name='w',
      file=file.path(odir, 'taudem_w.tif'), 
      type='GeoTIFF',
      description='watershed identifier raster from StreamNet'),
    
    # subbasins polygon geometry
    c(name='subb',
      file=file.path(odir, 'subbasins.shp'), 
      type='ESRI shapefile',
      description='polygonized subbasins, derived from `w`')
    
  )
  
  # save the table as csv (creating output directory)
  print(paste('> running TauDEM in output directory', odir))
  taudem.meta = my_metadata('taudem', files.towrite, overwrite=TRUE, data.dir=odir)
  
  # copy DEM and, if necessary, create burn-in streams raster 
  if(is.null(bsf))
  {
    print(' > writing DEM...')
    
    # write an unmodified copy of the DEM to the output directory
    writeRaster(dem, here(taudem.meta['dem', 'file']), 
                options=c('COMPRESS=NONE, TFW=YES'),
                format='GTiff', 
                overwrite=TRUE)
    
  } else {
    
    print(' > rasterizing streams for burn-in...')
    
    # create metadata list entry for burn-in streams raster
    bsf.entry = c(name='bsf',
                  file=file.path(odir, 'streams_toburn.tif'), 
                  type='GeoTIFF',
                  description='rasterized stream input, used for burn-in via `bsf`')
    
    # rewrite the metadata table csv
    taudem.meta = my_metadata('taudem', list(bsf.entry), overwrite=TRUE, data.dir=odir, v=F)
    
    # assign units to burn-in depth
    bdepth = set_units(bdepth, m)
    
    # make sure the streams network contains only LINESTRING geometries, drop any attributes
    bsf = st_geometry(bsf[st_is(bsf, 'LINESTRING'),])
    
    # convert streams to `Spatial`, add unit dummy field, rasterize
    bsf.sp = as(bsf, 'Spatial')
    bsf.sp$dummy = rep(1, length(bsf.sp))
    gRasterize(bsf.sp, dem, field='dummy', filename=here(taudem.meta['bsf', 'file']))
    
    # find cell numbers of non-NA cells in the streams raster 
    bsf.idx = Which(!is.na(raster(here(taudem.meta['bsf', 'file']))), cells=TRUE) 
    
    # extract elevations at all cells intersecting with a stream
    bsf.elev = set_units(extract(dem, bsf.idx), m)
    
    # decrement these elevations before writing DEM file as uncompressed GeoTIFF
    print('  > writing DEM...')
    dem[bsf.idx] = as.vector(bsf.elev - bdepth)
    writeRaster(dem, here(taudem.meta['dem', 'file']), 
                options=c('COMPRESS=NONE, TFW=YES'),
                format='GTiff', 
                overwrite=TRUE)
    
  }
  
  # load the DEM and compute the number of cells
  dem = raster(here(taudem.meta['dem', 'file']))
  ncell = length(dem) 
  
  # compute area in km^2 of single grid cell (assumes projection units of 'm')
  cell.area = set_units(prod(res(dem))/1e6, km^2)
  dem.area = ncell*cell.area
  
  # drop analysis is skipped when `nstream` threshold argument is provided
  do.dropanalysis = FALSE
  
  # handle unassigned stream delineation threshold
  if(! nstream > 0)
  {
    # zero or negative `nstream` triggers drop analysis
    do.dropanalysis = TRUE
    
    # set a range of thresholds to test by drop analysis
    nstream.min = max(set_units(0.1, km^2), 2*cell.area)
    nstream.max = min(set_units(100, km^2), dem.area/10)
    
    # corresponding number of cells
    nstream.min = as.integer(nstream.min/cell.area) + 1
    nstream.max = as.integer(nstream.max/cell.area) + 1
    
    # set default initial threshold (if `nstream` provided, set to 10X that)
    astream = set_units(1, km^2)
    
    # corresponding number of cells
    nstream = as.integer(astream/cell.area) + 1
    
  }
  
  
  # normalize paths for use in windows shell
  np = normalizePath(here(taudem.meta[,'file']), mustWork=FALSE)
  names(np) = rownames(taudem.meta)
  
  ## begin TauDEM worflow
  
  # 1. pit removal (in: `dem`; out: `fel`)
  print('  > removing pits...')
  arg = paste('-z', np['dem'], '-fel', np['fel'])
  shell(paste(sps, 'PitRemove', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 2. D8 geometry (in: `fel`; out: `sd8`, `p`)
  print(' > computing D8 flow directions and slopes...')
  arg = paste('-fel', np['fel'], '-sd8', np['sd8'], '-p', np['p'])
  shell(paste(sps, 'D8Flowdir', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 4. D8 contributing areas (in: `p`; out: `ad8`)
  print(' > computing D8 contributing area...')
  arg = paste('-p', np['p'], '-ad8', np['ad8'], '-nc')
  shell(paste(sps, 'AreaD8', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # if no outlet argument supplied, find main outlet using `ad8`...
  if(is.null(outlet.sf))
  {
    ad8 = raster(np['ad8'])
    outlet.sf = st_sfc(st_point(xyFromCell(ad8, which.max(getValues(ad8)))), crs=st_crs(dem))
    outlet.sf = st_sf(data.frame(id=1), geom=outlet.sf, crs=st_crs(dem))
    print('  > main outlet detected from AD8 and written to outlet.shp')
    
  } else {
    
    # ...otherwise discard any non-point geometry and attributes from user input
    outlet.sf = st_geometry(outlet.sf[st_is(outlet.sf, 'POINT'),])
    
    # add the `id` attribute
    outlet.sf = st_sf(data.frame(id = 1:length(outlet.sf)), geometry=outlet.sf)
    
  }
  
  # 3. D-infinity geometry (in: `fel`; out: `ang`, `slp`)
  print(' > computing D-infinity flow directions and slopes...')
  arg = paste('-fel', np['fel'], '-ang', np['ang'], '-slp', np['slp'])
  shell(paste(sps, 'DinfFlowdir', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 5. D-infinity contributing areas (in: `ang`; out: `sca`)
  print(' > computing D-infinity contributing area...')
  arg = paste('-ang', np['ang'], '-sca', np['sca'], '-nc')
  shell(paste(sps, 'AreaDinf', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 6. initial stream delineation (in: `ad8`; out: `sst`)
  print(paste(' > delineating streams with threshold of', nstream, 'cells...'))
  arg = paste('-ssa', np['ad8'], '-src', np['sst'], '-thresh', nstream)
  shell(paste(sps, 'Threshold', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # write outlet point(s) as shapefile
  print(' > writing outlets shapefile...')
  st_write(outlet.sf, np['outlet'], append=FALSE, quiet=TRUE)
  
  # 7. snap outlets (in: `p`, `sst`, `outlet`; out: `outlet_snap`)
  print('  > snapping outlets along D8 to nearest stream...')
  arg = paste('-p', np['p'], '-src', np['sst'], '-o', np['outlet'], '-om', np['outlet_snap'])
  shell(paste(sps, 'MoveOutletsToStreams', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # write projection info for `outlet_snap` (it's the same as for `outlet`)
  file.copy(gsub('.shp', '.prj', np['outlet']), gsub('.shp', '.prj', np['outlet_snap']))
  
  # 8. drop analysis (if requested)
  if(do.dropanalysis)
  {
    print(' > starting drop analysis')
    
    # create metadata table entry for drop analysis output files
    drop.entry = list(
      
      c(name='drop',
        file=file.path(odir, 'drop_analysis.txt'), 
        type='plaintext file',
        description='stream drop statistics, derived from `ssa` for various thresholds'),
      
      # candidate stream source pixels from Peuker-Douglas method
      c(name='ss',
        file=file.path(odir, 'taudem_ss.tif'), 
        type='GeoTIFF',
        description='stream skeleton delineated from `fel` by Peuker-Douglas algorithm'),
      
      # accumulated candidate stream source cells from Peuker-Douglas
      c(name='ssa',
        file=file.path(odir, 'taudem_ssa.tif'), 
        type='GeoTIFF',
        description='contributing area of stream source cells, from `ss`')
      
    )
    
    # rewrite the metadata table csv and update names list
    taudem.meta = my_metadata('taudem', drop.entry, overwrite=TRUE, data.dir=odir, v=F)
    np = normalizePath(here(taudem.meta[,'file']), mustWork=FALSE)
    names(np) = rownames(taudem.meta)
    
    # 8a. Douglas-Peucker stream delineation (in: `fel`; out: `ss`)
    print('  > delineating streams using Douglas-Peucker algorithm...')
    arg = paste('-fel', np['fel'], '-ss', np['ss'])
    shell(paste(sps, 'PeukerDouglas', arg), ignore.stderr=T,  ignore.stdout=T)
    
    # 8b. recompute D8 areas for drop analysis (in: `p`, `ss`, `outlet_snap`; out: `ssa`)
    print('  > computing D8 contributing area from Peuker-Douglas stream sources...')
    arg = paste('-p', np['p'], '-o', np['outlet_snap'], '-wg', np['ss'],  '-ad8', np['ssa'], '-nc')
    shell(paste(sps, 'AreaD8', arg), ignore.stderr=T,  ignore.stdout=T)
    
    # 8c. set reasonable default search interval and perform drop analysis
    drop.pars = paste(nstream.min, nstream.max, 100, 0)
    arg1 = paste('-p', np['p'], '-fel', np['fel'], '-ad8', np['ad8'], '-ssa', np['ssa'])
    arg2 = paste('-drp', np['drop'], '-o', np['outlet_snap'], '-par', drop.pars)
    shell(paste(sps, 'Dropanalysis', arg1, arg2), ignore.stderr=T,  ignore.stdout=T)
    
    # set channels threshold to optimum found in drop analysis
    drop.txt = readLines(np['drop'])
    nstream.string = strsplit(drop.txt[length(drop.txt)], 'Optimum Threshold Value: ')[[1]][2]
    nstream = as.integer(nstream.string) + 1
    astream = nstream * cell.area
    print(paste('  > drop analysis optimum:', nstream, 'cells, around', round(astream, 3), 'km^2'))
    
    # create metadata table entry for threshold
    threshold.entry = list(c(name='nstream',
                             file=NA, 
                             type='integer (optimum from drop analysis)',
                             description=paste('stream threshold:', nstream)))
    
  } else {
    
    
    # create metadata table entry for thresholds
    threshold.entry = list(c(name='nstream',
                             file=NA, 
                             type='integer (from user input)',
                             description=paste('stream threshold:', nstream)))
    
  }
  
  # rewrite the metadata table csv to include threshold values and update names list
  taudem.meta = my_metadata('taudem', threshold.entry, overwrite=TRUE, data.dir=odir, v=F)
  np = normalizePath(here(taudem.meta[,'file']), mustWork=FALSE)
  names(np) = rownames(taudem.meta)
  
  # 9. recompute D8 areas with snapped outlets (in: `p`, `outlet_snap`; out: `ad8`)
  print(' > computing D8 contributing area...')
  arg = paste('-p', np['p'], '-ad8', np['ad8'], '-o', np['outlet_snap'], '-nc')
  shell(paste(sps, 'AreaD8', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 10. repeat stream delineation with new threshold (in: `ad8`, `nstream`; out: `sst`)
  print(paste('  > delineating streams with threshold of', nstream, 'cells...'))
  arg = paste('-ssa', np['ad8'], '-src', np['sst'], '-thresh', nstream)
  shell(paste(sps, 'Threshold', arg), ignore.stderr=T,  ignore.stdout=T)
  
  # 11. run GridNet with snapped outlets (in: `p`, `outlet_snap`; out:  `gord`, `plen`, `tlen`)
  print('  > running GridNet...')
  arg1 = paste('-p', np['p'], '-plen', np['plen'], '-tlen', np['tlen'], '-gord', np['gord'])
  arg2 = paste('-o', np['outlet_snap'])
  shell(paste(sps, 'GridNet', arg1, arg2), ignore.stderr=T,  ignore.stdout=T)
  
  # 12. run StreamNet (in: `fel`, `p`, `ad8`, `outlet_snap`, `sst`; 
  # out: `tree`, `coord`, `demnet`, `w`, `ord`)
  print('  > running StreamNet...')
  arg1 = paste('-fel', np['fel'], '-p', np['p'], '-ad8', np['ad8'], '-ord', np['ord'])
  arg2 = paste('-o', np['outlet_snap'], '-src', np['sst'], '-tree', np['tree'])
  arg3 = paste('-coord', np['coord'], '-net', np['demnet'], '-w', np['w'])
  shell(paste(sps, 'StreamNet', arg1, arg2, arg3), ignore.stderr=T,  ignore.stdout=T)
  
  # write projection info for `demnet` (it's the same as for `outlet`)
  file.copy(gsub('.shp', '.prj', np['outlet']), gsub('.shp', '.prj', np['demnet']))
  
  # 13. run gdal_polygonize to create subbasins shapefile
  print('   > polygonizing subbasins...')
  my_gdal_polygonize(np['w'], np['subb'])
  
  # return table of file paths
  return(taudem.meta)
  
}

#' calls gdal_polygonize.py from OSGEO4W (replacement for raster::rasterToPolygons)
my_gdal_polygonize = function(infile, outfile)
{
  # ARGUMENTS:
  #
  # `infile`: string, full path to input GeoTIFF, whose values indicate polygon membership
  # `outfile`: string, full path to desired output ESRI shapefile (with extension '.shp')
  #
  # RETURN VALUE:
  #
  # None
  # 
  # DETAILS:
  #
  # This function is an R wrapper for the python script 'gdal_polygonize.py', one of the
  # processing algorithms that that ships with QGIS3. It assumes the OSGEO4W long term release
  # of QGIS3 is installed in the standard location for 64bit Windows 10. On other platforms it
  # should be possible to modify paths (`osgeo4w.path`, `envir.path`, `pyqgis.path`, `py.path`)
  # and shell command strings (`quiet.str`, `osgeo.str`, `envir.str`, `cd.str`) make things work.
  #
  # my_gdal_polygonize does the same thing as raster::rasterToPolygons(), but much faster. It
  # is meant to convert the raster `w` from TauDEM, the output of StreamNet indicating subbasin
  # membership, into a multipolygon geometry. This is the only use case I have tested.
  #
  # Note that this will overwrite all elements of `outfile` without warning (ie. the 'shp',
  # 'dbf', 'shx', 'prj' files with the same basename as `outfile`), so double check your paths.
  #
  
  # paths (these are platform dependent)
  osgeo4w.path = 'C:/Program Files/QGIS 3.10'
  envir.path = file.path(osgeo4w.path, 'bin/o4w_env.bat')
  pyqgis.path = file.path(osgeo4w.path, 'apps/Python37')
  py.path = file.path(pyqgis.path, 'Scripts/gdal_polygonize.py')
  
  # identify the four files associated with an ESRI shape"file"
  ext.esri = c('shp', 'dbf', 'shx', 'prj')
  outfile.nm = basename(outfile)
  outfile.fns = sapply(ext.esri, function(ext) gsub('.shp', paste0('.', ext), outfile.nm))
  outfile.paths = file.path(dirname(outfile), outfile.fns)
  
  # delete `outfile` and friends if they exist on disk already 
  idx.exists = file.exists(outfile.paths)
  if(any(idx.exists))
  {
    unlink(outfile.paths[idx.exists])
  }
  
  # arguments to gdal_polygonize.py 
  tif.str = paste0('"', normalizePath(infile), '"')
  shp.str = paste0('"', normalizePath(outfile, mustWork=FALSE), '"')
  args.str = paste('-8', tif.str, '-f "ESRI Shapefile"', shp.str, '-q')
  
  # command to suppress verbose output of TauDEM
  quiet.str = '@echo off'
  
  # command to set up environmental variables and search paths for python 
  osgeo.str = paste0('set OSGEO4W_ROOT=', normalizePath(osgeo4w.path))
  envir.str = paste('call', paste0('"', normalizePath(envir.path), '"'))
  
  # command to change directory to Python 3 exe (the one that ships with QGIS) 
  cd.str = paste('pushd', paste0('"', normalizePath(pyqgis.path), '"'))
  
  # the python call
  py.str = paste('python', paste0('"', normalizePath(py.path), '"'), args.str)
  
  # paste these command strings together and execute
  shell(paste(quiet.str, osgeo.str, envir.str, cd.str, py.str, sep=' && '))
  
}

#' snap point(s) (sfc) to nearest stream reach(es) in demnet, returning LINKNO (int)
my_demnet_snap = function(p, demnet, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `p`: sfc point, the location(s) to snap
  # `demnet`: sf object, the channel network shapefile produced by StreamNet in TauDEM
  # `quiet`: boolean, suppresses message about snap length if TRUE
  #
  # RETURN VALUE:
  #
  # list with two named entries:
  
  # `link`: integer vector, the 'LINKNO' from TauDEM for nearest stream reach line segment
  # `dist`: numeric vector (with units), distance to the line segment
  # 
  
  # snap the input point to the nearest line geometry and find its ID
  idx.snap = st_nearest_feature(p, demnet)
  linkno.snap = demnet$LINKNO[idx.snap]
  
  # find the snap distances
  d.snap = st_distance(p, demnet[idx.snap,], by_element=TRUE)
  
  # finish, printing message if requested
  distance.msg = paste0(round(d.snap,3), as.character(units(d.snap)))
  if(!quiet) print(paste('snap distance', distance.msg))
  return(list(link=linkno.snap, dist=d.snap)) 
}

#' find all upstream line segments in a channel network line geometry collection
my_upstream = function(root, demnet, linkno=NULL, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `root`: sf or sfc POINT(s), the location on/near the channel network to start from 
  # `demnet`: sf object, the channel network shapefile produced by StreamNet in TauDEM
  # `linkno`: (internal) integer, the link number to use in place of `root`
  # `quiet`: boolean, suppresses message about snap length if TRUE
  #
  # RETURN VALUE:
  #
  # A copy of the `demnet` sf object containing only the line segments upstream of `root`,
  # with a new attribute 'isroot' indicating the line segment on which `root` is located;
  # Or, if `root` contains more than one point, a list of such objects, one for each point.
  # 
  
  if(is.null(linkno))
  {
    # drop attributes of `root`, keeping only geometry
    root = st_geometry(root)
    root.n = length(root)
    
    # handle vectorized my_upstream calls
    if(root.n > 1)
    {
      # print progress bar and make storage list for output
      pb = txtProgressBar(min=1, max=root.n, style=3)
      demnet.out = vector(mode='list', length=root.n)
      
      # loop over each point in `root`, returning results in list
      for(idx.out in 1:root.n)
      {
        demnet.out[[idx.out]] = my_upstream(root[idx.out], demnet, linkno, quiet)
        setTxtProgressBar(pb, idx.out)
      }
      close(pb)
      return(demnet.out)
      
    }
    
    # snap the input point to the nearest line geometry and find its ID
    linkno.snap = my_demnet_snap(root, demnet, quiet)$link
    
    # recursive call to follow stream network upstream of this channel
    linkno.all = my_upstream(root, demnet, linkno.snap)
    
    # return the subset of demnet
    demnet.out = demnet[demnet$LINKNO %in% linkno.all,]
    demnet.out$isroot = rep(FALSE, nrow(demnet.out))
    demnet.out$isroot[demnet.out$LINKNO==linkno.snap] = TRUE
    return(demnet.out)
    
    
  } else {
    
    # lookup both upstream links
    idx.linkno = demnet$LINKNO==linkno
    up.linkno = c(demnet$USLINKNO1[idx.linkno], demnet$USLINKNO2[idx.linkno])
    
    # -1 denotes an endpoint (leaf) in the tree...
    idx.leaf = up.linkno==-1
    
    # recursive call to collect upstream branches along 1st link
    up.res1 = NULL
    if(!idx.leaf[1])
    {
      up.res1 = my_upstream(root, demnet, up.linkno[1])
    }
    
    # recursive call to collect upstream branches along 2nd link
    up.res2 = NULL
    if(!idx.leaf[2])
    {
      up.res2 = my_upstream(root, demnet, up.linkno[2])
    }
    
    # combine results 
    return(c(linkno, up.linkno[!idx.leaf], up.res1, up.res2))
    
  }
  
}

#' finds outlet location given link numbers on either side of a catchment boundary
my_catchment_outlet = function(demnet, subb, linkno1, linkno2, snap=10)
{
  # ARGUMENTS:
  #
  # `demnet`: sf LINESTRING object, the channel network shapefile from TauDEM's StreamNet
  # `subb`: sf MULTIPOLYGON object, subbasins shapefile with attribute 'DN' mapping to demnet$LINKNO
  # `linkno1`: integer, mapping to one of two stream reaches in different catchments   
  # `linkno2`: integer, mapping to the other reach 
  # `snap`: positive numeric (in metres), the maximum snapping distance
  #
  # RETURN VALUE:
  #
  # sfc POINT, the outlet location along the catchment boundary
  # 
  # DETAILS:
  #
  
  # coerce to integer vector
  linkno1 = as.integer(linkno1)
  linkno2 = as.integer(linkno2)
  
  # note if this is an outlet, and relabel to ensure that linkno1 flows into linkno2
  is.outlet = demnet$DSLINKNO[demnet$LINKNO == linkno1] == linkno2
  if(!is.outlet)
  {
    linkno1_old = linkno1
    linkno1 = linkno2
    linkno2 = linkno1_old
  }
  
  
  # find the line segments associated with the two link numbers
  link1 = st_geometry(demnet)[demnet$LINKNO == linkno1]
  link2 = st_geometry(demnet)[demnet$LINKNO == linkno2]
  
  # build the relevant boundary segment
  subb1 = st_make_valid(st_geometry(subb)[subb$DN == linkno1])
  subb2 = st_make_valid(st_geometry(subb)[subb$DN == linkno2])
  line.boundary = st_intersection(subb1, subb2)
  
  # intersect this boundary with the union of the two channel segments
  channel = st_union(link1, link2)
  pt.out = st_cast(st_intersection(channel, line.boundary), 'POINT')
  
  # assign units to snap distance
  snap = set_units(snap, m)
  
  # if this fails to find an intersection, try snapping the channel to the boundary first
  if(length(pt.out) == 0)
  {
    channel = st_snap(channel, line.boundary, tolerance=snap)
    pt.out = st_cast(st_intersection(channel, line.boundary), 'POINT')
    
  }
  
  # look for issue of channel glancing off boundary before/after true crossing
  if(length(pt.out) > 2)
  {
    # this will usually generate a pair of nearby points - compute interpoint distances
    pt.dmat = st_distance(pt.out)
    
    # find all point pairs lying withing snap distance
    idx.remove = rowSums(pt.dmat < snap) > 1
    
    # if all points fit this criterion, keep only the most isolated one
    if(sum(idx.remove) == length(pt.out))
    {
      idx.remove[which.max(rowSums(pt.dmat))] = TRUE 
    }
    
    # trim the output points list
    pt.out = pt.out[!idx.remove]
    
  }
  
  
  # if we still have multiple matches, select the one nearest to where the two channels meet
  if(length(pt.out) > 1)
  {
    # find the point at which both channels meet, and the nearest candidate outlet
    pt.join = st_intersection(link1, link2, tolerance=snap)
    idx.keep = which.min(st_distance(pt.out, pt.join))
    
    # trim the output points list
    pt.out = pt.out[idx.keep]
    
  }
  
  
  # #
  # plot(c(subb1, subb2))
  # plot(line.boundary, lwd=2, add=TRUE)
  # plot(channel, col='lightblue', add=TRUE)
  # plot(link1, add=TRUE, col='blue')
  # #
  
  
  return(pt.out)
  
}

#' given a subset of links in demnet, compute the catchment boundary and its in/outlets
my_delineate_catchment = function(demnet, subb, linkno)
{
  # ARGUMENTS:
  #
  # `demnet`: sf LINESTRING object, the channel network shapefile from TauDEM's StreamNet
  # `subb`: sf MULTIPOLYGON object, subbasins shapefile with attribute 'DN' mapping to demnet$LINKNO
  # `linkno`: integer vector, link numbers in the catchment of interest (mapping to `demnet$LINKNO`) 
  #
  # RETURN VALUE:
  #
  # A list of two geometry objects:
  #
  #   'poly': MULTIPOLYGON, the catchment boundary polygon
  #   'io': sf POINT, inlet/outlet points where channel network crosses catchment boundary
  # 
  # DETAILS:
  #
  # This function dissolves all catchment polygons in `subb` associated with the IDs provided
  # in `linkno`, producing an sfc polygon representing the catchment boundary for that subset of
  # the channel network. It also intersects this boundary with any stream reaches (in `demnet`)
  # that cross it, producing an sf object with the following attributes:
  #
  #   'inlet': boolean indicator, either FALSE (outlet) or TRUE (inlet)
  #   'ilink': integer, the associated (`demnet`) stream reach inside the catchment
  #   'olink1': integer, the associated (`demnet`) stream reach outside the catchment
  #   'olink2': integer (or NA), a possible 2nd associated stream reach outside the catchment
  #
  # Note that we can have two outlinks because it is possible for two stream channels to converge
  # at exactly that point where the network enters the catchment boundary. For most inlets this
  # won't be the case, and 'outlink2' will be NA.
  
  # find all catchment polygons for the channel network on the subset
  idx.subb = subb$DN %in% linkno
  
  # dissolve them into a single catchment boundary (LINESTRING, changed later)
  poly = st_cast(st_make_valid(st_union(st_geometry(subb[idx.subb,]))), 'MULTILINESTRING')
  
  # pull (cropped) demnet for this catchment and copy two components of full `demnet`
  demnet.sfc = st_geometry(demnet)
  dn.link = demnet$LINKNO
  dn.subb = demnet[dn.link %in% linkno, ]
  
  # identify downstream channels leaving catchment (two segments: i=inside, o=outside)
  dslink.unique = unique(dn.subb$DSLINKNO)
  out.olink = dslink.unique[!dslink.unique %in% dn.subb$LINKNO]
  out.ilink = dn.subb$LINKNO[match(out.olink, dn.subb$DSLINKNO)]
  outlet.n = length(out.ilink)
  
  # skip when no outlets found
  if(outlet.n > 0)
  {
    # compile outlets data into dataframe, apply intersection function along rows
    inlet.b = rep(FALSE, outlet.n)
    o.df = data.frame(inlet=inlet.b, ilink=out.ilink, olink1=out.olink, olink2=rep(NA, outlet.n))
    # o.sfc = apply(o.df, 1, function(x) st_intersection(st_union(demnet.sfc[dn.link %in% x[2:3]]),poly))
    o.sfc = apply(o.df, 1, function(x) my_catchment_outlet(demnet, subb, linkno1=x[2], linkno2=x[3]))
    
  } else {
    
    o.df = NULL
    o.sfc = NULL
  }
  
  # upstream channels are more complicated, since we can have (potentially) two outside links
  uslink1.unique = unique(dn.subb$USLINKNO1)
  uslink2.unique = unique(dn.subb$USLINKNO2)
  in.olink1 = uslink1.unique[!uslink1.unique %in% c(dn.subb$LINKNO, -1)]
  in.olink2 = uslink2.unique[!uslink2.unique %in% c(dn.subb$LINKNO, -1)]
  in.ilink1 = dn.subb$LINKNO[match(in.olink1, dn.subb$USLINKNO1)]
  in.ilink2 = dn.subb$LINKNO[match(in.olink2, dn.subb$USLINKNO2)]
  
  # compile into vectors and find unique within-watershed link numbers
  in.olink = c(in.olink1, in.olink2)
  in.ilink = c(in.ilink1, in.ilink2)
  in.ilink.unique = unique(in.ilink)
  inlet.n = length(in.ilink.unique)
  
  # skip when no inlets found
  if(inlet.n > 0)
  {
    # make an outlink data frame with one row per within-watershed link
    olink.list = lapply(in.ilink.unique, function(link) in.olink[link==in.ilink])
    olink.mat = t(sapply(olink.list, function(link) if(length(link)==2) {link} else {c(link, NA)}))
    olink.df = data.frame(olink1=olink.mat[,1], olink2=olink.mat[,2])
    
    # compile inlets data into dataframe, apply intersection function along rows
    i.df = cbind(data.frame(inlet=rep(TRUE, inlet.n), ilink=in.ilink.unique), olink.df)
    i.sfc = apply(i.df, 1, function(x) my_catchment_outlet(demnet, subb, linkno1=x[2], linkno2=x[3]))
    
  } else {
    
    i.df = NULL
    i.sfc = NULL
  }
  
  # plot(poly, col='red')
  # plot(x )
  # plot(i.sfc[[1]], add=TRUE)
  # plot(o.sfc[[1]], add=TRUE)
  # 
  # x = st_intersection(st_geometry(subb[subb$DN %in% o.df[,2],]), st_geometry(subb[subb$DN %in% o.df[,3],]))
  # 
  
  # merge inlets/outlets data with attributes (as sf), recast `poly`, and finish
  io.sf = st_sf(rbind(o.df, i.df), geometry=do.call(c, c(o.sfc, i.sfc)))
  return(list(poly=st_cast(poly,'MULTIPOLYGON'), io=io.sf))
  
}

#' merge catchments according to a minimum area rule, given the output of my_find_catchments
my_merge_catchments = function(boundary, io, pts, demnet, subb, areamin=NULL)
{
  # ARGUMENTS:
  #
  # `boundary`, sf MULTIPOLYONS object, the catchment boundaries (output of `my_find_catchments`)
  # `io`, sf POINTS object, the inlets/outlets (")
  # `pts`, sf POINTS object, the input to `my_find_catchments`
  # `demnet`: sf LINESTRING, the channel network (with 'catchment_id') field
  # `subb`: sf MULTIPOLYGON, subbasins shapefile with attribute 'DN' mapping to demnet$LINKNO
  # `areamin`: numeric (in km^2), the minimum desired catchment size
  #
  # RETURN VALUE:
  #
  # A list containing the four catchment datasets (`boundary`, `io`, `pts`, `demnet`),
  # appropriately modified so that any catchment with area less than `areamin` has been
  # merged with one of its neighbours
  # 
  # DETAILS:
  # 
  # With default NULL `areamin` value, the function returns the four catchment datasets
  # unchanged. Otherwise, it dissolves each of the too-small catchments with a neighbouring
  # catchment, updating all attributes and lists appropriately. 
  #
  # The algorithm iterates over the too-small catchments according to these rules:
  #
  # 1. the smallest of the too-small catchments is selected first
  # 1. upstream neighbours (if they exist) are preferred over the downstream one
  # 2. If there is more than one upstream choice, the one having fewest inlets is preferred
  # 3. In case of ties, the upstream neighbour with smallest area is selected 
  #
  # These rules are intended to both reduce variability in catchment area, and increase
  # the proportion of catchments having no inlets. 
  #
  
  # unpack link numbers from demnet
  idvals = boundary$catchment_id
  linklist = lapply(idvals, function(idval) demnet$LINKNO[demnet$catchment_id %in% idval])
  
  # halt if there are any NAs in this list, as that seems to crash R
  if(anyNA(unlist(linklist)))
  {
    stop('something went wrong mapping link numbers to catchments - check `demnet$LINKNO`')
  }
  
  # set up units for the threshold, and set default if necessary
  areamin = set_units(ifelse(is.null(areamin), 0, areamin), km^2)
  
  # identify catchments below the area threshold
  idx.toosmall = which(boundary$area < areamin)
  n.toosmall = length(idx.toosmall)
  
  # skip if all the catchments are big enough
  if(n.toosmall > 0)
  {
    # rearrange so that catchments with the fewest inlets get processed first
    idx.toosmall = idx.toosmall[order(boundary$n_inlet[idx.toosmall])]
    
    # print a progress message and start iterating
    merging.msg = paste0('with area < ', round(areamin, 2), 'km^2')
    print(paste('> merging', n.toosmall, 'catchments', merging.msg))
    pb = txtProgressBar(min=0, max=n.toosmall, style=3)
    n.todo = n.toosmall
    while(n.toosmall > 0)
    {
      # print progress message, grab data on first catchment from the too-small list
      setTxtProgressBar(pb, n.todo - n.toosmall)
      idx.pop = idx.toosmall[1]
      id.pop = boundary$catchment_id[idx.pop]
      io.pop = io[io$catchment_id==id.pop,]
      
      # identify a neighbouring catchment to merge with
      if(!any(io.pop$inlet))
      {
        # no inlets case: find the downstream catchment via outlet
        idx.outlet = which(!io.pop$inlet)
        outlet.linkno = io.pop$olink1[idx.outlet]
        idx.merge = which(sapply(linklist, function(link) any(outlet.linkno %in% link)))
        id.merge = boundary$catchment_id[idx.merge]
        
      } else {
        
        # inlets case: find the upstream neighbour(s)
        idx.inlet = which(io.pop$inlet)
        inlet.linkno = c(io.pop$olink1[idx.inlet], io.pop$olink2[idx.inlet])
        inlet.linkno = inlet.linkno[!is.na(inlet.linkno)]
        id.upstream = io$catchment_id[io$ilink %in% inlet.linkno]
        
        # count the number of inlets on the upstream neighbour(s) 
        n.up.inlet = boundary$n_inlet[boundary$catchment_id %in% id.upstream]
        if(sum(n.up.inlet == min(n.up.inlet)) == 1)
        {
          # pick the neighbour with fewest inlets, when this choice is unique
          id.merge = id.upstream[which.min(n.up.inlet)]
          
        } else {
          
          # pick the neighbour with fewest inlets AND smallest area
          id.upstream = id.upstream[n.up.inlet == min(n.up.inlet)]
          idx.upstream = which.min(boundary$area[boundary$catchment_id %in% id.upstream])
          id.merge = id.upstream[idx.upstream]
          
        }
        
        # find index for this catchment id in `boundary`
        idx.merge = which(boundary$catchment_id==id.merge)
        
      }
      
      # update input points and channel networks
      pts$catchment_id[pts$catchment_id == id.pop] = id.merge
      linklist[[idx.merge]] = do.call(c, linklist[c(idx.pop, idx.merge)])
      demnet$catchment_id[demnet$catchment_id == id.pop] = id.merge
      
      # recompute catchment geometry, inlets/outlets
      catchment.merge = my_delineate_catchment(demnet, subb, linklist[[idx.merge]])
      catchment.merge$io$catchment_id = id.merge
      io = rbind(io[!io$catchment_id %in% c(id.pop, id.merge), ], catchment.merge$io)
      
      # update boundary polygon and attributes for new merged catchment
      boundary.merge.df = data.frame(catchment_id=id.merge)
      boundary.merge.df$area = st_area(catchment.merge$poly)
      boundary.merge.df$n_inlet = sum(catchment.merge$io$inlet)
      boundary[idx.merge,] = st_sf(boundary.merge.df, geometry=catchment.merge$poly)
      
      # delete the old catchment from the channel network link list and boundaries
      linklist = linklist[-idx.pop]
      boundary = boundary[-idx.pop,]
      
      # update counter and to-do list
      idx.toosmall = which(boundary$area < areamin)
      idx.toosmall = idx.toosmall[order(boundary$n_inlet[idx.toosmall])]
      n.toosmall = length(idx.toosmall)
    }
    setTxtProgressBar(pb, n.todo)
    close(pb)
  }
  
  return(list(boundary=boundary, io=io, pts=pts, demnet=demnet))
}

#' delineate a set of mutually exclusive catchment polygons based on suggested outlet points
my_find_catchments = function(pts, demnet, subb, areamin=NULL, linklist=NULL)
{
  # ARGUMENTS:
  #
  # `pts`: sf or sfc POINT(s), the suggested outlet location(s)
  # `demnet`: sf LINESTRING, the channel network shapefile from TauDEM's StreamNet
  # `subb`: sf MULTIPOLYGON, subbasins shapefile with attribute 'DN' mapping to demnet$LINKNO
  # `linklist`: (internal) list of integer vectors, subsets of `demnet$LINKNO` upstream of `pts`
  # `areamin`: (optional) numeric (in km^2), the minimum desired catchment size
  #
  # RETURN VALUE:
  #
  # For NULL `linklist` (default behaviour), returns a named list of four sf objects:
  #
  #   'boundary', MULTIPOLYGON with one row per catchment, and primary key `catchment_id`
  #   'io', POINTS indicating the inlets and outlets of each catchment
  #   'pts', POINTS, a copy of input `pts` object, plus fields `catchment_id` and `demnet_linkno`
  #   'demnet', LINESTRING, a copy of input `pts` object, plus `catchment_id` field
  # 
  # non-NULL `linklist` is for recursive calls (internal use) - the function returns a named list
  # with entries 'pts', 'linklist', 'poly', 'io'; each is a list with one entry per catchment (eg.
  # the nth catchment has associated data pts[[n]], linklist[[n]], poly[[n]], and io[[n]]):
  #
  #   pts[[n]]: sf POINTS object, the subset of input points mapping to the catchment
  #   linklist[[n]]: integer vector, the upstream channel network as a subset of `demnet$LINKNO` 
  #   poly[[n]]: sfc MULTIPOLYGON, the catchment boundary
  #   io[[n]]: sf POINTS object, the inlets/outlets to the catchment (see `my_delineate_catchment`)
  # 
  # DETAILS:
  # 
  # This is an iterative algorithm for delineating catchments given a set of suggested outlet
  # locations in `pts`, and the output of TauDEM (where `subb` is the polygonized `w` raster).
  # Input points may be gage locations, or any other point of interest in `demnet`. The output is
  # a partitioning of `demnet` into catchments (linked together by inlet/outlet points) that aims
  # to place all suggested points at/near the main outlet of a catchment.
  #
  # For each input point, the algorithm computes the full upstream channel network, from which
  # it identifies catchments that contain no other input points apart from their main outlets (ie
  # leaf nodes). These catchments are recorded, then their channel networks and outlet points are
  # clipped from `demnet` and `pts`. The algorithm then calls itself with the cropped channel
  # network and `pts` list, eventually terminating when every channel upstream of the most
  # downstream location in `pts` has been assigned to a catchment.
  #
  # Note: `pts` locations are first snapped to the channel network, and are merged (ie treated as
  # identical) whenever they snap to the same channel in `demnet`. `areamin` (if supplied) merges
  # any catchments below the threshold size - see `my_merge_catchments` for details.
  # 
  
  
  # this initial step is slow - skip it by supplying precomputed list in `linklist`
  if(is.null(linklist))
  {
    # flag for initial call vs recursive calls
    is.final = TRUE
    
    # snap all `pts` locations to stream reaches in demnet, add link number attribute
    pts.snap = my_demnet_snap(pts, demnet, quiet=TRUE)
    pts['demnet_linkno'] = pts.snap$link
    pts.n = nrow(pts)
    
    # merge any input points that snap to same stream reach
    link.unique = unique(pts.snap$link)
    idx.pts.merge = match(link.unique, pts.snap$link)
    idx.pts.unmerge = match(pts.snap$link, link.unique)
    pts.merged = pts[idx.pts.merge,]
    pts.merged.n = nrow(pts.merged)
    
    # message if points are merged
    if(pts.n != pts.merged.n)
    {
      omit.n = pts.n - pts.merged.n
      omit.msg = paste0(omit.n, ' (of ', pts.n, ')')
      print(paste('> grouping', omit.msg, 'input points with non-unique channel mapping'))
    }
    
    # temporary IDs to unscramble everything later
    pts.merged$catchment_id = 1:pts.merged.n
    pts$catchment_id = pts.merged$catchment_id[idx.pts.unmerge]
    
    
    # find the associated subset of demnet for each `pts` point
    print(paste('> computing upstream channel networks for', pts.merged.n, 'input points'))
    demnet.list = my_upstream(pts.merged, demnet, quiet=TRUE)
    
    
    # copy the link numbers in same list structure
    linklist = lapply(demnet.list, function(demnet.sub) demnet.sub$LINKNO)
    
  } else {
    
    # flag for initial call vs recursive calls
    is.final = FALSE
    
    # if `linklist` is supplied, it is assumed the points are already merged
    pts.merged = pts
    pts.merged.n = nrow(pts.merged)
    
  }
  
  # with only one input point, we simply collect all remaining channels into one catchment
  if(pts.merged.n == 1)
  {
    # dissolve polygons for the channel network into boundary polygon, compute inlets/outlets
    print(paste0('> dissolving final catchment'))
    leaf.geometry = my_delineate_catchment(demnet, subb, linklist[[1]])
    o.poly = leaf.geometry$poly
    o.inlet = leaf.geometry$io
    o.inlet$catchment_id = pts.merged$catchment_id
    
    # storage in lists to conform with more general case below
    leaf.result = list(pts=pts.merged,
                       linklist=linklist,
                       poly=list(o.poly),
                       io=list(o.inlet))
    
  } else {
    
    # inclusion matrix: point j is found upstream of (or at) point i iff element [i,j] is TRUE
    incl.mat = sapply(linklist, function(linkno) pts.merged$demnet_linkno %in% linkno)
    
    # identify "leaves" of the tree, ie input points with no other (unprocessed) points upstream
    pts.isleaf = colSums(incl.mat) == 1
    leaves.n = sum(pts.isleaf)
    pts.msg  = paste0('(', pts.merged.n - leaves.n, ' point(s) remain)')
    print(paste('> dissolving catchments for', leaves.n, 'input points(s)', pts.msg))
    
    
    # build a demnet subset and catchment polygon for each leaf in a loop
    pb = txtProgressBar(min=0, max=leaves.n, style=3)
    o.poly = o.inlet = vector(mode='list', length=leaves.n)
    for(idx.leaf in 1:leaves.n)
    {
      # find the index in `linklist` for this catchment, and the link numbers
      idx.linklist = which(pts.isleaf)[idx.leaf]
      linkno = linklist[[idx.linklist]]
      
      # dissolve polygons for the channel network into boundary polygon, compute inlets/outlets
      leaf.geometry = my_delineate_catchment(demnet, subb, linkno)
      o.poly[[idx.leaf]] = leaf.geometry$poly
      o.inlet[[idx.leaf]] = leaf.geometry$io
      o.inlet[[idx.leaf]]$catchment_id = pts.merged$catchment_id[idx.linklist]
      
      # trim stream networks of all channels whose catchments overlap with current leaf
      idx.overlap = which(incl.mat[idx.linklist,])[which(incl.mat[idx.linklist,]) != idx.linklist]
      for(idx.o in idx.overlap)
      {
        # remove segments of overlap from the rest of the channel network
        idx.totrim = linklist[[idx.o]] %in% linkno
        linklist[[idx.o]] = linklist[[idx.o]][!idx.totrim]
      }
      
      # update progress bar
      setTxtProgressBar(pb, idx.leaf)
      
    }
    close(pb)
    
    # store these finished catchments in a list
    leaf.result = list(pts=pts.merged[pts.isleaf,],
                       linklist=linklist[pts.isleaf],
                       poly=o.poly,
                       io=o.inlet)
    
    # process any remaining branches and append their output
    if(sum(!pts.isleaf)>0)
    {
      # recursive call with inputs trimmed to remove current leaves
      branch.result = my_find_catchments(pts.merged[!pts.isleaf,],
                                         demnet,
                                         subb,
                                         areamin,
                                         linklist[!pts.isleaf])
      
      
      # append result to list from earlier
      leaf.result$pts = rbind(leaf.result$pts, branch.result$pts)
      leaf.result$linklist = c(leaf.result$linklist, branch.result$linklist)
      leaf.result$poly = c(leaf.result$poly, branch.result$poly)
      leaf.result$io = c(leaf.result$io, branch.result$io)
      
    }
  }
  
  # this part only happens at the very end (skipped in recursive calls) 
  if(is.final)
  {
    # determine order of `pts.merged` relative to `leaf.result$pts` 
    id.new = leaf.result$pts$catchment_id
    id.old = pts$catchment_id[idx.pts.merge]
    
    # compile io, sort, count the number of inlets for each catchment 
    io = do.call(rbind, leaf.result$io)
    io = io[order(io$catchment_id),]
    n.inlet = as.vector(by(io, io$catchment_id, function(x) sum(x$inlet)))
    
    # TODO: find the total number of upstream catchments
    
    # combine all polygons into an sfc object and compile attributes as data frame
    boundary.geom = do.call(c, leaf.result$poly) 
    boundary.df = data.frame(catchment_id = id.new,
                             area = st_area(boundary.geom),
                             n_inlet = n.inlet[match(id.new, unique(io$catchment_id))])
    
    # combine boundary polygons as sf object and reorder
    boundary = st_sf(boundary.df, geometry=boundary.geom)
    idx.boundary = order(boundary$catchment_id)
    boundary = boundary[idx.boundary,]
    n.catchment = nrow(boundary)
    
    
    # copy link number list, reordering to match boundaries
    linklist = leaf.result$linklist[idx.boundary]
    
    # append 'catchment_id' values to stream channels in demnet in a loop
    demnet.n = nrow(demnet)
    demnet = cbind(demnet, data.frame(catchment_id=rep(NA, demnet.n)))
    for(idx.catchment in 1:n.catchment)
    {
      idx.demnet = demnet$LINKNO %in% linklist[[idx.catchment]]
      demnet$catchment_id[idx.demnet] = boundary$catchment_id[idx.catchment]
    }
    
    # apply catchment area threshold (if provided) to these results 
    merge.results = my_merge_catchments(boundary, io, pts, demnet, subb, areamin)
    
    # unpack the results of `my_merge_catchments`
    boundary = merge.results$boundary
    demnet = merge.results$demnet
    io = merge.results$io
    pts = merge.results$pts
    n.catchment = nrow(boundary)
    
    # overwrite `catchment_id` with contiguous integers
    id.old = unique(boundary$catchment_id)
    boundary$catchment_id = match(boundary$catchment_id, id.old)
    demnet$catchment_id = match(demnet$catchment_id, id.old)
    io$catchment_id = match(io$catchment_id, id.old)
    pts$catchment_id = match(pts$catchment_id, id.old)
    
    # For USGS style `pts`, attempt to build catchment names from 'station_nm' field
    if(!(is.null(pts$station_nm)|is.null(pts$count_nu)))
    {
      # find the station name corresponding to the gage with the most records
      idvals = pts$catchment_id
      stn.names = as.vector(by(pts, idvals, function(x) x$station_nm[which.max(x$count_nu)]))
      
      # attempt to shorten these a bit
      stn.names.short = gsub(' ', '_', gsub('yellowstone river', 'main', tolower(stn.names)))
      stn.names.short = gsub('_ynp', '', stn.names.short)
      stn.names.short = gsub('_mt', '', stn.names.short)
      stn.names.short = gsub('ranger_station', 'stn', stn.names.short)
      stn.names.short = gsub('creek', 'c', stn.names.short)
      stn.names.short = gsub('cr', 'c', stn.names.short)
      stn.names.short = gsub('near', 'nr', stn.names.short)
      stn.names.short = gsub(',', '', stn.names.short)
      stn.names.short = gsub(',', '', stn.names.short)
      
      # add to boundary sf only when this produces unique names
      if(length(unique(stn.names.short)) == n.catchment)
      {
        boundary$catchment_name = stn.names.short
      }
    }
    
    print(paste('> finished delineating', n.catchment, 'catchments'))
    return(list(boundary=boundary, io=io, pts=pts, demnet=demnet))
  }
  
  # if not final, return results for this branch
  return(leaf.result)
}



#+ include=FALSE
#my_markdown('helper_analysis', 'R/analysis')
