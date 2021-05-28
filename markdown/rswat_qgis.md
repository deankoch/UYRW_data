rswat\_qgis
================
Dean Koch
2021-05-28

**Mitacs UYRW project**

**rswat\_qgis**: R functions for generating a SWAT+ model via QSWAT+,
and reading QSWAT+ files

Construction of a SWAT+ model usually starts in
[QGIS3](https://www.qgis.org/en/site/), where georeferenced data are
assembled and processed by the [QSWAT+
plugin](https://swat.tamu.edu/software/plus/) to generate a set of
shapefiles and tables that can be interpreted by SWAT+Editor. The latter
then creates the SWAT+ text input/output directory and populates it with
configuration files needed by the simulation executable.

Since QSWAT+ uses a point-and-click interface, it is difficult to
automate and document this model construction process. The functions
`qswat_setup` and `qswat_run` help get around this problem, by preparing
input files in a standardized, reproducible way (using outputs of the
`get_data` workflow) and by running QSWAT+/SWAT+Editor in a [PyQGIS
script](https://gitlab.com/rob-yerc/swat) that can be called from R (see
also [SWAT+ Automatic Workflow
(AW)](https://celray.github.io/docs/swatplus_aw/introduction.html) for a
similar, but not R-based. project from Celray James)

The shapefiles produced by QSWAT+ are not strictly necessary for running
SWAT+ simulations (only the text input/output directory is needed for
that), but they are helpful for visualizing spatial aspects of the
model. `qswat_read` and `qswat_plot` are utilities for loading and
viewing these files in R.

## libraries

[`here`](https://cran.r-project.org/web/packages/here/index.html)
simplifies paths involving a project working directory

``` r
library(here)
```

[`raster`](https://rspatial.org/raster/) handles raster data such as
GeoTIFFs

``` r
library(raster)
```

[`sf`](https://r-spatial.github.io/sf/) handles GIS data such as ESRI
shapefiles

``` r
library(sf)
```

[`tmap`](https://github.com/mtennekes/tmap) constructs pretty thematic
map graphics

``` r
library(tmap)
```

[`units`](https://cran.r-project.org/web/packages/units/index.html)
units for numerics

``` r
library(units)
```

[`dplyr`](https://dplyr.tidyverse.org/R) syntax simplification for
complex table operations

``` r
library(dplyr)
```

[`data.table`](https://cran.r-project.org/web/packages/data.table/index.html)
faster table loading

``` r
library(data.table)
```

## dependencies

There should be no R dependencies beyond the libraries listed above,
however note that these functions are written specifically to work with
output from the `get_data` workflow. We plan to work on generalizing
them so that they can be useful outside of the context of the UYRW
project.

Users will need to install the long-term release of
[QGIS3](https://qgis.org/en/site/forusers/download.html) (64 bit) along
with the [SWAT+ installer](https://swat.tamu.edu/software/plus/), which
includes QSWAT+, SWAT+Editor, and the SWAT+ model executable.

One of the challenges with this will be to identify paths needed for
setting up the PyQGIS environment and running the model. The code below
has hard-coded paths that work on my machine with Windows 10, QGIS-LTR,
and the latest SWAT+ releases installed with default settings. However
it may be a challenge to write a package that reliably finds the SWAT+
executable and OSGEO4W root for any given user.

## functions for running QSWAT+ and SWAT+ Editor

assembles required inputs for a QSWAT+ project

``` r
qswat_setup = function(cid, catchments, projdir=NULL, wipe=FALSE, config=NULL, quiet=FALSE)
{
  # `cid`: integer, the 'catchment_id' of the subwatershed in `catchments$boundary`
  # `catchments` list of sf objects, the output of `my_find_catchments(...)`
  # `projdir`: character, the path to the desired QSWAT+ project directory 
  # `wipe`: logical, indicating to erase all existing data in `projdir` 
  # `config`: named list, name-value pairs to add to the configuration JSON
  # `quiet`: logical, suppresses console messages
  #
  # this requires the UYRW_data workflow to be completed to generate the input data files
  # listed in the metadata CSVs loaded by `my_metadata()`.
  #
  # Note that since we are using the stock SSURGO/STATSGO mukeys, there should be no need to
  # import a custom usersoil database
  #
  # TODO: add lakes
  # Some data prep will be required for QSWAT+ to delineate channels correctly around lakes,
  # possibly using the [SWAT2Lake](https://projects.au.dk/wet/software/#c55181) QGIS plugin
  # described [here](https://www.sciencedirect.com/science/article/abs/pii/S1364815218302500).
  # This remains a work in progress.
  #
  # 
  
  # NAs are represented in the GeoTiff by an integer -- usually something large and negative.
  # The default value for this integer when using `raster::writeRaster` is so large that
  # it can cause an integer overflow error in one of the python modules used by QSWAT+ (at 
  # least on my 64bit machine). We instead use the value recommended in the QSWAT+ manual:
  tif.na.val = -32767
  
  # unpack input
  boundary = catchments$boundary[catchments$boundary$catchment_id == cid, ]
  io = catchments$io[catchments$io$catchment_id == cid, ]
  pts = catchments$pts[catchments$pts$catchment_id == cid, ]
  demnet = catchments$demnet[catchments$demnet$catchment_id == cid, ] %>% na.exclude
  
  # TODO: move this to a separate function (to be run in combination with SWAT+ Editor)
  # for now we are testing with the PNWNAMet weather data
  wname = 'pnwnamet_uyrw'
  
  # look up input data file locations
  dem.path = here(my_metadata('get_dem')['swat_dem', 'file'])
  soils.path = here(my_metadata('get_soils')['swat_soils_tif', 'file'])
  landuse.path = here(my_metadata('get_landuse')['swat_landuse_tif', 'file'])
  landuselu.path = here(my_metadata('get_landuse')['swat_landuse_lookup', 'file'])
  wdat.path = here(my_metadata('get_meteo')[wname, 'file'])
  subwatersheds.meta = my_metadata('make_subwatersheds')
  taudem.meta = my_metadata('taudem', data.dir=subwatersheds.meta['taudem', 'file'])
  
  # set default project directory
  if( is.null(projdir) ) 
  {
    # default name from USGS gage site name, default location from parent of dem raster file
    projnm = boundary$catchment_name
    projdir = here(file.path(dirname(dem.path), projnm))
  } 
  
  # project name is always the project directory name
  projnm = basename(projdir)
  
  # handle overwrite calls and create the directory if necessary
  if(wipe & file.exists(projdir)) unlink(projdir, recursive=TRUE)
  my_dir(projdir)
  
  # data inputs go into a subdirectory
  datadir = file.path(projdir, 'inputs')
  my_dir(datadir)
  
  # define the files to write
  {
    files.towrite = list(
      
      # path to QSWAT project folder
      c(name='proj',
        file=projdir, 
        type='directory',
        description='QSWAT+ project directory'),
      
      # watershed boundary polygon (the AOI, a subwatershed from `my_find_catchments`)
      c(name='boundary',
        file=file.path(datadir, 'boundary.geojson'), 
        type='GeoJSON',
        description='polygon delineating subwatershed for the SWAT+ model'),
      
      # DEM raster ('swat_dem' from 'get_dem.R', cropped to AOI)  
      c(name='dem',
        file=file.path(datadir, 'dem_in.tif'), 
        type='GeoTIFF',
        description='QSWAT+ DEM'),
      
      # land use raster for the UYRW ('swat_landuse_tif' from 'get_landuse.R', cropped to AOI)
      c(name='landuse',
        file=file.path(datadir, 'landuse_in.tif'), 
        type='GeoTIFF',
        description='SWAT+ land use classification'),
      
      # soils raster for the UYRW ('swat_tif' from 'get_soils.R', cropped to AOI)
      c(name='soils',
        file=file.path(datadir, 'soil_in.tif'), 
        type='GeoTIFF',
        description='SWAT soils classification, maps to soil table in SWAT+ database'),
      
      # lookup table for 'landuse' ('swat_landuse_lookup' from 'get_landuse.R')
      c(name='landuse_lookup',
        file=file.path(datadir, 'landuse_lookup_in.csv'), 
        type='CSV',
        description='integer code for landuse, maps to `plants_plt` table in SWAT+ database'), 
      
      # outlets shapefile (for now based on 'USGS_sites' from 'get_streamgages.R')
      c(name='outlets',
        file=file.path(datadir, 'outlets_in.shp'), 
        type='ESRI Shapefile',
        description='outlet point locations, used by QSWAT+ to delineate subbasins'),
      
      # streams shapefile (simplified 'flowlines' from 'get_basins.R')
      c(name='streams',
        file=file.path(datadir, 'streams_in.shp'), 
        type='ESRI Shapefile',
        description='stream geometries to "burn" into DEM prior to running TauDEM'),
      
      # directory to write SWAT weather data (input text files)
      c(name='wdat',
        file=file.path(datadir, wname),
        type='directory',
        description='directory for writing SWAT weather input text files'),
      
      # JSON containing metadata and parameters for QSWAT+ workflow in PyQGIS
      c(name='config',
        file=file.path(datadir, paste0(projnm, '.json')),
        type='JSON',
        description='configuration file for run_qswatplus.py module')
      
    )
  }
  
  # write metadata to csv in QSWAT+ project directory
  qswat.meta = my_metadata(projnm, files.towrite, overwrite=TRUE, data.dir=projdir, v=!quiet)
  
  # extract  boundary polygon, write GeoJSON, coerce to `sp` for compatibility with `raster`
  boundary.path = here(qswat.meta['boundary', 'file'])
  if( file.exists(boundary.path) ) unlink(boundary.path)
  st_write(boundary, boundary.path, quiet=quiet)
  boundary.sp = as(boundary, 'Spatial')
  
  # crop/mask the DEM, soils, and land use rasters
  dem = mask(crop(raster(dem.path) , boundary.sp), boundary.sp)
  landuse = mask(crop(raster(landuse.path) , boundary.sp), boundary.sp)
  soils = mask(crop(raster(soils.path) , boundary.sp), boundary.sp)
  
  # write to new location
  writeRaster(dem, here(qswat.meta['dem', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(landuse, here(qswat.meta['landuse', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(soils, here(qswat.meta['soils', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  
  # drop attributes from stream network before writing as shapefile 
  st_write(st_geometry(demnet), here(qswat.meta['streams', 'file']), append=FALSE, quiet=quiet)
  
  # copy outlet point geometries to shapefile, adding required QSWAT+ attributes 
  id.empty = as.integer(0)
  io.df = data.frame(ID=as.integer(1:nrow(io)), INLET=io$inlet, RES=id.empty, PTSOURCE=id.empty)
  st_write(st_sf(io.df, geom=st_geometry(io)), 
           here(qswat.meta['outlets', 'file']), 
           append=FALSE, 
           quiet=quiet)
  
  # open the table, drop redundant rows, copy to QSWAT+ project folder
  landuse.lu = read.csv(landuselu.path) %>% filter( Value %in% unique(landuse) )
  write.csv(landuse.lu, qswat.meta['landuse_lookup', 'file'], row.names=FALSE)
  
  # TODO: this next chunk goes into a different weather builder function
  # delete/create weather directory as needed
  wdat.dir = qswat.meta['wdat', 'file']
  if(dir.exists(wdat.dir)) unlink(wdat.dir, recursive=TRUE)
  my_dir(wdat.dir)
  
  # for now we are testing weather inputs with PNWNAMet data
  wdat = readRDS(wdat.path)
  
  # load weather data and set NA values for humidity (bugfix for SWAT+ Editor)
  wdat$tables$hmd = wdat$tables$tmax
  wdat$tables$hmd[] = NA
  
  # add 5km buffer for grabbing weather grid points
  boundary.buff = st_buffer(boundary, dist=set_units(5, km))
  include = as.vector(st_intersects(wdat$coords_sf, boundary.buff, sparse=FALSE))
  
  # load DEM and call the weather station data export function
  if(!quiet) print('writing weather station data files...')
  invisible(my_swat_wmeteo(wdat, exdir=wdat.dir, form='qswat', include=include, quiet=quiet))
  
  # TODO: change my_metadata() (and all dependencies) to use JSON
  # convert the metadata csv to JSON (adding line breaks for readability)
  qswat.meta.out = cbind(name=rownames(qswat.meta), data.frame(qswat.meta, row.names=NULL))
  
  # TODO: put these defaults in a JSON file in /data
  
  # derive initial (dummy) start/end dates from first 2 days of available weather
  wdat.start = min(wdat$dates)
  
  # set default channel drop threshold from earlier taudem analysis (may be too low)
  drop.channel = as.integer(gsub('stream threshold:', '', taudem.meta['nstream', 'description']))
  
  # set default stream drop threshold to 3X the channel drop
  drop.stream = 3 * drop.channel
  
  # SWAT+ Editor writes contents of TxtInOut, but it can be skipped if you only need geometries
  skip.editor = FALSE

  # define default QSWAT+ parameters
  {
    config.def.list = list(
      
      # the QSWAT+ project name
      c(name='name',
        file=projnm, 
        type='parameter',
        description='QSWAT+ project name'),
      
      # the source of the input weather data 
      c(name='wname',
        file=wname, 
        type='parameter',
        description='initial input weather data for SWAT+ simulation'),
      
      # initial value for start_yr
      c(name='start_yr',
        file=format(wdat.start, '%Y'), 
        type='parameter',
        description='initial value of start year for SWAT+ simulations'),
      
      # initial value for start_day 
      c(name='start_day',
        file=as.integer(format(wdat.start, '%d')), 
        type='parameter',
        description='initial value of start day for SWAT+ simulations'),
      
      # initial value for end_yr
      c(name='end_yr',
        file=format(wdat.start + 1, '%Y'), 
        type='parameter',
        description='initial value of end year for SWAT+ simulations'),
      
      # initial value for end_day 
      c(name='end_day',
        file=as.integer(format(wdat.start + 1, '%d')), 
        type='parameter',
        description='initial value of end day for SWAT+ simulations'),
      
      # channel drop threshold
      c(name='drop_channel',
        file=drop.channel, 
        type='parameter',
        description='threshold for channel delineation (in number of cells)'),
      
      # stream drop threshold
      c(name='drop_stream',
        file=drop.stream, 
        type='parameter',
        description='threshold for stream delineation (in number of cells)'),
      
      # note: this functionality (triggered by landuse 'WATR') seems to be gone in latest SWAT+ rev
      # reservoir threshold percentage
      c(name='res_thresh',
        file=101,
        type='parameter',
        description='threshold for resrevoir delineation (percent)'),
      
      # indicator to skip SWAT+ Editor routine (in initial QSWAT+ run)
      c(name='skip_editor',
        file=skip.editor, 
        type='parameter',
        description='whether to skip SWAT+ Editor call to write config text files')
    )
  }
  

  # overwrite with user supplied settings 
  if( !is.null(config) )
  {
    # config should be a named vector or list containing the new parameters
    for(nm in names(config))
    {
      idx.new = which(sapply(config.def.list, function(x) x['name'] == nm ))
      config.def.list[[idx.new]]['file'] = as.character(config[[nm]])
    }
  } 
  
  # reshape as dataframe
  config.def.out = data.frame(do.call(rbind, config.def.list))
  
  # update the metadata csv
  qswat.meta = my_metadata(projnm, config.def.list, overwrite=TRUE, data.dir=projdir, v=!quiet)
  
  # copy `config` to JSON
  config.path = qswat.meta['config', 'file']
  config.out = rbind(qswat.meta.out, config.def.out)
  writeLines(toJSON(config.out, pretty=TRUE), config.path)
  
  # finish
  return(qswat.meta)
}
```

run the QSWAT+ workflow for a project created by `qswat_setup`. Note
that this calls an external python module (not included in this repo)

``` r
qswat_run = function(qswat, quiet=FALSE)
{
  # `quiet`: logical, suppresses console messages
  # 'qswat': dataframe, the return value of `qswat_setup`
  #
  # DETAILS: alternative input for `qswat`: (character), the path to the JSON config
  # file created by `my_prepare_qswatplus`
  
  # handle dataframe input and extract json config file path
  jsonpath = qswat
  if(is.data.frame(jsonpath)) jsonpath = jsonpath['config', 'file']
  
  # path to the python launcher
  exepath = 'D:/UYRW_data/python/run_qswatplus.cmd'
  
  # call the launcher with this JSON file (runs a python script)
  system2(exepath, normalizePath(jsonpath), stdout=ifelse(quiet, FALSE, ''))
  if(!quiet) cat('\n>> finished')
}
```

write weather input text files for QSWAT and SWAT2012

``` r
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
```

## functions for opening QSWAT+ output files

read the shapefiles from a QSWAT+ project and generate some summary info

``` r
qswat_read = function(qswat)
{
  # In development
  #
  # ARGUMENTS:
  #
  # `qswat`: dataframe, the return value of `qswat_setup`
  #
  # RETURN VALUE:
  #
  # A list containing:
  # 'sta': list of model stats and info
  # 'dem': the input DEM (passed to TauDEM)
  # 'out': the input outlets (passed to TauDEM)
  # 'cha', 'hru', 'lsu', 'sub': watershed geometries generated by QSWAT+ 
  #
  # DETAILS:
  #
  # The input dataframe can also be specified by passing a character string in `qswat`
  # specifying the absolute path to the metadata CSV
  #
  
  # handle character string input
  #if( is.character(qswat) ) 
  
  # identify the shapefiles directory where we can find the HRU and LSU geometries 
  shpdir = file.path(qswat['proj', 'file'], 'Watershed/Shapes')
  tifdir = file.path(qswat['proj', 'file'], 'Watershed/Rasters')
  
  # load dem, outlets, channels and subbasins shapefiles
  dem = raster(qswat['dem', 'file'])
  out = read_sf(file.path(shpdir, 'outlets_in.shp'))
  riv = read_sf(file.path(shpdir, 'rivs1.shp'))
  subb = read_sf(file.path(shpdir, 'subs1.shp'))
  
  # 'hrus1.shp' appears to be an early iteration, before merging by dominant HRU
  hru2 = read_sf(file.path(shpdir, 'hrus2.shp'))
  
  # 'lsu1.shp' appears to be the same as 'lsus2.shp', but with fewer attributes
  lsu2 = read_sf(file.path(shpdir, 'lsus2.shp'))
  
  # merge LSU and HRU attribute data (they match because we picked "Dominant HRU" method)
  hru.df = inner_join(as.data.frame(st_drop_geometry(lsu2)), 
                      as.data.frame(st_drop_geometry(hru2)),
                      by = c('Subbasin', 'Channel', 'Landscape', 'X.Subbasin', 'Area'))
  
  # build an sf object from the merged dataset
  hru = st_sf(hru.df, geometry=lsu2$geometry)
  
  # these HRU-specific centroids are to replace the subbasin-level lat/long coordinates 
  hru.centroids = st_centroid(st_geometry(hru))
  hru.coords = st_coordinates(st_transform(hru.centroids, crs=4326))
  
  # add them to the dataframe along with DEM values and areas, remove some detritus
  hru.out = hru %>%
    mutate( long = set_units(hru.coords[, 1], degrees) ) %>%
    mutate( lat = set_units(hru.coords[, 2], degrees) ) %>%
    mutate( elev = set_units(extract(dem, st_sf(hru.centroids)), m) ) %>%
    mutate( area = st_area(st_geometry(hru)) ) %>%
    mutate( id = HRUS, frac = X.Subbasin ) %>%
    select( -c(Area, Lat, Lon, Elev, X.Landscape, X.Subbasin, HRUS) ) %>%
    select( id, Channel, LINKNO, everything() )
  
  # TODO: option to take a random sample of points from each HRU to get elevation medians
  # st_sample(st_geometry(hru), rep(10, nrow(hru)))
  
  # create watershed boundary polygon
  bou = st_union(subb)

  # extract the drop thresholds and compute area of one cell
  tcha = as.integer(qswat['drop_channel', 'file'])
  tsub = as.integer(qswat['drop_stream', 'file'])
  acell = set_units(prod(res(dem)), m^2)
  
  # bundle metadata output into a list
  stats = list(
    
    # project name and important files
    name = qswat['name', 'file'],
    
    # counts of subbasins, channels, hrus
    counts = c(nsub = nrow(subb),
               ncha = nrow(riv),
               nhru = nrow(hru)),

    # watershed total area
    area = set_units(st_area(st_union(subb)), km^2),
    elevation = c(min = set_units(cellStats(dem, min), m), 
                  median = set_units(cellStats(dem, median), m), 
                  max = set_units(cellStats(dem, max), m)),
    

    
    # copy the channels and subbasins (ie streams) thresholds, and express as areas
    threshold_area = c(cha=set_units(tcha * acell, km^2), sub=set_units(tsub * acell, km^2)),
    threshold_ncell = c(cha=as.integer(tcha), sub=as.integer(tsub)),
    
    # some useful file/directory paths
    paths = c(sql = file.path(qswat['proj', 'file'], 'swatplus_datasets.sqlite'),
              txtio = file.path(qswat['proj', 'file'], 'Scenarios/Default/TxtInOut'))
  )

  # return everything in a list
  return(list(sta=stats, dem=dem, out=out, cha=riv, hru=hru.out, lsu=lsu2, sub=subb, bou=bou))
  
}

# tmap-based plotting of various GIS components of a QSWAT+ project
qswat_plot = function(dat, r=NULL, titles=NULL, pal=NULL, style='cont', breaks=NULL, 
                      spos=NULL, legwd=0.2, addto=NULL)
{
  # 'dat': list, the qswat+ project data (output of qswat_read)
  # 'r': Raster, to plot in background (default is DEM)
  # 'titles': list of character vectors, with names 'main', 'sub', and 'legend'
  # 'pal': character vector, a colour palette to use for the background raster
  # 'style': character, raster plot style (see tmap::tm_raster)
  # 'breaks': list (or dataframe) supplying custom breakpoints and labels (see details)
  # 'spos': character vector, the scalebar position (passed to tmap::tm_layout)
  # 'legwd': numeric, fraction of plot width to use for legend 
  # 'addto': tmap object, an existing map to append to
  #
  # DETAILS:
  #
  # The defaults for 'titles$main' and `titles$sub` are, respectively, 'dat$stats$name'
  # (the project name) and three lines of count info (area, HRUs, subbasins) about the model,
  # extracted from `dat$stats` and `dat$bou`. The default for `titles$legend` is "elevation (m)",
  # unless `r` is supplied, in which case it is set to `names(r)`. Any user-supplied arguments
  # to `titles` will override these defaults.
  #
  # The 'titles' text is always written outside the plot frame on the right margin, with only
  # the main title bolded. Subtitles are separated from the legend title (below) with a
  # horizontal rule. If 'main' and 'sub' contain multiple entries, they are collapsed with the
  # separator '\n', so that each entry is printed on a separate line. Text size is scaled as
  # needed to fit the legend width (adjusted by `legwd`).
  #
  # A (km level) scalebar is automatically added with breaks spanning 1/4 of the plot frame
  # (east-west). If `spos` is missing, it is assigned automatically to the least crowded corner
  # apart from top-right (which already cluttered with legend items).
  #
  # See `?hcl.colors` for a wide range of good options for setting 'pal' 
  #
  # typical styles are 'cont' for continuous (the default) and 'pretty' for categorical.
  # Breakpoints are determined by ... when `breaks` is a dataframe, its non-character column
  # is interpreted as a lookup key for the (numerical) raster data, and the function automatically
  # sets the appropriate breakpoints
  #
  #
  
  # unpack `dat`
  stats = dat[['sta']]
  dem = dat[['dem']]
  out = dat[['out']]
  riv = dat[['cha']]
  subb = dat[['sub']]
  hru = dat[['hru']]
  lsu = dat[['lsu']]
  bou = dat[['bou']]
  
  # unpack `titles`
  msg.main = titles[['main']]
  msg.sub = titles[['sub']]
  msg.legend = titles[['legend']]
  
  # set default main title
  if( is.null(msg.main) )
  {
    # format in decimal degrees
    coords = round(abs(colMeans(st_drop_geometry(subb)[, c('Lat', 'Lon')])), 3)
    msg.coords = paste( paste0( paste0(coords, '\u00B0'), c('N', 'W') ), collapse=' ')
    
    # DMS alternative
    # coords = colMeans(st_drop_geometry(subb)[, c('Lat', 'Lon')])
    # NS.dms = gsub('d', '°', as.character(dd2dms(coords['Lat'], NS=TRUE)))
    # EW.dms = gsub('d', '°', as.character(dd2dms(coords['Lon'], NS=FALSE)))
    # msg.coords = paste0('(', NS.dms, ' ', EW.dms, ')')
    
    msg.main = paste0(gsub('_', ' ', stats$name), ' (', msg.coords, ')')
  }
  
  # collapse vectors as needed
  if( length(msg.main) > 1 ) msg.main = paste(msg.main, collapse='\n')
  
  # generate default subtitle as needed
  if( is.null(msg.sub) )
  {
    # computed from 'bou' (boundary geometry) and 'stats' in `dat`
    msg.sub = c(paste(round(set_units(stats$area, km^2), 1), 'square km'),
                paste(stats$counts['ncha'], 'channels'),
                paste(stats$counts['nsub'], 'subbasins')) 
  }

  # set default legend title
  if( is.null(msg.legend) )
  {
    # based on Raster name attribute
    msg.legend = paste(names(r), 'codes')
    
    # when using default `r` (DEM), add a better label
    if( is.null(r) ) msg.legend = 'elevation (m)'
  }

  # add horizontal rule above legend title and collapse subtitles as needed
  hrule = paste(rep('_', nchar(msg.legend)), collapse='')
  msg.legend = paste(c(hrule, msg.legend), collapse='\n')
  if( length(msg.sub) > 1 ) msg.sub = paste(msg.sub, collapse='\n')

  # default raster is DEM (override with `r=NA` to omit rasters)
  if( is.null(r) ) r = dem
  
  # determine scale bar positioning
  if( is.null(spos) )
  {
    # use quadrant intersection with boundary polygon to order the corners by crowdedness
    spacious.idx = which.min(st_area(st_intersection(st_make_grid(bou, n=c(2,2)), bou))[-4])
    
    # list of tm_layout arguments in matching order
    spacious = list(c('LEFT', 'BOTTOM'), 
                    c('RIGHT', 'BOTTOM'), 
                    c('LEFT', 'TOP'))

    # make the replacements
    if(is.null(spos)) spos = spacious[[ spacious.idx ]]
  }
  
  # default palette is like an improved rainbow()
  if( is.null(pal) ) pal = hcl.colors(1e3, palette='Dark 3')
  
  # set up legend breaks and labels as needed
  is.lookup = FALSE
  leg.labels = leg.breaks = NULL
  if( !is.null(breaks) )
  {
    # interpret dataframe input as lookup table
    if( is.data.frame(breaks) )
    {
      # force fixed style breakpoints
      is.lookup = TRUE
      
      # split into list
      breaks.df = breaks
      breaks = as.list(breaks.df)
    }
    
    # identify label/values entries
    breaks.cl = sapply(breaks, class)
    idx.label = breaks.cl == 'character'
    
    # breakpoints are converted to character when no labels supplied
    leg.breaks.in = breaks[[ which(breaks.cl != 'character') ]]
    if( sum(idx.label) > 0 ) leg.labels = breaks[[ which(idx.label) ]]
    #leg.labels = as.character(round(leg.breaks.in, 3))
    
    # for categorical plots convert midpoints to interval endpoints
    style.cont = style %in% c('cont', 'order', 'log10')
    if( ( length(leg.labels) == length(leg.breaks.in) ) & is.lookup )
    {
      # I think 
      style = 'fixed'
      bmin = min(leg.breaks.in)
      leg.breaks = c(bmin, leg.breaks.in + c(diff(leg.breaks.in)/2, 1))
      
    } else {
      
      # otherwise we leave them unchanged
      leg.breaks = leg.breaks.in
    }
  }
  
  # define breaks for the scale bar
  km.breaks = pretty(1e-3 * diff(st_bbox(bou)[c('xmin', 'xmax')]) / 4, 2, min.n=2)
  if( km.breaks[1] != 0 ) km.breaks = c(0, km.breaks)
  
  # initialize tmap object: for debugging, try design mode with tmap_design_mode(T)
  tmap.out = addto + tm_shape(bou) + tm_polygons(alpha=0) + tm_add_legend('title', title=msg.sub)
  
  # add raster layer (skipping on non-rasterLayer input)
  if( class(r) == 'RasterLayer' ) 
  {
    # pipe to the one already initialized
    tmap.out = tmap.out + 
      tm_shape(r) +
      tm_raster(legend.reverse=TRUE, 
                palette=pal, 
                style=style, 
                title=msg.legend,
                breaks=leg.breaks,
                labels=leg.labels)
  }
  
  # add geometries and aesthetics
  tmap.out = tmap.out +
    tm_shape(hru) + tm_polygons(alpha=0, border.col=adjustcolor('white', alpha=0.25), lwd=1) +
    tm_shape(subb) + tm_polygons(alpha=0, border.col='black') +
    tm_shape(riv) + tm_lines(alpha=0.1, lwd='Order', scale=5, legend.lwd.show=FALSE) +
    tm_shape(out) + tm_dots(size=0.5) + tm_shape(out) + tm_dots(size=0.2, col='grey50') +
    tm_scale_bar(breaks=km.breaks, position=spos, text.size=0.8) + 
    tm_grid(n.x=2, n.y=2, projection=4326, alpha=0.2) +
    tm_layout(main.title = msg.main,
              main.title.fontface ='bold',
              main.title.size = 1,
              main.title.position = 'right',
              legend.outside = TRUE,
              legend.outside.position = c('right', 'top'),
              legend.outside.size = legwd,
              legend.text.size = 0.8,
              inner.margins = rep(1e-2, 4),
              frame = FALSE)

  # finish
  return(tmap.out)
}
```
