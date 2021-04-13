helper\_get\_data.R
================
Dean Koch
2021-04-13

**Mitacs UYRW project**

**helper\_get\_data**: general helper functions for scripts in
/R/get\_data

This function is used in `get_weatherstations` to parse the time series
dates

``` r
# convert start/end year columns from GHCN data to a single (string) column for each "element"
my_ghcnd_reshape = function(idval, elemval)
{
  # query this combination of station id and element string in the full GHCND table
  idx.row = (ghcnd.df$id == idval) & (ghcnd.df$element == elemval)
  
  # if it exists, return a string of form "start-end", otherwise NA
  return(ifelse(!any(idx.row), NA, paste(ghcnd.df[idx.row, c('first_year', 'last_year')], collapse='-')))
}
```

This function is used by `get_meteo` to extract data from Ben Livneh’s
meteorological reconstruction dataset (NetCDF format), returning R data
frames and `raster`/`sf` objects

``` r
my_livneh_reader = function(nc, perim, buff=0)
{
  # Opens a Livneh meteorological data file and returns a list of R objects
  #
  # ARGUMENTS:
  #
  # `nc`, character vector path to the input NetCDF file (.nc or .bz2, see BEHAVIOUR below)
  # `perim`, a projected sf geometry delineating the perimeter of the area to fetch data
  # `buff`, numeric (metres) distance to extend the perimeter outwards
  #
  # RETURN VALUE:
  #
  # a named list of two lists:
  #
  # `tab`, a named list of five data frames:
  #   `coords`, the geographical coordinates (lat/long) of each grid point imported; and
  #   `pcp`, `tmax`, `tmin`, `wnd`; the daily values for each variable
  #
  # `spat`, a named list of five R objects
  #   `pts`, an sf object locating each grid point (in same projection as `perim`)
  #   `pcp`, `tmax`, `tmin`, `wnd`; rasterstacks with the daily values of each variable
  #
  # BEHAVIOUR: 
  #
  # If the input file path extension is 'bz2', the data are imported by decompressing to
  # a temporary file, which gets deleted at the end (`raster` does not seem to support
  # on-the-fly decompression)
  #
  # Argument `buff` allows inclusion of grid points lying adjacent (and near) the watershed. 
  #
  # Rows of the daily variables tables correspond to days of the month (1st row = 1st day,
  # etc). Each grid location gets its own column, with coordinates stored as rows in the
  # `coords` table (eg the nth row of `coords` gives the coordinates for the precipitation
  # time series in the nth column of `tab[['prec']]`.
  
  # handle bzipped input files via temporary decompressed copy
  is.compressed = substr(nc, nchar(nc)-3, nchar(nc)) == '.bz2'
  if(is.compressed)
  {
    nc.tempfile = tempfile(fileext='.nc')
    print(paste('decompressing', basename(nc), 'to temporary file,', basename(nc.tempfile)))
    nc.raw = memDecompress(readBin(nc, raw(), file.info(nc)$size))
    writeBin(nc.raw, nc.tempfile)
    nc = nc.tempfile
  }
  
  # define the variable names to extract
  livneh.vars = c(pcp='Prec', tmax='Tmax', tmin='Tmin', wnd='wind')
  
  # open NetCDF as raster (1st band, 1st variable) to extract grid data
  nc.r = raster(nc, varname=livneh.vars[1], band=1)
  
  # mask/crop to `perim` after applying `buff` metres of padding
  perim.t = as(st_transform(st_buffer(perim, buff), crs=st_crs(nc.r)), 'Spatial')
  nc.r.clip = mask(crop(nc.r, perim.t), perim.t)
  idx.na = values(is.na(nc.r.clip))
  
  # build the coordinates table
  coords.tab = data.frame(coordinates(nc.r.clip)) %>% filter(!idx.na)
  n.spatpts = nrow(coords.tab)
  colnames(coords.tab) = c('long', 'lat')
  rownames(coords.tab) = paste0('grid', 1:n.spatpts)
  
  # build the output `pts` sf object
  geopts.sf = st_as_sf(coords.tab, coords=colnames(coords.tab), crs=st_crs(nc.r))
  pts.sf = st_transform(cbind(geopts.sf, data.frame(name=rownames(coords.tab))), st_crs(perim))
  
  # initialize the other output tables
  n.days = nbands(nc.r)
  template.df = data.frame(matrix(NA, n.days, n.spatpts))
  colnames(template.df) = rownames(coords.tab)
  climdata.list = lapply(livneh.vars, function(x) template.df)
  
  # initialize rasterstack list
  rbrick.list = vector(mode='list', length=length(livneh.vars))
  names(rbrick.list) = names(livneh.vars)
  
  # loop to fill these tables and copy rasterstacks
  for(varname in names(livneh.vars))
  {
    rbrick.list[[varname]] = mask(crop(stack(nc, varname=livneh.vars[varname]), perim.t), perim.t)
    climdata.list[[varname]][] = t(values(rbrick.list[[varname]])[!idx.na, ])
  }
  
  # bundle everything into a list and finish, tidying up tempfiles as needed
  if(is.compressed) {unlink(nc.tempfile)}
  return(list(tab=c(list(coords=coords.tab), climdata.list), spat=c(list(pts=pts.sf), rbrick.list)))
  
}
```

This function is used by `get_meteo` to extract data from the ORNL
Daymet meteorological reconstruction dataset (NetCDF format), returning
R data frames and `raster`/`sf` objects It is very similar to
`my_livneh_reader`, except that the Daymet files are not bzipped, and
different meteorological variables are stored in different files.

In the current CRAN version of the `daymetr` package (v1.4), there is a
bug related to a mislabeling of projection information by the web
coverage service (WCS) at ORNL’s DAAC. Read about it in the bug reports
[here](https://github.com/bluegreen-labs/daymetr/issues/40),
[here](https://github.com/ropensci/FedData/issues/50), and
[here](https://github.com/bluegreen-labs/daymetr/issues/36)).

We fix this by manually rewriting the proj4 string after it is loaded
into R. Unfortunately this results in many warnings about invalid CRS
definitions (these can be safely ignored)

``` r
my_daymet_reader = function(nc, perim, buff=0)
{
  # Opens a (set of) Daymet meteorological data file(s) and returns a list of R objects
  #
  # ARGUMENTS:
  #
  # `nc`, named character vector of path(s) to the input NetCDF file(s). See below
  # `perim`, a projected sf geometry delineating the perimeter of the area to fetch data
  # `buff`, numeric (metres) distance to extend the perimeter outwards
  #
  # RETURN VALUE:
  #
  # a named list of two lists, whose lengths depend on the number input NetCDF files (`nc`).
  # A function typical call will provide the paths to the `prcp`, `tmin`, `tmax`, and `srad`
  # files (named using these strings, or similar). 
  #
  # `tab`, a named list of `length(nc)+1` data frames:
  #   `coords`, the geographical coordinates (lat/long) of each grid point imported; and
  #   named entries for each file in `nc`, containing the table of daily values for that
  #   variable.
  #
  # `spat`, a named list of `length(nc)+1` R objects
  #   `pts`, an sf object locating each grid point (in same projection as `perim`); and
  #   named entries for each file in `nc`, containing rasterstacks for each variable
  #
  # BEHAVIOUR: 
  #
  # Argument `buff` allows inclusion of grid points lying adjacent (and near) the watershed. 
  #
  # Rows of the daily variables tables correspond to days of the month (1st row = 1st day,
  # etc). Each grid location gets its own column, with coordinates stored as rows in the
  # `coords` table (eg if `nc` contains the entry `prcp` giving the path to a precipitation
  # data file, the nth row of `coords` gives the coordinates for the precipitation time series
  # in the nth column of `tab[['prcp']]`.
  #
  # The names of the data entries of output lists `tab` and `spat` are copied from the names
  # provided for the `nc` input. If this argument is unnamed, the function assigns them based
  # on the filenames provided.
  
  # warn if the `nc` paths are not named and auto-generate
  if(any(is.null(names(nc))))
  {
    in.vars = substr(basename(nc), 1, 4)
    print(paste('Warning: no name(s) supplied for', paste(basename(nc), sep=', ')))
    print(paste('using name(s)', paste(in.vars, sep=', ')))
    names(nc) = in.vars
    
  } else {
    
    in.vars = names(nc)
    
  }
  
  # open NetCDF as raster (1st band) to extract grid data
  nc.r = raster(nc[[1]], band=1)
  
  # overwrite proj4 string with corrected version (in units of 'km', instead of 'm')
  daymet.proj4 = '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +datum=WGS84 +no_defs'
  projection(nc.r) = daymet.proj4
  
  # mask/crop to `perim` after applying `buff` metres of padding
  perim.t = as(st_transform(st_buffer(perim, buff), crs=st_crs(nc.r)), 'Spatial')
  nc.r.clip = mask(crop(nc.r, perim.t), perim.t)
  idx.na = values(is.na(nc.r.clip))
  
  
  # build the output `pts` sf object
  coords.lcc.tab = data.frame(coordinates(nc.r.clip)) %>% filter(!idx.na)
  n.spatpts = nrow(coords.lcc.tab)
  colnames(coords.lcc.tab) = c('long', 'lat')
  rownames(coords.lcc.tab) = paste0('grid', 1:n.spatpts)
  geopts.sf = st_as_sf(coords.lcc.tab, coords=colnames(coords.lcc.tab), crs=st_crs(nc.r))
  pts.sf = st_transform(cbind(geopts.sf, data.frame(name=rownames(coords.lcc.tab))), st_crs(perim))
  
  # build the lat/long coordinates table
  coords.tab = st_coordinates(st_transform(geopts.sf, crs=4326))
  rownames(coords.tab) = rownames(coords.lcc.tab)
  
  # initialize the other output tables
  n.days = nbands(nc.r)
  template.df = data.frame(matrix(NA, n.days, n.spatpts))
  colnames(template.df) = rownames(coords.tab)
  climdata.list = lapply(nc, function(x) template.df)
  
  # initialize rasterstack list
  rbrick.list = vector(mode='list', length=length(nc))
  names(rbrick.list) = in.vars
  
  # loop to fill these tables and copy rasterstacks
  for(varname in in.vars)
  {
    # load and fix projection CRS
    nc.in = stack(nc[varname])
    projection(nc.in) = daymet.proj4
    rbrick.list[[varname]] = mask(crop(nc.in, perim.t), perim.t)
    climdata.list[[varname]][] = t(values(rbrick.list[[varname]])[!idx.na, ])
  }
  
  # bundle everything into a list and finish
  return(list(tab=c(list(coords=coords.tab), climdata.list), spat=c(list(pts=pts.sf), rbrick.list)))
  
}
```

get a listing of stations with records available through NRCS National
Water Climate Center

``` r
my_nwcc_list = function(csvpath=NULL)
{
  # ARGUMENTS:
  #
  # `csvpath`: (optional) character, the full path to the metadata file to write
  #
  # RETURN VALUE:
  #
  # dataframe with a row for each site
  #
  # DETAILS:
  #
  # Using `rvest`, builds a table of info on sites (including discontinued ones)
  # where records are available through NWCC, and returns their metadata as data.frame().
  # If `csvpath` is supplied, the dataframe is written to disk as a CSV file.
  #
  # based on snotelr::snotel_info
  
  # the URL to fetch from and a list of networks to query
  base.url = 'https://wcc.sc.egov.usda.gov'
  networks = c('scan',   # NRCS Soil Climate Analysis Network
               'snow',   # NRCS Snow Course Sites
               'coop',   # National Weather Service COOP stations
               'bor',    # reservoir stations, including all from Bureau of Reclamation 
               'sntl',   # NWCC SNOTEL and SCAN stations
               'msnt',   # Manual SNOTEL non-telemetered, non-real-time sites
               'usgs',   # streamflow stations, including all from USGS
               'mprc',   # Manual precipitation sites
               'sntlt',  # telemetered aerial markers with sensors (Snolite) 
               'clmind', # climate indices, such as Southern Oscillation or Trans-Nino
               'cscan',  # cooperative Soil Climate Analysis Network
               'nrcsxp', # ??
               'other')
  
  # build a query for each network
  query.pt1 = '/nwcc/yearcount?'
  query.pt2 = paste0('network=', networks)
  query.pt3 = '&counttype=listwithdiscontinued&state='
  query.url = paste0(base.url, query.pt1, query.pt2, query.pt3)
  query.n = length(query.url)
  
  # fetch the html using `rvest` in a loop, storing in list
  data.list = setNames(vector(mode='list', length=query.n), networks) 
  pb = txtProgressBar(max=query.n, style=3)
  for(idx.net in 1:query.n)
  {
    # download the html
    data.html = read_html(query.url[idx.net])
    setTxtProgressBar(pb, idx.net)
    
    # parse the table as dataframe and add to storage
    data.df = data.html %>% html_nodes(xpath="//table[3]") %>% html_table() %>% '[['(1)
    data.list[[idx.net]] = data.df
    
  }
  close(pb) 
  
  # bind all the tables together
  data.df = do.call(rbind, data.list)
  data.n = nrow(data.df)
  
  # clean up 'ts', parse sites, hucs, dates, interpret length-0 and 'unknown' as NA 
  data.df = data.df %>% 
    mutate(ts = gsub('\\(|\\)', '', ts)) %>%
    mutate(site_id = gsub('.+\\(|\\)', '', site_name)) %>%  
    mutate(site_nm = gsub('\\(.+', '', site_name)) %>%
    mutate(huc_id = gsub('.+\\(|\\)', '', huc)) %>%
    mutate(huc_nm = gsub('\\(.+', '', huc)) %>%
    mutate(huc_nm = gsub('^[0-9]+', '', huc_nm)) %>%
    mutate(huc_nm = gsub('^-', '', huc_nm)) %>%
    mutate(start_mo = match(tolower(gsub('.+-', '', start)), tolower(month.name))) %>%
    mutate(start_yr = as.integer(gsub('-.+', '', start))) %>%
    mutate(end_mo = match(tolower(gsub('.+-', '', enddate)), tolower(month.name))) %>%
    mutate(end_yr = as.integer(gsub('-.+', '', enddate))) %>%
    mutate(dc = end_yr!=2100) %>%
    mutate(end_yr = ifelse(end_yr==2100, format(Sys.Date(),'%Y'), end_yr)) %>%
    na_if('') %>% na_if('unknown') %>% na_if('unknown ') %>%
    select(-c(huc, start, enddate, wyear, site_name))
  
  # ft -> m conversion
  data.df$elev = set_units(set_units(data.df$elev, ft), m)
  
  # write as CSV if requested
  if(!is.null(csvpath)) write.csv(data.df, csvpath, row.names=FALSE)
  
  # finished
  return(data.df)
  
}
```

download datasets as CSV from the NRCS National Water Climate Center

``` r
my_nwcc_get = function(varname=NULL, sites=NULL, savedir=NULL, reuse=TRUE, retry=0)
{
  # ARGUMENTS:
  #
  # `varname`: character vector, the variable names to request from NWCC
  # `sites`: data frame, containing columns `site_id`, `state`, `ntwk`, (see `my_nwcc_list`) 
  # `savedir`: character vector, path to save downloaded CSV files
  # `reuse`: boolean, indicating to skip downloading any files found in `savedir`
  # `retry`: positive integer, indicating number of times to retry failed downloads 
  #
  # RETURN VALUE:
  # 
  # If `varname` is NULL, returns a named vector of descriptions of the variables that can
  # be requested using the NWCC website. `sites` and `savedir` are ignored in this case
  #
  # If `varname` is supplied, downloads the data files and returns a dataframe containing
  # their paths, source urls, and some metadata 
  #
  # DETAILS:
  #
  # Entries of `sites` are turned into data requests for NWCC (for all variable names in
  # `varname`) and downloaded in a loop, with downloads (as CSV) going in the direcory
  # `savedir`. `sites` should be a subset of rows from the dataframe returned by
  # `my_nwcc_list`.
  #
  # Destination filepaths for the downloads have the form 'savedir/site_<x>_<y>', where <x>
  # is the 'site_id' string for the site and <y> is one of 'daily', 'semimonthly', or
  # `semimonthly`. When `reuse==TRUE`, these files (if they exist already) will be preserved,
  # and the download skipped.
  # 
  # based on snotelr::snotel_download
  # See also: https://github.com/NCAR/rwrfhydro/blob/master/R/snotel_get.R
  #
  
  # define the base URL to fetch from, and a list of variables allowed in requests
  base.url = 'https://wcc.sc.egov.usda.gov'
  allvars = c(TMAX = 'air temperature maximum',
              TMIN = 'air temperature minimum',
              TOBS = 'air temperature observed',
              PREC = 'precipitation accumulation',
              PRCP = 'precipitation increment',
              RESC = 'reservoir storage volume',
              SNWD = 'snow depth',
              WTEQ = 'snow water equivalent',
              SRVO = 'stream volume, adjusted',
              PRES = 'barometric pressure',
              BATT = 'battery',
              BATV = 'battery average',
              BATX = 'battery maximum',
              BATN = 'battery minimum',
              ETIB = 'battery-eti precip guage',
              COND = 'conductivity',
              DPTP = 'dew point temperature',
              DIAG = 'diagnostics',
              SRDOX = 'discharge manual/external adjusted mean',
              DISO = 'dissolved oxygen',
              DISP = 'dissolved oxygen - percent saturation',
              DIVD = 'diversion discharge observed mean',
              DIV = 'diversion flow volume observed',
              HFTV = 'energy gain or loss from ground',
              EVAP = 'evaporation',
              FUEL = 'fuel moisture',
              FMTMP = 'fuel temperature internal',
              VOLT = 'generic voltage',
              TGSV = 'ground surface interface temperature average',
              TGSX = 'ground surface interface temperature maximum',
              TGSN = 'ground surface interface temperature minimum',
              TGSI = 'ground surface interface temperature observed',
              JDAY = 'julian date',
              MXPN = 'maximum',
              MNPN = 'minimum',
              NTRDV = 'net solar radiation average',
              NTRDX = 'net solar radiation maximum',
              NTRDN = 'net solar radiation minimum',
              NTRDC = 'net solar radiation observed',
              H2OPH = 'ph',
              PARV = 'photosynthetically active radiation (par) average',
              PART = 'photosynthetically active radiation (par) total',
              PRCPSA = 'precipitation increment - snow-adj',
              ETIL = 'pulse line monitor-eti guage',
              RDC = 'real dielectric constant',
              RHUM = 'relative humidity',
              RHUMV = 'relative humidity average',
              RHENC = 'relative humidity enclosure',
              RHUMX = 'relative humidity maximum',
              RHUMN = 'relative humidity minimum',
              REST = 'reservoir stage',
              SRDOO = 'river discharge observed mean',
              RVST = 'river stage level',
              SAL = 'salinity',
              SNWDV = 'snow depth average',
              SNWDX = 'snow depth maximum',
              SNWDN = 'snow depth minimum',
              WTEQV = 'snow water equivalent average',
              WTEQX = 'snow water equivalent maximum',
              WTEQN = 'snow water equivalent minimum',
              SMOV = 'soil moisture bars average',
              SMOC = 'soil moisture bars current',
              SMOX = 'soil moisture bars maximum',
              SMON = 'soil moisture bars minimum',
              SMS = 'soil moisture percent',
              SMV = 'soil moisture percent average',
              SMX = 'soil moisture percent maximum',
              SMN = 'soil moisture percent minimum',
              STV = 'soil temperature average',
              STX = 'soil temperature maximum',
              STN = 'soil temperature minimum',
              STO = 'soil temperature observed',
              SRAD = 'solar radiation',
              SRADV = 'solar radiation average',
              SRADX = 'solar radiation maximum',
              SRADN = 'solar radiation minimum',
              SRADT = 'solar radiation total',
              LRAD = 'solar radiation/langley',
              LRADX = 'solar radiation/langley maximum',
              LRADT = 'solar radiation/langley total',
              SRMV = 'stream stage (gauge height) average',
              SRMX = 'stream stage (gauge height) maximum',
              SRMN = 'stream stage (gauge height) minimum',
              SRMO = 'stream stage (gauge height) observed',
              SRVOX = 'stream volume, adjusted external',
              SRVOO = 'stream volume, observed',
              SNDN = 'snow density',
              SNRR = 'snow rain ratio',
              OI = 'teleconnection index',
              CDD = 'temperature, degree days of cooling',
              GDD = 'temperature, degree days of growing',
              HDD = 'temperature, degree days of heating ',
              TURB = 'turbidity',
              RESA = 'usable lake storage volume',
              PVPV = 'vapor pressure - partial',
              SVPV = 'vapor pressure - saturated',
              WLEVV = 'water level average',
              WLEVX = 'water level maximum',
              WLEVN = 'water level minimum',
              WLEV = 'water level observed',
              WTEMP = 'water temperature',
              WTAVG = 'water temperature average',
              WTMAX = 'water temperature maximum',
              WTMIN = 'water temperature minimum',
              WELL = 'well depth',
              WDIRV = 'wind direction average',
              WDIR = 'wind direction observed',
              WDIRZ = 'wind direction standard deviation',
              WDMVV = 'wind movement average',
              WDMVX = 'wind movement maximum',
              WDMVN = 'wind movement minimum',
              WDMV = 'wind movement observed',
              WDMVT = 'wind movement total',
              WSPDV = 'wind speed average',
              WSPDX = 'wind speed maximum',
              WSPDN = 'wind speed minimum',
              WSPD = 'wind speed observed',
              AWDC = 'leaf wetness duration current')
  
  # return this giant list of variable names and descriptions if `varname` not supplied
  if(is.null(varname)) return(allvars) 
  
  # check for invalid names
  idx.mismatch = ! varname %in% names(allvars)
  if(any(idx.mismatch))
  {
    # print a warning and remove the offending entries from `varname`
    names(allvars)[idx.mismatch]
    varname = varname[!idx.mismatch]
    warning(paste('variable name(s)', varname[idx.mismatch], 'not recognized'))
    if(length(varname)==0) return()
  }
  
  # check for missing `savedir`
  if(is.null(savedir)) error('savedir not assigned')
  
  # build a dataframe to hold URL and destination paths for the downloads
  ns = nrow(sites)
  period = c('daily', 'semimonthly', 'monthly')
  idnm = c('site_id', 'state', 'ntwk')
  destfile = cbind(sites[rep(1:ns, each=length(period)), idnm], data.frame(freq=rep(period, ns)))
  
  # build a query URL for each site and period combination
  query.period = paste0(period, '/start_of_period/')
  query.pt1 = paste0('/reportGenerator/view_csv/customSingleStationReport,metric/', query.period)
  query.pt2 = apply(destfile[,idnm], 1, function(x) paste(x, collapse=':'))
  query.pt3 = '|id=%22%22|name/POR_BEGIN,POR_END/'
  query.pt4 = paste0(paste0(paste0(varname, '::value'), collapse=','), '?fitToScreen=false')
  destfile$url = paste0(base.url, query.pt1, query.pt2, query.pt3, query.pt4)
  
  # build names and destination paths for the downloaded files
  fname = paste0(paste('site', destfile$site_id, destfile$freq, sep='_'), '.csv')
  destfile$path = file.path(savedir, fname)
  
  # identify files that exist already on disk and create a flag for download errors
  idx.skip = rep(FALSE, length(period)*ns)
  if(reuse) idx.skip = file.exists(destfile$path)
  n.todl = sum(!idx.skip)
  idx.problem = rep(FALSE, length(period)*ns)
  
  # skip if there are no files to download
  if(n.todl > 1) 
  {
    # fetch the datasets by site and period using `rvest` in a loop
    pb = txtProgressBar(max=n.todl, style=3)
    print(paste('downloading', n.todl, 'files from NWCC'))
    for(idx.dl in 1:n.todl)
    {
      # index in the destfile dataframe
      idx.row = which(!idx.skip)[idx.dl]
      url.row = destfile$url[idx.row]
      path.row = destfile$path[idx.row]
      
      # attempt to download the csv file
      dl = tryCatch(download.file(url.row, path.row, mode='wb', quiet=T),
                    error = function(cond) return(NA),
                    warning = function(cond) return(NA))
      
      # in case of warnings or errors, delete the downloaded file and set a flag
      if(is.na(dl))
      {
        unlink(path.row)
        idx.problem[idx.row] = TRUE
      }
      
      # pause for five seconds so we don't clobber the server
      setTxtProgressBar(pb, idx.dl)
      Sys.sleep(5)
      
    }
    close(pb) 
    
  } else {
    
    print('all requested files exist in savedir. Set reuse=FALSE to overwrite')
    
  }
  
  if(any(idx.problem))
  {
    # print a message
    problem.files = paste(basename(destfile$path[idx.problem]), collapse=', ')
    print(paste('there was a problem downloading site(s):', problem.sites))
    
    # if allowed, attempt the downloads again via recursive call
    if(retry > 0)
    {
      print('retrying...')
      my_nwcc_get(varname, sites[idx.problem,], savedir, reuse, retry-1)
      
    } else {
      
      # print a warning and add a flag to the output dataset
      warning(paste('failed to download', length(problem.files), 'files'))
      destfile$error = idx.problem
      
    }
  }
  
  return(destfile)
  
}
```

open a CSV file from the NRCS National Water Climate Center

``` r
my_nwcc_open = function(path, varname=NULL, period=NULL)
{
  # ARGUMENTS:
  #
  # `path`: character, the full path to the CSV file downloaded with `my_nwcc_get`
  # `varname`: (optional) character vector, variable names for the columns
  # `period`: (optional) character vector, indicating time series type ('daily' or 'semimonthly')
  #
  # RETURN VALUE:
  # 
  # A dataframe containing the parsed NWCC records, with empty columns omitted 
  #
  # DETAILS:
  #
  # If `period` is not supplied, the function attempts to detect it from the filename
  # (by grepping for the strings 'daily' or 'semimonthly' in the basename). These are the only
  # two types supported at the moment. Dates are set in semimonthly records to the start of the
  # period, and the field `period` is appended to tables of either type. 
  #
  # based on snotelr::snotel_download
  #
  
  # detect period if not supplied
  if(is.null(period))
  {
    is.daily = grepl('daily', basename(path))
    is.semi = grepl('semimonthly', basename(path))
    is.monthly = grepl('monthly', basename(path)) & !is.semi
    period = c('daily', 'semimonthly', 'monthly')[as.numeric(is.semi)+2*as.numeric(is.monthly)+1]
  }
  
  # open the file as text
  rawtxt = readLines(path)
  rawtxt.iscomment = which(grepl('^#', rawtxt))
  rawtxt.header = rawtxt[rawtxt.iscomment[length(rawtxt.iscomment)] + 1]
  
  # parse units into a form R understands
  rawtxt.header.matches = gregexpr('(?<=\\().*?(?=\\))', rawtxt.header, perl=T)
  varname.units = regmatches(rawtxt.header, rawtxt.header.matches)[[1]]
  varname.units = c('unitless', varname.units[! varname.units %in% c('gauge Height', 'par') ])
  varname.units[varname.units == 'pct'] = '%'
  varname.units[varname.units == 'unitless'] = NA
  
  # open again as table and format field names to match input
  site.df = read.csv(path, comment.char='#')
  if(is.null(varname)) varname = colnames(site.df)[-1]
  colnames(site.df) = c('date', varname)
  
  # drop empty columns and assign units 
  idx.drop = apply(site.df, 2, function(x) all(is.na(x)))
  site.df = site.df[, !idx.drop]
  varname.units = varname.units[!idx.drop]
  varname.hasunits = !is.na(varname.units)
  if(any(varname.hasunits))
  {
    # replace columns with unit-assigned versions in a loop 
    for(idx.unit in 1:sum(varname.hasunits))
    {
      unit.symbol = varname.units[varname.hasunits][idx.unit]
      cn = which(varname.hasunits)[idx.unit]
      site.df[,cn] = set_units(site.df[,cn], unit.symbol, mode='standard')
    }
  }
  
  # indicate period as a new field
  site.df$period = rep(period, nrow(site.df))
  
  # dates are easy to parse in daily files
  if(period=='daily')
  {
    site.df$date = as.Date(site.df$date)
    
  }
  
  # dates in semimonthly files get days set to 1 and 15 in each month
  if(period=='semimonthly')
  {
    site.df$date = gsub('1st Half', '1', site.df$date)
    site.df$date = gsub('2nd Half', '15', site.df$date)
    site.df$date = as.Date(site.df$date, '%b %d %Y')
    
  }
  
  # dates in monthly files get days set to 1
  if(period=='monthly')
  {
    site.df$date = as.Date(sapply(site.df$date, function(x) paste('1 ', x)), '%d %b %Y')
    
  }
  
  # finished
  return(site.df)
  
}
```

open a batch of CSV files from the NRCS National Water Climate Center

``` r
my_nwcc_import = function(files, varname=NULL)
{
  # ARGUMENTS:
  #
  # `files`: dataframe with character fields `path`, `period`, ``
  # `varname`: (optional) character vector, variable names for the columns
  # `trimresult`: (optional) boolean, indicating whether to omit suspected duplicates 
  #
  # RETURN VALUE:
  # 
  # A list of dataframes, in same length/order as the site listed in `files`. These contain
  # the raw data, with appropriately assigned units and dates. When a site request returns
  # no data (one or two 0-row CSV files), the list entry is a 
  #
  # DETAILS:
  #
  # Daily and semimonthly data are joined into a single dataframe for each site by 
  # assigning the 1st and 15th of the month as placeholder dates for any semimonthly 
  # records (field `period` identifies these records). Empty columns are then omitted from
  # output. `varname` optionally supplies column names as a vector the same length/order
  # as headers in the input CSV tables (not the output dataframes).
  #
  
  # define output storage
  site.id = unique(files$site_id)
  sites.n = length(site.id)
  sites.out = setNames(vector(mode='list', length=sites.n), paste0('site_', site.id))
  
  # check for missing files
  idx.missing = !file.exists(files$path)
  msg.missing = paste(basename(files$path[idx.missing]), collapse=', ')
  if(any(idx.missing)) warning(paste('file(s)', msg.missing, 'not found'))
  if(all(idx.missing)) stop('0 files loaded')
  files = files[!idx.missing,]
  
  # read the files in a loop
  pb = txtProgressBar(max=sites.n+1, style=3)
  print(paste('importing', nrow(files), 'NWCC data tables'))
  for(idx.site in 1:sites.n)
  {
    # each site is imported as a list of dataframes (one per file)
    idx.files = files$site_id == site.id[idx.site]
    in.path = files$path[idx.files]
    sites.out[[idx.site]] = lapply(in.path, function(path) my_nwcc_open(path, varname=varname))
    setTxtProgressBar(pb, idx.site)
    
  }
  
  # join into single dataframe at each site, dropping any empty ones
  joined.out = lapply(sites.out, function(x) merge(x[[1]], x[[2]], all=TRUE))
  setTxtProgressBar(pb, idx.site + 1)
  close(pb)
  
  # replace 0-row dataframes with NULL
  joined.n = sapply(joined.out, nrow)
  idx.empty = joined.n == 0
  joined.out[idx.empty] = rep(list(NULL), sum(idx.empty))
  if(any(idx.empty)) print(paste(sum(idx.empty), 'sites had no data'))
  
  # finished
  return(joined.out)
  
}

# TODO: work on fetching SNODAS grids
my_get_snodas = function()
{
  
  ftp.url = 'ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/'
  
  
  
}
```

search and download/extract files from the US NPS Data Store
(irmaservices.nps.gov)

``` r
my_get_irma = function(query='boundaries', resoid=NULL, dest=NULL, nmax=1000, ow=FALSE, uz=TRUE)
{
  # A very basic R implementation of the NPS REST API, documented here:
  # https://irmaservices.nps.gov/datastore/v4/documentation/datastore-api.html#/
  #
  # ARGUMENTS:
  #
  # `query`: character or integer vector, reference search query string(s) or id(s)
  # `resoid`: (optional) integer vector, resource ID numbers 
  # `dest`: character, path to destination folder for downloaded file(s)
  # `nmax`: integer, the maximum number of results to return
  # `ow`: logical, whether to overwrite existing files on disk
  # `uz`: logical, whether to extract files from zip archives into a subfolder of `dest`
  #
  # RETURN VALUE:
  #
  # When `query` is a character (vector) its contents are treated as search strings
  # and the function returns a dataframe of matches in the NPS digital reference database,
  # including reference ID codes (but downloads nothing).
  #
  # When `query` is a an integer (vector) its contents are treated as reference ID codes,
  # and the function returns a dataframe of file metadata associated with the references
  # (downloading the files only if `dest` is supplied).
  #
  # DETAILS:
  #
  # If requested by `uz=TRUE`, any zipped files are extracted to like-named subfolders
  # of `dest` (without deleting the original zip).
  #
  # Since there can be several files to a reference, `resoid` allows users to specify a
  # subset of files rather than downloading in batches by reference. These must be a subset
  # of the resource IDs associated with the reference IDs in `query`. Get a table containing
  # both keys by calling `my_get_irma` with `dest=NULL` (the default).
  #
  # `query` can also be a dataframe, in which case the function expects column(s)
  # 'referenceID' and, optionally, 'resourceID' (copied to `resoid`). eg. typical usage
  # would be to query a keyword to get a dataframe of relevant references, then (after
  # reviewing and selecting the desired entries, and possibly building a vector of resource
  # IDs), pass a subset of this dataframe back to `my_get_irma` to download the files.
  # 
  
  # URL for REST API
  url.irmabase = 'https://irmaservices.nps.gov/datastore/v4/rest'
  
  # dataframe `query` assumed to include column 'referenceId' (and possibly 'resourceId')
  if( is.data.frame(query) )
  {
    # un-tibble input
    query = data.frame(query)
    
    #  pull IDs from the dataframe if available
    if( is.null(resoid) & !is.null(query$resourceId) ) resoid = unique(query$resourceId)
    if( !is.null(query$referenceId) ) query = unique(query$referenceId)
    
  }

  # character type `query` interpreted as simple search string
  if( any(is.character(query)) )
  {
    # construct request URL (collapsing vectors into space delimited strings)
    query = paste(query, collapse='%20')
    gsub('/', '\\/', query)
    rest.search = paste0('QuickSearch?q=', query, '&top=', as.integer(nmax))
    url.rest = file.path(url.irmabase, rest.search)
    
    # download JSON to a tempfile
    json.temp = tempfile()
    download.file(url.rest, json.temp, quiet=TRUE)
    
    # open and parse the results as dataframe
    result.json = read_json(json.temp, simplifyVector=TRUE) 
    result.df = result.json$items 
    
    # halt on zero-length results
    if(length(result.df)==0) stop('search query matched no results')
    
    # clean up output
    result.df = result.df %>% 
      mutate( dateOfIssue = as.POSIXlt(gsub('T', ' ', dateOfIssue)) ) %>%
      select( referenceId, dateOfIssue, title ) %>% as_tibble

    # print a message
    n.response = result.json$pageDetail$responseCount
    n.total = result.json$pageDetail$totalCount
    cat(paste('showing', n.response, 'of', n.total, 'results'))
    
    # delete the tempfile and finish
    unlink(json.temp)
    return(result.df)
    
  }
  
  # handle vectorized requests
  if(length(query) > 1) 
  {
    # recursive call to build list of metadata about each query code
    result.list = lapply(query, function(x) my_get_irma(x, dest=NULL) %>% mutate(referenceId=x) )
    result.df = do.call(rbind, result.list) %>% select( referenceId, everything() )
    
  } else {
    
    # construct metadata request URL and open JSON result as dataframe via tempfile
    json.temp = tempfile()
    rest.dlmeta = file.path('Reference', as.integer(query), 'DigitalFiles')
    download.file(file.path(url.irmabase, rest.dlmeta), json.temp, quiet=TRUE)
    result.df = read_json(json.temp, simplifyVector=TRUE) 
    unlink(json.temp) 
    
    # clean up output
    result.df = result.df %>%
      mutate( lastUpdate = as.POSIXlt(gsub('T', ' ', lastUpdate)) ) %>%
      mutate( fileSize = set_units(set_units(fileSize, 'bytes'), 'megabytes') ) %>%
      select( fileName, everything() ) %>% as_tibble
  }
  
  # trim to supplied resource IDs, if needed
  if( is.null(resoid) ) resoid = result.df$resourceId
  result.df = result.df %>% filter( resourceId %in% resoid )
  result.n = nrow(result.df)
  if( result.n == 0 ) stop('supplied `resoid` does not match any records in `query`')
  
  # handle download requests
  if( !is.null(dest) )
  {
    # make the directory if it doesn't exist already and set up path(s)
    my_dir(dest)
    dest.paths = file.path(dest, result.df$fileName)
    
    # halt on filename collisions
    idx.exists = file.exists(dest.paths)
    msg.exists1 = paste(result.df$fileName[idx.exists], collapse=', ')
    msg.exists2 = paste('the following files were found on the destination path:', msg.exists1)
    if( length( unique(result.df$fileName) ) < result.n ) stop('non-unique destination filenames')
    if( !ow & any(idx.exists) ) stop(paste(msg.exists2, '\ntry setting `ow=TRUE`'))
    
    # download all files to disk
    msg.info = paste0('(', round(sum(result.df$fileSize), 2), ' MB total)...')
    cat(paste('downloading', result.n, 'files', msg.info))
    pb = txtProgressBar(0, result.n, style=3)
    for(idx.file in 1:result.n)
    {
      # download the file
      result.dl = download.file(result.df$downloadLink[idx.file], dest.paths[idx.file], mode='wb')
      setTxtProgressBar(pb, idx.file)
      
      # unzip if requested, to subfolder of `dest` named after the zip filename
      if(uz & result.df$extension[idx.file] == 'zip')
      {
        dest.dir = file.path(dest, gsub('.zip', '', result.df$fileName[idx.file]))
        unzip(dest.paths[idx.file], exdir = dest.dir)
      }
    }
    
    # append paths on disk to output dataframe
    result.df = result.df %>% mutate(path=dest.paths)
    
  }
  
  return(result.df)
  
}
```

This is a kludge combining various `FedData` functions with some of my
own code, in order to import STATSGO2 data and produce output similar to
FedData::get\_ssurgo. It uses data from
[NRCS](https://nrcs.app.box.com/v/soils), which is aggregated at the
state level. Instead of a `template` or SSA code argument, it takes a
state code, such as ‘mt’. Note that not all states are available at this
time (see here for the list)

``` r
my_get_statsgo = function(raw.dir, state, extraction.dir, label='UYRW')
{
  # raw.dir = path of the files extracted from the NRCS zip (this dir should have subdirs 'tabular', 'spatial') 
  # state = 2-letter state code, eg. 'MT' for Montana (case insensitive)
  # extraction.dir = absolute path of destination folder for CSV and shapefile data
  
  # get tables headers (these are not exported to user namespace!)
  tablesHeaders = getFromNamespace('tablesHeaders', 'FedData')
  tablesHeaders$component
  
  # this call adjusted to use STATSGO2 syntax for filenames
  mapunits <- sf::read_sf(paste0(raw.dir, "/spatial"), layer = paste0("gsmsoilmu_a_", tolower(state))) %>%
    sf::st_make_valid() %>%
    dplyr::group_by(AREASYMBOL, SPATIALVER, MUSYM, MUKEY) %>%
    dplyr::summarise()
  
  # same here
  if (.Platform$OS.type == "windows") 
  {
    files <- list.files(paste0(raw.dir, "/tabular"),full.names = T)
    tablesData <- lapply(files, function(file) {tryCatch(return(utils::read.delim(file, header = F, sep = "|", stringsAsFactors = F)), error = function(e) {return(NULL)})})
    names(tablesData) <- basename(files)
    tablesData <- tablesData[!sapply(tablesData, is.null)]
    
  } else {
    
    files <- list.files(paste0(raw.dir, "/tabular"), full.names = T)
    tablesData <- lapply(files, function(file) { tryCatch(return(utils::read.delim(file, header = F, sep = "|", stringsAsFactors = F)), error = function(e) {return(NULL)})})
    names(tablesData) <- basename(files)
    tablesData <- tablesData[!sapply(tablesData, is.null)]
  }
  
  # below is just copy pasted from 'FedData::get_ssurgo_study_area' (v2.5.7)
  SSURGOTableMapping <- tablesData[["mstab.txt"]][, c(1,5)]
  names(SSURGOTableMapping) <- c("TABLE", "FILE")
  SSURGOTableMapping[, "FILE"] <- paste(SSURGOTableMapping[,"FILE"], ".txt", sep = "")
  tablesData <- tablesData[as.character(SSURGOTableMapping[,"FILE"])]
  tablesHeads <- tablesHeaders[as.character(SSURGOTableMapping[,"TABLE"])]
  notNull <- (!sapply(tablesData, is.null) & !sapply(tablesHeads,is.null))
  tablesData <- tablesData[notNull]
  tablesHeads <- tablesHeads[notNull]
  tables <- mapply(tablesData, tablesHeads, FUN = function(theData,theHeader) {
    names(theData) <- names(theHeader)
    return(theData)
  })
  names(tables) <- names(tablesHeads)
  tables <- extract_ssurgo_data(tables = tables, mapunits = as.character(unique(mapunits$MUKEY)))
  
  # save results as ESRI shapefile and csv (adapted from FedData::get_ssurgo v2.5.7)
  suppressWarnings(rgdal::writeOGR(as(mapunits, 'Spatial'), dsn = normalizePath(paste0(extraction.dir,"/.")), layer = paste0(label, "_SSURGO_Mapunits"), driver = "ESRI Shapefile", overwrite_layer = TRUE))
  junk <- lapply(names(tables), function(tab) {readr::write_csv(tables[[tab]], path = paste(extraction.dir, "/", label, "_SSURGO_", tab, ".csv", sep = "")) })
  
  return(list(spatial = mapunits, tabular = tables))
}
```

This function parses a SSURGO/STATSGO2 style list of tables to extract
the soil data needed for SWAT+. These data are reshaped into a single
table `usersoil` (the return value) that maps mukeys to the relevant
information on the dominant soil component (ie the component with
maximal comppct\_r). The output table is formatted in the style expected
for the CSV file “usersoil.csv” in SWAT+ AW, with an additional column,
`pmiss` indicating for each mukey the percent of soil variables which
are missing (NA).

The code for this function is adapted from the missing data filler
script in [this
post](https://hydrologicou.wordpress.com/r-script-to-generate-missing-records-in-acrswat-ssurgo-database/),
which in turn is based on snippets from [this
example](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?root=aqp).

``` r
my_usersoil = function(soils.tab, my.mukeys=NA)
{
  # ARGUMENTS:
  #
  # `soils.tab` a named list of STATSGO2/SSURGO dataframes (containing `component`, `chorizon`, and `chtexturegrp`)
  # `my.mukeys` an (optional) vector of integer-valued mapunit keys to process and include in the output
  #
  # RETURN VALUE:
  #
  # a data.frame in the format of "usersoil.csv" (with columns `MUID`, `SEQN`, `SNAM`, etc)
  
  ## define some magic numbers
  
  # number of horizons to write for each soil component
  n.hz = 10
  
  # assume albedo is reduced to 60% for moist soil vs dry soil (as it is reported in SSURGO/STATSGO2)
  albedo.const = 0.6
  
  # van Bemmelen factor for converting organic matter weight to organic carbon content
  cbn.const = (1/0.58)
  
  # suppress overly verbose friendly warnings about grouping variables from dplyr
  options(dplyr.summarise.inform=FALSE) 
  
  ## prepare the source data tables
  
  # check for incomplete/missing `mukey` argument
  if(any(is.na(my.mukeys)))
  {
    print('NA(s) detected in `my.mukeys` argument, processing all mukeys in input dataset...')
    my.mukeys = sort(soils.tab$component$mukey)
  }
  
  # eliminate any duplicate mukey entries and count number of unique keys
  my.mukeys = unique(my.mukeys)
  n.mukeys = length(my.mukeys)
  
  # prepare components table by assigning (to each mukey) the soil component with max coverage 
  my.comp = data.frame(mukey=my.mukeys) %>%
    left_join(soils.tab$component, by='mukey') %>%  
    group_by(mukey) %>% 
    slice(which.max(comppct.r)) %>% 
    arrange(cokey) %>%
    as.data.frame()
  
  # prepare soil layer (horizon) data for the dominant components. Most components have multiple horizons
  my.chrz = my.comp %>%
    left_join(soils.tab$chorizon, by='cokey') %>%
    group_by(cokey)
  
  # prepare soil texture group data 
  my.chtx = soils.tab$chtexturegrp %>%
    filter(chkey %in% my.chrz$chkey) %>%
    arrange(chkey)
  
  ## prepare the left half of the table
  # all unnumbered parameters (ie those not specific to a single horizon) are stored here
  my.df.left = data.frame(
    
    # map unit key (mukey)
    MUID = my.comp$mukey,
    
    # soil sequence (component) key (cokey) for the dominant component
    SEQN = my.comp$cokey,
    
    # soil name (character code that serves as key to soils.csv lookup table for raster integer codes)
    SNAM = rep(NA, n.mukeys),
    
    # soil interpretation record (SIR, Soils-5, SOI-5, or S-5, a decommissioned/archived database)
    S5ID = rep(NA, n.mukeys),
    
    # percentage of the mapunit occupied by this component
    CMPPCT = my.comp$comppct.r,
    
    # number of soil layers in this component
    NLAYERS = my.chrz %>% 
      dplyr::summarize(n = n()) %>%
      pull(n), 
    
    # USDA Soil Survey hydrological group (A, B, C, D) 
    HYDGRP = my.comp$hydgrp,
    
    # max rooting depth of soil profile (in mm = 10*cm)
    SOL_ZMX = my.chrz %>% 
      dplyr::summarize(depth = 10*max(hzdepb.r)) %>% 
      pull(depth),
    
    # unavailable in SSURGO
    ANION_EXCL = rep(NA, nrow(my.comp)),
    
    # unavailable in SSURGO
    SOL_CRK = rep(NA, nrow(my.comp)),
    
    # texture description (not processed by the model)
    TEXTURE = my.chrz %>%
      dplyr::summarize(texture = my.chtx$texture[match(chkey, my.chtx$chkey)]) %>%
      dplyr::summarize(texture_code = paste0(texture, collapse='-')) %>%
      as.data.frame() %>%
      pull(texture_code)
    
  )
  
  ##  compile (as list) all parameters specific to a single horizon, later reshaped to form right half of table
  # lapply call iterates over horizon numbers, building a table (spanning all mukeys) for each one
  my.list.right = lapply(1:n.hz, function(hz.idx) my.chrz  %>% dplyr::summarize(
    
    # depth from soil surface to bottom of layer (in mm = 10*cm)
    SOL_Z = 10*hzdepb.r[hz.idx],
    
    # moist bulk density (Mg/m3)
    SOL_BD = dbovendry.r[hz.idx],
    
    # available water capacity (H20/soil)
    SOL_AWC = awc.r[hz.idx],
    
    # saturated hydraulic conductivity (in mm/hr = 3600 s/hr / 1000 micrometers/mm)
    SOL_K = 3.6*ksat.r[hz.idx],
    
    # organic carbon content (% of soil weight) estimated from organic matter via van Bemmelen factor
    SOL_CBN = cbn.const*om.r[hz.idx],
    
    # clay content (% of soil weight)
    CLAY = claytotal.r[hz.idx],
    
    # silt content (% of soil weight)
    SILT = silttotal.r[hz.idx],
    
    # sand content (% of soil weight)
    SAND = sandtotal.r[hz.idx],
    
    # rock fragment content (% of total weight)
    ROCK = 100 - sieveno10.r[hz.idx],
    
    # USLE equation soil erodibility (K) factor 
    USLE_K = kwfact[hz.idx],
    
    # electrical conductivity (dS/m)
    SOL_EC = ec.r[hz.idx],
    
    # soil CaCo3 (% of soil weight)
    CAL = caco3.r[hz.idx],
    
    # soil pH
    PH = ph1to1h2o.r[hz.idx]
    
  ) %>% 
    
    # for this next parameter, we re-use the single SSURGO value on all horizons
    # moist soil albedo (ratio of reflected/incident)
    mutate(SOL_ALB = albedo.const*my.comp$albedodry.r) %>% 
    
    # omit the cokeys (they are included as SEQN in my.df.left, above)
    select(-cokey) %>% 
    
    # and, finally, you've reached the end of this really long lapply call
    as.data.frame)
  
  # append horizon number to each column name in preparation for list -> table reshape
  for(idx.hz in 1:n.hz) { names(my.list.right[[idx.hz]]) = paste0(names(my.list.right[[idx.hz]]), idx.hz) }
  
  # reshape into a single table
  my.df.right = do.call(cbind, my.list.right)
  
  # bind the left and right sides of the table and reorder to match input `my.mukeys`
  usersoil.df = cbind(my.df.left, my.df.right)[match(my.mukeys, my.df.left$MUID),]
  
  # number of variables expected in the left table (omit SNAM, S5ID, ANION_EXCL, SOL_CRK)
  nvars.left = ncol(my.df.left) - 4
  
  # number of variables expected in the right table (depends on the number of horizons)
  nvars.right = usersoil.df$NLAYERS * ncol(my.list.right[[1]])
  nvars.all = nvars.left + nvars.right
  
  # add a column indicating the percent of expected variables which were NA
  usersoil.df$pmiss = sapply(1:n.mukeys, function(idx.r) sum(is.na(usersoil.df[idx.r, 1:nvars.all[idx.r]]))/nvars.all[idx.r])
  
  # add OBJECTID column and return the data frame
  return(cbind(data.frame(OBJECTID=1:n.mukeys), usersoil.df))
}
```

This function returns a (case-insensitive) expression-matching lookup
table for matching NVC plant community classifications to SWAT+ plant
codes. The former are documented
[here](http://usnvc.org/data-standard/natural-vegetation-classification),
and the latter can be found in the `plants_plt` table of the SWAT+
‘datasets’ SQLite file.

``` r
my_plants_plt = function(nvc.df=NULL)
{
  # ARGUMENTS:
  #
  # `nvc.df`: a dataframe with columns `NVC_DIV`, `NVC_MACRO`, and `NVC_GROUP`
  #
  # RETURN VALUE:
  #
  # the `nvc.df` dataframe with two appended columns, `swatcode` and `swatdesc`, containing
  # the matching SWAT+ plant code and its description. If `nvc.df` is not supplied, returns
  # the lookup table (as dataframe)
  # 
  # DETAILS:
  #
  # The lookup table is specified below, in a list of 1-row dataframes, each containing the fields `swatdesc`
  # and `swatcode`. These strings should match the `description` and `name` fields in a row of `plants_plt`.
  # The other 3 fields -- `kwdiv`, `kwmacro`, and `kwgroup` -- are keywords to filter the NCV divisions, macros,
  # and groups (a nested hierarchical classification), where the pipe operator `|` functions as a logical OR.
  #
  # Note that order matters in this list, as earlier entries are superceded by later ones whenever
  # there is any overlap in the subset of NVC classifications specified by the `kw*` arguments.
  
  plt.list = list(
    
    # (default value) generic for sparsely vegetated land
    data.frame(swatdesc='barren_or_sparsley_vegetated',
               swatcode='bsvg',
               kwdiv=''),
    
    # water bodies
    data.frame(swatdesc='wetlands_non_forested', 
               swatcode='wetn',
               kwdiv='open water'),

    # generic for cropland
    data.frame(swatdesc='agricultural_land_generic', 
               swatcode='agrl',
               kwdiv='agricultural'),
    
    # generic for hay and pasture (see Appendix A) and introduced grassland/forbland
    data.frame(swatdesc ='tall_fescue', 
               swatcode='fesc',
               kwdiv='hay|introduced'),
    
    # generic for shrublands (notice one instance is misspelled!)
    data.frame(swatdesc ='range_brush_temperate_mountain_systems', 
               swatcode='rngb_tems', 
               kwgroup='shrubland|shubland|scrub'),
    
    # generic for warmer/drier shrublands 
    data.frame(swatdesc ='range_brush_temperate_steppe', 
               swatcode='rngb_test', 
               kwgroup='deciduous shrubland|sagebrush scrub'),
    
    # generic for shrublands (notice one instance is misspelled!)
    data.frame(swatdesc ='mixed_grassland/shrubland', 
               swatcode='migs', 
               kwgroup='sagebrush stepp'),
    
    # generic for grasslands
    data.frame(swatdesc ='range_grasses_temperate_mountain_systems', 
               swatcode='rnge_tems', 
               kwgroup='prairie|grassland'),
    
    # generic for semi-arid grasslands
    data.frame(swatdesc ='range_grasses_temperate_steppe', 
               swatcode='rnge_test', 
               kwgroup='semi-desert grassland'),
    
    # generic for pine-dominated forests, which I assign to recently-disturbed areas (harvest/fire) 
    data.frame(swatdesc='pine',  
               swatcode='pine',
               kwdiv='forest|woodland|disturbed', 
               kwgroup='pine|disturbed'),
    
    # generic mixed-wood class
    data.frame(swatdesc='forest_mixed_temperate_mountain_systems', 
               swatcode='frst_tems', 
               kwdiv='forest|woodland',
               kwgroup='mountain-mahogany|swamp|riparian'),
    
    # generic for evergreen temperate steppe forests
    data.frame(swatdesc='forest_evergreen_temperate_steppe', 
               swatcode='frse_test', 
               kwdiv='forest|woodland', 
               kwgroup='plains|open woodland'),
    
    # I interpret `poplar` as a generic Populus class
    data.frame(swatdesc='poplar',  
               swatcode='popl',
               kwdiv='forest|woodland', 
               kwgroup='aspen|cottonwood'),
    
    # generic for non-pine evergreen temperate mountainous forests
    data.frame(swatdesc='forest_evergreen_temperate_mountain_systems', 
               swatcode='frse_tems', 
               kwdiv='forest|woodland', 
               kwgroup='fir|juniper'),
    
    # a specific class for lodgepole pine forests
    data.frame(swatdesc='lodge_pole_pine', 
               swatcode='ldgp', 
               kwdiv='forest|woodland',
               kwgroup='lodgepole'),
    
    # a more specific class for white-spruce forests
    data.frame(swatdesc='white_spruce',  
               swatcode='wspr',
               kwdiv='forest|woodland', 
               kwgroup='dry-mesic spruce'),
    
    # generic for forested wetlands
    data.frame(swatdesc ='wetlands_forested',  
               swatcode='wetf',
               kwgroup='swamp forest'),
    
    # generic for wet shrublands
    data.frame(swatdesc ='herbaceous_wetland', 
               swatcode='wehb', 
               kwgroup='fen|marsh|wet meadow|vernal pool'),
    
    # herbaceous alpine tundra 
    data.frame(swatdesc='herbaceous_tundra',  
               swatcode='tuhb',
               kwdiv='tundra'),
    
    # barren alpine tundra 
    data.frame(swatdesc='bare_ground_tundra', 
               swatcode='tubg', 
               kwdiv='tundra', 
               kwgroup='scree')
    
  ) 
  
  # combine rows from all dataframes, filling missing kw fields with empty character ('') 
  plt = plt.list %>% bind_rows %>% mutate_all(~replace(., is.na(.), ''))
  if( is.null(nvc.df) ) return(plt)
  
  # copy the input dataframe, appending two new (empty) columns
  nvc.out.df = nvc.df %>% mutate(swatcode=NA, swatdesc=NA)
  
  # fill in SWAT+ codes: note that later entries of `plt` overwrite earlier ones 
  for(idx.plt in 1:nrow(plt))
  {
    # grep for keywords in the NCV classifications
    idx.div = grepl(plt[idx.plt, 'kwdiv'], nvc.df$NVC_DIV, ignore.case=TRUE)
    idx.group = grepl(plt[idx.plt, 'kwgroup'], nvc.df$NVC_GROUP, ignore.case=TRUE)
    
    # write SWAT+ code to each matching entry 
    nvc.out.df[idx.div & idx.group, 'swatcode'] = plt[idx.plt, 'swatcode']
    nvc.out.df[idx.div & idx.group, 'swatdesc'] = plt[idx.plt, 'swatdesc']
  }
  
  return(nvc.out.df)
}
```

It took a bit of work to construct the function above: The following may
be helpful for improving the lookup table, or when extending it to new
areas, and/or new releases of SWAT+ and the NCV.

To begin cross-referencing SWAT+ plant codes with the NVC
classifications, we start with packages
[`DBI`](https://cran.r-project.org/web/packages/DBI/vignettes/DBI-1.html)
and [`RSQLite`](https://github.com/r-dbi/RSQLite) to interface with
SWAT+ SQLite databases For example, to load the `plants.plt` in
“swatplus\_datasets.sqlite”, do:

``` r
# library(DBI) 
# library(RSQLite)
# swat.ds.conn = dbConnect(SQLite(), 'C:/SWAT/SWATPlus/Workflow/editor_api/swatplus_datasets.sqlite')
# plants_plt = dbReadTable(swat.ds.conn, 'plants_plt')
# dbDisconnect(swat.ds.conn)
```

Some (now incomplete) reference information on the `plants_plt` data can
be found in [Appendix A of the SWAT IO file docs
(2012)](https://swat.tamu.edu/media/69419/Appendix-A.pdf). Since SWAT+
is fairly new, and under active development, I would expect this
document to be updated in the near future. For now, some guesswork was
need to match the `description` field to each [NVC group
category](http://usnvc.org/explore-classification/) in the UYRW.

[`data.tree`](https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html)
can be very helpful for parsing this kind of data, with clean printouts
of nested lists of strings.

``` r
# # print all available SWAT+ plant codes containing the following keywords
# kw = 'forest|woodland|pine'
# plants_plt %>% filter(grepl(kw, description, ignore.case=TRUE)) %>% pull(description)
# 
# # define some keywords to parse the NVC categories ('' means show all results at this level)
# kwdiv = 'forest|woodland'
# kwmacro = ''
# kwgroup = 'pine'
# 
# # build a copy of the land use table with explicit tree structure in column `pathString`
# landuse.tree = landuse.tab %>%
#   filter(grepl(kwdiv, NVC_DIV, ignore.case=TRUE)) %>%
#   filter(grepl(kwmacro, NVC_MACRO, ignore.case=TRUE)) %>%
#   filter(grepl(kwgroup, NVC_GROUP, ignore.case=TRUE)) %>%
#   mutate(pathString=paste('NVC', NVC_DIV, NVC_MACRO, NVC_GROUP, sep='/'))
# 
# # print tree structure, displaying the number of pixels and any existing assignments to SWAT+ plant codes
# print(as.Node(landuse.tree), 'n_uyrw', 'swatdesc', limit=125)
```
