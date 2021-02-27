#' ---
#' title: "get_helperfun.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **get_helperfun**: helper functions for the scripts in the UYRW_data repository 
#' 
#' This script is meant to be sourced by all other scripts in the repository. It defines some helper functions, 
#' directories for local storage, and some R code that loads commonly used datasets if they are detected in the data directory.
#' 

#'
#' ## libraries
#' These CRAN packages are quite useful, and are required by some of the scripts in the repository.
#' If any of these are not already installed on your machine, run `install.packages(...)` to get them.

#' [`raster`](https://rspatial.org/raster/) handles raster data such as GeoTIFFs
library(raster)

#' [`sf`](https://r-spatial.github.io/sf/) handles GIS data such as ESRI shapefiles
library(sf)

#' [`ggplot2`](https://ggplot2.tidyverse.org/) popular graphics package with high-level abstraction
library(ggplot2)  

#' [`tmap`](https://github.com/mtennekes/tmap) constructs pretty thematic map graphics
library(tmap)

#' [`dplyr`](https://dplyr.tidyverse.org/R) tidyverse-style manipulation of tabular data
library(dplyr)

#' ['RSQLite'](https://www.r-project.org/nosvn/pandoc/RSQLite.html) connects to SQLite databases
library(RSQLite)

#' ['data.table'](https://cran.r-project.org/web/packages/data.table/index.html) efficiently load large I/O files.
library(data.table)

#' [`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html) GDAL wrapper
library(gdalUtilities)

#' [`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf) web scraping
library(rvest) 

#' [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite) handle JSON I/O
library(jsonlite)

#'
#' ## project data

#' To avoid downloading things over and over again, we'll use a permanent storage location on disk ("/data").
#' This is where we store large data files and R object binaries, which are not suitable for git.
#' 
#' The `if(!file.exists(...))` conditionals preceding each code chunk indicate which files will be written in that section.
#' If the files are detected in the local data storage directory, then that code chunk can be skipped (to avoid redundant downloads,
#' *etc*), and the files are loaded from disk instead. 
#' 
#' We start by defining a project directory tree

#+ results='hide'
# 'graphics', 'markdown', 'data' are top level directories in the RStudio project folder
graphics.dir = 'graphics'
markdown.dir = 'markdown'
data.dir = 'data'

# subdirectories of `data` contain source files and their processed output 
src.subdir = 'data/source'
out.subdir = 'data/prepared'

#' Define a helper function for creating folders then create project folders as needed
my_dir = function(path) { if(!dir.exists(path)) {dir.create(path, recursive=TRUE)} }
lapply(here(c(data.dir, src.subdir, out.subdir, graphics.dir, markdown.dir)), my_dir)


#' This project will generate many files. To keep track of everything, each script gets a CSV table documenting every
#' file that it creates: its file path, its type, and a short description of the contents. This function handles the construction
#' of the table. To call up the table for a specific script, simply use `my_metadata(script.name)`.
#' 
# creates and/or adds to a data frame of metadata documenting a given script, and (optionally) writes it to disk as a CSV file 
my_metadata = function(script.name, entries.list=NA, overwrite=FALSE, use.file=TRUE, data.dir='data', v=TRUE)
{
  # ARGUMENTS:
  #
  # `script.name` is a string indicating the filename (without the .R extension) of the R script to document.
  # `entries.list` is a list of character vectors, whose entries are named: 'name', 'file', 'type', and 'description'.
  # `data.dir` is the subdirectory of the project folder in which to write the CSV file: /data/`script.name`_metadata.csv 
  # `use.file` is a boolean indicating whether to read/write the CSV file
  # `overwrite` allows an existing CSV file to be modified 
  # `v` boolean, set FALSE to suppress console message
  #
  # RETURN VALUE:
  #
  # a data frame containing a table of file info, combining the elements of `entries.list` with data from the CSV corresponding
  # to the script name (if it exists)
  #
  # BEHAVIOUR: 
  #
  # With use.file==FALSE, the function simply returns `entries.list`, reshaped as a data.frame. 
  #
  # With use.file==TRUE, the function looks for an existing CSV file, reads it, and combines it with the elements of `entries.list`.
  # New entries take precedence: ie. any element of `entries.list` whose 'name' field matches an existing row in the CSV will replace
  # that row. Elements with names not appearing the CSV are added to the top of the table in the order they appear in `entries.list`.
  #
  # Existing CSV files are never modified, unless `use.file` and `overwrite` are both TRUE, in which case the CSV is overwritten with
  # the modified data frame. If the CSV file does not already exist on disk, it will be created. The default `entries.list==NA`, 
  # combined with `overwrite=TRUE` and `use.file=TRUE` will overwrite the CSV with a default placeholder - a table containing only a
  # single row, which describes the CSV file itself.
  
  # define the CSV filename, and define a flag that will indicate to wipe it replace with a default if requested
  csv.relpath = file.path(data.dir, paste0(script.name, '_metadata.csv'))
  csv.wipe = FALSE
  
  # create the directory if necessary
  my_dir(dirname(here(csv.relpath)))
  
  # prepare the default one-row data.frame 
  entry.names = c('name', 'file', 'type', 'description')
  entry.default = c(name='metadata', file=csv.relpath, type='CSV', description=paste0('list files of files written by ', script.name, '.R'))
  
  # parse `entries.list` to check for wrong syntax or NA input
  if(!is.list(entries.list))
  {
    # if `entries.list` is not a list, it must be a single NA. Stop if it's a vector
    if(length(entries.list) > 1)
    {
      stop('entries.list must be either a list or (length-1) NA value')
      
    } else {
      
      # Catch non-NA, non-list input 
      if(!is.na(entries.list))
      {
        stop('entries.list must be either a list or (length-1) NA value')
      } 
    }
    
    # Recursive call to generate the data frame with default entry
    input.df = my_metadata(script.name, entries.list=list(entry.default), use.file=FALSE)
    
    # set the flag to overwrite with default one-row table, if requested
    if(overwrite) {csv.wipe = TRUE}
    
  } else {
    
    # entries.list is a list. Check for non-character vector inputs
    if(!all(sapply(entries.list, is.character)))
    {
      stop('all elements of entries.list must be character vectors')
    }
    
    # halt on incorrectly named vectors
    if(!all(sapply(entries.list, function(entry) all(names(entry)==entry.names))))
    {
      stop(paste0('each element of entries.list must be a named vector with names: ', paste(entry.names, collapse=', ')))
    }
    
    # entries.list is valid input. Construct the data frame
    input.df = data.frame(do.call(rbind, entries.list), row.names='name')
  }
  
  # data.frame() appears to ignore row.names when nrows==1. Detect this and fix it
  if(!is.null(input.df$name))
  {
    rownames(input.df) = input.df$name
    input.df = input.df[,-which(names(input.df)=='name')]
  }
  
  # if not reading a CSV from disk, we're finished
  if(!use.file)
  {
    return(input.df)
    
  } else {
    
    # create a second data frame to store any data from the csv on disk
    csv.df = my_metadata(script.name, entries.list=list(entry.default), use.file=FALSE)
    
    # look for the file on disk and load it if it exists
    if(file.exists(here(csv.relpath)) & !csv.wipe)
    {
      # load the csv data 
      csv.df = read.csv(here(csv.relpath), header=TRUE, row.names=1)
    }
    
    # identify any entries in csv.df with names matched in entries.list, update them, delete those rows from input.df
    names.updating = rownames(csv.df)[rownames(csv.df) %in% rownames(input.df)]
    csv.df[names.updating,] = input.df[names.updating,]
    input.df = input.df[!(rownames(input.df) %in% names.updating),]
    
    # merge the two data frames
    output.df = rbind(input.df, csv.df)
    if(overwrite)
    {
      # CSV file is written to disk
      if(v) print(paste('> writing metadata to:', csv.relpath))
      write.csv(output.df, here(csv.relpath))
      return(output.df)
      
    } 
    
    return(output.df)
    
  }
}


#' My R scripts are commented using a roxygen2 syntax that is interpretable by `rmarkdown`,
#' for conversion to markdown via pandoc. This convenience function renders the markdown file
#' for a given R script and writes to a file of the same name (but with a .md extension).
my_markdown = function(script.name, script.dir='R', markdown.dir='markdown')
{
  # ARGUMENTS:
  #
  # `script.name` is a string indicating the filename (without the .R extension) of the R script to document.
  # `script.dir` is a string indicating which folder in the project directory contains the R script.
  # `markdown.dir` is a string indicating a folder in the project directory to write the output file.
  #
  # RETURN VALUE:
  #
  # (null)
  #
  # BEHAVIOUR: 
  #
  # Writes the file <project directory>/<script.dir>/<script.name>.R, overwriting without warning
  
  # set up in/out files
  path.input = here(script.dir, paste0(script.name, '.R'))
  path.output = here(file.path(markdown.dir, paste0(script.name, '.md')))
  
  # note: run_pandoc=FALSE appears to cause output_dir to be ignored. So this call also generates an html file
  paste('rendering markdown file', path.output, 'from the R script', path.input)
  rmarkdown::render(path.input, clean=TRUE, output_file=path.output)
}


#' This function is used in `get_weatherstations` to parse the time series dates
# convert start/end year columns from GHCN data to a single (string) column for each "element"
my_ghcnd_reshape = function(idval, elemval)
{
  # query this combination of station id and element string in the full GHCND table
  idx.row = (ghcnd.df$id == idval) & (ghcnd.df$element == elemval)
  
  # if it exists, return a string of form "start-end", otherwise NA
  return(ifelse(!any(idx.row), NA, paste(ghcnd.df[idx.row, c('first_year', 'last_year')], collapse='-')))
}

#' This function is used by `get_meteo` to extract data from Ben Livneh's meteorological
#' reconstruction dataset (NetCDF format), returning R data frames and `raster`/`sf` objects 
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

#' This function is used by `get_meteo` to extract data from the ORNL Daymet meteorological
#' reconstruction dataset (NetCDF format), returning R data frames and `raster`/`sf` objects
#' It is very similar to `my_livneh_reader`, except that the Daymet files are not bzipped,
#' and different meteorological variables are stored in different files.
#' 
#' In the current CRAN version of the `daymetr` package (v1.4), there is a bug related to a
#' mislabeling of projection information by the web coverage service (WCS) at ORNL's DAAC.
#' Read about it in the bug reports [here](https://github.com/bluegreen-labs/daymetr/issues/40),
#' [here](https://github.com/ropensci/FedData/issues/50), and
#' [here](https://github.com/bluegreen-labs/daymetr/issues/36)).
#' 
#' We fix this by manually rewriting the proj4 string after it is loaded into R. Unfortunately
#' this results in many warnings about invalid CRS definitions (these can be safely ignored) 
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


#' get a listing of stations with records available through NRCS National Water Climate Center
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
  data.df$elev = units::set_units(units::set_units(data.df$elev, ft), m)
    
  # write as CSV if requested
  if(!is.null(csvpath)) write.csv(data.df, csvpath, row.names=FALSE)
  
  # finished
  return(data.df)
  
}
  
#' download datasets as CSV from the NRCS National Water Climate Center
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

#' open a CSV file from the NRCS National Water Climate Center
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
      site.df[,cn] = units::set_units(site.df[,cn], unit.symbol, mode='standard')
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

#' open a batch of CSV files from the NRCS National Water Climate Center
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


#' Some functions for dealing with SNODAS grids
my_get_snodas = function()
{
  
  ftp.url = 'ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/'
  
  
  
}




#' This is a kludge combining various `FedData` functions with some of my own code, in order to import STATSGO2 data and produce
#' output similar to FedData::get_ssurgo. It uses data from [NRCS](https://nrcs.app.box.com/v/soils), which is aggregated
#' at the state level. Instead of a `template` or SSA code argument, it takes a state code, such as 'mt'. Note that not all states 
#' are available at this time (see here for the list)
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


#' This function parses a SSURGO/STATSGO2 style list of tables to extract the soil data needed for SWAT+. These data
#' are reshaped into a single table `usersoil` (the return value) that maps mukeys to the relevant information on the 
#' dominant soil component (ie the component with maximal comppct_r). The output table is formatted in the style
#' expected for the CSV file "usersoil.csv" in SWAT+ AW, with an additional column, `pmiss` indicating for each 
#' mukey the percent of soil variables which are missing (NA).
#' 
#' The code for this function is adapted from the missing data filler script in
#' [this post](https://hydrologicou.wordpress.com/r-script-to-generate-missing-records-in-acrswat-ssurgo-database/),
#' which in turn is based on snippets from
#' [this example](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?root=aqp).
#' 
#' 
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


#' This function returns a (case-insensitive) expression-matching lookup table for 
#' matching NVC plant community classifications to SWAT+ plant codes. The former are documented
#' [here](http://usnvc.org/data-standard/natural-vegetation-classification), and the latter can
#' be found in the `plants_plt` table of the SWAT+ 'datasets' SQLite file.
#' 
my_plants_plt = function(nvc.df)
{
  # ARGUMENTS:
  #
  # `nvc.df`: a dataframe with columns `NVC_DIV`, `NVC_MACRO`, and `NVC_GROUP`
  #
  # RETURN VALUE:
  #
  # the `nvc.df` dataframe with two appended columns, `swatcode` and `swatdesc`, containing
  # the matching SWAT+ plant code and its description 
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
    
    # generic for sparsely vegetated or barren land
    data.frame(swatdesc='barren',
               swatcode='barr',
               kwdiv='barren|urban|mines'),
    
    # generic for sparsely vegetated land
    data.frame(swatdesc='barren_or_sparsley_vegetated',
               swatcode='bsvg',
               kwgroup='scree|badland'),
    
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

#' It took a bit of work to construct the function above: The following may be helpful for improving
#' the lookup table, or when extending it to new areas, and/or new releases of SWAT+ and the NCV.
#'  
#' To begin cross-referencing SWAT+ plant codes with the NVC classifications, we start with
#' packages [`DBI`](https://cran.r-project.org/web/packages/DBI/vignettes/DBI-1.html) and
#' [`RSQLite`](https://github.com/r-dbi/RSQLite) to interface with SWAT+ SQLite databases
#' For example, to load the `plants.plt` in "swatplus_datasets.sqlite", do:
# library(DBI) 
# library(RSQLite)
# swat.ds.conn = dbConnect(SQLite(), 'H:/UYRW_SWAT/SWATPlus/Databases/swatplus_datasets.sqlite')
# plants_plt = dbReadTable(swat.ds.conn, 'plants_plt')
# dbDisconnect(swat.ds.conn)

#' Some (now incomplete) reference information on the `plants_plt` data can be found in
#' [Appendix A of the SWAT IO file docs (2012)](https://swat.tamu.edu/media/69419/Appendix-A.pdf).
#' Since SWAT+ is fairly new, and under active development, I would expect this document to be
#' updated in the near future. For now, some guesswork was need to match the `description`
#' field to each [NVC group category](http://usnvc.org/explore-classification/) in the UYRW.
#' 
#' [`data.tree`](https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html) can 
#' be very helpful for parsing this kind of data, with clean printouts of nested lists of strings.
#' 

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

#' compute Nash–Sutcliffe model efficiency coefficient (NSE) 
my_nse = function(qobs, qsim, L=2, normalized=FALSE)
{
  # compute the standard NSE coefficient
  nse = 1 - ( sum( abs(qsim - qobs)^L ) / sum( abs(qobs - mean(qobs))^L ) )
  
  # normalize, if requested
  if(normalized)
  {
    nse = 1 / (2 - nse)
  }
  
  return(nse)
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
    bdepth = units::set_units(bdepth, m)
    
    # make sure the streams network contains only LINESTRING geometries, drop any attributes
    bsf = st_geometry(bsf[st_is(bsf, 'LINESTRING'),])
    
    # convert streams to `Spatial`, add unit dummy field, rasterize
    bsf.sp = as(bsf, 'Spatial')
    bsf.sp$dummy = rep(1, length(bsf.sp))
    gRasterize(bsf.sp, dem, field='dummy', filename=here(taudem.meta['bsf', 'file']))
    
    # find cell numbers of non-NA cells in the streams raster 
    bsf.idx = Which(!is.na(raster(here(taudem.meta['bsf', 'file']))), cells=TRUE) 
    
    # extract elevations at all cells intersecting with a stream
    bsf.elev = units::set_units(extract(dem, bsf.idx), m)
    
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
  cell.area = units::set_units(prod(res(dem))/1e6, km^2)
  dem.area = ncell*cell.area
  
  # drop analysis is skipped when `nstream` threshold argument is provided
  do.dropanalysis = FALSE
  
  # handle unassigned stream delineation threshold
  if(! nstream > 0)
  {
    # zero or negative `nstream` triggers drop analysis
    do.dropanalysis = TRUE
    
    # set a range of thresholds to test by drop analysis
    nstream.min = max(units::set_units(0.1, km^2), 2*cell.area)
    nstream.max = min(units::set_units(100, km^2), dem.area/10)
    
    # corresponding number of cells
    nstream.min = as.integer(nstream.min/cell.area) + 1
    nstream.max = as.integer(nstream.max/cell.area) + 1
    
    # set default initial threshold (if `nstream` provided, set to 10X that)
    astream = units::set_units(1, km^2)
    
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
  snap = units::set_units(snap, m)
  
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
  areamin = units::set_units(ifelse(is.null(areamin), 0, areamin), km^2)
  
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

#' assembles requisite inputs for a QSWAT+ project
my_setup_qswatplus = function(id, catchments, projdir=NULL, wipe=FALSE, config=NULL, quiet=FALSE)
{
  # `id`: integer, the 'catchment_id' of the subwatershed in `catchments$boundary`
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
  
  # NAs are represented in the GeoTiff by an integer -- usually something large and negative.
  # The default value for this integer when using `raster::writeRaster` is so large that
  # it can cause an integer overflow error in one of the python modules used by QSWAT+ (at 
  # least on my 64bit machine). We instead use the value recommended in the QSWAT+ manual:
  tif.na.val = -32767
  
  # unpack input
  boundary = catchments$boundary[catchments$boundary$catchment_id == id, ]
  io = catchments$io[catchments$io$catchment_id == id, ]
  pts = catchments$pts[catchments$pts$catchment_id == id, ]
  demnet = catchments$demnet[catchments$demnet$catchment_id == id, ] %>% na.exclude
  
  # for now we are testing with the PNWNAMet weather data
  wname = 'pnwnamet_uyrw'
  
  # look up input data file locations
  dem.path = here(my_metadata('get_dem')['swat_dem', 'file'])
  soils.path = here(my_metadata('get_soils')['swat_soils_tif', 'file'])
  landuse.path = here(my_metadata('get_landuse')['swat_landuse_tif', 'file'])
  landuselu.path = here(my_metadata('get_landuse')['swat_landuse_lookup', 'file'])
  wdat.path = here(my_metadata('get_meteo')[wname, 'file'])
  
  # set default project directory
  if( is.null(projdir) ) 
  {
    # default name from USGS gage site name
    projnm = paste0('swat_', boundary$catchment_name)
    
    # default location from parent of dem raster file
    projdir = here(file.path(dirname(dem.path), projnm))
  } 
  
  # project name is the same as project directory name
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
  qswat.meta = my_metadata(projnm, files.towrite, overwrite=TRUE, data.dir=datadir, v=!quiet)
  if(!quiet) print(qswat.meta[, c('type', 'description')])

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
  boundary.buff = st_buffer(boundary, dist=units::set_units(5, km))
  include = as.vector(st_intersects(wdat$coords_sf, boundary.buff, sparse=FALSE))
  
  # load DEM and call the weather station data export function
  if(!quiet) print('writing weather station data files...')
  invisible(my_swat_wmeteo(wdat, exdir=wdat.dir, form='qswat', include=include, quiet=quiet))
  
  # TODO: change my_metadata() (and all dependencies) to use JSON?
  # convert the metadata csv to JSON (adding line breaks for readability)
  qswat.meta.out = cbind(name=rownames(qswat.meta), data.frame(qswat.meta, row.names=NULL))
  
  # derive initial (dummy) start/end dates from first 2 days of available weather
  wdat.start = min(wdat$dates)
  
  # TODO: put these defaults in a JSON file in /data
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
        description='initial value of end day for SWAT+ simulations')
      
    )
  }
  
  # reshape as dataframe
  config.def.out = data.frame(do.call(rbind, config.def.list))

  # overwrite with user supplied settings 
  if( !is.null(config) )
  {
    # TODO

  } 
  
  # copy the metadata csv and `config` to JSON
  config.path = qswat.meta['config', 'file']
  config.out = rbind(qswat.meta.out, config.def.out)
  writeLines(toJSON(config.out, pretty=TRUE), config.path)

  # finish
  return(config.out)
}

#' run the QSWAT+ workflow for a project created by `my_prepare_qswatplus`
my_run_qswatplus = function(jsonpath, quiet=FALSE)
{
  # `quiet`: logical, suppresses console messages
  # 'jsonpath': character, the path to the JSON config file created by `my_prepare_qswatplus`
  #
  # or the dataframe returned by that function call
  
  # handle dataframe input
  if(is.data.frame(jsonpath)) jsonpath = qswat_meta$file[qswat_meta$name=='config']

  # path to the python launcher
  exepath = 'H:/YERC/swat/run_qswatplus.cmd'
  
  # call the launcher with this JSON file (runs a python script)
  system2(exepath, normalizePath(jsonpath), stdout=ifelse(quiet, FALSE, ''))
  if(!quiet) cat('\n>> finished')
}

#' run the SWAT+ executable
my_run_simulation = function(textio, exe=NULL, quiet=FALSE)
{
  # `quiet`: logical, suppresses console messages
  # 'textio': character, path to the text I/O directory for the SWAT+ model 
  # 'exe', character string path to swat executable (.exe, likely in SWATEditor directory)
  
  # TODO: detect this in pyqgis module
  # set the default executable path
  if(is.null(exe)) exe = 'C:/SWAT/SWATPlus/Workflow/editor_api/swat_exe/rev59.3_64rel.exe'
  
  # shell command prefix to change to the directory (avoids changing R's working directory)
  shell.prefix = paste0('pushd ', normalizePath(textio), ' &&')
  
  # build system call and run SWAT
  syscall.string = paste(shell.prefix, tools::file_path_sans_ext(normalizePath(exe)))
  invisible(shell(syscall.string, intern=quiet))
  if(!quiet) cat('\n>> finished')
}



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

#+ include=FALSE
#my_markdown('get_helperfun')
