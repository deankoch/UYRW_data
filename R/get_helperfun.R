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


#' NAs are represented in the GeoTiff by an integer -- usually something large and negative.
#' The default value for this integer when using `raster::writeRaster` is so large that
#' it can cause an integer overflow error in one of the python modules used by QSWAT+ (at 
#' least on my 64bit machine). We instead use the value recommended in the QSWAT+ manual:
tif.na.val = -32767

#' This project will generate many files. To keep track of everything, each script gets a CSV table documenting every
#' file that it creates: its file path, its type, and a short description of the contents. This function handles the construction
#' of the table. To call up the table for a specific script, simply use `my_metadata(script.name)`.
#' 
# creates and/or adds to a data frame of metadata documenting a given script, and (optionally) writes it to disk as a CSV file 
my_metadata = function(script.name, entries.list=NA, overwrite=FALSE, use.file=TRUE, data.dir='data')
{
  # ARGUMENTS:
  #
  # `script.name` is a string indicating the filename (without the .R extension) of the R script to document.
  # `entries.list` is a list of character vectors, whose entries are named: 'name', 'file', 'type', and 'description'.
  # `data.dir` is the subdirectory of the project folder in which to write the CSV file: /data/`script.name`_metadata.csv 
  # `use.file` is a boolean indicating whether to read/write the CSV file
  # `overwrite` allows an existing CSV file to be modified 
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
      print(paste('writing to', csv.relpath))
      write.csv(output.df, here(csv.relpath))
      return(output.df)
      
    } 
    
    return(output.df)
    
  }
}


#' My R scripts are commented using a roxygen2 syntax that is interpretable by `rmarkdown`. This convenience function
#' renders the markdown file for a given R script and writes to a file of the same name (but with a .md extension).
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

#' In addition to having different file and data structures, SWAT2012 and SWAT+ (currently) differ
#' in the default parameter sets that are included in each release. For example, while most of the
#' plant growth parameters appearing SWAT2012's reference database 'crop' table can be found in the
#' SWAT+ database, there are many plant codes in SWAT+ that have not yet been added to SWAT2012.
#' 
#' This helper function translates the relevant data from the SWAT+ reference database into a form
#' that can be written to the SWAT2012 reference database (mdb), so the missing plant codes can be
#' used in a SWAT2012 model
#' 
my_plants2swat = function(crop_in, swatplus_db)
{
  # ARGUMENTS:
  #
  # `crop_in`: 'crop' table (as data frame) from the SWAT2012's MS Access (mdb) reference database
  # `swatplus_db`: path to the mySQL (sql) reference database 
  #
  # RETURN VALUE:
  #
  # A named list with two entries;
  #
  # `crop_out`: data frame in the same form as `crop_in`, appended with any data on plant codes
  # found in `swatplus_db` but missing from `crop_in`. 
  #
  # `lookup`: a data frame matching the plant codes assigned in `crop_out` to their original
  # values in the SWAT+ database. These plant codes are modified only when it is necessary to
  # satisfy SWAT2012's 4-letter naming requirement. Auto-generated names are all of the form 'ZZ**'
  #
  # BEHAVIOUR: 
  #
  # Note that two fields in the 'crop' table, 'FERTFIELD' and 'OpSchedule', are assigned the default
  # values of 0 and 'RNGB' in all appended rows, as we could not find suitable surrogates in
  # `swatplus_db`. These fields appear to be missing from the SWAT theory and I/O documentation. As
  # far as we can tell, they are unimportant to our implementation of SWAT/SWAT+
  
  # open the SWAT+ database to pull a copy of four tables containing land use parameters
  swatplus.con = RSQLite::dbConnect(RSQLite::SQLite(), swatplus_db)
  
  # ... the main data table...
  plants.plt = dbReadTable(swatplus.con, 'plants_plt')
  
  # ... and linked tables for Manning's 'n' for overland flow, runoff curve numbers
  landuse.lum = dbReadTable(swatplus.con, 'landuse_lum')
  ovn.lum = dbReadTable(swatplus.con, 'ovn_table_lum')
  cn.lum = dbReadTable(swatplus.con, 'cntable_lum')
  
  # close the connection
  RSQLite::dbDisconnect(swatplus.con)
  
  # The SWAT+ database table 'plants_plt' (copied into R as `plants.plt`) contains most of the
  # parameters needed in the 'crop' table in the SWAT2012 database. The field names are different,
  # however, so these need to be translated. A handful of parameters also need to be fetched from
  # other tables. These are linked to 'plants_plt' via 'landuse_lum'
  
  # identify plant codes common to both databases
  codes.common = crop_in$CPNM[crop_in$CPNM %in% toupper(plants.plt$name)]
  
  # identify plant codes in 'plants_plt' which are missing from SWAT2012 database
  codes.toadd = plants.plt$name[!(toupper(plants.plt$name) %in% codes.common)]
  
  # fetch variables located outside `plants.plt`, append as new columns
  ovn.vn = 'ovn_mean'
  cn.vn = c('cn_a', 'cn_b', 'cn_c', 'cn_d')
  idx.lum = match(plants.plt$name, sapply(landuse.lum$name, function(nm) strsplit(nm, '_lum')[[1]]))
  plants.plt[, ovn.vn] = ovn.lum[landuse.lum$ov_mann_id[idx.lum], ovn.vn]
  plants.plt[, cn.vn] = cn.lum[landuse.lum$cn2_id[idx.lum], cn.vn]
  
  # identify 'trees' and translate 'plnt_typ' to integer code (see 'swat2009-theory.pdf', p.311)
  plants.plt$plnt_typ[plants.plt$bm_tree_acc>0 & plants.plt$bm_tree_max>0] = 'trees'
  idc.codes = c(NA, NA, NA, 'warm_annual', 'cold_annual', 'perennial', 'trees')
  plants.plt$plnt_typ = match(plants.plt$plnt_typ, idc.codes)
  
  # add dummy data to two missing fields
  plants.plt$FERTFIELD = rep(0, nrow(plants.plt))
  plants.plt$OpSchedule = rep('RNGB', nrow(plants.plt))
  
  # All of the necessary data has now been added to `plants.plt`. Next we trim this table to include
  # only plant codes which are missing from `crop_in` and modify field names and row keys to match
  # with the syntax of SWAT2012's 'crop'.
  
  # create a translation table (SWAT+/plants_plt -> SWAT2012/crop)
  swat2012.lu = c(CPNM='name',
                  IDC='plnt_typ',
                  CROPNAME='description',
                  BIO_E='bm_e',
                  HVSTI='harv_idx',
                  BLAI='lai_pot',
                  FRGRW1='frac_hu1', 
                  LAIMX1='lai_max1', 
                  FRGRW2='frac_hu2', 
                  LAIMX2='lai_max2', 
                  DLAI='hu_lai_decl', 
                  CHTMX='can_ht_max', 
                  RDMX='rt_dp_max', 
                  T_OPT='tmp_opt', 
                  T_BASE='tmp_base', 
                  CNYLD='frac_n_yld',
                  CPYLD='frac_p_yld',
                  BN1='frac_n_em',
                  BN2='frac_n_50',
                  BN3='frac_n_mat',
                  BP1='frac_p_em',
                  BP2='frac_p_50',
                  BP3='frac_p_mat',
                  WSYF='harv_idx_ws',
                  USLE_C='usle_c_min',
                  GSI='stcon_max',
                  VPDFR='vpd',
                  FRGMAX='frac_stcon',
                  WAVP='ru_vpd',
                  CO2HI='co2_hi',
                  BIOEHI='bm_e_hi',
                  RSDCO_PL='plnt_decomp',
                  OV_N='ovn_mean',
                  CN2A='cn_a',
                  CN2B='cn_b',  
                  CN2C='cn_c',  
                  CN2D='cn_d', 
                  FERTFIELD='FERTFIELD',
                  ALAI_MIN='lai_min',
                  BIO_LEAF='bm_tree_acc',
                  MAT_YRS='yrs_mat',
                  BMX_TREES='bm_tree_max',
                  EXT_COEF='ext_co',
                  BM_DIEOFF='bm_dieoff',
                  OpSchedule='OpSchedule')
  
  # translate column names of `plants.plt` (SWAT+) to those of `crop` (SWAT2012)
  plants.tocrop = plants.plt[plants.plt$name %in% codes.toadd, swat2012.lu]
  colnames(plants.tocrop) = names(swat2012.lu)
  
  # append new row key columns
  keys.tocrop = (1:length(codes.toadd)) + nrow(crop_in) + 1
  plants.tocrop = cbind(data.frame(OBJECTID=keys.tocrop, ICNUM=keys.tocrop), plants.tocrop)
  
  # generate new 4-letter codes (of form 'ZZ**') to replace longer SWAT+ plant codes
  idx.namechange = nchar(plants.plt$name) > 4 
  seq.namechange = (1:sum(idx.namechange)) - 1
  suffix.namechange = cbind(LETTERS[(seq.namechange %/% 26) + 1], LETTERS[(seq.namechange %% 26) + 1])
  codes.namechange = paste0('ZZ', apply(suffix.namechange, 1, paste, collapse=''))
  
  # lookup vector that switches all codes to uppercase, and subs in 4-letter versions as needed
  codes.new = setNames(toupper(plants.plt$name), nm=plants.plt$name)
  codes.new[idx.namechange] = codes.namechange
  plants.tocrop$CPNM = codes.new[match(plants.tocrop$CPNM, names(codes.new))]
  
  # return output as list
  return(list(crop_out=plants.tocrop, lookup=codes.new))
  
}


#' Convenience functions below are for handling I/O text files associated with SWAT2012
#' simulations, and for managing parameters and system calls to the SWAT executables

#' reads daily simulation output text files from SWAT+
my_read_output = function(out.path, varname=character(0), set.units=TRUE, add.dates=TRUE)
{
  # ARGUMENTS:
  #
  # `out.path`, character string, full path to one of the SWAT+ output (.txt) files
  # `varname`, character string vector, names of the variables to load (or 'all')
  # `set.units`, boolean indicating whether to assign units to output columns
  # `add.dates`, boolean indicating whether to append a Date object column
  #
  # RETURN VALUE:
  #
  # If `varname` is unspecified, returns a list containing the time period and variable
  # names available, as well as the vector of unique geometry ID codes (`set.units`, and
  # `add.dates` are ignored in this case).
  #
  # If `varname` is a string, or vector of strings, returns a data frame containing the 
  # numerical data from any and all matching variable names. 
  #
  # DETAILS:
  # 
  # Any unmatched string(s) in `varname` are ignored, with the exception of `varname='all'`,
  # which prompts the function to return all available variables.
  #
  # Three indexing variables are included in every data frame: 'jday' (Julian day in year),
  # 'yr' (year), and 'gis_id' (ID code for this feature in QSWAT+ shapefiles). For non-
  # indexing variables, the columns of the data frames are of type Unit, with units assigned
  # based on the headers data of the output text file.
  #
  # `add.dates==TRUE` consolidates the 'jday', 'mon', 'day', 'yr' columns into a single 
  # vector of Date objects (ie they are omitted from the data frame). 
  #
  
  # line numbers of the headers for variable names and units
  ln.header = c(varname=2, unit=3)
  
  # scan the headers line to get all listed output variable names
  out.varname = scan(out.path, what=character(), skip=ln.header['varname']-1, nlines=1)
  out.unit = scan(out.path, what=character(), skip=ln.header['unit']-1, nlines=1)
  n.varname = length(out.varname)
  
  # the first few columns are keys for location/time - their names appear on the units line
  n.key = length(out.unit) - n.varname
  is.key = 1:length(out.unit) %in% 1:n.key
  key.varname = out.unit[is.key]
  out.unit = out.unit[!is.key]
  
  # replace unusual units with strings that R can understand and name them to match variables
  out.unit[out.unit == 'ha-m'] = 'ha m'
  out.unit[out.unit %in% c('kgN', 'kgP')] = 'kg'
  names(out.unit) = out.varname
  
  # build a list of column types for keys, then for the rest of the data matrix
  key.class = c(rep('integer', n.key-1), 'character')
  out.class = rep('numeric', n.varname)
  
  # complete set of column classes and names
  load.what = c(key.class, out.class)
  all.varname = c(key.varname, out.varname)
  
  # three of the key columns will be imported even when they aren't specified in `varname`
  key.include = c('jday', 'yr', 'gis_id')
  
  # two special `varname` assignments prompt the function to parse all available variables 
  if(all(varname == 'all') | length(varname)==0)
  {
    # default behaviour with `varname` unassigned is to scan for info about a simulation
    if(length(varname)==0)
    {
      # retrieve important key columns only (ignore numerical data)
      varname = key.include
      return.data = FALSE
      
    } else {
      
      # this mode reads the entire file returning all data
      varname = all.varname
      return.data = TRUE
      
    }
    
  } else {
    
    # important keys are appended to any supplied variable name arguments
    varname = unique(c(key.include, varname))
    return.data = TRUE
    
  }
  
  # load the requested columns and efficiently skip the others using `fread`
  load.idx = all.varname %in% varname
  load.varname = all.varname[load.idx]
  dat.out = as.data.frame(setNames(fread(out.path, 
                                         header=FALSE, 
                                         skip=max(ln.header),  
                                         colClasses=load.what, 
                                         drop=which(!load.idx)), load.varname))
  
  # collect info from key columns
  n.obs = nrow(dat.out)
  gis.ids = unique(dat.out$gis_id)
  start.date = as.Date(paste(dat.out$yr[1], dat.out$jday[1], sep='-'), format='%Y-%j')
  end.date = as.Date(paste(dat.out$yr[n.obs], dat.out$jday[n.obs], sep='-'), format='%Y-%j')
  dates = c(first=start.date, last=end.date)
  n = c(n_obs=n.obs, n_days=n.obs/length(gis.ids))
  
  # return this information in default mode
  if(!return.data)
  {
    # return the info in a list
    return(list(n=n, dates=dates, name=all.varname, gis_ids=gis.ids))
    
  }
  
  # pull column names before making final changes
  dat.names = names(dat.out)
  
  # units are assigned to the state variables if requested
  if(set.units)
  {
    # loop over column names matching non-key (ie state variable) values
    for(idx.col in which(!(dat.names %in% key.varname)))
    {
      # units were read from headers line
      unit.col = out.unit[names(dat.out)[idx.col]]
      dat.out[,idx.col] = units::set_units(dat.out[,idx.col], unit.col, mode='standard')
    }
  }
  
  # assign dates if requested 
  include.names = dat.names
  if(add.dates)
  {
    # define the sequence of dates appearing in the dataset
    ts.dates = seq(start.date, end.date, 'day')
    
    # this should be equivalent to `as.Date(paste(yr, mon, day, sep='-')`, but faster
    all.dates = rep(ts.dates, each=n['n_obs']/n['n_days'])
    
    # append to `dat.out` as new column
    dat.out = setNames(cbind(all.dates, dat.out), c('date', dat.names))
    
    # omit redundant columns
    include.names = names(dat.out)[!(names(dat.out) %in% c('jday', 'mon', 'day', 'yr'))]
  }
  
  return(dat.out[, include.names])
}

#' reads simulation output text files from SWAT2012: output.rch, output.sub, output.hru
my_swat_rout = function(output.path, varname=character(0), origin.date=character(0))
{
  # ARGUMENTS:
  #
  # `output.path`, character string, path to either 'output.rch', 'output.sub', or 'output.hru'
  # `varname`, character string vector of variable names to load from the output file
  # `origindate`, (optional) 'Date' class object specifying first day of simulation period
  #
  # RETURN VALUE:
  #
  # A data frame containing the data from the files at `output.path` (with comment lines 1-9
  # omitted). By default returns the entire dataset, but a subset of columns can be specified in
  # `varname`.
  #
  # If `origindate` is supplied, the function appends a column containing the dates for
  # each data row (note that this assumes a daily time step). It also returns the 'RCH', 'MON'
  # columns (if they are not already specified in varname)
  
  # line number of the headers
  out.idx = 9
  
  # identify the output text file type
  out.ext = gsub('.', '', extension(output.path), fixed=TRUE)
  
  # define key fields that vary by output type
  key.names = list(rch=c('RCH', 'GIS', 'MON'), 
                   sub=c('SUB'), 
                   hru=c('LULC', 'HRU', 'GIS', 'SUB', 'MGT'))[[out.ext]]
  
  # scan the headers line and note that R can't parse the delimiters in use here
  out.header = paste0(scan(output.path, what=character(), skip=out.idx-1, nlines=1), collapse=' ')
  
  # bugfix for 'LAT Q' variable in .sub files: switch to 'LAT_Q' 
  out.header = gsub('LAT Q(mm)LATNO3', 'LAT_Q ', out.header, fixed=TRUE)
  
  # use regexp to identify whitespace and units delimiting individual variables
  rch.regexp = '(km2)|(cms)|(tons)|(mg/L)|(kg)|(mg)|(ct)|(TOT)|(Mg/l)|(degc)'
  sub.regexp = '(mm)|(kg/ha)|(kg/h)|(t/ha)|(mic/L)'
  hru.regexp = '(dgC)|(MJ/m2)'
  out.regexp = paste0(c('(\\s)', rch.regexp, sub.regexp, hru.regexp), collapse='|')
  out.header.parsed = strsplit(out.header, out.regexp)[[1]]
  out.header.parsed = out.header.parsed[nchar(out.header.parsed) > 0]
  
  # bugfix: field 'MON' is listed in header of hru and sub files, but not in numeric table
  if(out.ext != 'rch') { out.header.parsed = out.header.parsed[out.header.parsed!='MON'] }
    
  # bugfix: first column is unnamed in sub and rch files, but it appears to be a dummy variable
  if(out.ext != 'hru') { out.header.parsed = c('DUMMY', out.header.parsed) }
  
  # default behaviour is to load all fields except 'DUMMY'
  if(length(varname)==0)
  {
    varname = out.header.parsed[out.header.parsed != 'DUMMY']
    
  } else {
    
    # the integer fields should always be returned
    varname = unique(c(key.names, varname))
    
  }
  
  # count the number of requested fields
  n.col = length(out.header.parsed) 
  
  # build a list of column types - most are doubles
  fread.what = rep('numeric', n.col)
  fread.what[out.header.parsed == 'DUMMY'] = 'character'
  fread.what[out.header.parsed %in% key.names] = 'integer'
  fread.what[out.header.parsed == 'LULC'] = 'character'
  
  # verify that the requested variable name was printed to this output file
  idx.varname.found = varname %in% out.header.parsed
  if(!all(idx.varname.found))
  {
    err.msg1 = paste('The following variable name(s) not found in', basename(output.path), ':')
    err.msg2 = paste(varname[!idx.varname.found], collapse=', ')
    stop(paste(err.msg1, err.msg2, collapse='\n'))
  }
  
  # reorder varname to match ordering in headers
  varname = varname[order(match(varname, out.header.parsed))]
  
  # index the columns to keep and drop
  idx.read = out.header.parsed %in% varname
  idx.drop = which(!idx.read)
  
  # load the requested columns using fread
  dat.out = fread(output.path, 
                  header=FALSE, 
                  skip=out.idx, 
                  col.names=varname, 
                  colClasses=fread.what, 
                  drop=idx.drop)
  
  # warn of any coercion errors in the numeric data and replace invalid entries with NA
  idx.col.invalid = sapply(dat.out, class) != fread.what[idx.read]
  if(any(idx.col.invalid))
  {
    # identify the variables and print the warning
    names.col.invalid = names(dat.out)[idx.col.invalid]
    print('warning: type coercion failed in the following variable(s)')
    print(names.col.invalid)
    print('coercing to numeric...')
    
    # perform the coercion
    invisible(sapply(names.col.invalid, function(vn) {
      set(dat.out, j=varname, value=as.numeric(dat.out[[vn]]))
    }))
    
  }
  
  # if dates requested, append the row, then finish
  if(length(origin.date)>0)
  {
    n.id = length(unique(dat.out[[toupper(out.ext)]]))
    n.days = nrow(dat.out)/n.id
    dates.out = rep(seq.Date(as.Date(origin.date), by='day', length.out=n.days), each=n.id)
    return(cbind(date=dates.out, dat.out))
    
  } else {
    
    return(dat.out)
    
  }
  
}

#' system call to run a SWAT simulation
my_call_swat = function(textio.dir, exec_path)
{
  # ARGUMENTS:
  #
  # `textio.dir`, character string path to the text input file directory of a SWAT2012 model
  # `exec_path`, character string path to swat executable (.exe, likely in SWATEditor directory)
  #
  
  # shell command prefix to change to the directory (avoids changing R's working directory)
  shell.prefix = paste0('pushd ', normalizePath(textio.dir), ' &&')
  
  # build system call and run SWAT
  syscall.string = paste(shell.prefix, tools::file_path_sans_ext(normalizePath(exec_path)))
  shell(syscall.string)
}

#' parse SWAT text file lines to return: value, name, comment fields for non-vector parameters
my_swat_readpar = function(linetext)
{
  # ARGUMENTS:
  #
  # `linetext`, character string vector, the (line by line) raw text from a SWAT config file 
  #
  # RETURN VALUE:
  #
  # dataframe with nrows equal to the number of lines in `linetext` that match a standard
  # non-vector variable assignment pattern (see DETAILS). The five named columns are:
  #
  # line = (integer) line number, indicates entry of `linetext` that was parsed
  # name = (character) the parameter name (usually UPPERCASE)
  # value = (character) the parameter value
  # comment = (character) the comment string (if any)
  # type = (character) either 'character', 'integer', 'numeric', or 'unknown'
  #
  # DETAILS:
  #
  # Most SWAT parameters appear as lines of the form "<value>|<name>:<comment>", where <value>
  # may be of type character, integer, or real. This function parses a line of text in this
  # syntax, separating the three components, trimming whitespace, and returning the result as
  # character.
  #
  # Type detection is based on the presence/absence of non-numeric characters and the decimal
  # symbol ('.'). According to the SWAT docs, the distinction between integers are reals is
  # important - eg. there could be problems associated with writing the integer 3 as a float
  # such as "3.00" (and vice versa).
  # 
  # When there is no template value assigned in the input `linetext` to pattern-match, the 
  # function looks for a comment that ends with 'file' (indicating 'character' type), and
  # failing that, it assigns 'unknown' type.
  # 
  
  # regexp to find standard pattern
  idx.match = grepl('(.+)(\\|)(.+)(:)(.+)', linetext)
  linetext.m = linetext[idx.match]
  
  # split into <value><name><comment> (literal '0-1' is bugfix for missing ':' in 'BFLO_DIST')
  out.ms = strsplit(linetext.m, '(\\s*\\|\\s*)|(\\s*:\\s*)|(\\s0-1\\s)', perl=TRUE)
  
  # trim leading and trailing whitepace
  out.mst = lapply(out.ms, function(vec) gsub('($\\s*)|(^\\s*)', '', vec, perl=TRUE))
  
  # reshape into matrix, fuse comments that got split because they have a ':' symbol
  out.list = lapply(out.mst, function(vec) c(vec[2:1], paste(vec[-(1:2)], collapse=': ')))
  
  # initialize output dataframe
  df.out = cbind(which(idx.match), data.frame(do.call(rbind, out.list)))
  names(df.out) = c('line', 'name', 'value', 'comment')
  
  # initialize output type column with 'integer'
  df.out$type = 'integer'
  
  # determine character and numeric value types
  df.out$type[grepl('[0-9,A-z]\\.[A-z]', df.out$value, perl=TRUE)] = 'character'
  df.out$type[grepl('\\d+\\.\\d+', df.out$value, perl=TRUE)] = 'numeric'
  
  # deal with unknowns
  idx.unknown = df.out$value == ''
  df.out$type[idx.unknown] = 'unknown'
  df.out$type[ endsWith(df.out$comment, 'file') & idx.unknown ] = 'character'
  
  # finish
  return(df.out)
}

#' write parameter values and raw text lines to SWAT configuration text files
my_swat_writepar = function(textpath, newval=character(0), wipe=integer(0), dpmax=6)
{
  # ARGUMENTS:
  #
  # `textpath`, full path to the swat text config file
  # `newval`, named vector or list of new parameter values to overwrite the existing ones
  # `wipe`, (optional) integer vector of line numbers to replace, overrides names of `newval` 
  # `dpmax`, integer number of decimal places (maximum) to use for 'real' type variables
  #
  # RETURN VALUE:
  #
  # A data frame with columns 'written' and 'old' indicating the existing values and the ones
  # that were overwritten
  # 
  # DETAILS:
  #
  # `wipe` is used to overwrite entire lines in the text file, by specifying their line numbers.
  # Any names in `newval` are ignored in this case, and its entries must be of character type
  # (or coercible to character). If `wipe` is assigned but `newval` is not, the function simply
  # returns the text at the specified lines without overwriting anything.
  # 
  # When `wipe` is unassigned, the names of `newval` should match SWAT parameter names in the file
  # at `textpath`. The function then parses the file to overwrite the entries of `newval` in the
  # correct place. The entries of `newval` may be character, numeric or integer - they will be
  # converted to text automatically, with whitespace and truncation of decimals handled to
  # satisfy `dpmax` (whenever there is space)
  #
  # Note that the existing parameter value text in `textpath` is parsed to determine parameter
  # type and the appropriate character representation. eg. if variable 'FOO' in `textpath` is an
  # integer (with no decimal place) then `newval$FOO` will be coerced to integer, possibly by
  # rounding.
  #
  
  # whitespace separating parameters from comment delimiter
  suffix.s = strrep(' ', 4)

  # open the file and parse for SWAT variables
  text.raw = readLines(textpath)
  text.parsed = my_swat_readpar(text.raw)
  
  # unpack inputs
  names.new = names(newval)
  n.names = length(names.new)
  n.text = length(text.raw)
  n.newval = length(newval)
  n.wipe = length(wipe)
  
  # conditional for overwriting entire lines
  if(n.wipe>0)
  {
    # check for invalid lines
    if(any(wipe < 1 | wipe > n.text))
    {
      err.msg = paste0('error: valid line numbers for `wipe` are 1,...', n.text)
      stop(err.msg)
    }
    
    # check for unmatched input
    if(n.wipe != n.newval)
    {
      print(paste('error:', n.wipe, 'line numbers supplied for', n.newval, 'strings'))
    }
    
    # check for replacement text argument
    if(n.newval>0)
    {
      # copy the old text
      text.raw.new = text.raw
      
      # write the lines
      text.raw.new[wipe] = sapply(newval, as.character)
      writeLines(text.raw.new, textpath)
      
      # return the old and new lines in a data frame
      return(data.frame(old=text.raw[wipe], new=text.raw.new[wipe]))
      
    } else {
      
      # return only the old text
      return(text.raw[wipe])
      
    }
    
  # conditional for replacing parameter values
  } else {
    
    # check that we have valid names as input
    idx.parsed = match(names.new, text.parsed$name)
    if(anyNA(idx.parsed))
    {
      err.msg1 = paste('variable name(s)', missing.names, 'not found')
      err.msg2 = paste('\n', basename(textpath), 'was not modified')
      stop(err.msg1, err.msg2)
    }
    
    # grab line numbers and index special types
    newval.ln = text.parsed$line[idx.parsed]
    newval.type = text.parsed$type[idx.parsed]
    idx.num = newval.type == 'numeric'
    idx.int = newval.type == 'integer'
    idx.char = newval.type %in% c('character', 'unknown')
    
    # coerce everything to character
    newval.s = sapply(newval, as.character)
    
    # reformat special types as needed then drop leading whitespace
    newval.num = sapply(newval[idx.num], function(x) round(x, dpmax))
    newval.int = sapply(newval[idx.int], function(x) round(x, dpmax))
    newval.s[idx.num] = format(newval.num, nsmall=dpmax, scientific=FALSE)
    newval.s[idx.int] = format(newval.int, nsmall=0, scientific=FALSE)
    newval.s = gsub(' ', '', newval.s)
    
    # grab full line text from the matching lines
    linetext = setNames(text.raw[newval.ln], nm=names.new)
    
    # grab string containing the parameter with whitespace
    partxt = sapply(linetext, function(string) strsplit(string, '\\|.+')[[1]])
    
    # identify the space we have for the parameter
    par.maxn = sapply(partxt, nchar) - nchar(suffix.s)
    
    # truncate (as needed) to stay within max width
    idx.trunc = sapply(newval.s, nchar) > par.maxn
    if(any(idx.trunc))
    {
      newval.s[idx.trunc] = mapply(function(x,y) substr(x, 1, y), 
                                   newval.s[idx.trunc], 
                                   par.maxn[idx.trunc])
      
      # warn if any character strings have been truncated
      idx.truncchar = idx.trunc & idx.char
      if(any(idx.truncchar))
      {
        print(paste('warning: string(s)',
                    paste(names.new[idx.truncchar], collapse='. '), 
                    'were truncated')) 
      }
      
      # warn if truncation affected accuracy of numerics substantially
      idx.accucheck = idx.trunc & (idx.num | idx.int)
      if(any(idx.accucheck))
      {
        num.in = as.numeric(newval[idx.accucheck])
        num.out = as.numeric(newval.s[idx.accucheck])
        rel.diff = 100 * (num.in - num.out) / num.in
        if(any(rel.diff > 1))
        {
          print('warning: truncation produced relative change(s) of:')
          print(paste(paste0(rel.diff, '%'), collapse=', '))
          print(paste('in variables:', paste(names.new[idx.accucheck], collapse='. ')))
        }
      }
      
    }
    
    # pad with leading whitespace to produce strings of exactly max width
    newval.s[!idx.trunc] = mapply(function(x,y,z) paste0(strrep(' ', y-z), x),
                                  newval.s[!idx.trunc],
                                  par.maxn[!idx.trunc],
                                  nchar(newval.s[!idx.trunc]))

    
    # build replacement strings, adding terminal whitespace 
    linetext.out =  mapply(gsub, partxt, paste0(newval.s, suffix.s), linetext)
  
    # copy changes to SWAT text file
    text.raw.new = text.raw
    text.raw.new[newval.ln] = linetext.out
    writeLines(text.raw.new, textpath)
    
    # return the old and new lines in a data frame
    return(data.frame(old=text.raw[newval.ln], new=text.raw.new[newval.ln]))
    
  }
  
}

#' read/write SWAT text files for vector-valued parameter assignments
my_swat_rwvec = function(textpath, newval=character(0))
{
  # ARGUMENTS:
  #
  # `textpath`, full path to the swat text config file
  # `newval`, optional named list of (numeric or integer) vectors to overwrite the existing ones
  # 
  # RETURN VALUE:
  #
  # A (possibly empty) list of lists, one per vector-valued parameter in the SWAT file appearing
  # in `newval` or, if `newval` unassigned, the complete list of vector-valued parameters avilable
  # in the file. List entries are named according to their SWAT variable names, and contain the
  # following elements:
  #
  # ln = (integer) line number where the vector appears in the text file
  # old = (integer or numeric) vector of existing parameter values
  # new = (if specified in `newval`) the replacements for the existing values
  #
  # DETAILS:
  #
  # If `newval` is assigned, its names should match the names of the SWAT vector parameters to
  # overwrite. Note these are names for the full vector, the individual entries of which are
  # unnamed (here, and in the SWAT config files/docs). The function will overwrite the existing
  # values in the text file, handling padding and truncation appropriately
  
  # spacing info, literal strings to parse, and associated variable name for i/o control vectors
  pos.io = c(n.entries=20, len.entries=4, n.prec=0)
  key.io = c(IPDVAR = 'Reach output variables',
             IPDVAB = 'Subbasin output variables',
             IPDVAS = 'HRU output variables',
             IPDHRU = 'HRU data to be printed')
  
  # do the same for for elevation bands
  pos.el = c(n.entries=10, len.entries=8, n.prec=3)
  key.el = c(ELEVB = 'Elevation at center of elevation bands [m]',
             ELEVB_FR = 'Fraction of subbasin area within elevation band')
  
  # do the same for for climate change variables
  pos.cc = c(n.entries=6, len.entries=8, n.prec=3)
  key.cc = c(RFINC1 = 'Climate change monthly rainfall adjustment (January - June)',
             RFINC2 = 'Climate change monthly rainfall adjustment (July - December)',
             TMPINC1 = 'Climate change monthly temperature adjustment (January - June)',
             TMPINC2 = 'Climate change monthly temperature adjustment (July - December)',
             RADINC1 = 'Climate change monthly radiation adjustment (January - June)',
             RADINC2 = 'Climate change monthly radiation adjustment (July - December)',
             HUMINC1 = 'Climate change monthly humidity adjustment (January - June)',
             HUMINC2 = 'Climate change monthly humidity adjustment (July - December)')
  
  # compile everything into one data frame
  key.all = rbind(data.frame(comment=key.io, do.call(rbind, rep(list(pos.io), length(key.io)))),
                  data.frame(comment=key.el, do.call(rbind, rep(list(pos.el), length(key.el)))),
                  data.frame(comment=key.cc, do.call(rbind, rep(list(pos.cc), length(key.cc)))))
  
  # check `newval` for unmatched entries wrt the keys defined above
  newval.names = names(newval)
  idx.newval.matched = newval.names %in% rownames(key.all)
  if(sum(idx.newval.matched) != length(newval))
  {
    unmatched.char = paste(newval.names[!idx.newval.matched], collapse=', ')
    stop(paste('error: unrecognized variable(s)', unmatched.char))
  }
  
  # scan the text file for comment strings, omit non-matches, increment line number
  linetext = readLines(textpath)
  match.ln = sapply(key.all$comment,  function(pattern) grep(pattern, linetext, fixed=TRUE))
  match.ln = setNames(unlist(match.ln) + 1, rownames(key.all)[sapply(match.ln, length)>0])
  
  # finish if no matches are found
  if(length(match.ln)==0)
  { 
    # returning empty list 
    return(vector(mode='list', length=0))
    
  } 
  
  # initialize output list with line number
  out.list = lapply(match.ln, function(x) list(ln=x))
  
  # set up write-mode
  if(length(newval.names) > 0)
  {
    # check for unmatched write input variables wrt the variable names in the file
    idx.newval.matched = newval.names %in% names(match.ln)
    if(any(!idx.newval.matched))
    {
      unmatched.char = paste(newval.names[!idx.newval.matched], collapse=', ')
      err.msg = paste('error: variable(s)', unmatched.char, 'not found in', basename(textpath))
      stop(err.msg)
    }
    
    # make a copy of the full text to modify 
    linetext.new = linetext
  }
  
  # grab full text from matched line numbers
  match.linetext = setNames(linetext[match.ln], names(match.ln))
  
  # handle i/o control type vectors by pattern matching
  idx.io = names(match.linetext) %in% names(key.io)
  if(any(idx.io))
  {
    # names of the variables of this type
    io.names = names(match.linetext)[idx.io]
    
    # split at whitespace, tidy up, convert to numeric, append to output list
    io.split = strsplit(match.linetext[idx.io], '\\s+')
    io.val = lapply(io.split, function(string) as.numeric(string[sapply(string, nchar) > 0]))
    
    # append to output list
    out.list[io.names] = mapply(function(x, y) { c(x, list(old=y)) },
                                x = out.list[io.names],
                                y = io.val,
                                SIMPLIFY = FALSE)
    
    # grab the subset of `newval` to write (if any)
    idx.io.newval = io.names %in% names(newval)
    if(any(idx.io.newval))
    {
      # add zeroes to complete any missing fields of the i/o vectors
      io.newval = mapply(function(x, y) { c(x, rep(0, y-length(x))) },
                         x = newval[names(newval) %in% io.names],
                         y = key.all[io.names[idx.io.newval], 'n.entries'],
                         SIMPLIFY = FALSE)
      
      # format the numeric/integer values as character
      io.newval.s = lapply(io.newval, function(int) as.character(int))
 
      # compute amount of whitespace to lead with (if any)
      io.newval.nchar = lapply(io.newval.s, nchar)
      io.newval.nws = mapply(function(x, y) { y - x },
                                x = io.newval.nchar,
                                y = key.all[io.names[idx.io.newval], 'len.entries'],
                                SIMPLIFY = FALSE)
      
      # check for input too large to fit in specified width
      idx.newval.toobig = sapply(io.newval.nws, function(x) any(x < 0))
      toobig.names = names(which(idx.newval.toobig))
      if(any(idx.newval.toobig))
      {
        toobig.char = paste(toobig.names, collapse=', ')
        toobig.max = paste(key.all[toobig.names, 'len.entries'], collapse=', ')
        print(paste('error: invalid newval for variable(s)', toobig.char))
        print(paste('width as character exceeded upper limit(s) of', toobig.max))
      }
      
      # add leading whitespace and delimiters as needed
      io.newval.spad = mapply(function(x, y) { paste(paste0(strrep(' ', x), y), collapse='') },
                                 x = io.newval.nws,
                                 y = io.newval.s,
                                 SIMPLIFY = FALSE)
      
      # add modifications to file text and append to output list
      linetext.new[match.ln[names(io.newval.spad)]] = unlist(io.newval.spad)
      out.list[names(io.newval.spad)] = mapply(function(x, y) { c(x, list(new=y)) },
                                               x = out.list[names(io.newval.spad)],
                                               y = io.newval,
                                               SIMPLIFY = FALSE)
      
    }
  }
  
  # handle other vectors by exact character start/stop rules
  idx.other = !idx.io
  if(any(idx.other))
  {
    # names of the variables of this type
    other.names = names(match.linetext)[idx.other]
    
    # set up character start positions
    startpos = mapply(function(x, y) { 1 + x*( (1:y) - 1 ) }, 
                      x = key.all[other.names, 'len.entries'],
                      y = key.all[other.names, 'n.entries'])
    
    # set up character end positions
    endpos = mapply(function(x, y) { x + y - 1 },
                    x = key.all[other.names, 'len.entries'],
                    y = startpos)
    
    # extract the vectors as character then coerce to numeric
    other.val = mapply(function(x, y, z) { as.numeric(substring(x, y, z)) }, 
                       x = match.linetext, 
                       y = startpos, 
                       z = endpos)
    
    # append to output list
    out.list[other.names] = mapply(function(x, y) { c(x, list(old=y)) },
                                   x = out.list[other.names],
                                   y = other.val,
                                   SIMPLIFY = FALSE)
    
    # grab the subset of `newval` to write (if any)
    idx.other.newval = other.names %in% names(newval)
    if(any(idx.other.newval))
    {
      # format the numeric/integer values as character
      other.newval = newval[names(newval) %in% other.names]
      other.newval.s = lapply(other.newval, function(num) sprintf('%.3f', num))
      
      # compute amount of whitespace to lead with (if any)
      other.newval.nchar = lapply(other.newval.s, nchar)
      other.newval.nws = mapply(function(x, y) { y - x },
                                x = other.newval.nchar,
                                y = key.all[other.names[idx.other.newval], 'len.entries'],
                                SIMPLIFY = FALSE)
      
      # check for input too large to fit in specified width
      idx.newval.toobig = sapply(other.newval.nws, function(x) any(x < 0))
      toobig.names = names(which(idx.newval.toobig))
      if(any(idx.newval.toobig))
      {
        toobig.char = paste(toobig.names, collapse=', ')
        toobig.max = paste(key.all[toobig.names, 'len.entries'], collapse=', ')
        print(paste('error: invalid newval for variable(s)', toobig.char))
        print(paste('width as character exceeded upper limit(s) of', toobig.max))
      }
      
      # add leading whitespace as needed
      other.newval.spad = mapply(function(x, y) { paste(paste0(strrep(' ', x), y), collapse='') },
                                 x = other.newval.nws,
                                 y = other.newval.s,
                                 SIMPLIFY = FALSE)
      
      # add modifications to file text and build output list
      linetext.new[match.ln[names(other.newval.spad)]] = unlist(other.newval.spad)
      out.list[names(other.newval.spad)] = mapply(function(x, y) { c(x, list(new=y)) },
                                                  x = out.list[names(other.newval.spad)],
                                                  y = other.newval,
                                                  SIMPLIFY = FALSE)
      
    }
    
  }
  
  # write the changes (if any) to the file 
  if(length(newval.names) > 0)
  {
    writeLines(linetext.new, textpath)
    out.list = out.list[newval.names]
  }
  
  # return list of current and new parameter values
  return(out.list)
}

#' return a list of SWAT text input files and associated variable names 
my_swat_scan = function(textio.dir)
{
  # ARGUMENTS:
  #
  # `textio.dir`, character string path to the text input file directory of a SWAT2012 model
  #
  # RETURN VALUE:
  #
  # A list of all text input configuration files for the SWAT model (all located in the `textio.dir`),
  # and the SWAT variable names available for reading/writing. List entries are named according
  # to the file extension (eg. cio, bsn, etc), and are themselves lists of named character vectors:
  # 
  #  file = the filename(s) associated to this extension
  #  link = external filename(s) referenced in this/these files
  #  scalar = SWAT variable names for single-valued parameters listed in this file
  #  vector = SWAT variable names for vector-valued parameters listed in this file
  #
  # Note that 'link' and 'vector' are included only for relevant files (ie where they are nonempty)
  # 
  # DETAILS:
  #
  # The first two entries of the return list ('cio' and 'bsn') are global parameter files, which
  # apply to all subbasins equally. The remainder ('sub', 'hru', 'mgt', 'gw', 'sep'), refer to a set
  # of files (one per subbasin), which share identically named parameters that can vary at the subbasin
  # level. These parameters are only listed once (rather than repeated for each subbasin)
  #
  
  # scan all existing files in the directory 
  all.fn = list.files(textio.dir)
  
  ## master watershed file ('file.cio')
  
  # open 'file.cio' and parse for standard SWAT variables
  cio.path = file.path(textio.dir, 'file.cio')
  cio.raw = readLines(cio.path, warn=FALSE)
  cio.df = my_swat_readpar(cio.raw)
  
  # pull file references (ignoring unassigned ones) and numeric/integer variable names
  idx.cio.char = (cio.df$type=='character') & (cio.df$value != '')
  cio.fn.standard = setNames(cio.df$value[idx.cio.char], cio.df$name[idx.cio.char])
  
  # define literal strings that precede some special filename entries
  cio.lb = c(fig='Watershed Configuration',
             pcp='Precipitation Files', 
             tmp='Temperature Files',
             atm='ATMOSPERIC DEPOSITION') # <- this typo intentional!
  
  # find these strings and parse the subsequent line number
  cio.ln = apply(sapply(cio.lb, function(pattern) grepl(pattern, cio.raw)), 2, which)
  cio.fn.special = setNames(gsub(' ', '', cio.raw[1+cio.ln]), nm=names(cio.ln))
  
  # pull all remaining scalar-valued variable names
  cio.scalar = cio.df$name[!idx.cio.char]
  
  # pull vector-valued parameter names
  cio.vector = names(my_swat_rwvec(cio.path))
  
  ## descendants (.bsn, .fig)
  
  # open the open basin input (.bsn) file and pull scalars, file references (ignoring unassigned ones)
  bsn.path = file.path(textio.dir, cio.fn.standard['BSNFILE'])
  bsn.raw = readLines(bsn.path, warn=FALSE)
  bsn.df = my_swat_readpar(bsn.raw)
  bsn.char.idx = (bsn.df$type=='character') & (bsn.df$value != '')
  bsn.fn.standard = setNames(bsn.df$value[bsn.char.idx], bsn.df$name[bsn.char.idx])
  bsn.scalar = bsn.df$name[!bsn.char.idx]
  
  # open watershed configuration file (.fig) and pull file references
  fig.raw = readLines(file.path(textio.dir, cio.fn.special['fig']), warn=FALSE)
  fig.ln = which(grepl('Subbasin: [1-9]+', fig.raw))
  fig.id = sapply(strsplit(fig.raw[fig.ln], 'Subbasin:'), function(x) as.integer(x[length(x)]))
  fig.fn = setNames(gsub(' ', '', fig.raw[fig.ln + 1], fixed=TRUE), paste0('sub_', fig.id))
  sub.n = length(fig.fn)
  
  ## scan sub-watershed level files (`sub.n` of them)
  
  # in addition to parameters in the .sub files, some are found in linked files 
  sub.special.toread = c('hru'=T, 'mgt'=T, 'sol'=F, 'chm'=F, 'gw'=T, 'sep'=T)
  
  # this pattern grabs the linked filenames
  sub.regexp = paste(paste0('.', names(sub.special.toread), '\\s*'), collapse='|')
  
  # open subbasin files (.sub) in a loop, writing file details into list
  sub.fn.list = vector(mode='list', length=sub.n) 
  for(idx.sub in 1:sub.n)
  {
    # pull file references
    sub.raw = readLines(file.path(textio.dir, fig.fn[idx.sub]), warn=FALSE)
    sub.df =  my_swat_readpar(sub.raw)
    sub.char.idx = (sub.df$type=='character') & (sub.df$value != '')
    sub.fn.standard = setNames(sub.df$value[sub.char.idx], sub.df$name[sub.char.idx])
    
    # parse special file references
    sub.special.raw = sub.raw[1+which(grepl('HRU: General', sub.raw))]
    sub.fn.special = paste(strsplit(sub.special.raw, sub.regexp)[[1]], names(sub.special.toread), sep='.')
    names(sub.fn.special) = names(sub.special.toread)
    
    # copy filenames to list
    sub.fn.list[[idx.sub]] = c(sub.fn.special, sub.fn.standard)
    
  }
  
  # flatten list into data frame, appending 'sub' column
  sub.fn.all = cbind(sub=fig.fn, data.frame(do.call(rbind, sub.fn.list), row.names=names(fig.fn)))
  
  # open files corresponding to first subbasin to get scalars (they should all be the same)
  ext.toread = c('sub', names(sub.special.toread)[sub.special.toread])
  sub.scalar.list = setNames(vector(mode='list', length=length(ext.toread)), ext.toread) 
  for(ext in ext.toread)
  {
    # scan for SWAT variable names and copy them to the list
    sub.path = file.path(textio.dir, sub.fn.all[1, ext])
    sub.df = my_swat_readpar(readLines(sub.path, warn=FALSE))
    sub.char.idx = (sub.df$type=='character') & (sub.df$value != '')
    sub.scalar.list[[ext]] = sub.df$name[!sub.char.idx]

  }
  
  # organize all sub-watershed files and parameter names into list
  out.sub = lapply(setNames(nm=ext.toread), function(ext) {
    list(file=setNames(sub.fn.all[,ext], rownames(sub.fn.all)), 
         scalar=sub.scalar.list[[ext]])
  })
  
  
  # pull vector-valued parameter names from .sub files and add to list
  out.sub$sub$vector = names(my_swat_rwvec(file.path(textio.dir, sub.fn.all[1, 'sub'])))
  
  # compile everything for cio 
  out.cio = list(cio=list(file='file.cio', 
                          link=c(cio.fn.standard, cio.fn.special), 
                          scalar=cio.scalar,
                          vector=cio.vector))
  
  # compile everything for bsn
  out.bsn = list(bsn=list(file=cio.fn.standard['BSNFILE'], 
                          link=bsn.fn.standard, 
                          scalar=bsn.scalar))
  
  # finish
  return(c(out.cio, out.bsn, out.sub))
  
}

#' read/write SWAT configuration text file parameters 
my_swat_rw = function(textio.dir, param=character(0), textio.scan=list(), subb=integer(0))
{
  # ARGUMENTS:
  #
  # `textio.dir`, character string path to the text input file directory of a SWAT2012 model
  # `param`, vector (or list) of either SWAT variable names (character), or numeric values to assign
  # `textio.scan`, (optional) list, the result of `my_swat_scan(textio.dir)`
  # `subb`, (optional) integer vector of subbasin IDs
  #
  # RETURN VALUE:
  #
  # ...
  # 
  # DETAILS:
  #
  # ...
  # 
  
  # examples 
  # my_swat_rw(textio.dir)
  # my_swat_rw(textio.dir, param='IDAL')
  # my_swat_rw(textio.dir, param=c(IDAL=NA))
  # my_swat_rw(textio.dir, param='LAT_ORGN')
  # my_swat_rw(textio.dir, param=c('IDAL', 'LAT_ORGN'))
  # my_swat_rw(textio.dir, param=c('LAT_ORGN'=1))
  # my_swat_rw(textio.dir, param=c(IDAL=273, LAT_ORGN=0))
  # my_swat_rw(textio.dir, param='IPDVAR')
  # my_swat_rw(textio.dir, param=list(IPDVAR=c(3,5,10,15)))
  # my_swat_rw(textio.dir, param=list(IPDVAR=c(1,2)))
  # my_swat_rw(textio.dir, param='RFINC1')
  # my_swat_rw(textio.dir, param=list(RFINC1=c(0,0,0,0,0,0)))
  
  # this argument can be used to prevent repetitive file reads
  if(length(textio.scan)==0)
  {
    # scan directory and read in all config files
    textio.scan = my_swat_scan(textio.dir)
  } 
  
  # make a vector of available variable names
  all.parnames = unlist(sapply(textio.scan, function(cfg) c(cfg$scalar, cfg$vector)), use.names=F)
  
  # if `param` is unassigned, return a data frame summarizing available SWAT parameters
  if(length(param)==0)
  {
    # print message and return available variable names
    print('`param` argument specifies the SWAT parameter(s) to read/write') 
    return(sort(all.parnames))
  }
  
  # if `param` is unnamed, it is treated as a vector/list of parameter names to query
  input.names = names(param)
  if(is.null(input.names))
  {
    # call the function again with `param` a named list of NAs to prompt read-mode
    param.new = setNames(as.list(rep(NA, length(param))), unlist(param))
    return(my_swat_rw(textio.dir, param=param.new, textio.scan=textio.scan, subb=subb))
    
  }
  
  # convert vectors to lists
  param = as.list(param)
  input.names = names(param)
  
  # check that names of `param` are all valid SWAT parameter names before proceeding
  idx.valid = input.names %in% all.parnames
  if(!all(idx.valid))
  {
    err.msg1 = 'error: failed to match element(s) of input `param` to SWAT variables: '
    err.msg2 = paste(input.names[!idx.valid], collapse=', ')
    stop(err.msg1, err.msg2)
    
  }
  
  # handle `param` lists with more than one element using recursive call
  if(length(param)>1)
  {
    # apply the function to each list element in `param` using lapply
    out.list = lapply(seq_along(param), function(idx) {
      param.singleton = setNames(param[idx], input.names[idx]) 
      my_swat_rw(textio.dir, param=param.singleton, textio.scan=textio.scan, subb=subb)
      })
    
    # finish with list output
    return(setNames(out.list, input.names))
  
  }
  
  # `param` will be of length 1 if we have gotten this far: find its type
  idx.fn.scalar = sapply(textio.scan, function(cfg) input.names %in% cfg$scalar)
  idx.fn.vector = sapply(textio.scan, function(cfg) input.names %in% cfg$vector)
  is.scalar = any(idx.fn.scalar)
  
  # build path to container file(s) - always prefer subbasin-level files (which have priority)
  idx.fn = which(idx.fn.scalar | idx.fn.vector)
  if(length(idx.fn) == 2)
  {
    idx.fn = idx.fn[-1]
  }
  fn = textio.scan[[idx.fn]]$file
  textio.file = file.path(textio.dir, fn)
  
  # check if parameter is global or subbasin-level (appearing in multiple files)
  is.global = length(textio.file) == 1
  
  # when a subset of subbasins is specified, only scan the associated subset of files
  if(length(subb) > 0 & !is.global)
  {
    # extract the subbasin IDs
    subb.all = as.integer(gsub('sub_', '', names(fn)))
    
    # check for unmatched input subbasin IDs
    idx.subb.valid = subb %in% subb.all
    if(any(!idx.subb.valid))
    {
      err.msg = paste('error: subbasin IDs', paste(subb[idx.subb.valid], collapse=', '), 'not found')
      stop(err.msg)
    }
    
    # take the subset
    idx.subb = match(subb, subb.all)
    fn = fn[idx.subb]
    textio.file = textio.file[idx.subb]
  }

  # non-vector and vector cases are handled by separate helper functions
  if(is.scalar)
  {
    # read mode:
    if(is.na(unlist(param)))
    {
      # read in all parameter values (from all selected files)
      textio.df = lapply(textio.file, function(x) my_swat_readpar(readLines(x, warn=FALSE)))
      
      # extract the requested parameter(s)
      param.idx = textio.df[[1]]$name == input.names
      param.type = textio.df[[1]]$type[param.idx]
      param.out = as(sapply(textio.df, function(df) df$value[param.idx]), param.type)
      
      # if returning from multiple subbasins, name according to subbasin ID
      if(!is.global)
      {
        names(param.out) = names(fn) 
        
      } else {
        
        # otherwise set name to SWAT variable name
        names(param.out) = input.names 
      }
      
      # return the requested parameters 
      return(param.out)
      
      
    } else {
      # write mode:
      
      # write mode for global simply calls another helper function
      if(is.global)
      {
        return(my_swat_writepar(textio.file, newval=unlist(param)))
        
      } else {
        
        out.list = lapply(textio.file, function(x) my_swat_writepar(x, newval=unlist(param)))
        return(setNames(out.list, names(fn)))
      }
    }
    
  } else {
    # handle vector-valued parameters
    
    # read mode:
    if(anyNA(unlist(param)))
    {
      # read in all vector-valued parameters using a helper function
      param.out = lapply(textio.file, function(x) my_swat_rwvec(x)[[input.names]]$old)
      
      # if returning from multiple subbasins, name according to subbasin ID
      if(!is.global)
      {
        names(param.out) = names(fn) 
        
      } else {
        
        # otherwise set name to SWAT variable name
        names(param.out) = input.names 
      }
      
      # return the requested parameters 
      return(param.out)
      
    } else {
      # write mode:
      
      # write mode for global simply calls another helper function
      if(is.global)
      {
        out.rwvec = my_swat_rwvec(textio.file, newval=param)
        out.df = data.frame(do.call(rbind, out.rwvec[[1]][c('old', 'new')]))
        return(setNames(list(out.df), input.names))
        
      } else {
        
        out.rwvec = lapply(textio.file, function(x) my_swat_rwvec(x, newval=param))
        out.list = lapply(out.rwvec, function(x) data.frame(do.call(rbind, x[[1]][c('old', 'new')]))) 
        return(setNames(out.list, names(fn)))
      }
    }
    
  }
  
  #print(input.names)
  
  
  # # If the value of `param[[1]]` is NA, we are in read-mode
  # if(!is.na(param[[1]]))
  # {
  #   # coerce to list and call the function again
  #   return(my_swat_rw(textio.dir, as.list(param), textio.scan=textio.scan))
  # }
  
  
  
}

#' write weather input text files for QSWAT and SWAT2012
my_swat_wmeteo = function(wdat, exdir, form='qswat', include=logical(0), suffix='')
{
  # ARGUMENTS:
  #
  # `wdat`: list of time series data: 'coords_sf', 'dates', 'tables', 'elevation' (see DETAILS) 
  # `exdir`: character string, path to directory to write output files
  # `form` : character string specifying output structure, either 'qswat' (the default) or 'swat'
  # `include`: (optional) boolean vector of same length as `wdat$coords_sf`, for writing subsets
  # `suffix`: (optional) suffix to append to filenames
  #
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
  vn.list = list(pcp='pcp', slr='slr', wnd='wnd', hmd='hmd', tmp=c('tmin', 'tmax'))
  
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
  coords.tab = setNames(data.frame(coords.geo, row.names=coords$name), c('LONG', 'LAT'))
  coords.tab$ELEVATION = wdat$elevation[include]
  
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
    l2.string = paste(c('Lati  ', substring(as.character(coords.tab[['LAT']]), 1, 4)), collapse=' ')
    l3.string = paste(c('Long  ', substring(as.character(coords.tab[['LONG']]), 1, 4)), collapse=' ')
    l4.string = paste(c('Elev  ', substring(as.character(coords.tab[['ELEV']]), 1, 4)), collapse=' ')
    
    # define paths to the output files (one per variable)
    wstn.path = sapply(vn.in, function(vn) file.path(exdir, paste0(vn, suffix, '.txt')))
    
    # prepare Julian date-year strings that start each line
    dates.string = format(wdat$dates[include], '%Y%j')
    
    # write the variables one at a time in a loop
    pb = txtProgressBar(max=length(vn.in), style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      l1.string = paste('Station ', paste(svn[[vn]], collapse=','))
      print(paste('writing', basename(wstn.path[vn]), 'to directory', exdir))
      
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
      setTxtProgressBar(pb, idx.vn)
      
    }
    close(pb) 
    
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
            LAT=coords.tab['LAT'],
            LONG=coords.tab['LONG'],
            ELEVATION=coords.tab['ELEVATION'])
    })
    
    # define paths to the output files (with time series stored separately from station location info)
    wstn.path = sapply(names(svn), function(fn) file.path(exdir, paste0(fn, suffix, '.txt')))
    wstn.ts.path = lapply(svn, function(fn) setNames(file.path(exdir, paste0(fn, '.txt')), nm=coords$name))
    
    # write the station location data
    sapply(names(svn), function(vn) write.csv(wdat.wstn[[vn]], file=wstn.path[vn], quote=F, row.names=F))
    
    # the first line of each time series data file is the origin date (without spaces) 
    origin.string = paste0(gsub('-', '', origin.date))
    
    # write the station time series data in a loop
    pb = txtProgressBar(max=length(vn.in)*n.coords, style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      print(paste('writing', n.coords, vn, 'files to directory', exdir))
      
      # loop over grid point locations
      for(idx.coords in 1:n.coords)
      {
        # identify point name and output path for the text file
        stn.name = coords$name[idx.coords]
        out.path = wstn.ts.path[[vn]][stn.name]
        setTxtProgressBar(pb, idx.coords + (idx.vn-1)*n.coords)
        
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
    close(pb) 
    
    # finish and return filenames in list
    return(list(exdir=exdir,
                stations=setNames(basename(wstn.path), vn.in), 
                data=lapply(wstn.ts.path, basename)))
  }
  
}

########## DEPRECATED

#' #' Weather data enters the SWAT2012 as input txt files, one for each variable at each observation
#' #' point in space. These locations are called "weather stations", although they need not come from
#' #' actual gages. We will use data gridded climatic reconstruction models to train the SWAT model.
#' #' This function converts the gridded data to input txt files that can be read by QSWATSWAT2012. 
#' my_grid2wstn = function(wdat.in, vn.in, bd.poly.in, dem.in, exdir)
#' {
#'   # ARGUMENTS:
#'   #
#'   # `wdat.in`: list, time series data, in form generated by "get_meteo.R" (daymet, livneh, or pnwnamet)
#'   # `vn.in`: named list of character vectors. Maps variable names in `wdat.in` to output filenames
#'   # `bd.poly.in`: sf polygon, to use as mask for fetching a subset of grid points
#'   # `dem.in`: raster, a digital elevation map of same projection as `bd.poly.in` covering its extent
#'   # `exdir`: path to directory to write weather station and time series input txt files for SWAT2012
#'   #
#'   # RETURN VALUE:
#'   #
#'   # a named list with two entries:
#'   #
#'   #   `stations`: paths to txt files containing weather station coordinates/elevation
#'   #   `data`: paths to individual time series txt files for each weather station
#'   #
#'   # BEHAVIOUR: 
#'   #
#'   # writes the subset of the time series data in `wdat.in` enclosed by `bd.poly.in` to the text
#'   # files listed in (return list entry) `data`. Data on grid point locations and elevations are 
#'   # written to the txt files listed in `stations`.
#'   #
#'   # `vn.in` controls the filenames and the grouping of variables. Each list entry of vn.in produces
#'   # its own set of txt files (one per weather station location), which are named to correspond with
#'   # the naming scheme in the list. The character vector in each of these list entries specifies
#'   # which variable(s) (from `wdat.in`) to include in a given batch of text files. When the vector
#'   # includes multiple variables, they are lumped together (as comma-delimited rows). Otherwise, each
#'   # variable gets its own text file.
#'   
#'   
#'   # make index of relevant grid points
#'   wdat.pts = st_crop(wdat.in$coords_sf, bd.poly.in)
#'   n.pts = nrow(wdat.pts)
#'   
#'   # extract elevation data and coordinates at these grid points
#'   wdat.coords = wdat.in$coords[match(wdat.pts$name, rownames(wdat.in$coords)),]
#'   wdat.elev = extract(dem.tif, wdat.pts)
#'   
#'   # build list of unique filenames (without txt extension) for each variable's time series
#'   wdat.fn = setNames(nm=names(vn.in))
#'   wdat.ts.fn = lapply(wdat.fn, function(fn) setNames(paste(fn, wdat.pts$name, sep='_'), nm=wdat.pts$name))
#'   
#'   # build wdat station file tables for SWAT as matrices of text
#'   wdat.wstn = lapply(setNames(nm=names(vn.in)), function(vn) { 
#'     cbind(ID=1:nrow(wdat.coords), 
#'           NAME=wdat.ts.fn[[vn]], 
#'           LAT=wdat.coords[,2],
#'           LONG=wdat.coords[,1],
#'           ELEVATION=wdat.elev)
#'   })
#'   
#'   # define paths to the output files
#'   wstn.path = sapply(wdat.fn, function(fn) file.path(exdir, paste0(fn, '.txt')))
#'   wstn.ts.path = lapply(wdat.ts.fn, function(fn) setNames(file.path(exdir, paste0(fn, '.txt')), nm=names(fn)))
#'   
#'   # write the station location data
#'   sapply(wdat.fn, function(vn) write.csv(wdat.wstn[[vn]], file=wstn.path[vn], quote=F, row.names=F))
#'   
#'   # the first line of each time series data file is the origin date (without spaces) 
#'   origin.string = paste0(gsub('-', '', wdat.in$dates[1]))
#'   
#'   
#' }

# # read output.rch data
# my_read_rch = function(rch.path, varname=character(0), origin.date=character(0))
# {
#   # ARGUMENTS:
#   #
#   # `rch.path`, character string giving path to the "output.rch" file from a SWAT simulation
#   # `varname`, character string vector of variable names to load from the output file
#   # `origindate`, (optional) 'Date' class object specifying first day of simulation period
#   #
#   # RETURN VALUE:
#   #
#   # A data frame containing the data from "output.rch" (with comment lines 1-9 omitted).
#   # By default returns the entire dataset, but a subset of columns can be specified in
#   # `varname`.
#   #
#   # If `origindate` is supplied, the function appends a column containing the dates for
#   # each data row (note that this assumes a daily time step). It also returns the 'RCH', 'MON'
#   # columns (if they are not already specified in varname)
#   
#   # line number of the headers
#   rch.idx = 9
#   
#   # scan the headers line and note that R can't parse the delimiters in use here
#   rch.header = paste0(scan(rch.path, what=character(), skip=rch.idx-1, nlines=1), collapse=' ')
#   
#   # use regexp to identify whitespace and units delimiting individual variables
#   rch.regexp = '(\\s)|(km2)|(cms)|(tons)|(mg/L)|(kg)|(mg)|(ct)|(TOT)|(Mg/l)|(degc)'
#   rch.header.parsed = strsplit(rch.header, rch.regexp)[[1]]
#   rch.header.parsed = rch.header.parsed[nchar(rch.header.parsed) > 0]
#   
#   # the first column/field seems to be a dummy variable (string 'REACH' begins each line?)
#   rch.header.parsed = c('DUMMY', rch.header.parsed[sapply(rch.header.parsed, nchar) > 0])
#   n.col = length(rch.header.parsed) 
#   
#   # build a list of column types - most are doubles
#   fread.what = rep('numeric', n.col)
#   fread.what[rch.header.parsed %in% c('DUMMY')] = 'character'
#   fread.what[rch.header.parsed %in% c('RCH', 'GIS', 'MON')] = 'integer'
#   
#   # default behaviour is to load all fields except 'DUMMY'
#   if(length(varname)==0)
#   {
#     varname = rch.header.parsed[rch.header.parsed != 'DUMMY'] 
#   }
#   
#   # both the 'RCH' and 'MON' fields are needed to parse dates
#   if(length(origin.date)>0)
#   {
#     if(!('RCH' %in% varname))
#     {
#       varname = c('RCH', varname)
#     }
#     
#     if(!('MON' %in% varname))
#     {
#       varname = c('MON', varname)
#     }
#   }
#   
#   # verify that the requested variable name was printed to this output file
#   idx.varname.found = varname %in% rch.header.parsed
#   if(!all(idx.varname.found))
#   {
#     err.msg1 = paste('The following variable name(s) not found in', basename(rch.path), ':')
#     err.msg2 = paste(varname[!idx.varname.found], collapse=', ')
#     stop(paste(err.msg1, err.msg2, collapse='\n'))
#   }
#   
#   # reorder varname to match ordering in headers
#   varname = varname[order(match(varname, rch.header.parsed))]
#   
#   # index the columns to keep and drop
#   idx.read = rch.header.parsed %in% varname
#   idx.drop = which(!idx.read)
#   
#   # load the requested columns using fread
#   dat.out = fread(rch.path, 
#                   header=FALSE, 
#                   skip=rch.idx, 
#                   col.names=varname, 
#                   colClasses=fread.what, 
#                   drop=idx.drop)
#   
#   # warn of any coercion errors in the numeric data and replace invalid entries with NA
#   idx.col.invalid = sapply(dat.out, class) != fread.what[idx.read]
#   if(any(idx.col.invalid))
#   {
#     # identify the variables and print the warning
#     names.col.invalid = names(dat.out)[idx.col.invalid]
#     print('warning: type coercion failed in the following variable(s)')
#     print(names.col.invalid)
#     print('coercing to numeric...')
#     
#     # perform the coercion
#     invisible(sapply(names.col.invalid, function(vn) {
#       set(dat.out, j=varname, value=as.numeric(dat.out[[vn]]))
#     }))
#     
#   }
#   
#   # if dates requested, append the row, then finish
#   if(length(origin.date)>0)
#   {
#     n.rch = length(unique(dat.out[['RCH']]))
#     n.days = nrow(dat.out)/n.rch
#     dates.out = rep(seq.Date(as.Date(origin.date), by='day', length.out=n.days), each=n.rch)
#     return(cbind(date=dates.out, dat.out))
#     
#   } else {
#     
#     return(dat.out)
#     
#   }
#   
# }
#' list all writeable parameters, or look up the line numbers corresponding to a particular set
#' my_swat_parln = function(filetext, parname, nextline=FALSE)
#' {
#'   # ARGUMENTS:
#'   #
#'   # `filetext`, character string vector, the (line by line) raw text from a SWAT config file 
#'   # `parname`, character string vector, the SWAT variable names (usually all uppercase) to look up
#'   # `nextline`, boolean indicating to match `parname` exactly (without the '|', see DETAILS)
#'   #
#'   # RETURN VALUE:
#'   #
#'   # if `parname==''`, return value is a named vector of line numbers for all parameters found in
#'   # `filetext`. Otherwise, it is a named integer vector of same length as `parname`, matching the
#'   # input SWAT variable names to the line numbers on which their values can be found in `filetext`.
#'   # If multiple matches are found for any of the input variables, the function prints an error and
#'   # returns NA
#'   #
#'   # DETAILS:
#'   #
#'   # Most SWAT parameters are followed on the same line by a '|' symbol, indicating the beginning
#'   # of a comment which supplies the variable name and a short explanation. Most of these are floats,
#'   # a few are integers, and a few are character strings indicating filenames.
#'   #
#'   # Other filenames are listed without comment - instead there is a description on the preceding
#'   # line, and there is no '|' symbol or uppercase variable name to pattern-match. To parse these
#'   # lines, users should set the `nextline=TRUE`, which looks for exact matches to `parname` in any
#'   # part of a line's text, and returns the subsequent line number.
#'   
#'   # prepare the string literals to search for, handling special case differently
#'   sl = paste0('| ', parname)
#'   if(special) { sl = parname }
#'   
#'   # for each of these, search all lines for any instances of the string: produces matrix of booleans...
#'   slmat = sapply(sl, function(pattern) {
#'     sapply(filetext, function(linetext) grepl(pattern, linetext, fixed=TRUE)) })
#'   
#'   # ... where each input parameter name correspond to a column, each row a line number
#'   ln = apply(as.matrix(slmat), 2, which)
#'   
#'   # count the number of matches found for each input parameter name
#'   if(length(ln) == 0)
#'   {
#'     # when none of the `parname` values are found in `filetext`, `ln` becomes `integer(0)`
#'     n.matches = rep(0, length(parname))
#'     
#'   } else {
#'     
#'     n.matches = sapply(ln, length)
#'   }
#'   
#'   # if any of the `parname` entries didn't find a match, warn user
#'   if(any(n.matches==0))
#'   {
#'     print('error: the following variables were not found in input text')
#'     print(parname[n.matches==0])
#'   }
#'   
#'   # error in case of multiple matches
#'   if(any(n.matches>1))
#'   {
#'     print('warning: multiple matches for one or more entries of `parname`. Match counts:')
#'     print(setNames(apply(slmat, 2, sum), nm=parname))
#'     print('returning NA')
#'     return(NA)
#'   }
#'   
#'   # handle special case
#'   if(special) { ln = ln + 1 }
#'   
#'   # handle case of empty input (`parname=''`), which matches all variables
#'   if(identical(parname, ''))
#'   {
#'     # parse the variable names by grabbing line text attribute left by `grepl`
#'     parname.raw = sapply(strsplit(attr(ln, 'dimnames')[[1]], ':|\\|'), function(xx) xx[[2]])
#'     parname = gsub(' ', '', parname.raw)
#'   }
#'   
#'   # finish
#'   return(setNames(as.vector(ln), nm=parname))
#'   
#' }
#' 
#' #' read/write numeric parameters in SWAT configuration text files
#' my_swat_rwnum = function(textpath, parname=character(0), newval=numeric(0), type='f')
#' {
#'   # ARGUMENTS:
#'   #
#'   # `textpath`, character string giving the full path to the swat config file
#'   # `parname`, vector of character strings giving the SWAT parameter names to query
#'   # `newval`, (optional) vector of new parameter values, to overwrite the existing ones
#'   # `type`, character string providing data type: either 'f' = floating point, or 'i' = integer
#'   #
#'   # RETURN VALUE:
#'   #
#'   # A numeric vector of the same length as `parname` (or `newval`, if `parname` is not supplied; See
#'   # below). This reports the (numeric) values of the parameters as they are written in the SWAT text
#'   # file located at `textpath`.
#'   # 
#'   # If `newval` is supplied, its values are written to the SWAT text file. When `newval` is unnamed, 
#'   # the function uses the ordering in `parname` to match values to parameter names. Otherwise, the
#'   # names of the `newval` entries are interpreted as SWAT variable names, and `parname` is ignored
#'   # (and can be ommitted).
#'   
#'   # set (max) number of decimal places to use in character representations
#'   dpmax = 6
#'   
#'   # set a whitespace suffix for parameters (space separating it from comment)
#'   suffix.s = strrep(' ', 4)
#'   
#'   # give priority to names of `newval` when they are provided in combination with `parname`
#'   if(!is.null(names(newval))) { parname = names(newval) }
#'   
#'   # open the file and parse for relevant line numbers 
#'   text.raw = readLines(textpath)
#'   parname.ln = my_swat_parln(text.raw, parname)
#'   linetext = setNames(text.raw[parname.ln], nm=parname)
#'   
#'   # regular expression to trim whitespace and comments
#'   comment.regexp = '\\|.+'
#'   
#'   # visual representation of regexp matches, useful for debugging:
#'   #library(stringr)
#'   #str_view_all(linetext, comment.regexp)
#'   
#'   # grab string containing the parameter (with whitespace) and parse as numeric
#'   partxt = sapply(linetext, function(string) strsplit(string, comment.regexp)[[1]])
#'   parval = sapply(partxt, as.numeric)
#'   
#'   # conditional for write-mode
#'   if(length(newval) > 0)
#'   {
#'     # identify position of comment start position (not sure if always pos 20)
#'     partxt.n = sapply(partxt, nchar)
#'     par.maxn = partxt.n - nchar(suffix.s)
#'     
#'     # for floating point, we round to a known precision level (set by dpmax)
#'     if(type=='f') { newval.s = format(round(newval, dpmax), nsmall=dpmax) }
#'     
#'     # SWAT requires the decimal point be omitted for integer type
#'     if(type=='i') { newval.s = format(round(newval, dpmax), nsmall=0) }
#'     
#'     # truncate (as needed) to stay within max width
#'     idx.trunc = sapply(newval.s, nchar) > par.maxn
#'     if(any(idx.trunc))
#'     {
#'       newval.s[idx.trunc] = sapply(which(idx.trunc), function(idx) {
#'         substr(newval.s[idx], 1, par.maxn[idx]) })
#'       
#'     }
#'     
#'     # pad with leading whitespace to produce strings of exactly max width
#'     if(any(!idx.trunc))
#'     {
#'       newval.s[!idx.trunc] = sapply(which(!idx.trunc), function(idx) {
#'         paste0(strrep(' ', par.maxn[idx] - nchar(newval.s[idx])), newval.s[idx]) })
#'       
#'     }
#'     
#'     # warn if truncation produced any precision issues detectable in R (15 decimal places or so)
#'     idx.imprecise = as.numeric(newval.s) != unname(newval)
#'     if(any(idx.imprecise))
#'     {
#'       print('warning: supplied parameter value(s) required truncation, as follow:')
#'       print(rbind(original=newval, truncated=as.numeric(newval.s))[, idx.imprecise, drop=FALSE])
#'       
#'     }
#'     
#'     # add terminal whitespace
#'     newval.s = paste0(newval.s, suffix.s)
#'     
#'     # substitute replacement values into old line strings
#'     linetext.out =  mapply(gsub, partxt, newval.s, linetext)
#'     text.raw[parname.ln] = linetext.out
#'     
#'     # write changes to SWAT text file
#'     writeLines(text.raw, textpath)
#'     
#'   }
#'   
#'   # return (original) parameter values
#'   return(setNames(parval, nm=parname))
#' }

#' #' read/write integer vectors in SWAT configuration text files
#' my_swat_rwvec = function(textpath, parname=character(0), newval=numeric(0))
#' {
#'   # ARGUMENTS:
#'   #
#'   # `textpath`, character string giving the full path to the swat config file
#'   # `parname`, vector of character strings giving the SWAT parameter names to query
#'   # `newval`, (optional) list of new integer vectors, to replace the ones listed in `linetxt`
#'   #
#'   # RETURN VALUE:
#'   #
#'   # A list of integer vectors, the same length as `parname` (or `newval`, if `parname` is not
#'   # supplied; See below). This reports the integer vectors as they are written in the SWAT text
#'   # file located at `textpath`.
#'   # 
#'   # If `newval` is supplied, its values are written to the SWAT text file. When `newval` is unnamed, 
#'   # the function uses the ordering in `parname` to match values to parameter names. Otherwise, the
#'   # names of `newval` entries are interpreted as SWAT variable names, and `parname` is ignored (and
#'   # can be ommitted).
#'   
#'   # set a whitespace delimiter for vector entries
#'   delim.s = strrep(' ', 3)
#'   
#'   # give preference to names of `newval` when they are provided in combination with `parname`
#'   if(!is.null(names(newval))) { parname = names(newval) }
#'   
#'   # open the file and parse for relevant line numbers 
#'   text.raw = readLines(textpath)
#'   parname.ln = my_swat_parln(text.raw, parname, special=TRUE)
#'   linetext = setNames(text.raw[parname.ln], nm=parname)
#'   
#'   # regular expression to identify delimiters
#'   delim.regexp = '[\\s]+'
#'   
#'   # visual representation of regexp matches, useful for debugging:
#'   #library(stringr)
#'   #str_view_all(linetext, delim.regexp)
#'   
#'   # parse string containing the parameters (with whitespace) as integer
#'   parval = lapply(linetext, function(txt) unlist(read.table(textConnection(txt)), use.names=FALSE))
#'   
#'   # conditional for write-mode
#'   if(length(newval) > 0)
#'   {
#'     # compute lengths of existing parameters
#'     parval.len = sapply(parval, length)
#'     newval.len = sapply(newval, length)
#'     
#'     # if any of the new parameter vectors are longer, truncate them and warn the user
#'     idx.over = newval.len > parval.len
#'     if(any(idx.over))
#'     {
#'       print('warning: supplied parameter vectors(s) longer than existing vectors.')
#'       newval.trunc = mapply(function(x, y) x[1:y], newval[idx.over], parval.len[idx.over])
#'       newval[idx.over] = newval.trunc
#'       newval.len = sapply(newval, length)
#'     }
#'     
#'     # pad replacement integer vectors with zeros if necessary
#'     newval.padded = mapply(function(x, y, z) c(x, rep(0, z-y)), 
#'                            newval, 
#'                            newval.len, 
#'                            parval.len,
#'                            SIMPLIFY=FALSE)
#'     
#'     # coerce to character strings
#'     newval.s = sapply(newval.padded, function(vec) paste(c('', vec), collapse=delim.s))
#'     
#'     # substitute replacement values into old line strings
#'     text.raw[parname.ln] = newval.s
#'     
#'     # write changes to SWAT text file
#'     writeLines(text.raw, textpath)
#'     
#'   }
#'   
#'   # return (original) parameter values
#'   return(setNames(parval, nm=parname))
#' }
############


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