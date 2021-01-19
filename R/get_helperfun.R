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

#' read daily simulation output text files from SWAT+
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
  # vector of Date objects. Note this is only possible with daily data, since monthly and
  # yearly summaries have no single defined `Date`
  #
  
  # line numbers of the headers for variable names and units
  ln.header = c(varname=2, unit=3)
  
  # check that the file exists
  if(!file.exists(out.path))
  {
    stop(paste('file', basename(out.path), 'was not found at the specified path'))
  }
  
  # scan the headers line to get all listed output variable names
  out.varname = scan(out.path, what=character(), skip=ln.header['varname']-1, nlines=1, quiet=TRUE)
  out.unit = scan(out.path, what=character(), skip=ln.header['unit']-1, nlines=1, quiet=TRUE)
  n.varname = length(out.varname)
  n.unit = length(out.unit)
  
  # ignore empty 'type' fields, which mess up R's delimeter parsing
  if('type' %in% out.varname)
  {
    out.varname = out.varname[out.varname!='type']
    n.varname = length(out.varname)
  }
  
  # first few fields are keys for location/time - their names sometimes appear on units line
  if(n.varname < n.unit)
  {
    # key names on same line as units
    n.key = n.unit - n.varname
    is.key = 1:n.unit %in% 1:n.key
    key.varname = out.unit[is.key]
    out.unit = out.unit[!is.key]
    
  } else {
    
    n.key = n.varname - n.unit
    is.key = 1:n.varname %in% 1:n.key
    key.varname = out.varname[is.key]
    out.varname = out.varname[!is.key]
    n.varname = length(out.varname)
  }

  # build a dictionary of unusual unit strings and their R-readable equivalents
  unit.lu = do.call(rbind, list(c('ha-m', 'ha m'),
                                c('kgN', 'kg'),
                                c('kgP', 'kg'),
                                c('kg/ha_N', 'kg/ha'),
                                c('kg/ha_P', 'kg/ha'),
                                c('mg_pst', 'mg'),
                                c('tha', 't ha'),
                                c('kgha', 'kg ha'),
                                c('mj/m^2', 'mJ/m^2'),
                                c('mton', 'tonne'),
                                c('frac', ''),
                                c('----', ''),
                                c('---', ''),
                                c('___', '')))
  
  # note: I'm assuming 'mton' means 'metric tonne', and not 'milliton' (ie kilogram)
  
  # swap in the R-readable unit strings and name the units 
  idx.replace = out.unit %in% unit.lu[,1]
  out.unit[idx.replace] = unit.lu[match(out.unit[idx.replace], unit.lu[,1]), 2]
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
  
  # index of columns to load
  load.idx = all.varname %in% varname
  
  # verify that there is at least one row of data
  dat.test = scan(out.path, what=character(), skip=max(ln.header), nlines=1, quiet=TRUE)
  if(length(dat.test) > 0)
  {

    # load the requested columns and efficiently skip the others using `fread`
    dat.out = as.data.frame(setNames(fread(out.path, 
                                           header=FALSE, 
                                           skip=max(ln.header),  
                                           colClasses=load.what, 
                                           drop=which(!load.idx)), nm=all.varname[load.idx]))
    
  } else {
    
    # case: file is empty: create empty data frame with correct column names, classes
    dat.out = read.table(text='', 
                         colClasses=load.what[load.idx], 
                         col.names=all.varname[load.idx])
    
  }
  
  # collect info from key columns
  n.obs = nrow(dat.out)
  gis.ids = unique(dat.out$gis_id)
  start.date = as.Date(paste(dat.out$yr[1], dat.out$jday[1], sep='-'), format='%Y-%j')
  end.date = as.Date(paste(dat.out$yr[n.obs], dat.out$jday[n.obs], sep='-'), format='%Y-%j')
  dates = c(first=start.date, last=end.date)
  n.loc = length(gis.ids)
  
  # case: file is empty: 
  if(nrow(dat.out)==0)
  {
    dates = c(first=NA, last=NA)
    gis.ids = NA
    tstep = 'unknown'
    counts = c(n.obs=0, n.loc=0)
    
  } else {
    
    # parse daily, monthly, or yearly by looking at first two entries from the first gis_id
    is.incr = apply(dat.out[c(1, n.loc+1), c('jday', 'yr')], 2, diff)
    tstep = c('day', 'year', 'monthly')[c(as.logical(is.incr), !any(is.incr))]
    counts = setNames(c(n.obs, n.loc, n.obs/n.loc), c('n_obs', 'n_geo', paste0('n_', tstep)))
    
  }

  # return this information in default mode
  if(!return.data)
  {
    # return the info in a list
    return(list(n=counts, dates=dates, name=all.varname, gis_ids=gis.ids))
    
  }
  
  # copy column names before (possibly) taking subset later on
  dat.names = names(dat.out)
  include.names = dat.names
  
  # units are assigned to the state variables if requested
  if(set.units)
  {
    # loop over column names matching non-key (ie state variable) values
    for(idx.col in which(!(dat.names %in% key.varname)))
    {
      # units were read from headers line
      unit.col = out.unit[names(dat.out)[idx.col]]
      
      # `set_units` doesn't seem to have a mode for undefined so we code it as '' 
      if(unit.col != '')
      {
        dat.out[,idx.col] = units::set_units(dat.out[,idx.col], unit.col, mode='standard')
      }
      
    }
  }
  
  # assign dates if requested 
  if(add.dates & tstep=='day')
  {
    # define a sequence of Date objects to match the period in the output file
    ts.dates = seq(start.date, end.date, tstep)
    
    # this should be equivalent to `as.Date(paste(yr, mon, day, sep='-')`, but faster
    all.dates = rep(ts.dates, each=n.loc)
    
    # append to `dat.out` as new column
    dat.out = setNames(cbind(all.dates, dat.out), c('date', dat.names))
    
    # omit redundant columns
    include.names = names(dat.out)[!(names(dat.out) %in% c('jday', 'mon', 'day', 'yr'))]
  }
  
  return(dat.out[, include.names])
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
my_taudem = function(dem, odir, streams=NULL, burn.depth=10)
{
  # ARGUMENTS:
  #
  # `dem`: raster object, a digital elevation model (units of meters)
  # `odir`: path to the directory to write output files
  # `streams`: sf object, the line geometry for drainage reinforcement ('burn-in')
  # `bdepth`: numeric (in meters), the vertical height to subtract from the DEM for burn-in
  #
  # RETURN VALUE:
  #
  # A named vector of character strings supplying paths to the TauDEM output files
  # 
  # DETAILS:
  #
  # Note that any point 

  # create the output directory if necessary
  my_dir(odir)
  
  # copy DEM and burn-in streams if necessary 
  dem.ipath = file.path(odir, 'dem_in.tif')
  if(is.null(streams))
  {
    # write an unmodified copy of the DEM to the output directory
    writeRaster(dem, dem.ipath, 
                options=c('COMPRESS=NONE, TFW=YES'),
                format='GTiff', 
                overwrite=TRUE)
    
  } else {
    
    # assign units to burn-in depth
    burn.depth = units::set_units(burn.depth, m)
    
    # set path to write in output directory
    streams.ipath = file.path(odir, 'streams_toburn.tif')
    
    
    
    streams = st_geometry(streams[st_is(streams, 'LINESTRING'),])
    
    # make sure the streams network contains only LINESTRING geometries, drop attributes
    streams = st_cast(st_geometry(streams[!st_is(streams, 'POINT'),]), 'LINESTRING')
    
    # convert streams to `Spatial`, add unit dummy field, rasterize
    streams.sp = as(streams, 'Spatial')
    streams.sp$dummy = rep(1, length(streams.sp))
    gRasterize(streams.sp, dem, field='dummy', filename=streams.ipath)
    
    # find cell numbers of non-NA cells in the streams raster 
    streams.idx = Which(!is.na(raster(streams.ipath)), cells=TRUE) 
    
    # extract elevations at all cells intersecting with a stream
    streams.elev = units::set_units(extract(dem, streams.idx), m)
    
    # decrement these elevations and write burned DEM file as uncompressed GeoTIFF
    dem[streams.idx] = as.vector(streams.elev - burn.depth)
    writeRaster(dem, dem.ipath, 
                options=c('COMPRESS=NONE, TFW=YES'),
                format='GTiff', 
                overwrite=TRUE)
    
  }

  
  # 
  
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