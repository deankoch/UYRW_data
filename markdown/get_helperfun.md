get\_helperfun.R
================
Dean Koch
2020-11-06

**Mitacs UYRW project**

**get\_helperfun**: helper functions for the scripts in the UYRW\_data
repository

This script is meant to be sourced by all other scripts in the
repository. It defines some helper functions, directories for local
storage, and some R code that loads commonly used datasets if they are
detected in the data directory.

## libraries

These CRAN packages are quite useful, and are required by some of the
scripts in the repository. If any of these are not already installed on
your machine, run `install.packages(...)` to get them.
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

[`ggplot2`](https://ggplot2.tidyverse.org/) popular graphics package
with high-level abstraction

``` r
library(ggplot2)  
```

[`tmap`](https://github.com/mtennekes/tmap) constructs pretty thematic
map graphics

``` r
library(tmap)
```

[`dplyr`](https://dplyr.tidyverse.org/R) tidyverse-style manipulation of
tabular data

``` r
library(dplyr)
```

[‘RSQLite’](https://www.r-project.org/nosvn/pandoc/RSQLite.html)
connects to SQLite databases

``` r
library(RSQLite)
```

## project data

To avoid downloading things over and over again, we’ll use a permanent
storage location on disk (“/data”). This is where we store large data
files and R object binaries, which are not suitable for git.

The `if(!file.exists(...))` conditionals preceding each code chunk
indicate which files will be written in that section. If the files are
detected in the local data storage directory, then that code chunk can
be skipped (to avoid redundant downloads, *etc*), and the files are
loaded from disk instead.

We start by defining a project directory tree

``` r
# 'graphics', 'markdown', 'data' are top level directories in the RStudio project folder
graphics.dir = 'graphics'
markdown.dir = 'markdown'
data.dir = 'data'

# subdirectories of `data` contain source files and their processed output 
src.subdir = 'data/source'
out.subdir = 'data/prepared'
```

Define a helper function for creating folders then create project
folders as needed

``` r
my_dir = function(path) { if(!dir.exists(path)) {dir.create(path, recursive=TRUE)} }
lapply(here(c(data.dir, src.subdir, out.subdir, graphics.dir, markdown.dir)), my_dir)
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL

NAs are represented in the GeoTiff by an integer – usually something
large and negative. The default value for this integer when using
`raster::writeRaster` is so large that it can cause an integer overflow
error in one of the python modules used by QSWAT+ (at least on my 64bit
machine). We instead use the value recommended in the QSWAT+ manual:

``` r
tif.na.val = -32767
```

This project will generate many files. To keep track of everything, each
script gets a CSV table documenting every file that it creates: its file
path, its type, and a short description of the contents. This function
handles the construction of the table. To call up the table for a
specific script, simply use `my_metadata(script.name)`.

``` r
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
```

My R scripts are commented using a roxygen2 syntax that is interpretable
by `rmarkdown`. This convenience function renders the markdown file for
a given R script and writes to a file of the same name (but with a .md
extension).

``` r
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
```

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
  #   `prec`, `tmax`, `tmin`, `wind`; the daily values for each variable
  #
  # `spat`, a named list of five R objects
  #   `pts`, an sf object locating each grid point (in same projection as `perim`)
  #   `prec`, `tmax`, `tmin`, `wind`; rasterstacks with the daily values of each variable
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

  # define the variable names to extract (ignore case in output names)
  livneh.vars = c('Prec', 'Tmax', 'Tmin', 'wind')
  names(livneh.vars) = tolower(livneh.vars)
  
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
# swat.ds.conn = dbConnect(SQLite(), 'H:/UYRW_SWAT/SWATPlus/Databases/swatplus_datasets.sqlite')
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

In addition to having different file and data structures, SWAT2012 and
SWAT+ (currently) differ in the default parameter sets that are included
in each release. For example, while most of the plant growth parameters
appearing SWAT2012’s reference database ‘crop’ table can be found in the
SWAT+ database, there are many plant codes in SWAT+ that have not yet
been added to SWAT2012.

This helper function translates the relevant data from the SWAT+
reference database into a form that can be written to the SWAT2012
reference database (mdb), so the missing plant codes can be used in a
SWAT2012 model

``` r
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
