helper\_main.R
================
Dean Koch
2021-03-25

**Mitacs UYRW project**

**helper\_main**: general helper functions for all scripts in the
UYRW\_data repository

This script is meant to be sourced by all other scripts in the
repository. It defines some helper functions and directories for local
storage.

## libraries

These CRAN packages are quite useful, and are at many stages in the
repository workflow. If any of these are not already installed on your
machine, run `install.packages(...)` to get them.
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

[‘data.table’](https://cran.r-project.org/web/packages/data.table/index.html)
for large I/O files

``` r
library(data.table)
```

[`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html)
GDAL wrapper

``` r
library(gdalUtilities)
```

[`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf) web
scraping

``` r
library(rvest) 
```

[`units`](https://cran.r-project.org/web/packages/units/index.html)
units for numerics

``` r
library(units)
```

[`jsonlite`](https://cran.r-project.org/web/packages/jsonlite) handle
JSON I/O

``` r
library(jsonlite)
```

## global variables

``` r
# TODO: wrap these in a function or make their variable names more unique

# 'graphics', 'markdown', 'data' are top level directories in the RStudio project folder
graphics.dir = 'graphics'
markdown.dir = 'markdown'
data.dir = 'data'

# subdirectories of `data` contain source files and their processed output 
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# missing data field (NA) is coded as "-99.0"
tif.na.val = -99
```

## project data

To avoid downloading things over and over again, we’ll use a permanent
storage location on disk (/data). This is where we store large data
files and R object binaries, which are not suitable for git.

The `if(!file.exists(...))` conditionals preceding each code chunk
indicate which files will be written in that section. If the files are
detected in the local data storage directory, then that code chunk can
be skipped (to avoid redundant downloads, *etc*), and the files are
loaded from disk instead.

We start by defining a project directory tree Define a helper function
for creating folders then create project folders as needed

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

This project will generate many files. To keep track of everything, each
script gets a CSV table documenting every file that it creates: its file
path, its type, and a short description of the contents. This function
handles the construction of the table. To call up the table for a
specific script, simply use `my_metadata(script.name)`.

``` r
my_metadata = function(script.name, entries.list=NA, overwrite=FALSE, use.file=TRUE, data.dir='data', v=TRUE)
{
  # creates and/or adds to a data frame of metadata documenting a given script, and (optionally)
  # writes it to disk as a CSV file 
  #
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
```

My R scripts are commented using a roxygen2 syntax that is interpretable
by `rmarkdown`, for conversion to markdown via pandoc. This convenience
function renders the markdown file for a given R script and writes to a
file of the same name (but with a .md extension).

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
  # Writes the file <project directory>/<markdown.dir>/<script.name>.md, overwriting without warning
  
  # set up in/out files
  path.input = here(script.dir, paste0(script.name, '.R'))
  path.output = here(file.path(markdown.dir, paste0(script.name, '.md')))
  
  # note: run_pandoc=FALSE appears to cause output_dir to be ignored. So this call also generates an html file
  paste('rendering markdown file', path.output, 'from the R script', path.input)
  rmarkdown::render(path.input, clean=TRUE, output_file=path.output)
}
```

R has a built-in library of `map` objects, the geographical coordinates
of adminstrative boundaries and point locations (eg states, countries,
cities) in list form. Since we are working with the `sf` package in a
projected coordinate system it’s handy to have a helper function to wrap
`maps::map` calls

``` r
my_maps = function(db, outcrs=NULL)
{
  # ARGUMENTS:
  #
  # `db`, character, name of geographical database (see `help(package='maps')`)
  # `outcrs`, integer or character, passed to `st_transform`
  #
  # RETURN VALUE:
  #
  # sf object containing all polygons from the maps database
  #
  # DETAILS: 
  #
  # Output sf will have columns 'name' (the polygon name, usually a placename or qualifier)
  # and 'mapsdb' (the name of the maps database it came from).
  #
  # If `db` matches none of the exported databases from `maps`, the function loads the
  # `world` database and does a case insensitive search among its 1000+ names. If this
  # matches nothing, the function returns an empty  
  # 
  
  # default is unprojected lat/long
  crs.geo = 4326
  if( is.null(outcrs)) outcrs = crs.geo
  
  # hack to check if supplied database is a valid name 
  maps.nmspc = get('.__NAMESPACE__.', inherits=FALSE, envir=asNamespace('maps', base.OK=FALSE))
  db.found = paste0(db, 'MapEnv') %in% names(maps.nmspc$lazydata)
  
  # fetch the data if `db` is lazyloaded from maps
  if(db.found)
  {
    # open the database and extract polygon names and coordinates
    maps.out = maps::map(db, plot=F, fill=T)
    db.nm = maps.out$names
    db.xy = do.call(cbind, maps.out[c('x', 'y')])
    
    # distinct polygons are separated by NAs in the coords vectors
    x.isdelim = is.na(db.xy[,1])
    
    # make an indexing vector to split over NA delimiters (omitting them)
    idx.out = split( seq_along(db.xy[,1])[ !x.isdelim ], cumsum( x.isdelim )[ !x.isdelim ] )
    db.list = lapply(setNames(idx.out, db.nm), function(x) db.xy[x,])
    
    # convert this list to a single sf object
    db.sfc = lapply(db.list, function(x) st_sfc(st_polygon(list(x)), crs=crs.geo))
    db.sf = st_sf(data.frame(nm=names(db.sfc), db=db), do.call(c, db.sfc))

    # clean up column names, transform to `outcrs` projection, and finish
    names(db.sf) = c('name', 'mapsdb', 'geometry')
    return( st_transform( st_set_geometry(db.sf, 'geometry'), crs=outcrs ) )
    
  } else {
    
    # otherwise grep for the supplied string in `world` database
    world.result = my_maps('world')
    idx.world = grepl(db, world.result$name, ignore.case=TRUE)
    
    # return any matches, and print a warning if there were none
    if(sum(idx.world) == 0) warning('no matches for supplied `db`')
    return( world.result[idx.world,] )
  }
}
```
