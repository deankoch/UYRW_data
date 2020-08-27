#' ---
#' title: "get_helperfun.R"
#' author: "Dean Koch"
#' date: "August 26, 2020"
#' output: github_document
#' ---
#'
#' **MITACS UYRW project** 
#' 
#' **get_helperfun**: helper functions for the scripts in the UYRW_data repository 
#' 
#' This script is meant to be sourced by all other scripts in the repository. It defines some helper functions, 
#' directories for local storage, and some R code that loads commonly used datasets if they are detected in the data directory.
#' 

#'
#' ## libraries
#' These CRAN packages are quite useful, and are required by most of the scripts in the repository, so I load them by default.
#' If any of these are not already installed on your machine, run `install.packages(...)` to get them:

#' `sf` handles GIS data such as shapefiles
library(sf)

#' `tmap` constructs nice ggplot2-based thematic map graphics.
library(tmap)


#'
#' ## project data

#' To avoid downloading things over and over again, we'll use a permanent storage location on disk ("/data").
#' This is where we store large data files and R object binaries, which are not suitable for git.
#' 
#' The `if(!file.exists(...))` conditionals preceding each code chunk indicate which files will be written in that section.
#' If the files are detected in the local data storage directory, then code chunk can be skipped (to avoid redundant downloads,
#' *etc*), and the files are loaded from disk instead. 
#' 
#' We start by defining a project directory tree and a list of files and metadata to download

#+ results='hide'
# 'graphics', 'markdown', 'data' are top level directories in the RStudio project folder
graphics.dir = 'graphics'
markdown.dir = 'markdown'
data.dir = 'data'

# subdirectories of `data` contain source files and their processed output 
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# a helper function for creating folders
my_dir = function(path) { if(!dir.exists(path)) {dir.create(path, recursive=TRUE)} }

# create folders as needed
lapply(here(c(data.dir, src.subdir, out.subdir, graphics.dir, markdown.dir)), my_dir)


# creates and/or adds to a data frame of metadata documenting a given script, and (optionally) writes it to disk as a CSV file 
my_metadata = function(script.name, entries.list=NA, overwrite=FALSE, use.file=TRUE, data.dir='data')
{
  # ARGUMENTS:
  #
  # `script.name` is a string indicating the filename (without the .R extension) of the R script to document.
  # `entries.list` is a list of character vectors, whose entries are named: 'name', 'file', 'type', and 'description'.
  # `data.dir` is the subdirectory of the project folder in which to write the CSV file: /data/`script.name_metadata`.csv 
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
  # the modified data frame. If the CSV file does not already exist on disk, it will be created. The default `entries.list`==NA, 
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
    
    # otherwise look for the file on disk and load it if it exists, creating a second data frame
    if(file.exists(here(csv.relpath)) & !csv.wipe)
    {
      # load the csv data 
      csv.df = read.csv(here(csv.relpath), header=TRUE, row.names=1)
      
      # identify any entries with names matched in entries.list, update them, and delete those rows from entries.list
      names.updating = rownames(csv.df)[rownames(csv.df) %in% rownames(input.df)]
      csv.df[names.updating,] = input.df[names.updating,]
      input.df = input.df[!(rownames(input.df) %in% names.updating),]
    }
    
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


#+ include=FALSE
# render as markdown by uncommenting the following line (note: run_pandoc=FALSE causes output_dir to be ignored)
#rmarkdown::render(here(paste0('get_helperfun', '.R')), clean=TRUE, output_file=here(file.path(markdown.dir, paste0('get_helperfun', '.md'))))