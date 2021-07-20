#' ---
#' title: "download_gfs"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **download_gfs**: uses `rgee` to download weather datasets from GFS
#' 
# TODO: write up markdown and hide the full loop

#' 
#' ## R libraries and storage

# load helper functions
library(here)
source(here('R/helper_main.R'))
source(here('R/rgee/helper_gee.R'))

# make download directory for the GeoTIFFs from this collection
gfs.path = 'NOAA/GFS0P25'
dest = here(src.subdir, 'rgee', gfs.path)
dest.json = file.path(dest, 'gfs_forecast_files.json')
my_dir(dest)

# start up the Python GEE API and R interface 
ee_Initialize()

# load the URYW boundary and a padded version to use for masking
basins.meta = my_metadata('get_basins')
uyrw = readRDS( here(basins.meta['boundary', 'file']) )
boundary = readRDS( here(basins.meta['boundary_padded', 'file']) )

# get a list of info about the GFS 384hr collection
info = rgee_info(gfs.path)

# set up dates to download
today = as.Date(Sys.time())
dates = seq.Date(info$dates[1], info$dates[2], by='day')

# call the downloader
gfs.files = rgee_gfs_forecast(dates, boundary, dest, info)

# save list of files to disk as JSON
jsonlite::toJSON(gfs.files, pretty=TRUE) %>% write(dest.json)

#+ include=FALSE
#library(here)
#source(here('R/helper_main.R'))
#my_markdown('download_gfs', 'R/rgee')