#' ---
#' title: "example.R"
#' author: "Dean Koch"
#' date: "June 19, 2020"
#' output: github_document
#' ---
#'
#' MITACS UYRW project
#' A basic example script for getting started working with watershed data in R.
#' For now, this simply follows the example on the `nhdplusTools` github page with some minor modifications

#' Start by downloading some GIS data on the hydrology of the UYR upstream of Big Timber, MT.
#' The `nhdplusTools` package is needed for fetching this data (uncomment below to install), 
#' and the `sf` package is used for handling GIS data
# install.packages('nhdplusTools')
library(nhdplusTools)
library(sf)

#' A starting location is needed to define the watershed. I will use Big Timber, Montana, and define the UYRW to 
#' include all catchments upstream. The `AOI` package will interfacing with OpenStreetMaps to get lat/long info 
#' from placenames. This package is loaded automatically with package `HydroData`, which we will use later on to  
#' pull more hydrology datasets (uncomment two lines below to install from github using devtools).
#library(devtools)
#install_github('mikejohnson51/HydroData')
library(HydroData)

# define a source outlet from which to explore upstream 
BigTimber.pt = geocode(location='Big Timber', pt=TRUE)

#' package nhdplusTools will use the COMID from Big Timber to delineate the watershed and download the relevant data.
#' To avoid downloading things over and over again, first define a permanent storage location on disk:

# get a COMID for the source outlet
BigTimber.comid = discover_nhdplus_id(BigTimber.pt)

# use the `here` package to define a subdirectory of the RStudio project folder, and create it as needed
library(here)
data.dir = here('bigfiles') # These are large binaries, not suitable for git
if(!dir.exists(data.dir))
{
  dir.create(data.dir, recursive=TRUE)
  print(paste('data directory', data.dir, 'created'))
  
} else {
  
  print(paste('data directory', data.dir, 'exists'))
}

# define a filename for the hydrology GeoPackage (gpkg) file from NHD
BigTimber.nhdfile = file.path(data.dir, 'nhd_basic.gpkg')

#' These next few lines will use the source outlet location (BigTimber.pt) to find/download relevant watershed geometries.
#' I've wrapped them in a "if file.exists" conditional to skip this step after the data are downloaded once. 
if(!file.exists(BigTimber.nhdfile))
{
  # download a line geometry defining flowlines upstream of Big Timber, MT
  BigTimber.flowlines = navigate_nldi(list(featureSource='comid', featureID=BigTimber.comid), mode='upstreamTributaries', data_source = '')
  
  # notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
  print(BigTimber.flowlines$nhdplus_comid)
  
  # download geometries defining catchements, water bodies, and the full flowline network
  subset_nhdplus(comids=BigTimber.flowlines$nhdplus_comid, output_file=BigTimber.nhdfile, nhdplus_data='download')
}

#' Note there is a warning message on the last call, indicating that the package has not been tested on such a large watershed 
#' (indicating I should go through by hand later to verify it fetched everything).
#' 
#' Once the data are downloaded, we can load them and print out a map of the study area

# load the watershed data...
BigTimber.flowline = read_sf(BigTimber.nhdfile, 'NHDFlowline_Network')
BigTimber.catchment = read_sf(BigTimber.nhdfile, 'CatchmentSP')
BigTimber.waterbody = read_sf(BigTimber.nhdfile, 'NHDWaterbody')

# ...and plot them
plot(st_geometry(BigTimber.flowline), col='blue', main='Upper Yellostone River Watershed (upstream of Big Timber)', cex.main=0.5)
plot(BigTimber.pt, cex=1.5, lwd=2, col='red', add=TRUE)
plot(st_geometry(BigTimber.catchment), col=rgb(0, 0, 0, alpha=0.2), border=NA, add=TRUE)
plot(st_geometry(BigTimber.waterbody), col=rgb(0, 0, 1, alpha=0.5), border=NA, add=TRUE)


#' Work in progress below...
#+ eval=FALSE

# AOI package (loaded by HydroData) has a nice interactive map feature
aoi_map(BigTimber.pt)

#+ include=FALSE
# Convert to markdown by running the following line (uncommented)...
# rmarkdown::render(here('example.R'), run_pandoc=FALSE, clean=TRUE)
# ... or to html ...
# rmarkdown::render(here('example.R'))
