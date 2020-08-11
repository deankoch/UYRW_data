#' ---
#' title: "example.R"
#' author: "Dean Koch"
#' date: "June 19, 2020"
#' output: github_document
#' ---
#'
#' MITACS UYRW project
#' A basic example script for getting started working with watershed data in R.
#' 
#' For now, this simply follows the example on the `nhdplusTools` github page with some minor modifications.
#' A user guide for the NHDPlus dataset is
#' <a href="https://tinyurl.com/y54rlqja" target="_blank">available here</a>, and a 
#' <a href="https://tinyurl.com/y54rlqja" target="_blank">data dictionary here</a>.

## libaries

#' I'll start by downloading some GIS data on the hydrology of the UYR upstream of Big Timber, MT. The `nhdplusTools` 
#' package can be used to fetch this data without having to navigate the USGS website (uncomment below to install the package) 
# install.packages('nhdplusTools')
library(nhdplusTools)

#' The `sf` package is needed for handling GIS data. The `smoothr` package is handy for simplifying complex spatial features
#' and generating polygons from rasters.
library(sf)
library(smoothr)

#' To define the watershed, we need a starting location. I will use Big Timber, Montana, and define the UYRW to 
#' include all catchments upstream. The `AOI` package interfaces with OpenStreetMaps (OSM) to get lat/long info 
#' from placenames. This package is loaded automatically with package `HydroData`, which we will use later on to  
#' pull more hydrology datasets (uncomment two lines below to install from github using devtools).
#library(devtools)
#install_github('mikejohnson51/HydroData')
library(HydroData)

#' Data on geographical landmarks and highways can be downloaded from OSM using the overpass API via the `osmdata` package
library(osmdata)

#' The `here` package is helpful for defining working directories on portable code
library(here)

## starting location

# define a source outlet from which to explore upstream 
BigTimber.pt = geocode(location='Big Timber', pt=TRUE)

# get a COMID for the source outlet
BigTimber.comid = discover_nhdplus_id(BigTimber.pt)

## downloading the data

#' package nhdplusTools will use the COMID from Big Timber to delineate the watershed and download the relevant data.
#' To avoid downloading things over and over again, first define a permanent storage location on disk:

# use the `here` package to define a subdirectory of the RStudio project folder, and create it as needed
data.dir = here('bigfiles') # These are large binaries, not suitable for git
if(!dir.exists(data.dir))
{
  dir.create(data.dir, recursive=TRUE)
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

## data prep

#' Once the data are downloaded, we can load them into R as sfc objects

# load the watershed data
BigTimber.flowline = read_sf(BigTimber.nhdfile, 'NHDFlowline_Network')
BigTimber.catchment = read_sf(BigTimber.nhdfile, 'CatchmentSP')
BigTimber.waterbody = read_sf(BigTimber.nhdfile, 'NHDWaterbody')

#' There are many thousands of catchments...
print(nrow(BigTimber.catchment))

#' ...Their union delineates the entire UYR watershed as a single polygon. Holes in this polygon may emerge if the 
#' catchements boundaries don't perfectly align (try plotting `st_union(BigTimber.catchment)`). These can be filled 
#' using the *fill_holes* function in the `smoothr` package.
UYRW.poly = fill_holes(st_union(BigTimber.catchment, by_feature=FALSE), threshold=1e6)

#' The water body geometries fetched by `nhdplusTools` include some invalid geometries (self-intersections) and features 
#' lying outside this watershed. Clean up this sfc object before continuing...
BigTimber.waterbody = st_intersection(st_make_valid(BigTimber.waterbody), UYRW.poly)

#' ...and just to be sure, do the same for the flowlines
BigTimber.flowline = st_intersection(st_make_valid(BigTimber.flowline), UYRW.poly)

#' It will be useful to have a line geometry representing the main stem and main tributaries of the river

# find and join all line segments labeled as the 'Yellowstone River'
Yellowstone.flowline = BigTimber.flowline[BigTimber.flowline$gnis_name == 'Yellowstone River',]

# fix self-intersection issues
Yellowstone.flowline = st_make_valid(st_union(Yellowstone.flowline, by_feature=FALSE))

## visualization

# create a directory for storing graphics
graphics.dir = here('graphics')
if(!dir.exists(graphics.dir))
{
  dir.create(graphics.dir, recursive=TRUE)
}

# plot the watershed flowlines and water bodies as a png file
#+ eval=FALSE
png(file.path(graphics.dir, 'UYRW_flowlines.png'), height=800, width=400)
  plot(st_geometry(UYRW.poly), col='greenyellow', main='Upper Yellowstone River Watershed \n(upstream of Big Timber)')
  plot(st_geometry(BigTimber.flowline), col='dodgerblue3', add=TRUE)
  plot(st_geometry(Yellowstone.flowline), col='dodgerblue4', lwd=2, add=TRUE)
  plot(st_geometry(BigTimber.waterbody), col='deepskyblue3', border='deepskyblue4', add=TRUE)
  plot(BigTimber.pt, cex=1.5, lwd=2, col='red', add=TRUE)
dev.off()







#' Work in progress below...
#+ eval=FALSE

# AOI package (loaded by HydroData) has a nice interactive map feature
aoi_map(BigTimber.pt)

#+ include=FALSE
# Convert to markdown by running the following line (uncommented)...
# rmarkdown::render(here('example.R'), run_pandoc=FALSE, clean=TRUE)
# ... or to html ...
# rmarkdown::render(here('example.R'))
