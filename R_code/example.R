#' ---
#' title: "example.R"
#' author: "Dean Koch"
#' date: "June 19, 2020"
#' output: github_document
#' ---
#'
#' MITACS UYRW project
#' A basic example script for working with watershed data in R.
#' This simply follows the example on the nhdplusTools github page with some minor modifications

# install and load the NHDPlus Tools package for fetching data, and load sf for GIS
install.packages('nhdplusTools')
library(nhdplusTools)
library(sf)

# install the AOI and HydroData packages from github using devtools
library(devtools)
install_github('mikejohnson51/HydroData')
library(HydroData)
# hydrodata has TONS of good links to data sources, incl sensor gages

# define an origin point from which to explore outward
YellowstoneLake.pt = geocode(location='Yellowstone Lake', pt=TRUE)

# define a source outlet from which to explore upstream 
BigTimber.pt = geocode(location='Big Timber', pt=TRUE)

# AOI package (loaded by HydroData) has a nice interactive map feature
aoi_map(BigTimber.pt)

# get a COMID for the source outlet
BigTimber.comid = discover_nhdplus_id(BigTimber.pt)

# download a line geometry defining flowlines upstream of Big Timber, MT
BigTimber.flowlines = navigate_nldi(list(featureSource='comid', featureID=BigTimber.comid), mode='upstreamTributaries', data_source = '')

# notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
print(BigTimber.flowlines$nhdplus_comid)

# download geometries defining catchements, water bodies, and the full flowline network
BigTimber.nhdfile = tempfile(fileext = '.gpkg')
subset_nhdplus(comids=BigTimber.flowlines$nhdplus_comid, output_file=BigTimber.nhdfile, nhdplus_data='download')

# load these data...
BigTimber.flowline = read_sf(BigTimber.nhdfile, 'NHDFlowline_Network')
BigTimber.catchment = read_sf(BigTimber.nhdfile, 'CatchmentSP')
BigTimber.waterbody = read_sf(BigTimber.nhdfile, 'NHDWaterbody')

# ...and plot them
plot(st_geometry(BigTimber.flowline), col='blue', main='UYRW upstream of Big Timber, MT')
plot(BigTimber.pt, cex=1.5, lwd=2, col='red', add=TRUE)
plot(st_geometry(BigTimber.catchment), col=rgb(0, 0, 0, alpha=0.2), border=NA, add=TRUE)
plot(st_geometry(BigTimber.waterbody), col=rgb(0, 0, 1, alpha=0.5), border=NA, add=TRUE)

#+ include=FALSE
# Convert to markdown by running the following line (uncommented)...
# rmarkdown::render(here('R-code/example.R'), run_pandoc=FALSE, clean=TRUE)
# ... or to html ...
# rmarkdown::render(here('R-code/example.R'))
