#' ---
#' title: "get_outlet.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_outlet**: downloads the USGS
#' [GAP/LANDFIRE](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-overview)
#' National Terrestrial Ecosystems dataset, an approximately 30m x 30m gridded land cover map of the US (see metadata 
#' [here](https://www.sciencebase.gov/catalog/file/get/573cc51be4b0dae0d5e4b0c5?f=__disk__5d/11/f4/5d11f4366a3402f7e0d23ffa77258a4e12f04809&transform=1)).
#' Each pixel is mapped to a US National Vegetation Classification (NVC) code, providing an inventory of typical plant assemblages
#' that we can use to assign SWAT+ plant growth paramaters. 
#' 
#' This code fetches the data, warps to our reference coordinate system, and prepares SWAT+ AW input files.
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' should be run before this script.

#'
#' ## libraries
#' [`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html) provides a wrapper
#' for GDAL calls to warp the land use raster, and the base package
#' [`grid`](https://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid-package.html) allows more control
#' over plot layouts.
#' [`colorspace`](https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html)
#' provides some predefined color palettes for plotting.
#' See the [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries 
library(here)
source(here('R/get_helperfun.R'))
library(raster)
library(gdalUtilities)
library(grid)
library(colorspace)


x = 'C:/SWAT/SWATPlus/Workflow/example_data/shapefiles/outlets.shp'
x = 'H:/UYRW_data/robit/QSWATPlus/testdata/ravn_outlet.shp'


#+ include=FALSE
# Development code
#my_markdown('get_outlet')
