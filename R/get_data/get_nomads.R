#' ---
#' title: "get_nomads.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_nomads**: download and import North American Mesoscale (NAM) and other archived forecasts
#' 
#' ## libraries
#' [`rNOMADS`](https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf) retrieves datasets from the
#' NOAA archives. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(rNOMADS)

# load CRS info list and watershed polygon from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))

# prepare a mask in geographical coordinates
uyrw.poly.geo = st_transform(uyrw.poly, crs=crs.list$epsg.geo)

# define 3km buffer around the perimeter of the study area
uyrw.buff = 3e3 # meters






#'
#' ## rNOMADS
NOMADSArchiveList(gfsanl)
x = CheckNOMADSArchive('namanl', '20200101')
ArchiveGribGrab('namanl', '20050101', 0, 0)
xx = raster('H:\\UYRW_data/20200101_0000_003.grb2', band=5)
yy = crop(xx, as(st_transform(uyrw.poly, st_crs(xx)), 'Spatial'))
plot(yy)




#+ include=FALSE
# Development code
#my_markdown('get_nomads')
