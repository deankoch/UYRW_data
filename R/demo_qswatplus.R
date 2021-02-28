#' ---
#' title: "demo_qswatplus.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_qswatplus.R**: demonstrate R-based model building with QSWAT+, simulation, and calibration
#' 
#' IN DEVELOPMENT
#' 
#' The (get_\*.R) URYW_data R scripts fetched the data required for SWAT+ modeling in the UYRW,
#' and make_subwatersheds.R partitioned the area of interest into subwatersheds, appropriately
#' sized for sequential model-building. This script demonstrates how to construct and fit the
#' SWAT+ model to one of these subwatersheds 
#' 
#' Input data files are clipped to the subwatershed extent and reformatted for compatibility with
#' QSWAT+, and a project directory is created to hold a copy of this input data, along with a JSON
#' describing the model configuration. Model construction is then completed in python, via a system
#' call to a launcher for a custom PyQGIS module, based on QSWAT+. This requires the user have the
#' OSGEO4W-LTR release of QGIS installed in the default location (for Windows 10 64bit), as well as
#' QSWAT+ and SWAT+Editor.
#' 
#' The python code and launcher in development [here](https://gitlab.com/rob-yerc/swat) 
#'
#' The following scripts should be run first to fetch and process data inputs:
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md)
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md)
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md)
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)

#'
#' ## libraries
#' For now we load the `rswat_*` functions (for SWAT+ I/O) by sourcing a helper function script. We plan
#' to eventually release this as an R package. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(here)
source(here('R/get_helperfun.R'))
source('R/swat/get_swathelperfun_develop.R')

#'
#' ## project data
#+ echo=FALSE

# metadata from previous R scripts in the workflow
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
subwatersheds.meta = my_metadata('get_subwatersheds')
streamgages.meta = my_metadata('get_streamgages')
meteo.meta = my_metadata('get_meteo')

# load the USGS gage data and subwatersheds identified in make_subwatersheds.R
usgs = readRDS(here(streamgages.meta['USGS_data', 'file']))
usgs.catchments = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))

# other useful geometries
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))

#' ## test 'run_qwswatplus.py' module on an example subwatershed
#' 
#' To keep it simple, we start by looking for a subwatershed with no inlets (ie the full catchment
#' upstream of a gage) and a long streamflow record that overlaps with our weather input data
#' (PNWNAMet, 1945-2012). We also avoid areas with large lakes (eg. Yellowstone Lake) as we don't
#' have support yet for reservoirs in the QSWAT+ workflow.

# identify subwatersheds with no inlets
id.leaf = usgs.catchments$boundary %>% filter(n_inlet == 0) %>% pull(catchment_id)
n.leaf = length(id.leaf)
n.nonleaf = nrow(usgs.catchments$boundary) - n.leaf

# define some aesthetics for plotting
uyrw.title = paste(n.leaf + n.nonleaf, 'subwatersheds with 3+ year records (blues = no inlets)')
pal.nonleaf = rainbow(n.nonleaf, s=0.4, v=0.8, start=0.1, end=0.15)
pal.leaf = rainbow(n.leaf, s=0.4, v=0.8, start=0.5, end=0.6)

# plot full watershed and its partitioning into subwatersheds, highlighting catchments of interest
plot(st_geometry(usgs.catchments$boundary['catchment_name']), lwd=2, col=pal.nonleaf, border='white', main=uyrw.title)
plot(st_geometry(usgs.catchments$boundary)[id.leaf], col=pal.leaf, border='white', add=TRUE)
plot(st_geometry(usgs.catchments$demnet)[usgs.catchments$demnet$Order > 2], col='blue', add=TRUE)
plot(st_geometry(uyrw.mainstem), col='blue', lwd=3, add=TRUE)
plot(st_geometry(uyrw.waterbody), col='lightblue', add=TRUE)
plot(st_geometry(usgs.catchments$pts), pch=16, col='red', add=TRUE)
plot(st_geometry(usgs.catchments$pts), col='orange', add=TRUE)

# pick one (id #11) near Corwin Springs with a nice long record, extract its data
id.eg = 11
pts.eg = usgs.catchments$pts[usgs.catchments$pts$catchment_id==id,]
boundary.eg = usgs.catchments$boundary[usgs.catchments$boundary$catchment_id==id,]
demnet.eg = usgs.catchments$demnet[usgs.catchments$demnet$catchment_id==id,]
usgs.eg = usgs$dat[[pts.eg$site_no]]

# highlight it on the plot
plot(st_geometry(boundary.eg), add=TRUE, col='black')
plot(st_geometry(boundary.eg), add=TRUE, border='white')
plot(st_geometry(demnet.eg), col='grey40', add=TRUE, lwd=0.5)
plot(st_geometry(pts.eg), pch=16, col='yellow', add=TRUE)
plot(st_geometry(pts.eg), col='red', add=TRUE)

#' helper functions compile all the necessary data to run QSWAT+

# write QSWAT+ input files then set up the SWAT+ model via PyQGIS
qswat_meta = my_setup_qswatplus(id.eg, usgs.catchments)
my_run_qswatplus(qswat_meta)

# for now we use the PNWNAmet analysis for weather - load this dataset
meteo.eg = readRDS(here(meteo.meta[qswat_meta$file[qswat_meta$name=='wname'], 'file']))

# open the project files with rswat
textio = file.path(qswat_meta$file[qswat_meta$name=='proj'], 'Scenarios/Default/TxtInOut')
ciopath = file.path(textio, 'file.cio')
cio = rswat_cio(ciopath, ignore='decision_table', reload=TRUE)

# change the simulation time to match overlap of weather and gage time series
usgs.test = usgs$dat[[which.max(usgs$sf$count_nu)]]
range(usgs.test$date)
range(meteo$dates)
date.start = as.Date('1980-01-01')
date.end = as.Date('1990-01-01')

# write the changes to disk
time.sim = rswat_open('time.sim')
time.sim$day_start = as.integer(format(date.start, '%d'))
time.sim$yrc_start = as.integer(format(date.start, '%Y'))
time.sim$day_end = as.integer(format(date.end, '%d'))
time.sim$yrc_end = as.integer(format(date.end, '%Y'))
rswat_write(time.sim, preview=F)

# define the outputs to write (ie print) in the text files
print.prt = rswat_open('print.prt')[[1]]
print.prt$day_start = as.integer(format(date.start, '%d'))
print.prt$yrc_start = as.integer(format(date.start, '%Y'))
print.prt$day_end = as.integer(format(date.end, '%d'))
print.prt$yrc_end = as.integer(format(date.end, '%Y'))
rswat_write(print.prt, preview=F)

# define the files to write
ofiles = c('basin_cha', 'basin_sd_cha', 'channel', 'channel_sd')
print.prt = rswat_open('print.prt')[[5]]
print.prt$yearly = 'n'
print.prt$daily[print.prt$objects %in% ofiles] = 'y'
rswat_write(print.prt, preview=F)

# run a simulation
my_run_simulation(textio)
#rswat_open('')


#+ eval=FALSE
#my_markdown('demo_qswatplus.R')
