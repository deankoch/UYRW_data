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

#' ## libraries
#' Start by sourcing two helper scripts
#' ([helper_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md) and
#' [helper_analysis.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md))
#' which set up required libraries and directories and define some utility functions.
library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))

#' load the rswat package
source(here('R/rswat.R'))

# numeric optimization
library(dfoptim)


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

# pick one near Corwin Springs with a recent record
id.eg = usgs.catchments$pts %>% 
  filter(catchment_id %in% id.leaf) %>%
  filter(end_date > as.Date('1950-01-01')) %>%
  filter(!grepl('Yellowstone Lk', station_nm)) %>%
  filter(count_nu == max(count_nu)) %>%
  pull(catchment_id)

# extract its data
pts.eg = usgs.catchments$pts[usgs.catchments$pts$catchment_id==id.eg,]
boundary.eg = usgs.catchments$boundary[usgs.catchments$boundary$catchment_id==id.eg,]
demnet.eg = usgs.catchments$demnet[usgs.catchments$demnet$catchment_id==id.eg,]
usgs.eg = usgs$dat[[pts.eg$site_no]]

# highlight it on the plot
plot(st_geometry(boundary.eg), add=TRUE, col='black')
plot(st_geometry(boundary.eg), add=TRUE, border='white')
plot(st_geometry(demnet.eg), col='grey40', add=TRUE, lwd=0.5)
plot(st_geometry(pts.eg), pch=16, col='yellow', add=TRUE)
plot(st_geometry(pts.eg), col='red', add=TRUE)

#' helper functions compile all the necessary files to run QSWAT+ and build the model

#######
#######
##### testing: different catchments
id.egs = usgs.catchments$pts %>% 
  filter(catchment_id %in% id.leaf) %>%
  filter(end_date > as.Date('1950-01-01'))
id.eg = id.egs$catchment_id[5]
#######
#######
#######

# write QSWAT+ input files then set up the SWAT+ model via PyQGIS
qswat.meta = qswat_setup(id.eg, usgs.catchments, wipe=F)
qswat_run(qswat.meta)


#' ## load the SWAT+ project files
#' 
#+ eval=FALSE

# open the project text files with `rswat`
textio = file.path(qswat.meta$file[qswat.meta$name=='proj'], 'Scenarios/Default/TxtInOut')
cio = rswat_cio(file.path(textio, 'file.cio'), ignore='decision_table', reload=TRUE)

# plot the watershed shapefiles
wsh = qswat_read(qswat.meta)
qswat_read(qswat.meta, draw=TRUE)


#######
#######
##### testing:

# yy = st_read('H:/UYRW_data/data/prepared/swat_mill_c_nr_pray/test/Watershed/Shapes/lsus2.shp')
# xx = st_read('H:/UYRW_data/data/prepared/swat_mill_c_nr_pray/test/Watershed/Shapes/reservoirs0.shp')
# 
# plot(st_geometry(yy))
# plot(st_geometry(xx)[xx$RES == 1], add=T, pch=16, col='blue')
# plot(st_geometry(xx)[xx$RES == 1], add=T, pch=2, cex=3, col='red')



# we need some way of identifying mountainous channel basins and flagging them
read.csv(qswat.meta$file[qswat.meta$name=='landuse_lookup'])
plot(raster(qswat.meta$file[qswat.meta$name=='landuse'])==15)


# try computing some elevation stats across the floodplain LSUs
idx.floodplain = wsh$hru$Landscape == 'Floodplain'
lsu.all = st_geometry( wsh$hru[idx.floodplain, ] )


x = lapply(wsh$hru %>% group_split(Channel), function(x) st_union(st_geometry(x)))
str(x)
lsu.all

# make a dataframe to store info on all hru geometries




for(idx in 1:5)
{
 lsu = lsu.all[idx]
 #plot(lsu)

  lsu.sp = as(lsu, 'Spatial')
  
  dem.lsu = mask(crop(wsh$dem, lsu.sp), lsu.sp)
  lsu.min = cellStats(dem.lsu, min)
  lsu.med = cellStats(dem.lsu, median)
  lsu.mea = cellStats(dem.lsu, mean)
  lsu.max = cellStats(dem.lsu, max)
  lsu.ncell = ncell(dem.lsu)

  print(c(lmin = lsu.min, 
          median = lsu.med, 
          mean = lsu.mea, 
          max = lsu.max, 
          nc = lsu.ncell))
}



#######
#######
#######



#######
#######
# after much searching, I don't there is a mapping from HRU to aquifer in the SWAT+ tables
# rswat_find('aqu', intext=TRUE)%>% pull(file) %>% unique
# rswat_find('aqu011', intext=TRUE, fuzzy=2) %>% pull(file) %>% unique
# however their metadata seems to be unique enough to match them up
aquifer.con = rswat_open('aquifer.con')
hru.con = rswat_open('hru.con')
aquifer.con$lat %in% hru.con$lat
aquifer.con$elev %in% wsh$sub$Elev 

aquifer.con$elev %in% wsh$sub$Elev 

hru1 = read_sf(file.path(shpdir, 'hrus1.shp'))

# coordinates of aquifers appear to match with subbasin centroids 
aqu.centroid = st_transform(st_sfc(st_multipoint(as.matrix(aquifer.con[-39,c('lon', 'lat')])), crs=4326), st_crs(wsh$hru))
plot(st_geometry(wsh$sub))
plot(aqu.centroid, add=TRUE)
sort(aquifer.con$lat)
# it seems there are two aquifers per subbasin
#######
#######

#' ## run a simulation

# for now we use the PNWNAmet analysis for weather - load this dataset
meteo.eg = readRDS(here(meteo.meta[qswat_meta$file[qswat_meta$name=='wname'], 'file']))



# change the simulation time to match overlap of weather and gage time series
#usgs.test = usgs$dat[[which.max(usgs$sf$count_nu)]]
range(usgs.eg$dat[[1]]$date)
plot(flow~date, data=usgs.eg$dat[[1]])
range(meteo.eg$dates)

# this gage has two observation periods, separated by decades. There's a helper function for this
dates.all = my_split_series(usgs.eg$dat[[1]]$date, meteo.eg$dates)
print(sapply(dates.all, length))
idx.dates = which.max( sapply(dates.all, length) )
dates = dates.all[[idx.dates]]
range(dates)

# pull a copy of a subset of gage data to focus on
gage = usgs.eg$dat[[1]] %>% filter(date %in% dates)

# run a simulation with default settings fresh out of SWAT+ Editor
dev.off()
my_gage_objective(gage, textio, quiet=F, draw=1)

##
#' #' look for snow bands
#' cio
#' rswat_open('pcp.cli')
#' rswat_find('pcp_grid136.pcp', intext=TRUE)
#' rswat_open('topography.hyd')
#' rswat_open('hydrology.hyd')
#' rswat_open('weather-sta.cli')
#' 
#' This functionality appears to be missing from SWAT+

# check otu 


#' ## run a calibration (many simulations)

# define the parameters to calibrate in a dataframe (based on Grusson et al 2015)
cal.snow = rswat_find(include='snow.sno')
cal.misc = rswat_find('esco|can_max|surq_lag')
cal.gw = rswat_find('gw|revap|alpha_bf|rchg_dp')

# we will calibrate the deep aquifer separately 
idx.deep = grepl('deep', rswat_open('aquifer.aqu')$name)
cal.gw.deep = cal.gw %>% mutate( i = which(idx.deep) )
cal.gw.shal = cal.gw %>% mutate( i = -which(idx.deep) )

# rbind everything and send to objective function maker
cal = rbind(cal.snow, cal.misc, cal.gw.deep, cal.gw.shal)
obj = my_objective(cal, gage, textio)

# initialize lower/upper bounds, fix the mismatches
obj.bds = sapply(obj()$name, my_bounds)
obj.bds[,'cov50'] = c(0,1)

# starting values, and some control options
par.init = colSums(obj.bds)/2
control = list(maxfeval=1000, maximize=TRUE)

# close any opened graphics device
dev.off()

# we are ready to use the optimizers in `dfoptim` - try hooke-jeeves
hjkb.res = hjkb(par.init, obj, obj.bds[1,], obj.bds[2,], control, draw=TRUE)

# the current parameter set for the model will depend on the contents of `x in
# the last call to `obj(x)`. Since the optimizer (hjkb) handled these calls, we
# can't be sure of what that was, (ie what's currently written to the IO files).
# To be safe we overwrite everything with the optima returned in `hjkb.res$par`
# TODO: eliminate this step
# run the simulation once more with the optimized parameters
obj(hjkb.res$par)

# plot the results 
dev.off()
my_gage_objective(gage, textio, quiet=FALSE, draw=1, exec=F)

# check out sensitivity of the fall_tmp variable
n.test = 10
cal.test = cal[1,]
obj = my_objective(cal.test, gage, textio)
seq.test = seq(-10, 10, length=n.test)
color = rainbow(n.test)
dev.off()
for(idx in 1:n.test) print( obj(seq.test[idx], draw=color[idx]) )
# TODO: make this a function, where it returns the test values and results
# in a dataframe and returns the parameter to its original value afterwards

# reset fall_tmp to optimum found earlier
obj(2)

#' A more complicated example: NRCS curve numbers (cn_*)

# find the group classifications for our soils and associated land use
rswat_find('cn')
cn.grp = rswat_open('soils.sol') %>% filter(!is.na(hyd_grp)) %>% pull(hyd_grp)
cn.nm = rswat_open('landuse.lum')$cn2

# 'cntable.lum' has the full list of CN values; only a few are relevant 
i.lum = rswat_open('cntable.lum')$name %in% cn.nm
cal.entry = rswat_find(paste0('cn_', unique(tolower(cn.grp))), include='cntable.lum')
cal.test = do.call(rbind, lapply(which(i.lum), function(x) cal.entry %>% mutate(i=x)))

#' Another complicated example: soils.sol

# this is a ragged table so we need to filter the NA rows
cal.entry = rswat_find('awc', include='soils.sol')
i.sol = !is.na(rswat_open('soils.sol')$awc)
cal.test = do.call(rbind, lapply(which(i.sol), function(x) cal.entry %>% mutate(i=x)))

# build the objective function and copy current parameters
obj = my_objective(cal.test, gage, textio)
cn.old = obj()

# run the test
n.test = 10
color = rainbow(n.test)
seq.test = sapply(seq(0, 1, length=n.test), function(x)  rep(x, nrow(cn.old)))

# execute
dev.off()
for(idx in 1:n.test) print( obj(seq.test[,idx], draw=color[idx]) )

# reset parameters
obj(cn.old$value, refresh=T)

#' try getting baseflow to increase

# focus on warm-up period (to look for effects of initial values)
print.prt = rswat_open('print.prt')[[1]]
print.prt$nyskip = 0
rswat_write(print.prt, preview=F)

# restrict attention to two years
dates.warmup = rep(min(gage$date), 2) + c(0, 2*365)
dates.test = seq(dates.warmup[1], dates.warmup[2], by='day')
gage.test = gage[gage$date %in% dates.test,]
my_gage_objective(gage.test, textio, draw=1)

# initial changes to try to bump up sensitivity
aquifer.aqu = rswat_open('aquifer.aqu')
aquifer.aqu$rchg_dp = 1
aquifer.aqu$dep_wt = aquifer.aqu$dep_bot
rswat_write(aquifer.aqu, preview=F)

# select a variable to tweak
cal.test = rswat_find('alpha_bf')
obj = my_objective(cal.test, gage.test, textio)
obj(100, draw='blue')

# grid search
n.test = 100
for(idx in seq(0, 1, length=n.test)) obj(idx, draw=TRUE)


rswat_find('snow001', intext=TRUE)
rswat_open('snow.sno')


#+ eval=FALSE
#my_markdown('demo_qswatplus.R')
