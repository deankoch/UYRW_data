#' ---
#' title: "make_taudem.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **make_taudem.R**: copies data on Mill Creek watershed for input to QSWAT3 (WORK IN PROGRESS)
#' 
#' This code runs the TauDEM (v5) workflow to generate a set of input files for SWAT2012 that can
#' be selected in QSWAT3 from the 'use existing watershed' panel.
#' 
#' Much of the code is adapted from the example script provided by the TauDEM developers at Utah State
#' University, [linked here](https://hydrology.usu.edu/taudem/taudem5/TauDEMRScript.txt). 
#'
#' 
#' The following scripts should be run first to fetch and process data inputs:
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md),
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md), and
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md).
#' 

#'
#' ## libraries
#'  See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(here)
source(here('R/get_helperfun.R'))
library(gdalUtilities)

#'
#' ## project data
#' 

# path to TauDEM binaries [available here](http://hydrology.usu.edu/taudem/taudem5.0/downloads.html)
td.dir = 'C:/SWAT/SWATEditor/TauDEM5Bin'

# load DEM, stream network
qswat.meta = my_metadata('make_qswat')
dem.tif = raster(qswat.meta['swat_dem_tif', 'file'])
streams.sf = read_sf(qswat.meta['swat_streams', 'file'])

# ********************
# set up a directory for temporary files
td.tempdir = file.path(here(qswat.meta['swat_proj', 'file']), 'taudem_temp')
my_dir(td.tempdir)


# set threshold for stream delineation (used as initial value if flag below set to TRUE)
thresh.stream = 300

# flag to use stream drop analysis to determine an optimal delineation threshold
thresh.optimize = TRUE



#'
#' ## drainage reinforcement (AKA stream network "burn-in") 
#' 

# set the burn-in depth (in metres)
burn.depth = units::set_units(10, m)

# ********************
streams.tif.path = file.path(td.tempdir, 'streams_toburn.tif')

# convert streams to `Spatial`, add unit dummy field, rasterize
streams.sp = as(streams.sf, 'Spatial')
streams.sp$dummy = 1
gRasterize(streams.sp, dem.tif, field='dummy', filename=streams.tif.path)

# find cell numbers of non-NA cells in the streams raster 
streams.idx = Which(!is.na(raster(streams.tif.path)), cells=TRUE) 
  
# extract elevations at all cells intersecting with a stream
streams.elev = units::set_units(extract(dem.tif, streams.idx), m)

# decrement these elevations and write burned DEM file as uncompressed GeoTIFF
dem.burned.tif = dem.tif
dem.burned.tif[streams.idx] = as.vector(streams.elev - burn.depth)
# ********************
dem.burned.path = file.path(td.tempdir, 'dem_burned.tif')
writeRaster(dem.burned.tif, dem.burned.path, 
            options=c('COMPRESS=NONE, TFW=YES'),
            format='GTiff', 
            overwrite=TRUE)

#'
#' ## TauDEM workflow
#' 

# we being all shell commands by changing to the TauDEM directory (avoids changing R's WD)
shell.prefix = paste0('pushd ', normalizePath(td.dir), ' &&')

# Remove pits
# ********************
dem.fel.path = file.path(td.tempdir, 'dem_fel.tif')
syscall.string = paste(shell.prefix, 'PitRemove', 
                       '-z', normalizePath(dem.burned.path), 
                       '-fel', normalizePath(dem.fel.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.fel.path))



# compute D8 flow directions and slopes
# ********************
dem.p.path = file.path(td.tempdir, 'dem_p.tif')
dem.sd8.path = file.path(td.tempdir, 'dem_sd8.tif')
syscall.string = paste(shell.prefix, 'D8Flowdir', 
                       '-fel', normalizePath(dem.fel.path),
                       '-sd8', normalizePath(dem.sd8.path, mustWork=FALSE),
                       '-p', normalizePath(dem.p.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.p.path))
#plot(raster(dem.sd8.path))



# compute D-infinity flow directions and slopes
# ********************
dem.ang.path = file.path(td.tempdir, 'dem_ang.tif')
dem.slp.path = file.path(td.tempdir, 'dem_slp.tif')
syscall.string = paste(shell.prefix, 'DinfFlowdir', 
                       '-fel', normalizePath(dem.fel.path),
                       '-ang', normalizePath(dem.ang.path, mustWork=FALSE), 
                       '-slp', normalizePath(dem.slp.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.ang.path))
#plot(raster(dem.slp.path))



# compute D8 contributing area
# ********************
dem.ad8.path = file.path(td.tempdir, 'dem_ad8.tif')
syscall.string = paste(shell.prefix, 'AreaD8', 
                       '-p', normalizePath(dem.p.path),
                       '-ad8', normalizePath(dem.ad8.path, mustWork=FALSE),
                       '-nc')
shell(syscall.string)
#zoom(raster(dem.ad8.path))



# compute D-infinity contributing area
# ********************
dem.sca.path = file.path(td.tempdir, 'dem_sca.tif')
syscall.string = paste(shell.prefix, 'AreaDinf', 
                       '-ang', normalizePath(dem.ang.path),
                       '-sca', normalizePath(dem.sca.path, mustWork=FALSE),
                       '-nc')
shell(syscall.string)
#plot(raster(dem.sca.path))



# Initial stream delineation with arbitrary threshold (this value set above)
# ********************
dem.src.path = file.path(td.tempdir, 'dem_src.tif')
syscall.string = paste(shell.prefix, 'Threshold', 
                       '-ssa', normalizePath(dem.ad8.path),
                       '-src', normalizePath(dem.src.path, mustWork=FALSE),
                       '-thresh', thresh.stream)
shell(syscall.string)
#plot(raster(dem.src.path))



# Snap outlets to stream reaches (takes input shapefile for approx outlet locations)
# ********************
dem.outlets.path = file.path(td.tempdir, 'outlets_snapped.shp')
syscall.string = paste(shell.prefix, 'MoveOutletsToStreams', 
                       '-p', normalizePath(dem.p.path),
                       '-src', normalizePath(dem.src.path),
                       '-o', normalizePath(here(qswat.meta['swat_outlets', 'file'])),
                       '-om', normalizePath(dem.outlets.path, mustWork=FALSE))
shell(syscall.string)



# compute D8 contributing area upstream of outlets
# ********************
dem.ad8o.path = file.path(td.tempdir, 'dem_ad8o.tif')
syscall.string = paste(shell.prefix, 'AreaD8',
                       '-p', normalizePath(dem.p.path),
                       '-o', normalizePath(dem.outlets.path),
                       '-ad8', normalizePath(dem.ad8o.path, mustWork=FALSE),
                       '-nc')
shell(syscall.string)
#plot(raster(dem.ad8o.path))



# Grid Network: compute (Strahler) order and lengths of flow paths
# ********************
dem.plen.path = file.path(td.tempdir, 'dem_plen.tif')
dem.tlen.path = file.path(td.tempdir, 'dem_tlen.tif')
dem.gord.path = file.path(td.tempdir, 'dem_gord.tif')
syscall.string = paste(shell.prefix, 'GridNet', 
                       '-p', normalizePath(dem.p.path),
                       '-plen', normalizePath(dem.plen.path, mustWork=FALSE),
                       '-tlen', normalizePath(dem.tlen.path, mustWork=FALSE),
                       '-gord', normalizePath(dem.gord.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.plen.path))
#plot(raster(dem.tlen.path))
#plot(raster(dem.gord.path))



# Use local topography to generate Peuker-Douglas stream skeleton  
# ********************
dem.ss.path = file.path(td.tempdir, 'dem_ss.tif')
syscall.string = paste(shell.prefix, 'PeukerDouglas', 
                       '-fel', normalizePath(dem.fel.path),
                       '-ss', normalizePath(dem.ss.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.ss.path))



# compute D8 contributing area upstream of outlets for Peuker-Douglas stream skeleton
# ********************
dem.ssa.path = file.path(td.tempdir, 'dem_ssa.tif')
syscall.string = paste(shell.prefix, 'AreaD8', 
                       '-p', normalizePath(dem.p.path),
                       '-o', normalizePath(dem.outlets.path),
                       '-ad8', normalizePath(dem.ssa.path, mustWork=FALSE),
                       '-wg', normalizePath(dem.ss.path),
                       '-nc')
shell(syscall.string)
#plot(raster(dem.ssa.path))



# find an optimal threshold (in the sense of highest detail level)
if(thresh.optimize)
{
  # perform stream drop analysis upstream of outlets -> lower bound for delineation thresholds
  # ********************
  dem.drp.path = file.path(td.tempdir, 'dem_drp.txt')
  syscall.string = paste(shell.prefix, 'DropAnalysis', 
                         '-p', normalizePath(dem.p.path),
                         '-fel', normalizePath(dem.fel.path),
                         '-ad8', normalizePath(dem.ad8.path),
                         '-ssa', normalizePath(dem.ssa.path),
                         '-drp', normalizePath(dem.drp.path, mustWork=FALSE),
                         '-o', normalizePath(dem.outlets.path),
                         '-par 5 500 25 0')
  shell(syscall.string)
  dem.drp.txt = readLines(dem.drp.path)
  dem.drp.string = dem.drp.txt[length(dem.drp.txt)]
  thresh.stream.optimum = as.numeric(strsplit(dem.drp.string, 'Optimum Threshold Value: ')[[1]][2])
  print(dem.drp.string)
  
} else {
  
  # if we aren't optimizing, use the original threshold value
  thresh.stream.optimum = thresh.stream
  
}


# Stream delineation upstream of outlets using a threshold 
# ********************
dem.src1.path = file.path(td.tempdir, 'dem_src1.tif')
syscall.string = paste(shell.prefix, 'Threshold', 
                       '-ssa', normalizePath(dem.ssa.path),
                       '-src', normalizePath(dem.src1.path, mustWork=FALSE),
                       '-thresh', thresh.stream.optimum)
shell(syscall.string)
#plot(raster(dem.src1.path))


# Construct stream network shapefile and watershed grid
# ********************
dem.ord.path = file.path(td.tempdir, 'dem_ord.tif')
dem.tree.path = file.path(td.tempdir, 'dem_tree.dat')
dem.coord.path = file.path(td.tempdir, 'dem_coord.dat')
dem.net.path = file.path(td.tempdir, 'dem_net.shp')
dem.w.path = file.path(td.tempdir, 'dem_w.tif')
syscall.string = paste(shell.prefix, 'StreamNet', 
                       '-fel', normalizePath(dem.fel.path),
                       '-p', normalizePath(dem.p.path),
                       '-ad8', normalizePath(dem.ad8.path),
                       '-src', normalizePath(dem.src1.path),
                       '-o', normalizePath(dem.outlets.path),
                       '-ord', normalizePath(dem.ord.path, mustWork=FALSE),
                       '-tree', normalizePath(dem.tree.path, mustWork=FALSE),
                       '-coord', normalizePath(dem.coord.path, mustWork=FALSE),
                       '-net', normalizePath(dem.net.path, mustWork=FALSE),
                       '-w', normalizePath(dem.w.path, mustWork=FALSE))
shell(syscall.string)
#plot(raster(dem.ord.path))
#plot(raster(dem.w.path))
#streamnet = st_read(dem.net.path)
#plot(st_geometry(streamnet), add=TRUE, lwd=streamnet$Order)



# compute distance to streams
dem.dd8.path = file.path(td.tempdir, 'dem_dd8.tif')
syscall.string = paste(shell.prefix, 'D8HDistToStrm', 
                       '-p', normalizePath(dem.p.path),
                       '-src', normalizePath(dem.src1.path),
                       '-dist', normalizePath(dem.dd8.path, mustWork=FALSE))
shell(syscall.string)                  
#plot(raster(dem.dd8.path))



# create watershed shapefile from raster with fast polygonize function in stars package
# (adapted from createWatershedShapefile function in delineation.py module from QSWAT)
library(stars)
wshed.sf = st_make_valid(st_as_sf(st_as_stars(raster(dem.w.path)), as_points=FALSE, merge=TRUE))
wshed.out.sf = wshed.sf %>% 
  rename(Subbasin = 'dem_w.tif') %>% 
  mutate(PolygonID = 1:nrow(wshed.sf)) %>%
  mutate(Area = st_area(.)) %>%
  arrange(PolygonID, Area, Subbasin)

# for reference, the shapefile from a QSWAT run:
zz = st_read('H:/UYRW_data/data/prepared/qswat/millcreek/Source/swat_demwshed.shp')

# write to disk
# ********************
dem.wshed.path = file.path(td.tempdir, 'dem_wshed.shp')
st_write(wshed.out.sf, dem.wshed.path)




#my_markdown('make_taudem')