#' ---
#' title: "make_qswatplus.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **make_swatplus.R**: copies data on Mill Creek watershed for input to QSWAT+ and/or SWAT+AW
#' 
#' The (get_\*.R) URYW_data R scripts save our watershed data in a format convenient for
#' analysis in R. This script takes a subset of these data (corresponding to a sub-watershed),
#' converts to the appropriate file structure, and saves a copy to a project folder, where it
#' can be input to QSWAT+ to construct a SWAT+ model for that sub-watershed.
#' 
#' This model construction is done in QSWAT+ (a PyQGIS module), not R, and I have no plans to
#' translate its code to R. However it should be possible to reproduce all of those (GUI based)
#' steps via system calls to the
#' [SWAT+ Automatic Workflow (AW)](https://celray.github.io/docs/swatplus_aw/introduction.html)
#' executable, which means the entire SWAT+ model construction workflow could be wrapped into a
#' single R function call. This is something we're working on right now.
#'
#' The following scripts should be run first to fetch and process data inputs:
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md)
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md)
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md)
#' 

#'
#' ## libraries
#' See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(smoothr)
library(here)
source(here('R/get_helperfun.R'))


#'
#' ## project data
#+ echo=FALSE

# assign a subdirectory of the UYRW project data folder to store QSWAT+ input files
swat.dir = file.path(data.dir, 'prepared/qswatplus')

# assign a project name. This defines the names of the QSWAT project file and directory
swat.name = 'millcreek'
swat.projdir = file.path(swat.dir, swat.name)

#+ echo=FALSE
# define the files to write
{
  files.towrite = list(
    
    # path to subdirectory with QSWAT inputs
    c(name='swat_source',
      file=swat.dir, 
      type='directory',
      description='QSWAT+ I/O directory'),
    
    # path to QSWAT project folder
    c(name='swat_proj',
      file=swat.projdir, 
      type='directory',
      description='QSWAT+ project directory'),
    
    # project subbasin boundary polygon (subset of 'boundary' from 'get_basins.R')
    c(name='swat_boundary',
      file=file.path(swat.dir, paste0(swat.name, '_boundary.rds')), 
      type='sf polygon object',
      description='polygon identifying subset of UYRW to set up QSWAT+'),
    
    # DEM raster ('swat_dem' from 'get_dem.R', cropped to AOI)  
    c(name='swat_dem_tif',
      file=file.path(swat.dir, paste0(swat.name, '_dem.tif')), 
      type='GeoTIFF',
      description='QSWAT+ DEM'),
    
    # land use raster for the UYRW ('swat_landuse_tif' from 'get_landuse.R', cropped to AOI)
    c(name='swat_landuse_tif',
      file=file.path(swat.dir, paste0(swat.name, '_landuse.tif')), 
      type='GeoTIFF',
      description='SWAT+ land use classification'),
    
    # soils raster for the UYRW ('swat_tif' from 'get_soils.R', cropped to AOI)
    c(name='swat_soils_tif',
      file=file.path(swat.dir, paste0(swat.name, '_soil.tif')), 
      type='GeoTIFF',
      description='SWAT soils classification, maps to soil table in SWAT+ database'),
    
    # lookup table for 'swat_landuse_tif' ('swat_landuse_lookup' from 'get_landuse.R')
    c(name='swat_landuse_lookup',
      file=file.path(swat.dir, paste0(swat.name, '_landuse_lookup.csv')), 
      type='CSV',
      description='integer code for swat_landuse_tif, maps to `plants_plt` table in SWAT+ database'), 
    
    # outlets shapefile (for now based on 'USGS_sites' from 'get_streamgages.R')
    c(name='swat_outlets',
      file=file.path(swat.dir, paste0(swat.name, '_outlets.shp')), 
      type='ESRI Shapefile',
      description='outlet point locations, used by QSWAT+ to delineate subbasins'),
    
    # streams shapefile (simplified 'flowlines' from 'get_basins.R')
    c(name='swat_streams',
      file=file.path(swat.dir, paste0(swat.name, '_streams.shp')), 
      type='ESRI Shapefile',
      description='stream geometries to "burn" into DEM prior to running TauDEM'),
    
    # lakes shapefile (simplified 'waterbody' from 'get_basins.R')
    c(name='swat_lakes',
      file=file.path(swat.dir, paste0(swat.name, 'swat_lakes.shp')), 
      type='ESRI Shapefile',
      description='polygons in the UYRW representing water bodies'),
    
    # directory to write SWAT wdat input text files
    c(name='swat_weatherstn',
      file=file.path(swat.dir, 'PNWNAmet'),
      type='directory',
      description='directory for writing SWAT weather input text files')

  )
}

# write metadata to csv (store in QSWAT+ input directory, creating it if needed)
makeqswat.meta = my_metadata(swat.name, files.towrite, overwrite=TRUE, data.dir=swat.dir)
print(makeqswat.meta[, c('file', 'type')])

# (temporary) copy the csv to /data so it gets picked up by github
makeqswat.meta = my_metadata('make_qswatplus', files.towrite, overwrite=TRUE)

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/make_qswatplus_metadata.csv)
#' in the `/data` directory.
#' 

#' First we load the datasets prepared so far:

# load project file metadata
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
landuse.meta = my_metadata('get_landuse')
soils.meta = my_metadata('get_soils')
streamgages.meta = my_metadata('get_streamgages')
weatherstations.meta = my_metadata('get_weatherstations')
meteo.meta = my_metadata('get_meteo')

# load some of the watershed data created by 'get_basins.R'
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
poi.list = readRDS(here(basins.meta['poi', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))

# load USGS stream gage station points and time series
usgs.dat = readRDS(here(streamgages.meta['USGS_data', 'file']))

#'
#' ## define a subbasin of the UYRW to set up
#+ warning=FALSE
subb.poly.path = here(makeqswat.meta['swat_boundary', 'file'])
if(!file.exists(subb.poly.path))
{
  
  # load the mill creek data for testing purposes
  millcreek.list = readRDS(here(basins.meta['millcreek', 'file']))
  
  # define its drainage boundary and save to disk
  subb.poly = millcreek.list$boundary
  saveRDS(subb.poly, subb.poly.path)
  
  
} else {
  
  # load boundary polygon from disk
  subb.poly = readRDS(subb.poly.path)
  
}

# coerce to `sp` object for compatibility with `raster`
subb.poly.sp = as(subb.poly, 'Spatial')

# crop flowlines, waterbodies, and outlet geometries to this subbasin 
subb.flowline = suppressWarnings(st_intersection(uyrw.flowline, subb.poly))
subb.waterbody = suppressWarnings(st_intersection(uyrw.waterbody, subb.poly))
subb.usgs.sf = suppressWarnings(st_intersection(usgs.dat$sf, subb.poly))


#'
#' ## copy DEM, soils, and land use rasters
#' 
#' The DEM created by `get_dem.R` should be cropped and masked to replace out-of-watershed areas with NAs
#' before loading it into the QSWAT+ workflow. This chunk writes the masked data for the DEM, as well as
#' the soils and land use rasters, setting the custom NA flag.
#'

# set the file paths to write
dem.path = here(makeqswat.meta['swat_dem_tif', 'file'])
landuse.path = here(makeqswat.meta['swat_landuse_tif', 'file'])
soils.path = here(makeqswat.meta['swat_soils_tif', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(dem.path, landuse.path, soils.path))))
{
  # crop/mask the rasters and write to new location
  dem.tif = raster(here(dem.meta['swat_dem', 'file']))
  landuse.tif = raster(here(landuse.meta['swat_landuse_tif', 'file']))
  soils.tif = raster(here(soils.meta['swat_soils_tif', 'file']))
  swat.dem.tif = mask(crop(dem.tif , subb.poly.sp), subb.poly.sp)
  swat.landuse.tif = mask(crop(landuse.tif , subb.poly.sp), subb.poly.sp)
  swat.soils.tif = mask(crop(soils.tif , subb.poly.sp), subb.poly.sp)
  writeRaster(swat.dem.tif, dem.path, NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(swat.landuse.tif, landuse.path, NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(swat.soils.tif, soils.path, NAflag=tif.na.val, overwrite=TRUE)
}

#' 
#' ## write shapefiles for stream reaches               
#' QGIS can't read the (.rds) files that we are using to store geometries, so these must be 
#' rewritten as ESRI shapefiles (.shp).
#' 
#' Some data prep will be required for QSWAT+ to delineate channels correctly around lakes,
#' possibly using the [SWAT2Lake](https://projects.au.dk/wet/software/#c55181) QGIS plugin
#' described [here](https://www.sciencedirect.com/science/article/abs/pii/S1364815218302500).
#' This remains a work in progress.
#'

# set the file paths to write
streams.path = here(makeqswat.meta['swat_streams', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(streams.path))))
{
  # make sure the streams network contains only LINESTRING geometries, drop attributes
  streams.sf = st_cast(st_geometry(subb.flowline[!st_is(subb.flowline, 'POINT'),]), 'LINESTRING')
  st_write(streams.sf, streams.path, append=FALSE)
  
} else {
  
  streams.sf = st_read(streams.path)
  
}

#' QSWAT+ will "burn" these stream reach line geometries into the DEM raster prior to running the
#' TauDEM workflow. This means that the elevation at each pixel overlapping with a stream reach
#' defined here is (temporarily) lowered by a fixed amount, so that TauDEM is forced to detect a 
#' stream reach along that path.
#' 

#'
#' ## define outlet points and write as shapefile
#' QSWAT+ build the watershed model by delineating channels and subbasins, which are in part based on
#' the locations of outlets of interest. For example, to connect SWAT+ to discharge data, we need the
#' model to predict flow at those points where the time series measurements were taken. Outlets should
#' therefore be defined at every stream gage location, and anywhere else that we intend to produce
#' forecasts.
#' 
#'   
#' 
# set the threshold to use for snapping points to watercourses
outlet.snapval = units::set_units(10, 'meters')

#' For now include only USGS gages 
if(!file.exists(here(makeqswat.meta['swat_outlets', 'file'])))
{
  # pull the stream gage stations, snap to nearest flowline point
  outlets.swat = st_geometry(subb.usgs.sf)
  outlets.swat = st_snap(outlets.swat, subb.flowline, outlet.snapval)
  
  # (temporary) for debugging: omit all but main outlet 
  outlets.swat = outlets.swat[1,]
  
  # add required fields for QSWAT/QSWAT+
  n.pts = length(outlets.swat)
  outlets.df = data.frame(ID=as.integer(1:n.pts), INLET=as.integer(0), RES=as.integer(0), PTSOURCE=as.integer(0))
  outlets.swat = st_sf(outlets.df, geom=outlets.swat)
  
  # write to disk as ESRI shapefile
  st_write(outlets.swat, here(makeqswat.meta['swat_outlets', 'file']), append=FALSE)
  
}

#' copy the land use lookup table

# open the table and use cropped landuse raster to determine rows to drop
landuse.lookup = read.csv(here(landuse.meta['swat_landuse_lookup', 'file']))
landuse.lookup = landuse.lookup[landuse.lookup$Value %in% unique(raster(landuse.path)),]

# copy to QSWAT+ project folder
landuse.lookup.src = here(landuse.meta['swat_landuse_lookup', 'file'])
landuse.lookup.dest = here(makeqswat.meta['swat_landuse_lookup', 'file'])
if(!file.exists(landuse.lookup.dest))
{
  file.copy(from=landuse.lookup.src, to=landuse.lookup.dest, overwrite=T)
}

#' Note that since we are using the stock SSURGO/STATSGO mukeys, there should be no need to
#' import a custom usersoil database
#' 

# For now we use Ben Livneh's climatic reconstruction 
#wdat.in = readRDS(here(meteo.meta['livneh_uyrw', 'file']))
#+ eval=FALSE
wstn.dir = makeqswat.meta['swat_weatherstn', 'file']
if(!dir.exists(wstn.dir))
{
  # testing PNWNAMet data
  wdat.in = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
  
  # add in NA values (-99.0) for humidity (SWAT+ Editor bugfix)
  wdat.in$tables$hmd = wdat.in$tables$tmax
  wdat.in$tables$hmd[] = NA
  
  # create the weather station data directory
  my_dir(wstn.dir)
  
  # add 3km buffer to boundary for intersection with weather grid points
  subb.poly.buff = st_buffer(subb.poly, dist=units::set_units(3, km))
  
  # find index inside this boundary
  idx.include = as.vector(st_intersects(wdat.in$coords_sf, subb.poly.buff, sparse=FALSE))
  
  # load DEM and call the weather station data export function
  print('writing weather station data files...')
  my_swat_wmeteo(wdat.in, exdir=wstn.dir, form='qswat', include=idx.include)

}


#+ eval=FALSE
# development code

if(0)
{
  # TRY SETTING UP A SWAT+ AW config
  
  # scan all files in source directory
  src.fn = list.files(swat.dir)
  
  # make a new folder for this workflow
  wf.subdir = 'data'
  wf.dir = here(file.path(swat.dir, wf.subdir))
  my_dir(wf.dir)
  
  # make subdirectories
  wf.rasters.dir = file.path(wf.dir, 'rasters')
  wf.shape.dir = file.path(wf.dir, 'shapefiles')
  wf.tables.dir = file.path(wf.dir, 'tables')
  my_dir(wf.rasters.dir)
  my_dir(wf.shape.dir)
  my_dir(wf.tables.dir)
  
  # copy the rasters
  file.copy(dem.path, file.path(wf.rasters.dir, 'dem.tif'))
  file.copy(landuse.path, file.path(wf.rasters.dir, 'landuse.tif'))
  file.copy(soils.path, file.path(wf.rasters.dir, 'soils.tif'))

  # copy streams shapefiles
  streams.src = tools::file_path_sans_ext(basename(makeqswat.meta['swat_streams', 'file']))
  streams.fn = src.fn[grepl(streams.src, src.fn, fixed=TRUE)]
  streams.path = here(file.path(swat.dir, streams.fn))
  file.copy(streams.path, file.path(wf.shape.dir, streams.fn))
  
  # copy outlets shapefiles
  outlets.src = tools::file_path_sans_ext(basename(makeqswat.meta['swat_outlets', 'file']))
  outlets.fn = src.fn[grepl(outlets.src, src.fn, fixed=TRUE)]
  outlets.path = here(file.path(swat.dir, outlets.fn))
  file.copy(outlets.path, file.path(wf.shape.dir, outlets.fn))
  
  # copy landuse table 
  landuse.lookup = read.csv(here(landuse.meta['swat_landuse_lookup', 'file']))
  #landuse.lookup$Landuse = toupper(landuse.lookup$Landuse)
  colnames(landuse.lookup) = c('LANDUSE_ID', 'SWAT_CODE')
  landuse.lookup.path = file.path(wf.tables.dir, 'landuse_lookup.csv')
  write.csv(landuse.lookup, file=landuse.lookup.path, row.names=F)
  
  # copy usersoil table after pruning and copying MUID field to SNAM 
  usersoil.ref = read.csv(here(out.subdir, 'usersoil.csv'))
  usersoil.path = file.path(wf.tables.dir, 'usersoil.csv')
  usersoil.out = usersoil.ref[usersoil.ref$MUID %in% unique(raster(soils.path)),]
  usersoil.out$SNAM = usersoil.out$MUID
  write.csv(usersoil.out, file=usersoil.path, row.names=F)
  
  # workaround for QSWAT+ bug not prompting for csv file
  # soils.sql = dbConnect(RSQLite::SQLite(), here(swat.dir, 'test', 'test.sqlite'))
  # dbWriteTable(soils.sql, 'usersoil', usersoil.out, overwrite=TRUE)
  # dbDisconnect(soils.sql)
  
  test.sql = dbConnect(RSQLite::SQLite(), 'C:/SWAT/SWATPlus/Workflow/test/tester/tester.sqlite')
  dbListTables(test.sql)
  dbReadTable(test.sql, 'plants_plt')$name
  
  # make a soils lookup csv
  soilcsv.path = file.path(wf.tables.dir, 'soil_lookup.csv')
  write.csv(data.frame(SOIL_ID=usersoil.out$SNAM, SNAM=usersoil.out$SNAM), file=soilcsv.path, row.names=F)
  
}

if(0)
{
  # AFTER RUNNING QSWAT+, SWAT+EDITOR, SWAT SIMULATION EXECUTABLE
  
  # try opening the channel variables output file and printing the contents
  txtio.subdir = 'millcreek/Scenarios/Default/TxtInOut'
  textio.dir =  here(swat.dir, txtio.subdir)
  out.path = file.path(textio.dir, 'channel_sd_day.txt')
  my_read_output(out.path)

  # data retrieval with proper units appears to work with all other output files
  head(my_read_output(out.path=file.path(textio.dir, 'deposition_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'channel_sd_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'channel_sd_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'channel_sdmorph_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'channel_sdmorph_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'aquifer_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'aquifer_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_aqu_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_aqu_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_cha_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_cha_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_ls_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_ls_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_nb_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_nb_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_psc_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_psc_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_pw_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_pw_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_res_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_res_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_sd_cha_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_sd_cha_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_sd_chamorph_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_sd_chamorph_yr.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_wb_day.txt'), varname='all'))
  head(my_read_output(out.path=file.path(textio.dir, 'basin_wb_yr.txt'), varname='all'))
  

  xx = my_read_output(out.path, varname=c('precip', 'flo_in', 'flo_out'))
  yy = xx %>% filter(gis_id==2)
  
  plot(yy$date, yy$flo_out, pch='')
  lines(yy$date, yy$flo_out)
}
#

# # load the usersoil table
# usersoil.path = here(out.subdir, 'usersoil.csv')
# usersoil = read.csv(usersoil.path)
# 
# # load the the mukeys list for the study area
# mukeys = unique(raster(soils.path))
# 
# # index all the relevant mukeys, check that none are missing from usersoil table
# mukeys.all = unique(usersoil$MUID)
# if(!all(mukeys %in% mukeys.all)) {print('CRITICAL ERROR: raster mukey missing from usersoil')}
# idx.usersoil = usersoil$MUID %in% mukeys
# 
# # make a copy of the relevant rows of the usersoil table, copying MUID into SNAM
# swat.usersoil = usersoil %>% 
#   filter(idx.usersoil) %>%
#   select(-HYDGRP_ORIG, -Notes) %>%
#   mutate(SNAM = MUID)
# 
# # create the lookup table for the raster
# swat.soil.lookup = swat.usersoil %>%
#   mutate(SOIL_ID=MUID) %>%
#   select(SOIL_ID, SNAM)
# 
# # finally, write the lookup table 
# swat.usersoil.path = here(swat.dir, 'millcreek_soil.csv')
# write.csv(swat.soil.lookup, swat.usersoil.path, row.names=FALSE)

#+ eval=FALSE
#my_markdown('make_qswatplus')
