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
  soils.tif = raster(here(soils.meta['swat_tif', 'file']))
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
  outlets.swat = st_sf(data.frame(ID=1:n.pts, RES=0, INLET=0, PTSOURCE=0), geom=outlets.swat)
  
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

wstn.dir = makeqswat.meta['swat_weatherstn', 'file']
if(!dir.exists(wstn.dir))
{

  # testing PNWNAMet data
  wdat.in = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
  
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
#my_markdown('make_qswatplus')