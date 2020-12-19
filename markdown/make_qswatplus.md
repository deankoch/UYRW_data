make\_qswatplus.R
================
Dean Koch
2020-12-18

**Mitacs UYRW project**

**make\_swatplus.R**: copies data on Mill Creek watershed for input to
QSWAT+ and/or SWAT+AW

The (get\_\*.R) URYW\_data R scripts save our watershed data in a format
convenient for analysis in R. This script takes a subset of these data
(corresponding to a sub-watershed), converts to the appropriate file
structure, and saves a copy to a project folder, where it can be input
to QSWAT+ to construct a SWAT+ model for that sub-watershed.

This model construction is done in QSWAT+ (a PyQGIS module), not R, and
I have no plans to translate its code to R. However it should be
possible to reproduce all of those (GUI based) steps via system calls to
the [SWAT+ Automatic Workflow
(AW)](https://celray.github.io/docs/swatplus_aw/introduction.html)
executable, which means the entire SWAT+ model construction workflow
could be wrapped into a single R function call. This is something we’re
working on right now.

The following scripts should be run first to fetch and process data
inputs:
[get\_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
[get\_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md)
[get\_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
[get\_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md)
[get\_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
[get\_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md)

## libraries

See the [get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
for other required libraries

``` r
library(smoothr)
library(here)
source(here('R/get_helperfun.R'))
```

## project data

    ## [1] "writing to data/prepared/qswatplus/millcreek_metadata.csv"

    ##                                                                     file              type
    ## swat_source                                      data/prepared/qswatplus         directory
    ## swat_proj                              data/prepared/qswatplus/millcreek         directory
    ## swat_boundary             data/prepared/qswatplus/millcreek_boundary.rds sf polygon object
    ## swat_dem_tif                   data/prepared/qswatplus/millcreek_dem.tif           GeoTIFF
    ## swat_landuse_tif           data/prepared/qswatplus/millcreek_landuse.tif           GeoTIFF
    ## swat_soils_tif                data/prepared/qswatplus/millcreek_soil.tif           GeoTIFF
    ## swat_landuse_lookup data/prepared/qswatplus/millcreek_landuse_lookup.csv               CSV
    ## swat_outlets               data/prepared/qswatplus/millcreek_outlets.shp    ESRI Shapefile
    ## swat_streams               data/prepared/qswatplus/millcreek_streams.shp    ESRI Shapefile
    ## swat_lakes               data/prepared/qswatplus/millcreekswat_lakes.shp    ESRI Shapefile
    ## swat_weatherstn                         data/prepared/qswatplus/PNWNAmet         directory
    ## metadata                  data/prepared/qswatplus/millcreek_metadata.csv               CSV

    ## [1] "writing to data/make_qswatplus_metadata.csv"

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/make_qswatplus_metadata.csv)
in the `/data` directory.

First we load the datasets prepared so far:

``` r
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
```

## define a subbasin of the UYRW to set up

``` r
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
```

## copy DEM, soils, and land use rasters

The DEM created by `get_dem.R` should be cropped and masked to replace
out-of-watershed areas with NAs before loading it into the QSWAT+
workflow. This chunk writes the masked data for the DEM, as well as the
soils and land use rasters, setting the custom NA flag.

``` r
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
```

## write shapefiles for stream reaches

QGIS can’t read the (.rds) files that we are using to store geometries,
so these must be rewritten as ESRI shapefiles (.shp).

Some data prep will be required for QSWAT+ to delineate channels
correctly around lakes, possibly using the
[SWAT2Lake](https://projects.au.dk/wet/software/#c55181) QGIS plugin
described
[here](https://www.sciencedirect.com/science/article/abs/pii/S1364815218302500).
This remains a work in progress.

``` r
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
```

    ## Reading layer `millcreek_streams' from data source `H:\UYRW_data\data\prepared\qswatplus\millcreek_streams.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 144 features and 1 field
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 522547.1 ymin: 4999492 xmax: 550023.4 ymax: 5028649
    ## projected CRS:  WGS 84 / UTM zone 12N

QSWAT+ will “burn” these stream reach line geometries into the DEM
raster prior to running the TauDEM workflow. This means that the
elevation at each pixel overlapping with a stream reach defined here is
(temporarily) lowered by a fixed amount, so that TauDEM is forced to
detect a stream reach along that path.

## define outlet points and write as shapefile

QSWAT+ build the watershed model by delineating channels and subbasins,
which are in part based on the locations of outlets of interest. For
example, to connect SWAT+ to discharge data, we need the model to
predict flow at those points where the time series measurements were
taken. Outlets should therefore be defined at every stream gage
location, and anywhere else that we intend to produce forecasts.

``` r
# set the threshold to use for snapping points to watercourses
outlet.snapval = units::set_units(10, 'meters')
```

For now include only USGS gages

``` r
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
```

copy the land use lookup table

``` r
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
```

Note that since we are using the stock SSURGO/STATSGO mukeys, there
should be no need to import a custom usersoil database

``` r
# For now we use Ben Livneh's climatic reconstruction 
#wdat.in = readRDS(here(meteo.meta['livneh_uyrw', 'file']))
```

``` r
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
```

``` r
# development code

if(0)
{
  # AFTER RUNNING QSWAT+, SWAT+EDITOR, SWAT EXECUTABLE
  
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
```

``` r
#my_markdown('make_qswatplus')
```
