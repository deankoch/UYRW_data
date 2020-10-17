make\_qswatplus.R
================
Dean Koch
2020-10-16

**Mitacs UYRW project**

**make\_swat.R**: copies data on Mill Creek watershed for input to
QSWAT+

The (get\_\*.R) URYW\_data R scripts have saved our watershed data in a
format convenient for analysis in R. Some changes are required to
certain files to make them readable in a QSWAT+ project, eg. R binaries
(.rds files) containing geometries must be simplified and exported to
ESRI shapefile, and rasters must be masked with a (non-default) NA flag.

This code prepares the required inputs to QSWAT+ for watershed
configuration. Users can then initialize a SWAT+ model by running QSWAT+
(from QGIS) and pointing it to these input files

In future, we plan to move the QSWAT+ “delineate watershed” step to an R
script (by calling the TauDEM executables from R), so that users can
document/control more of the configuration parameters, and initialize
their SWAT+ projects using the “predefined watershed” option.
Eventually, we hope to incorporate the  
[SWAT+ Automatic Workflow
(AW)](https://celray.github.io/docs/swatplus_aw/introduction.html), a
python codebase for automating a SWAT+ model configuration. Models
constructed in this way are more easily reproduced, as they bypass the
standard point-and-click steps done in the QGIS3 GUI when building SWAT+
models.

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

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
qswatplus.meta = my_metadata('make_qswatplus', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/make_qswatplus_metadata.csv"

``` r
print(qswatplus.meta[, c('file', 'type')])
```

    ##                                                                file           type
    ## swat_source                                 data/prepared/qswatplus      directory
    ## swat_dem_tif                   data/prepared/qswatplus/swat_dem.tif        GeoTIFF
    ## swat_landuse_tif           data/prepared/qswatplus/swat_landuse.tif        GeoTIFF
    ## swat_soils_tif                data/prepared/qswatplus/swat_soil.tif        GeoTIFF
    ## swat_landuse_lookup data/prepared/qswatplus/swat_landuse_lookup.csv            CSV
    ## swat_soils_lookup      data/prepared/qswatplus/swat_soil_lookup.csv            CSV
    ## swat_usersoil             data/prepared/qswatplus/swat_usersoil.csv            CSV
    ## swat_outlets               data/prepared/qswatplus/swat_outlets.shp ESRI Shapefile
    ## swat_lakes                   data/prepared/qswatplus/swat_lakes.shp ESRI Shapefile
    ## swat_streams               data/prepared/qswatplus/swat_streams.shp ESRI Shapefile
    ## metadata                           data/make_qswatplus_metadata.csv            CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/make_swat_metadata.csv)
in the `/data` directory.

The directory structure defined above will write the SWAT+ data files to
their expected paths [as outlined
here](https://celray.github.io/docs/swatplus_aw/data.html).

``` r
# load project file metadata
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
landuse.meta = my_metadata('get_landuse')
soils.meta = my_metadata('get_soils')
streamgages.meta = my_metadata('get_streamgages')

# load some of the watershed data created by 'get_basins.R'
# uyrw.poly gets overwritten below by mill creek polygon for debugging ************************************
#uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
poi.list = readRDS(here(basins.meta['poi', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))

# test mill creek  ***************************************************************************************
millcreek.list = readRDS(here(basins.meta['millcreek', 'file']))

# switch to mill creek boundary ***************************************************************************
uyrw.poly = millcreek.list$boundary
uyrw.poly.sp = as(uyrw.poly, 'Spatial')

# crop to mill creek ***************************************************************************************
uyrw.flowline = st_intersection(uyrw.flowline, uyrw.poly)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all geometries

``` r
uyrw.waterbody = st_intersection(uyrw.waterbody, uyrw.poly)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all geometries

``` r
# load USGS stream gage station points and attributes
paramcodes.df = read.csv(here(streamgages.meta['USGS_paramcodes', 'file']), colClasses='character')
usgs.sf = readRDS(here(streamgages.meta['USGS_sites', 'file']))
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']

# crop to mill creek ***************************************************************************************
usgs.sf = st_intersection(usgs.sf, uyrw.poly) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all geometries

NAs are represented in the GeoTiff by an integer – usually something
large and negative. Unfortunately the default value for this integer
when using `raster::writeRaster` is so large that (in 64bit QSWAT+
v1.2.3) it can cause an integer overflow error in one of the python
modules used by QSWAT+. We instead use the value recommended in the
QSWAT+ manual:

``` r
# define the NA flag integer value for rasters
tif.na.val = -32767
```

## copy DEM, soils, and land use rasters

The DEM created by `get_dem.R` must be cropped and masked to replace
out-of-watershed areas with NAs before loading it into the QSWAT+
workflow. This chunk writes the masked data for the DEM, as well as the
soils and land use rasters, setting the custom NA flag.

``` r
# create the QSWAT+ source directory if necessary
my_dir(here(qswatplus.meta['swat_source', 'file']))

# set the file paths to write
dem.path = here(qswatplus.meta['swat_dem_tif', 'file'])
landuse.path = here(qswatplus.meta['swat_landuse_tif', 'file'])
landuselu.path = here(qswatplus.meta['swat_landuse_lookup', 'file'])
soils.path = here(qswatplus.meta['swat_soils_tif', 'file'])
soilslu.path = here(qswatplus.meta['swat_soils_lookup', 'file'])
usersoil.path = here(qswatplus.meta['swat_usersoil', 'file'])



# run the chunk if any of these files don't exist
if(any(!file.exists(c(dem.path, landuse.path, landuselu.path, soils.path, soilslu.path, usersoil.path))))
{
  # to use `raster` functions, coerce URYW boundary polygon to 'Spatial' type (from package `sp`)
  # moved above for debugging **************************************************************************
  # uyrw.poly.sp = as(uyrw.poly, 'Spatial')
  
  # copy, crop, mask the DEM 
  dem.tif = raster(here(dem.meta['dem', 'file']))
  swat.dem.tif = mask(crop(dem.tif , uyrw.poly.sp), uyrw.poly.sp)

  # load the soils and land use rasters
  swat.soils.tif = raster(here(soils.meta['swat_tif', 'file']))
  swat.landuse.tif = raster(here(landuse.meta['swat_landuse_tif', 'file']))
  
  # crop and mask ***************************************************************************************
  swat.soils.tif = mask(crop(swat.soils.tif, uyrw.poly.sp), uyrw.poly.sp)
  swat.landuse.tif = mask(crop(swat.landuse.tif, uyrw.poly.sp), uyrw.poly.sp)
  
  # write the rasters to disk with new NA integer code
  writeRaster(swat.dem.tif, dem.path, overwrite=TRUE, NAflag=tif.na.val)
  writeRaster(swat.soils.tif, soils.path, overwrite=TRUE, NAflag=tif.na.val)
  writeRaster(swat.landuse.tif, landuse.path, overwrite=TRUE, NAflag=tif.na.val)
  
  # copy the attribute tables for soil and landuse
  file.copy(here(landuse.meta['swat_landuse_lookup', 'file']), landuselu.path, overwrite=TRUE)
  file.copy(here(soils.meta['swat_lookup', 'file']), soilslu.path, overwrite=TRUE)
  file.copy(here(soils.meta['swat_usersoil', 'file']), usersoil.path, overwrite=TRUE)
  
  # testing  ***************************************************************************************

  # change column headers for SWAT+
  xx = read.csv(here(soils.meta['swat_lookup', 'file']))
  names(xx) = c('SOIL_ID', 'SNAM')
  write.csv(xx, soilslu.path, row.names=FALSE)
  # xx = read.csv(usersoil.path)
  # xx.names = names(xx)
  # xx.names[4] = 'name'
  # write.csv(xx, soilslu.path, row.names=FALSE)
  
}
```

## write shapefiles for lakes and stream reaches

QGIS can’t read the (.rds) files that we are using to store geometries,
so these must be rewritten as ESRI shapefiles (.shp), and some data prep
is required for QSWAT+ to delineate channels correctly around lakes

There is a bug in the current version of QSWAT+ (v1.2.3) where the
plugin fails to add lakes during watershed delineation (“computing
topology” stage), probably due to some overlooked changes in the
QGIS2-\>QGIS3 update. The developer has posted a patched module file
(QSWATTopology.py) on [google
groups](https://groups.google.com/g/qswatplus/c/uDQD80gdXd4/m/2Iq05SUNBQAJ);
this file should be downloaded and copied over prior to running the
workflow.

Another bug in the current version causes an error indicating that
QSWAT+ has failed to construct a shapefile related to aquifers. This is
harmless (the model doesn’t require that file), and [the warning can be
safely
ignored](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ).

``` r
# set the file paths to write
lakes.path = here(qswatplus.meta['swat_lakes', 'file'])
streams.path = here(qswatplus.meta['swat_streams', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(lakes.path, streams.path))))
{
  # make sure the streams network contains only LINESTRING geometries, drop attributes
  streams.sf = st_cast(st_geometry(uyrw.flowline[!st_is(uyrw.flowline, 'POINT'),]), 'LINESTRING')
  
  # these steps unnecessary in mill creek ********************************************************************
  #
  # # identify adjacent polygons (indicating lakes split into multiple shapes)
  # lakes.adjmat = st_intersects(uyrw.waterbody, sparse=FALSE)
  # n.lakes = nrow(uyrw.waterbody)
  # idx.tomerge = lapply(1:n.lakes, function(idx.row) 
  #   idx.row + c(0, which(lakes.adjmat[idx.row, upper.tri(lakes.adjmat)[idx.row,]])))
  # idx.tomerge = idx.tomerge[sapply(idx.tomerge, length) > 1]
  # 
  # 
  # # depth-first search to find all components of each lake
  # adjmat = lakes.adjmat
  # idx.row = 1
  # dfs = function(adjmat, idx.row, visited=NA)
  # {
  #   n.poly = nrow(adjmat)
  #   if(anyNA(visited)) visited = rep(FALSE, n.poly)
  #   visited[idx.row] = TRUE
  #   retval = idx.row
  #   for(idx.col in which(!visited))
  #   {
  #     if(!visited[idx.col] & adjmat[idx.row, idx.col])
  #     { 
  #       retval = c(retval, dfs(adjmat, idx.col, visited))
  #     } 
  #   }
  #   return(retval)
  # }
  # 
  # 
  # # this dplyr style code can be used for the final dissolve at the end
  # lakes.sf = uyrw.waterbody %>% 
  #   mutate(lakeID = sapply(1:n.lakes, function(idx) sort(dfs(adjmat, idx))[1])) %>%
  #   group_by(lakeID) %>%
  #   summarize(areasqkm = sum(areasqkm))
  
  
  # make sure the lakes network contains only POLYGON geometries, drop attributes
  lakes.sf = st_cast(st_geometry(uyrw.waterbody), 'POLYGON')
  n.lakes = length(lakes.sf)
  
  # omit any islands from lakes
  lakes.sf = do.call(c, lapply(1:n.lakes, function(idx) 
    fill_holes(lakes.sf[idx,], threshold=st_area(lakes.sf[idx,]))))
  
  # add SWAT+ attributes -- need to specify these as reservoirs (RES=1) or ponds (RES=2)
  #lakes.df = data.frame(ID=1:n.lakes, RES=rep(2, n.lakes))
  lakes.df = data.frame(ID=1:n.lakes, RES=c(2,1))
  lakes.sf = st_sf(lakes.df, geom=lakes.sf)
  
  # omit lake 1
  lakes.sf = lakes.sf[2,]
  
  # try omitting all lakes below a surface area threshold
  #idx.omit = st_area(lakes.sf) < units::set_units(1, 'km2')
  #lakes.sf = lakes.sf[!idx.omit, ]
  
  # omit lake 7
  #lakes.sf = lakes.sf[-7,]
  
  # write as ESRI shapefile
  #st_write(lakes.sf[!idx.omit, ], lakes.path)
  
  # debugging **************************************************************************************
  st_write(lakes.sf, lakes.path)
  st_write(streams.sf, streams.path)
  
} else {
 
  lakes.sf = st_read(lakes.path)
  streams.sf = st_read(streams.path)
  
}
```

    ## Reading layer `swat_lakes' from data source `H:\UYRW_data\data\prepared\qswatplus\swat_lakes.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 2 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 534550.6 ymin: 5002063 xmax: 534676.1 ymax: 5002452
    ## projected CRS:  WGS 84 / UTM zone 12N
    ## Reading layer `swat_streams' from data source `H:\UYRW_data\data\prepared\qswatplus\swat_streams.shp' using driver `ESRI Shapefile'
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

This is helpful especially in flat areas, where the DEM does not provide
sufficient detail to reliably predict the watercourses that are observed
on the ground; as well as in areas where a watercourse meanders at the
sub-pixel level, and we would otherwise underestimate stream length.

## define outlet points and write as shapefile

SWAT+ build its watershed model by delineating channels and subbasins,
which are in part based on the locations of outlets of interest. For
example, to connect the model to discharge data, we need the SWAT+ model
to predict flow at those points where the time series measurements were
taken. Outlets should therefore be defined at every stream gage
location, and anywhere else that we intend to produce forecasts.

``` r
# set the threshold to use for snapping points to watercourses
outlet.snapval = units::set_units(10, 'meters')
```

For demonstration purposes I will include only the 5 stream gage
stations that are currently operational

``` r
if(!file.exists(here(qswatplus.meta['swat_outlets', 'file'])))
{
  # prune to time-series data, add some attributes about time series length
  usgs.ts.sf = usgs.sf[usgs.sf$data_type_cd %in% c('dv', 'iv', 'id'),]
  usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.sf$startyear
  idx.streamflow = usgs.ts.sf$parm_cd == paramcode.streamflow
  
  # pull the stream gage stations that are currently operational, snap to nearest flowline point
  # for mill creek, there is only one time series ****************************************************
  idx.current = 1
  #idx.current = usgs.ts.sf$endyear == 2020
  outlets.swat = st_geometry(usgs.ts.sf[idx.streamflow & idx.current,])
  outlets.swat = st_snap(outlets.swat, uyrw.flowline, outlet.snapval)
  
  # reproject the Carter's Bridge point, snap to nearest flowline point, add to the outlets set
  # let QSWAT+ detect the main outlet ****************************************************************
  #cb.pt = st_transform(poi.list$pt[['cartersbridge']], crs.list$epsg)
  #cb.pt = st_transform(poi.list$pt[['cartersbridge']], crs.list$epsg)
  #cb.pt.snapped = st_snap(cb.pt, uyrw.mainstem, outlet.snapval)
  #outlets.swat = c(st_geometry(cb.pt.snapped), outlets.swat)
  
  # add required fields for SWAT+
  n.pts = length(outlets.swat)
  outlets.swat = st_sf(data.frame(ID=1:n.pts, RES=0, INLET=0, PTSOURCE=0), geom=outlets.swat)
  
  # find a reasonable point for the lake outlet
  plot(st_geometry(lakes.sf))
  plot(streams.sf, add=TRUE)
  xx = st_intersection(st_geometry(lakes.sf), streams.sf)
  xx = xx[!st_is(xx, 'POINT')]
  xx.centroid = st_centroid(xx)
  yy = st_snap(xx.centroid, xx, tolerance=st_length(xx)/8)
  yy = st_nearest_points(xx.centroid, xx)
  yy = st_cast(yy, 'POINT')[2]
  plot(st_geometry(lakes.sf))
  plot(streams.sf, add=TRUE)
  plot(yy, pch=20, col='red', add=TRUE)
  plot(xx.centroid, pch=20, col='green', add=TRUE)
  
  # build an outlets geometry from this
  outlets.swat.lakes = st_sf(data.frame(ID=2, RES=1, INLET=0, PTSOURCE=0), geom=yy)
  
  outlets.swat = rbind(outlets.swat, outlets.swat.lakes)

  # write to disk as ESRI shapefile
  st_write(outlets.swat, here(qswatplus.meta['swat_outlets', 'file']), overwrite=TRUE)
  
}
```

All of the files necessary to run the full QSWAT+ watershed delineation
on Mill Creek have now been set up. To set up a SWAT+ model, first
follow the installation instructions at the [QSWAT+
homepage](https://swat.tamu.edu/software/qswat/), then apply the patch
to
[QSWATTopology.py](https://groups.google.com/g/qswatplus/c/uDQD80gdXd4/m/2Iq05SUNBQAJ)),
before opening QGIS3 and loading the QSWATPlus\_64 plugin. This will
prompt for a project directory and name (pick anything convenient), then
display the QSWAT+ workflow menu.

From there, it’s a matter of selecting a few configuration parameters
and specifying input files: If this R script completed successfully,
these input files are all ready to go in `swat.dir`, and the entire
process (including the length TauDEM computations) takes about 5-10
minutes.

NEXT STEPS:

1)  prepare weather and stream discharge datasets as CSV files, write to
    SQL database
2)  test a SWAT+ simulation
3)  try model calibration, compare to hydrograph to model with default
    settings
4)  look into automating the TauDEM workflow in R, import into QSWAT+ as
    “predefined watershed”
5)  If (4) is a possibility, create an R function to tune the channel
    threshold automatically
6)  automate the definitions of reservoir/ponds/wetlands based on
    main/trib channel locations
7)  try SWAT+ AW for automating the full process
8)  look into defining full UYRW watershed by joining multiple submodels
