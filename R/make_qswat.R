#' ---
#' title: "make_qswat.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **make_qswat.R**: copies data on Mill Creek watershed for input to QSWAT3 (WORK IN PROGRESS)
#' 
#' The (get_\*.R) URYW_data R scripts have saved our watershed data in a format convenient for
#' analysis in R. Some changes are required to certain files to make them readable in a QSWAT
#' project, eg. R binaries (.rds files) containing geometries must be simplified and exported to
#' ESRI shapefile, and rasters must be masked with a (non-default) NA flag.
#' 
#' This code prepares the required inputs to QSWAT for watershed configuration. Users can then
#' initialize a SWAT2012 model by running QSWAT (from QGIS) and pointing it to these input files
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
#' ['RODBC'](https://db.rstudio.com/odbc/) connects to MS Access databases using ODBC drivers. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(smoothr)
library(here)
source(here('R/get_helperfun.R'))
library(RODBC)

#'
#' ## project data

# assign a subdirectory of the UYRW project data folder to store QSWAT I/O files
swat.dir = file.path(data.dir, 'prepared/qswat')

# assign a project name. This defines the names of the QSWAT project (qgz) file and directory
swat.name = 'millcreek'
swat.projdir = file.path(swat.dir, swat.name)
swat.qgz = file.path(swat.dir, paste0(swat.name, '.qgz'))

#+ echo=FALSE
# define the files to write
{
files.towrite = list(
  
  # path to subdirectory with QSWAT inputs
  c(name='swat_source',
    file=swat.dir, 
    type='directory',
    description='QSWAT I/O directory'),
  
  # path to QSWAT project folder
  c(name='swat_proj',
    file=swat.projdir, 
    type='directory',
    description='QSWAT project directory'),
  
  # QGIS project file for QSWAT model
  c(name='swat_qgz',
    file=swat.qgz, 
    type='directory',
    description='QSWAT project directory'),
  
  # DEM raster ('swat_dem' from 'get_dem.R', cropped to AOI)  
  c(name='swat_dem_tif',
  file=file.path(swat.dir, 'swat_dem.tif'), 
  type='GeoTIFF',
  description='SWAT DEM'),
  
  # land use raster for the UYRW ('swat_landuse_tif' from 'get_landuse.R', cropped to AOI)
  c(name='swat_landuse_tif',
    file=file.path(swat.dir, 'swat_landuse.tif'), 
    type='GeoTIFF',
    description='SWAT land use classification'),
  
  # soils raster for the UYRW ('swat_tif' from 'get_soils.R', cropped to AOI)
  c(name='swat_soils_tif',
    file=file.path(swat.dir, 'swat_soil.tif'), 
    type='GeoTIFF',
    description='SWAT soils classification, maps to soil table in SWAT database'),
  
  # lookup table for 'swat_landuse_tif' (modified 'swat_landuse_lookup' from 'get_landuse.R')
  c(name='swat_landuse_lookup',
    file=file.path(swat.dir, 'swat_landuse_lookup.csv'), 
    type='CSV',
    description='integer code for swat_landuse_tif, maps to `crop` table in SWAT database'), 
  
  # lookup table for 'swat_soils_tif'
  c(name='swat_soil_lookup',
    file=file.path(swat.dir, 'swat_soils_lookup.csv'), 
    type='CSV',
    description='integer code for swat_soils_tif, maps to `usersoil` table in SWAT database'), 
  
  # outlets shapefile (largely based on 'USGS_sites' from 'get_streamgages.R')
  c(name='swat_outlets',
    file=file.path(swat.dir, 'swat_outlets.shp'), 
    type='ESRI Shapefile',
    description='outlet point locations, used by SWAT to delineate subbasins'),
  
  # streams shapefile (simplified 'flowlines' from 'get_basins.R')
  c(name='swat_streams',
    file=file.path(swat.dir, 'swat_streams.shp'), 
    type='ESRI Shapefile',
    description='stream geometries to "burn" into DEM prior to running TauDEM')
  
)
}

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
qswat.meta = my_metadata('make_qswat', files.towrite, overwrite=TRUE)
print(qswat.meta[, c('file', 'type')])

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/make_qswat_metadata.csv)
#' in the `/data` directory.
#' 
#' Originally, the workflow below involved copying some data files in R, then switching to QGIS3 to
#' initialize a QSWAT project, then switching back to R to modify the reference database. That
#' intermediate step of initializing the project simply creates some directories, copies a reference
#' database, and initializes a QGIS project file.
#' 
#' To streamline this process and make it repeatable, I have initialized a QSWAT project (in QGIS
#' v3.10.10-A, QSWAT3 64bit v1.1.1) with the name 'template', without adding a DEM or any further
#' inputs. The resulting project files are zipped as the file 'qswat_template.zip' in the UYRW data
#' directory. Its contents are copied (with `swat.name` replacing 'template') in the code below, and
#' the reference database is modified appropriately with the necessary data. This should allow users to
#' open the (qgz) project file as an 'existing project', and complete the QGIS workflow without
#' returning to R. 
#' 
#' Note that the project directory defined above in `swat.projdir` is overwritten without warning
#' so make sure this path is correctly specified and does not contain any old project files you wish
#' to keep.
#' 
#+ eval=FALSE

# unzip the template QSWAT project files 
swat.proj.parent = dirname(swat.projdir)
my_dir(swat.proj.parent)
unzip(here(data.dir, 'qswat_template.zip'), exdir=swat.proj.parent)

# rename (mdb) database and project directory to match desired project name 
if(file.exists(swat.projdir)) {unlink(swat.projdir, recursive=TRUE)}
file.rename(file.path(swat.proj.parent, 'uyrw_template'), swat.projdir)
file.rename(file.path(swat.projdir, 'uyrw_template.mdb'), file.path(swat.projdir, paste0(swat.name, '.mdb')))

# the project (qgz) file is extracted so we can change some xml entries, then source zip is deleted
temp.zip = file.path(swat.proj.parent, 'uyrw_template.qgz')
temp.qgs = file.path(swat.proj.parent, 'uyrw_template.qgs')
temp.qgd = file.path(swat.proj.parent, 'uyrw_template.qgd')
temp.qgs.out = file.path(swat.proj.parent, paste0(swat.name, '.qgs'))
temp.qgd.out = file.path(swat.proj.parent, paste0(swat.name, '.qgd'))
unzip(temp.zip, exdir=swat.proj.parent)
unlink(temp.zip)

# modify the XML by a simple search-and-replace then delete the temp file
swat.xml = readLines(temp.qgs)
swat.xml.out = gsub(pattern='uyrw_template', replace=swat.name, x=swat.xml)
writeLines(swat.xml.out, con=temp.qgs.out)
unlink(temp.qgs)

# rename the (empty) qgd file then package both files as (qgz) zip and delete tempfiles
file.rename(temp.qgd, temp.qgd.out)
zip(swat.qgz, files=c(temp.qgs.out, temp.qgd.out))
unlink(c(temp.qgs.out, temp.qgd.out))

# define path to project reference database
swatref.db.path = here(swat.dir, swat.name, 'QSWATRef2012.mdb')

#' The above code replaced `swat.projdir` and `swat.qgz` with a fresh QSWAT project configuration
#' that can be opened as an existing project in QSWAT. Before we do that, the chunks below will
#' modify the reference database 'QSWATRef2012.mdb' in `swat.projdir` with our soils and land use
#' data, and copy over the meteorological data as text file inputs.
#' 
#' First we load the datasets prepared so far:

# load project file metadata
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
landuse.meta = my_metadata('get_landuse')
soils.meta = my_metadata('get_soils')
streamgages.meta = my_metadata('get_streamgages')
weatherstations.meta = my_metadata('get_weatherstations')

# load some of the watershed data created by 'get_basins.R'
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
poi.list = readRDS(here(basins.meta['poi', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))

# load USGS stream gage station points and attributes
paramcodes.df = read.csv(here(streamgages.meta['USGS_paramcodes', 'file']), colClasses='character')
usgs.sf = readRDS(here(streamgages.meta['USGS_sites', 'file']))
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']


#'
#' ## define a subbasin of the UYRW to set up
#+ warning=FALSE

# load the mill creek data for testing purposes
millcreek.list = readRDS(here(basins.meta['millcreek', 'file']))

# define its drainage boundary
subbasin.poly = millcreek.list$boundary
subbasin.poly.sp = as(subbasin.poly, 'Spatial')

# crop flowlines, waterbodies, and outlet geometries to this subbasin 
subbasin.flowline = st_intersection(uyrw.flowline, subbasin.poly)
subbasin.waterbody = st_intersection(uyrw.waterbody, subbasin.poly)
subbasin.usgs.sf = st_intersection(usgs.sf, subbasin.poly) 



#'
#' ## copy DEM, soils, and land use rasters
#' 
#' The DEM created by `get_dem.R` must be cropped and masked to replace out-of-watershed areas with NAs
#' before loading it into the QSWAT+ workflow. This chunk writes the masked data for the DEM, as well as
#' the soils and land use rasters, setting the custom NA flag.
#'
# create the QSWAT source directory if necessary
my_dir(here(qswat.meta['swat_source', 'file']))

# set the file paths to write
dem.path = here(qswat.meta['swat_dem_tif', 'file'])
landuse.path = here(qswat.meta['swat_landuse_tif', 'file'])
soils.path = here(qswat.meta['swat_soils_tif', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(dem.path, landuse.path, soils.path))))
{
  # crop/mask the rasters and write to new location
  dem.tif = raster(here(dem.meta['swat_dem', 'file']))
  landuse.tif = raster(here(landuse.meta['swat_landuse_tif', 'file']))
  soils.tif = raster(here(soils.meta['swat_tif', 'file']))
  swat.dem.tif = mask(crop(dem.tif , subbasin.poly.sp), subbasin.poly.sp)
  swat.landuse.tif = mask(crop(landuse.tif , subbasin.poly.sp), subbasin.poly.sp)
  swat.soils.tif = mask(crop(soils.tif , subbasin.poly.sp), subbasin.poly.sp)
  writeRaster(swat.dem.tif, dem.path, NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(swat.landuse.tif, landuse.path, NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(swat.soils.tif, soils.path, NAflag=tif.na.val, overwrite=TRUE)
}

#' 
#' ## write shapefiles for stream reaches               
#' QGIS can't read the (.rds) files that we are using to store geometries, so these must be 
#' rewritten as ESRI shapefiles (.shp).
#' 
#' Some data prep will be required for QSWAT to delineate channels correctly around lakes,
#' possibly using the [SWAT2Lake](https://projects.au.dk/wet/software/#c55181) QGIS plugin
#' described [here](https://www.sciencedirect.com/science/article/abs/pii/S1364815218302500).
#' This remains a work in progress.
#'   

# set the file paths to write
streams.path = here(qswat.meta['swat_streams', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(streams.path))))
{
  # make sure the streams network contains only LINESTRING geometries, drop attributes
  streams.sf = st_cast(st_geometry(subbasin.flowline[!st_is(subbasin.flowline, 'POINT'),]), 'LINESTRING')
  st_write(streams.sf, streams.path)
  
} else {
 
  streams.sf = st_read(streams.path)
  
}

#' QSWAT will "burn" these stream reach line geometries into the DEM raster prior to running the
#' TauDEM workflow. This means that the elevation at each pixel overlapping with a stream reach
#' defined here is (temporarily) lowered by a fixed amount, so that TauDEM is forced to detect a 
#' stream reach along that path.
#' 
#' This is helpful especially in flat areas, where the DEM does not provide sufficient detail to 
#' reliably predict the watercourses that are observed on the ground; as well as in areas where a
#' watercourse meanders at the sub-pixel level, and we would otherwise underestimate stream length.
#' 

#'
#' ## define outlet points and write as shapefile
#' SWAT build its watershed model by delineating channels and subbasins, which are in part based on
#' the locations of outlets of interest. For example, to connect SWAT to discharge data, we need the
#' model to predict flow at those points where the time series measurements were taken. Outlets should
#' therefore be defined at every stream gage location, and anywhere else that we intend to produce
#' forecasts.
#' 
#'   
#' 
# set the threshold to use for snapping points to watercourses
outlet.snapval = units::set_units(10, 'meters')

#' For demonstration purposes I will include only USGS gages 
if(!file.exists(here(qswat.meta['swat_outlets', 'file'])))
{
  # prune to time-series data, add some attributes about time series length
  usgs.ts.sf = subbasin.usgs.sf[subbasin.usgs.sf$data_type_cd %in% c('dv', 'iv', 'id'),]
  usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.sf$startyear
  idx.streamflow = usgs.ts.sf$parm_cd == paramcode.streamflow
  
  # pull the stream gage stations, snap to nearest flowline point
  outlets.swat = st_geometry(usgs.ts.sf[idx.streamflow,])
  outlets.swat = st_snap(outlets.swat, subbasin.flowline, outlet.snapval)
  
  # add required fields for SWAT+
  n.pts = length(outlets.swat)
  outlets.swat = st_sf(data.frame(ID=1:n.pts, RES=0, INLET=0, PTSOURCE=0), geom=outlets.swat)

  # write to disk as ESRI shapefile
  st_write(outlets.swat, here(qswat.meta['swat_outlets', 'file']), overwrite=TRUE)
  
}

#' Land use data
#' 
#' Certain plant-code entries appear in the database that ships with SWAT+, but are missing from
#' the database included with SWAT2012. This chunk builds a SWAT2012-compatible `crop` table from
#' the SWAT+ 'swatplus_datasets.sqlite' database, containing all of these missing entries.  
#' 
#' Using the instructions at https://leowong.ca/blog/connect-to-microsoft-access-database-via-r/,
#' I am able to read and write MS Access mdb files. This chunk modifies the demo database to
#' append the plant-code entries and a lookup table for our land use raster
#' 
#+ eval=FALSE

# paths to SWAT+ reference database
swatplus.db.path = 'C:/SWAT/SWATPlus/Databases/swatplus_datasets.sqlite'

# set the (MS Access) driver string to use in ODBC calls
mdb.string = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)};'

# open the reference database and pull a copy of the 'crop' table
swatref.con = odbcDriverConnect(paste0(mdb.string, 'DBQ=', swatref.db.path))
crop2012 = sqlFetch(swatref.con, 'crop')

# process SWAT+ database using a helper function to import missing rows
plants2swat.list = my_plants2swat(crop2012, swatplus.db.path)
sqlSave(swatref.con, dat=plants2swat.list$crop_out, tablename='crop', rownames=FALSE, append=TRUE)
odbcClose(swatref.con)

# save modified lookup table for compatibility with SWAT2012 codes (uppercase, 4-letter)
landuse.csv = read.csv(here(landuse.meta['swat_landuse_lookup', 'file']))
landuse.codes = plants2swat.list$lookup[match(landuse.csv$Landuse, names(plants2swat.list$lookup))]
landuse.swat2012 = data.frame(LANDUSE_ID=landuse.csv$Value, SWAT_CODE=landuse.codes)
write.csv(landuse.swat2012, here(qswat.meta['swat_landuse_lookup', 'file']), row.names=FALSE)


#' soils data
#' 
#' Our soils raster is derived from SSURGO/STATSGO2, one of the default databases supported in
#' both SWAT2012 and SWAT+. However, as we have encountered errors when attempting to import the  
#' (separately downloaded) mdb file containing the 'usersoil' table in the current version of QSWAT,
#' we will instead import this table manually by overwriting the default one in the reference
#' database and creating a lookup table. 
#' 
#' This extra step will be useful later on anyway, as we may wish to introduce custom soil parameter
#' sets that are not included in SSURGO or STATSGO2. By importing a custom 'usersoil', we control
#' exactly which parameters are used.
#+ eval=FALSE

# path to SWAT2012 soils database (mdb)
ssurgo.db.path = 'H:/UYRW_installers/SWAT_US_SSURGO_Soils.mdb'

# load the SSURGO database and the mukeys list for the study area
mukeys = unique(raster(here(qswat.meta['swat_soils_tif', 'file'])))

# grab a copy of the reference database 'usersoil' then close the connection
ssurgo.con = odbcDriverConnect(paste0(mdb.string, 'DBQ=', ssurgo.db.path))
usersoil.ref = sqlFetch(ssurgo.con, 'SSURGO_Soils')
odbcClose(ssurgo.con)

# index all the relevant mukeys, check that none are missing from the mdb table
mukeys.ssurgo = unique(usersoil.ref$MUID)
if(!all(mukeys %in% mukeys.ssurgo)) {print('CRITICAL ERROR: raster mukey missing from usersoil')}
idx.usersoil = usersoil.ref$MUID %in% mukeys

# make a copy of the relevant rows of the usersoil table, modifying SNAM to unique text ID
swat.usersoil = usersoil.ref %>% 
  filter(idx.usersoil) %>%
  select(-HYDGRP_ORIG, -Notes) %>%
  mutate(SNAM=paste0('UYRW', MUID))

# overwrite in reference table
swatref.con = odbcDriverConnect(paste0(mdb.string, 'DBQ=', swatref.db.path))
sqlDrop(swatref.con, 'usersoil', errors=FALSE)
sqlSave(swatref.con, dat=swat.usersoil, tablename='usersoil', rownames=FALSE, safer=FALSE)
odbcClose(swatref.con)

# create the lookup table for the raster
swat.soil.lookup = swat.usersoil %>%
  mutate(SOIL_ID=MUID) %>%
  select(SOIL_ID, SNAM)

# finally, write the lookup table 
write.csv(swat.soil.lookup, here(qswat.meta['swat_soil_lookup', 'file']), row.names=FALSE)


#' At this point, the user should be able to open the QSWAT project in QGIS3 and complete the
#' watershed delineation and HRU definition workflow. Eventually we hope to have these steps
#' completed automatically, by calling python modules using the `reticulate` package. 
#' 
#' For now, users must complete some model configuration steps in the QGIS GUI. Mostly this is
#' a matter of specifying the paths to the qgz, tif, shp, and csv files generated above. Recall
#' these paths are listed in the 'make_qswat_metadata.csv' table.
#' 
#' Watershed Delineation
#' 
#' (1) In QGIS, open QSWAT plugin, select 'Existing Project' -> select qgz project file
#' (2) Click 'Delineate Watershed'. Under 'Delineate Watershed' tab, Select the DEM tif
#' (3) Check 'Burn in existing stream network' -> browse to streams shapefile
#' (4) Set a stream delineation threshold area (I use 2km^2)
#' (5) check 'Use an inlets/outlets shapefile' -> browse to outlets shapefile
#' (6) Adjust number of processes (if using MPI), then click 'Create Watershed' to run TauDEM
#' (7) Dialogue box will be unresponsive while TauDEM is running. Click 'OK' when it finishes 
#' 
#' HRUs
#' 
#' (8) back to QSWAT dialogue box -> Click 'Create HRUs'
#' (9) Click '...' to select the landuse tif, Select 'Use csv file' from dropdown menu
#' (10) Click '...' to select the soils tif, Select 'usersoil' option under 'Soil data'
#' (11) Click 'Read' -> browse to landuse and soils lookup csv files when prompted
#' (12) Select HRU definition method (I use 'Dominant HRU') and click 'Create HRU'
#' 
#' If at any point during setup QSWAT3 reports an error, it is recommended to delete the project
#' directory and start over to debug, as the plugin can have unpredictable behaviour after errors.
#' 
#' If all steps complete without errors, they should produce a working QSWAT3 model. Users can
#' then run SWATEDitor.exe or click the 'Edit Inputs and Run SWAT' button on the main QSWAT
#' dialogue to set up climatological inputs and model parameters required for simulations.





#+ include=FALSE
# Development code
# #['xml2'](https://cran.r-project.org/web/packages/xml2/vignettes/modification.html) parses and writes to XML files
# library(xml2)
# 
# # open the xml and replace all references to template with new project name/path 
# swat.xml = read_xml(file.path(swat.proj.parent, 'uyrw_template.qgs'))
# xml_attr(swat.xml, 'projectname') = swat.name
# xml_set_text(xml_child(swat.xml, search='title'), swat.name) 


#my_markdown('make_qswat')