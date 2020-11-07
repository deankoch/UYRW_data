#' ---
#' title: "make_qswatplus.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **make_swat.R**: copies data on Mill Creek watershed for input to QSWAT+ (WORK IN PROGRESS)
#' 
#' The (get_\*.R) URYW_data R scripts have saved our watershed data in a format convenient for
#' analysis in R. Some changes are required to certain files to make them readable in a QSWAT+
#' project, eg. R binaries (.rds files) containing geometries must be simplified and exported to
#' ESRI shapefile, and rasters must be masked with a (non-default) NA flag.
#' 
#' This code prepares the required inputs to QSWAT+ for watershed configuration. Users can then
#' initialize a SWAT+ model by running QSWAT+ (from QGIS) and pointing it to these input files
#' 
#' In future, we plan to move the QSWAT+ "delineate watershed" step to an R script (by calling
#' the TauDEM executables from R), so that users can document/control more of the configuration
#' parameters, and initialize their SWAT+ projects using the "predefined watershed" option.
#' Eventually, we hope to incorporate the  
#' [SWAT+ Automatic Workflow (AW)](https://celray.github.io/docs/swatplus_aw/introduction.html),
#' a python codebase for automating a SWAT+ model configuration. Models constructed in this way
#' are more easily reproduced, as they bypass the standard point-and-click steps done in the
#' QGIS3 GUI when building SWAT+ models.
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

# define a subdirectory of the project data folder to designate for QSWAT+ inputs
swat.dir = file.path(data.dir, 'prepared/qswatplus')

# define the files to write
{
files.towrite = list(
  
  # path to subdirectory with QSWAT+ inputs
  c(name='swat_source',
    file=swat.dir, 
    type='directory',
    description='QSWAT+ project source files'),
  
  # DEM raster ('swat_dem' from 'get_dem.R', cropped to AOI)  
  c(name='swat_dem_tif',
  file=file.path(swat.dir, 'swat_dem.tif'), 
  type='GeoTIFF',
  description='SWAT+ DEM'),
  
  # land use raster for the UYRW ('swat_landuse_tif' from 'get_landuse.R', cropped to AOI)
  c(name='swat_landuse_tif',
    file=file.path(swat.dir, 'swat_landuse.tif'), 
    type='GeoTIFF',
    description='SWAT+ land use classification'),
  
  # soils raster for the UYRW ('swat_tif' from 'get_soils.R', cropped to AOI)
  c(name='swat_soils_tif',
    file=file.path(swat.dir, 'swat_soil.tif'), 
    type='GeoTIFF',
    description='SWAT+ soils classification, maps to soil table in SWAT+ database'),
  
  # lookup table for 'swat_landuse_tif' (copy of 'swat_landuse_lookup' from 'get_landuse.R')
  c(name='swat_landuse_lookup',
    file=file.path(swat.dir, 'swat_landuse_lookup.csv'), 
    type='CSV',
    description='swat_landuse_tif integer code lookup table, maps to plants_plt SWAT+ dataset'), 
  
  # outlets shapefile (largely based on 'USGS_sites' from 'get_streamgages.R')
  c(name='swat_outlets',
    file=file.path(swat.dir, 'swat_outlets.shp'), 
    type='ESRI Shapefile',
    description='outlet point locations, used by SWAT+ to delineate subbasins'),
  
  # lakes shapefile (simplified 'waterbody' from 'get_basins.R')
  c(name='swat_lakes',
    file=file.path(swat.dir, 'swat_lakes.shp'), 
    type='ESRI Shapefile',
    description='polygons in the UYRW representing water bodies'),
  
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
qswatplus.meta = my_metadata('make_qswatplus', files.towrite, overwrite=TRUE)
print(qswatplus.meta[, c('file', 'type')])

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/make_qswatplus_metadata.csv)
#' in the `/data` directory.
#' 

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
# create the QSWAT+ source directory if necessary
my_dir(here(qswatplus.meta['swat_source', 'file']))

# set the file paths to write
dem.path = here(qswatplus.meta['swat_dem_tif', 'file'])
landuse.path = here(qswatplus.meta['swat_landuse_tif', 'file'])
landuselu.path = here(qswatplus.meta['swat_landuse_lookup', 'file'])
soils.path = here(qswatplus.meta['swat_soils_tif', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(dem.path, landuse.path, landuselu.path, soils.path))))
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

  # copy the land use lookup table
  file.copy(here(landuse.meta['swat_landuse_lookup', 'file']), landuselu.path, overwrite=TRUE)
}

#' 
#' ## write shapefiles for lakes and stream reaches               
#' QGIS can't read the (.rds) files that we are using to store geometries, so these must be 
#' rewritten as ESRI shapefiles (.shp), and some data prep is required for QSWAT+ to delineate
#' channels correctly around lakes
#'   
#' There is a bug in the current version of QSWAT+ (v1.2.3) where the plugin fails to add lakes during
#' watershed delineation ("computing topology" fails, perhaps due to a syntax change in the QGIS v2->v3
#' update). The developer has posted a patched module file (QSWATTopology.py) on
#' [google groups](https://groups.google.com/g/qswatplus/c/uDQD80gdXd4/m/2Iq05SUNBQAJ); this file
#' should be downloaded and copied over prior to running the workflow.
#' 
#' Another bug in the current version causes an error indicating that QSWAT+ has failed to construct
#' a shapefile related to aquifers. This is harmless (the model doesn't require that file), and
#' [the warning can be safely ignored](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ).
#'                                                                                                                                                                                                                                                                                                                                                                                 

# set the file paths to write
lakes.path = here(qswatplus.meta['swat_lakes', 'file'])
streams.path = here(qswatplus.meta['swat_streams', 'file'])

# run the chunk if any of these files don't exist
if(any(!file.exists(c(lakes.path, streams.path))))
{
  # make sure the streams network contains only LINESTRING geometries, drop attributes
  streams.sf = st_cast(st_geometry(subbasin.flowline[!st_is(subbasin.flowline, 'POINT'),]), 'LINESTRING')
  
  # these steps unnecessary in mill creek ********************************************************************
  #
  # # identify adjacent polygons (indicating lakes split into multiple shapes)
  # lakes.adjmat = st_intersects(subbasin.waterbody, sparse=FALSE)
  # n.lakes = nrow(subbasin.waterbody)
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
  # lakes.sf = subbasin.waterbody %>% 
  #   mutate(lakeID = sapply(1:n.lakes, function(idx) sort(dfs(adjmat, idx))[1])) %>%
  #   group_by(lakeID) %>%
  #   summarize(areasqkm = sum(areasqkm))
  
  
  # make sure the lakes network contains only POLYGON geometries, drop attributes
  lakes.sf = st_cast(st_geometry(subbasin.waterbody), 'POLYGON')
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

#' QSWAT+ will "burn" these stream reach line geometries into the DEM raster prior to running the
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
#' SWAT+ build its watershed model by delineating channels and subbasins, which are in part based on the 
#' locations of outlets of interest. For example, to connect the model to discharge data, we need the SWAT+
#' model to predict flow at those points where the time series measurements were taken. Outlets should therefore
#' be defined at every stream gage location, and anywhere else that we intend to produce forecasts.
#' 
#'   
#' 
# set the threshold to use for snapping points to watercourses
outlet.snapval = units::set_units(10, 'meters')

#' For demonstration purposes I will include only the 5 stream gage stations that are currently operational 
if(!file.exists(here(qswatplus.meta['swat_outlets', 'file'])))
{
  # prune to time-series data, add some attributes about time series length
  usgs.ts.sf = subbasin.usgs.sf[subbasin.usgs.sf$data_type_cd %in% c('dv', 'iv', 'id'),]
  usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
  usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.sf$startyear
  idx.streamflow = usgs.ts.sf$parm_cd == paramcode.streamflow
  
  # pull the stream gage stations that are currently operational, snap to nearest flowline point
  # for mill creek, there is only one time series ****************************************************
  idx.current = 1
  #idx.current = usgs.ts.sf$endyear == 2020
  outlets.swat = st_geometry(usgs.ts.sf[idx.streamflow & idx.current,])
  outlets.swat = st_snap(outlets.swat, subbasin.flowline, outlet.snapval)
  
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

#' All of the files necessary to run the full QSWAT+ watershed delineation on Mill Creek
#' have now been set up. To set up a SWAT+ model, first follow the installation instructions at
#' the [QSWAT+ homepage](https://swat.tamu.edu/software/qswat/), then apply the patch to
#' [QSWATTopology.py](https://groups.google.com/g/qswatplus/c/uDQD80gdXd4/m/2Iq05SUNBQAJ)),
#' before opening QGIS3 and loading the QSWATPlus_64 plugin. This will prompt for a project
#' directory and name (pick anything convenient), then display the QSWAT+ workflow menu.
#' 
#' From there, it's a matter of selecting a few configuration parameters and specifying input
#' files: If this R script completed successfully, these input files are all ready to go in
#' `swat.dir`, and the entire process (including the length TauDEM computations) takes about
#' 5-10 minutes. 
#' 
#' NEXT STEPS:
#' 
#' 1) prepare weather and stream discharge datasets as CSV files, write to SQL database
#' 2) test a SWAT+ simulation
#' 3) try model calibration, compare to hydrograph to model with default settings
#' 4) look into automating the TauDEM workflow in R, import into QSWAT+ as "predefined watershed" 
#' 5) If (4) is a possibility, create an R function to tune the channel threshold automatically
#' 6) automate the definitions of reservoir/ponds/wetlands based on main/trib channel locations
#' 7) try SWAT+ AW for automating the full process
#' 8) look into defining full UYRW watershed by joining multiple submodels

#' Land use data
#' 
#' Certain plant-code entries appear in the database that ships with SWAT+, but are missing from
#' the database included with SWAT2012. This chunk builds a SWAT2012-compatible `crop` table from
#' the SWAT+ 'swatplus_datasets.sqlite' database, containing all of these missing entries.  
#' 

# following instructions at https://leowong.ca/blog/connect-to-microsoft-access-database-via-r/
library(RODBC)
odbc.driver.string = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)};'
#mdb.path = 'H:/UYRW_data/data/prepared/qswatplus/mc_swat2012/mc1/QSWATRef2012.mdb'
mdb.path = 'C:/SWAT/SWATEditor/Databases/QSWATRef2012.mdb'
odbc.string = paste0(odbc.driver.string, 'DBQ=', mdb.path)
mdb.con = odbcDriverConnect(odbc.string)
crop2012 = sqlFetch(mdb.con, 'crop')

#' The SWAT database table 'crop' is now copied to the R dataframe `crop2012`. The goal is to
#' update this table with the values found in the SWAT+ mySQL database, so that we can use the
#' same land use raster data with both SWAT and SWAT+.

# consider adding this library to helper_functions
library(RSQLite)

# set path to 'swatplus_datasets.sqlite'
data.sql.path = 'C:/SWAT/SWATPlus/Databases/swatplus_datasets.sqlite'

# import the main plant growth table:
data.sql = dbConnect(RSQLite::SQLite(), data.sql.path)
plants.plt = dbReadTable(data.sql, 'plants_plt') # main data table

# move this to the end of the chunk 
# dbDisconnect(data.sql)

# identify plant codes common to both databases
codes.common = crop2012$CPNM[crop2012$CPNM %in% toupper(plants.plt$name)]

# identify plant codes in 'plants_plt' which are missing from SWAT2012 database
codes.toadd = plants.plt$name[!(toupper(plants.plt$name) %in% codes.common)]

#' The SWAT+ database table 'plants_plt' (copied into R as `plants.plt`) contains most of the
#' parameters needed in the 'crop' table in the SWAT2012 database. The field names are different,
#' however, so these need to be translated. A handful of parameters also need to be fetched from
#' other tables. These are linked to 'plants_plt' via 'landuse_lum'

# open linked tables for Manning's 'n' for overland flow, runoff curve numbers
landuse.lum = dbReadTable(data.sql, 'landuse_lum')
ovn.lum = dbReadTable(data.sql, 'ovn_table_lum')
cn.lum = dbReadTable(data.sql, 'cntable_lum')

# fetch find variables located outside `plants.plt`, append as new columns
ovn.vn = 'ovn_mean'
cn.vn = c('cn_a', 'cn_b', 'cn_c', 'cn_d')
idx.lum = match(plants.plt$name, sapply(landuse.lum$name, function(nm) strsplit(nm, '_lum')[[1]]))
plants.plt[, ovn.vn] = ovn.lum[landuse.lum$ov_mann_id[idx.lum], ovn.vn]
plants.plt[, cn.vn] = cn.lum[landuse.lum$cn2_id[idx.lum], cn.vn]

# identify 'trees' and translate 'plnt_typ' to integer code (see 'swat2009-theory.pdf', p.311)
plants.plt$plnt_typ[plants.plt$bm_tree_acc>0 & plants.plt$bm_tree_max>0] = 'trees'
idc.codes = c(NA, NA, NA, 'warm_annual', 'cold_annual', 'perennial', 'trees')
plants.plt$plnt_typ = match(plants.plt$plnt_typ, idc.codes)

# add dummy data to two missing fields
plants.plt$FERTFIELD = rep(0, nrow(plants.plt))
plants.plt$OpSchedule = rep('RNGB', nrow(plants.plt))

#' The 'FERTFIELD' and 'OpSchedule' fields (added above) do not appear in any of the SWAT
#' theory or I/O documentation that I have seen. As far as I can tell, these give details about
#' agricultural operations that are not relevant to our implementation of SWAT/SWAT+
#' 
#' All of the necessary data has now been added to `plants.plt`. We now trim this table to include
#' only plant codes which are missing from `crop2012` and modify field names and row keys to match
#' with the syntax of SWAT2012's 'crop'.

# create a translation table (SWAT+/plants_plt -> SWAT2012/crop)
swat2012.lu = c(CPNM='name',
                IDC='plnt_typ',
                CROPNAME='description',
                BIO_E='bm_e',
                HVSTI='harv_idx',
                BLAI='lai_pot',
                FRGRW1='frac_hu1', 
                LAIMX1='lai_max1', 
                FRGRW2='frac_hu2', 
                LAIMX2='lai_max2', 
                DLAI='hu_lai_decl', 
                CHTMX='can_ht_max', 
                RDMX='rt_dp_max', 
                T_OPT='tmp_opt', 
                T_BASE='tmp_base', 
                CNYLD='frac_n_yld',
                CPYLD='frac_p_yld',
                BN1='frac_n_em',
                BN2='frac_n_50',
                BN3='frac_n_mat',
                BP1='frac_p_em',
                BP2='frac_p_50',
                BP3='frac_p_mat',
                WSYF='harv_idx_ws',
                USLE_C='usle_c_min',
                GSI='stcon_max',
                VPDFR='vpd',
                FRGMAX='frac_stcon',
                WAVP='ru_vpd',
                CO2HI='co2_hi',
                BIOEHI='bm_e_hi',
                RSDCO_PL='plnt_decomp',
                OV_N='ovn_mean',
                CN2A='cn_a',
                CN2B='cn_b',  
                CN2C='cn_c',  
                CN2D='cn_d', 
                FERTFIELD='FERTFIELD',
                ALAI_MIN='lai_min',
                BIO_LEAF='bm_tree_acc',
                MAT_YRS='yrs_mat',
                BMX_TREES='bm_tree_max',
                EXT_COEF='ext_co',
                BM_DIEOFF='bm_dieoff',
                OpSchedule='OpSchedule')

# translate column names of `plants.plt` (SWAT+) to those of `crop` (SWAT2012)
plants.tocrop = plants.plt[plants.plt$name %in% codes.toadd, swat2012.lu]
colnames(plants.tocrop) = names(swat2012.lu)

# append new row key columns
keys.tocrop = (1:length(codes.toadd)) + nrow(crop2012) + 1
plants.tocrop = cbind(data.frame(OBJECTID=keys.tocrop, ICNUM=keys.tocrop), plants.tocrop)

# generate new 4-letter codes (of form 'ZZ**') to replace longer SWAT+ plant codes
idx.namechange = nchar(plants.plt$name) > 4 
seq.namechange = (1:sum(idx.namechange)) - 1
suffix.namechange = cbind(LETTERS[(seq.namechange %/% 26) + 1], LETTERS[(seq.namechange %% 26) + 1])
codes.namechange = paste0('ZZ', apply(suffix.namechange, 1, paste, collapse=''))

# lookup vector that switches all codes to uppercase, and subs in 4-letter versions as needed
codes.new = setNames(toupper(plants.plt$name), nm=plants.plt$name)
codes.new[idx.namechange] = codes.namechange
plants.tocrop$CPNM = codes.new[match(plants.tocrop$CPNM, names(codes.new))]

#' Finally we update the SWAT2012 database by appending this table to 'crop'. We also create a
#' new land use lookup table in which the plant codes are of the (uppercase, 4-letter) form used
#' by SWAT2012 

# append SWAT+ plants data to SWAT2012 database, close the connection
# mdb.path = 'H:/UYRW_data/data/prepared/qswatplus/mc_swat2012/mc1/QSWATRef2012.mdb'
# odbc.string = paste0(odbc.driver.string, 'DBQ=', mdb.path)
# mdb.con = odbcDriverConnect(odbc.string)
sqlSave(mdb.con, dat=plants.tocrop, tablename='crop', rownames=FALSE, append=TRUE)
sqlSave(mdb.con, dat=plants.tocrop, tablename='cropdefault', rownames=FALSE, append=TRUE)
odbcClose(mdb.con)

# create modified landuse lookup CSV for SWAT2012
new.landuse.csv = read.csv(here(landuse.meta['swat_landuse_lookup', 'file']))
new.landuse.csv$Landuse = codes.new[match(new.landuse.csv$Landuse, names(codes.new))]

# write to disk
landuse.csv.path = here('data/prepared/qswatplus/swat2012_landuse_lookup.csv')
write.csv(new.landuse.csv, landuse.csv.path, row.names=FALSE)

############
############
# The us_soils_v3.pdf instructions say to copy the SWAT_US_SSURGO_Soils.mdb database to the
# SWATEditor/Databases folder

# check that the SWAT2012 version of SSURGO database contains all our mukeys
mukeys = unique(raster(soils.meta['swat_tif', 'file']))

odbc.driver.string = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)};'
mdb.path = 'C:/SWAT/SWATEditor/Databases/SWAT_US_SSURGO_Soils.mdb'
odbc.string = paste0(odbc.driver.string, 'DBQ=', mdb.path)
mdb.con = odbcDriverConnect(odbc.string)
sqlTables(mdb.con)
soils2012 = sqlFetch(mdb.con, 'SSURGO_Soils')
odbcClose(mdb.con)

# index all the relevant mukeys, check that none are missing from the mdb table
mukeys2012 = unique(soils2012$MUID)
all(mukeys %in% mukeys2012)
idx.usersoil = soils2012$MUID %in% mukeys

# check for bad datapoints (empty or negative)
usersoil = soils2012
usersoil.classes = sapply(soils2012, class)
usersoil.cha = usersoil[, usersoil.classes == 'character']
usersoil.num = usersoil[, usersoil.classes != 'character']
any(usersoil.num<0)
any(is.na(usersoil.num))
any(is.na(usersoil.cha))

# there are some NA text fields. Fill them with dummy data
problem.fields = names(usersoil.cha)[sapply(usersoil.cha, anyNA)]
for(vn in problem.fields)
{
  new.column = usersoil[,vn]
  new.column[is.na(new.column)] = 'A'
  usersoil[,vn] = new.column
}

# match OBJECTID with MUKEY
usersoil = cbind(data.frame(OBJECTID=usersoil$MUID), usersoil)

# try writing the usersoil table to the SWATEditor reference database
mdb.path = 'C:/SWAT/SWATEditor/Databases/QSWATRef2012.mdb'
odbc.string = paste0(odbc.driver.string, 'DBQ=', mdb.path)
mdb.con = odbcDriverConnect(odbc.string)
sqlDrop(mdb.con, 'usersoil', errors=FALSE)
sqlSave(mdb.con, dat=usersoil[idx.usersoil,], tablename='usersoil', rownames=FALSE, safer=FALSE)
odbcClose(mdb.con)

# make a soil lookup table
soils.csv.path = here('data/prepared/qswatplus/swat2012_soil_lookup.csv')
write.csv(data.frame(VALUE=mukeys, NAME=mukeys), soils.csv.path, row.names=FALSE)


# # drop the old table then add the new one, then close the connection
# mdb.classes = usersoil.classes
# mdb.classes[mdb.classes=='numeric'] = 'double'
# sqlDrop(mdb.con, 'SSURGO_Soils', errors=FALSE)
# sqlSave(mdb.con, dat=usersoil[idx.usersoil,], 
#         tablename='SSURGO_Soils', 
#         rownames=FALSE)

# try exporting a custom usersoil table
write.csv(usersoil[idx.usersoil,], 'data/prepared/swat2012_usersoil.csv', row.names=FALSE)







# # compare the tables among plant codes common to both databases
# codes.common = crop2012$CPNM[crop2012$CPNM %in% plants.tocrop$CPNM]
# 
# idx.orig = match(codes.common, crop2012$CPNM)
# idx.modi = match(codes.common, plants.tocrop$CPNM)
# idx.vars = 31:40
# idx.rows = 1:10
# plants.tocrop[idx.modi[idx.rows], idx.vars]
# crop2012[idx.orig[idx.rows], 2 + idx.vars]
# 
# xx = crop2012 %>% slice(idx.orig) %>% 
#   select(-OBJECTID, -ICNUM, -CPNM, -CROPNAME, -FERTFIELD, -OpSchedule) %>% 
#   as.matrix
# 
# yy = plants.tocrop %>% slice(idx.modi) %>% 
#   select(-CPNM, -CROPNAME) %>% 
#   as.matrix
#   
# apply(xx -yy, 2, median)





#########
library(RSQLite)
data.sql.path = 'C:/SWAT/SWATPlus/Databases/swatplus_datasets.sqlite'
data.sql = dbConnect(RSQLite::SQLite(), data.sql.path)
tablenames = dbListTables(data.sql)
plants.plt = dbReadTable(data.sql, 'plants_plt')

for(idx in 1:length(tablenames))
{
  print(tablenames[idx])
  print(head(dbReadTable(data.sql, tablenames[idx])))
}

yy = as.integer(landuse.lum %>% pull(id))
xx = sapply(landuse.lum %>% pull(name), function(nm) strsplit(nm, '_lum', fixed=TRUE)[[1]])
zz = data.frame(id=yy, lum=as.vector(xx))
nrow(zz)
match(zz$lum, plants.plt$name)


# from https://swatplus.gitbook.io/docs/user/editor/inputs/land-use-management
landuse.lum = dbReadTable(data.sql, 'landuse_lum')
landuse.lum %>% pull(id, name)
# landuse_lum (name field, without '_lum' suffix) matches entries of plants_plt to integer keys:
# cn2_id: links to cntable_lum, containing curve number data (CN2A-D)
# ov_mann_id: links to ovn_table_lum, containing Manning n value for overland flow (OV_N)
# 
dbReadTable(data.sql, 'ovn_table_lum')
dbReadTable(data.sql, 'cntable_lum')
dbReadTable(data.sql, 'cons_prac_lum')


# this file exported from SWAT2012 MS Access database (R can't open these files)
crop.csv.path = 'C:/Users/deank/Documents/crop.csv'
crop2012 = read.csv(crop.csv.path)
str(crop2012)

# load existing SWAT+ landuse lookup
landuse.csv = read.csv(here(landuse.meta['swat_landuse_lookup', 'file']))
landuse.codes = landuse.csv$Landuse

# check for missing entries
sum(!(landuse.codes %in% tolower(crop2012$CPNM))) # 14 missing
sum(landuse.codes %in% tolower(crop2012$CPNM)) # 6 are there

plants.plt[plants.plt$name %in% landuse.codes[!(landuse.codes %in% tolower(crop2012$CPNM))],]

# compare a row from both tables to look for common parameters
plants.plt[plants.plt$name=='alfa',]
crop2012[crop2012$CPNM=='ALFA',]

crop2012$OpSchedule




crop2012[, names(swat2012.lu[swat2012.lu==''])]

plants.plt[plants.plt$name=='ldgp', !(colnames(plants.plt) %in% swat2012.lu[swat2012.lu!=''])]



tolower(crop2012$CPNM) %in% plants.plt$name

colnames(crop2012)
xx = crop2012[,names(swat2012.lu[swat2012.lu!=''])]
yy = plants.plt[,swat2012.lu[swat2012.lu!='']]

plants.plt[, !(colnames(plants.plt) %in% swat2012.lu[swat2012.lu!=''])]
  
head(xx)
head(yy)

# check if we can find all the same parameters in the SWAT+ database
names(crop2012)
toupper(names(plants.plt))

tail(crop2012)

# take a stab at a translation


crop2012$OpSchedule

str(plants.plt)

# 


wgn.sql.path = 'C:/SWAT/SWATPlus/Databases/swatplus_wgn.sqlite'
wgn.sql = dbConnect(RSQLite::SQLite(), wgn.sql.path)
dbListTables(wgn.sql)



#' Weather data
#' 
#' 
#' 


wgn = dbReadTable(wgn.sql, 'wgn_cfsr_world')
head(wgn)
nrow(wgn)

wgn.mon = dbReadTable(wgn.sql, 'wgn_cfsr_world_mon')
head(wgn.mon)
#wgn.mon %>% group_by(wgn_id)
xx = unique(wgn.mon$wgn_id)
length(xx)

#####

wgn = dbReadTable(wgn.sql, 'wgn_us')
head(wgn)
nrow(wgn)

wgn.mon = dbReadTable(wgn.sql, 'wgn_us_mon')
head(wgn.mon)
#wgn.mon %>% group_by(wgn_id)
xx = unique(wgn.mon$wgn_id)
length(xx)
###

statsgo.ref = dbReadTable(soils.sql, 'statsgo')
dbDisconnect(soils.sql)

###


###


# # load the data
# ghcnd.sf = readRDS(here(weatherstations.meta['ghcnd', 'file']))
# ghcnd.list = readRDS(here(weatherstations.meta['ghcnd_data', 'file']))
# 
# # find the stations with data relevant to SWAT+
# # for now I use only 1950s data (corresponding to the discharge time series) *****************************
# yrs.target = 1950:1960
# var.target = setNames(nm=c('PRCP', 'TMAX', 'TMIN'))
# # these codes explained at https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
# 
# #
# #precipitation, temperature, solar radiation, wind speed and relative humidity
# #should be mm, degree centigrade, MJ/m², m/s, and decimal fraction, respectively
# 
# # helper function to identify time series that overlap with a target range
# my_parsedate = function(yrs.range, yrs.target)
# {
#   # yrs.range should be a character vector with entries of the form 'year-year'
#   # yrs.target is a integer vector of target years
#   #
#   # yrs.range is converted to integer and checked against yrs.target. The function
#   # returns a boolean vector (same length as yrs.range) indicating for each entry
#   # if there is any overlap with target years 
#   
#   # initialize output vector
#   idx.out = rep(FALSE, length(yrs.range))
#   idx.na = is.na(yrs.range)
#   
#   # parse the years defining each (non-NA) range as integers
#   yr.start = sapply(strsplit(yrs.range[!idx.na], '-'), function(yr.range) as.integer(yr.range[1]))
#   yr.end = sapply(strsplit(yrs.range[!idx.na], '-'), function(yr.range) as.integer(yr.range[1]))
#   
#   # identify the keepers and update boolean output values, finish 
#   idx.keep = sapply(1:sum(!idx.na), function(idx) any(yr.start[idx]:yr.end[idx] %in% yrs.target))
#   idx.out[!idx.na] = idx.keep
#   return(idx.out)
# }
# 
# # find all relevant station entries 
# idx.keep.byvar = lapply(var.target, function(varname) my_parsedate(ghcnd.sf %>% pull(varname), yrs.target))
# idx.keep = Reduce('|', idx.keep.byvar)
# 
# # build the station metadata SWAT+ table
# id.keep = ghcnd.sf$id[idx.keep]



#+ include=FALSE
# Development code

# The directory structure defined above will write the SWAT+ data files to their expected paths
# [as outlined here](https://celray.github.io/docs/swatplus_aw/data.html). 


# The R package `SWATplusR` is still in development, so it should be installed using `devtools::install_github`
# library(devtools)
# devtools::install_github("chrisschuerz/SWATplusR")
#library(SWATplusR)
#my_markdown('make_qswatplus')
