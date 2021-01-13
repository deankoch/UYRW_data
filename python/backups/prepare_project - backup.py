# Generate a SWAT+ project directory with reference database and geometry inputs

# Based on: prepare_project.py from the SWAT+ AW v.1.0.4 (retrieved Sept 9,
# 2020) references https://celray.github.io/docs/swatplus_aw/introduction.html
# and https://docs.qgis.org/3.4/pdf/en/QGIS-3.4-PyQGISDeveloperCookbook-en.pdf

# This modification is meant to simplify the SWAT+AW codebase by customizing it
# for use with the data products prepared in the UYRW_data project. The main
# differences in our implementation: 
# 
# 1) QSWAT+ methods are imported from the user's QGIS installation directory
# instead of the SWAT+AW bundle. This gives us more control over the version of
# QSWAT+ used to build the model, and the model configuration options.
#
# 2) Source file paths are discovered from the *_metadata.csv file created in
# our R workflow. This avoids unnecessary copies of potentially large data
# files.
#
# 3) Tabular soil and landuse parameters are copied directly from the SWAT+
# reference database into the project SQLite database, instead of being imported
# via CSV intermediaries. This avoids some bugs related to file structure
# changes in the switch from SWAT2012 and SWAT+, and makes it easier for us to
# keep up with any updates to these source databases.  

'''----------------- Python Packages --------------'''

import os
import sys
from shutil import copyfile

# regular expressions
import re

# spatial libraries
import gdal
import osr

# better handling of Windows paths in python3
from pathlib import Path

# pandas data frames with alias pd
import pandas as pd

'''----------------- PyQGIS and QSWAT dependencies  --------------'''

# progress message
print('\n     >> loading PyQGIS and QSWAT3 modules')

# import PyQGIS modules (to do: replace wild imports with specifics)
from qgis.core import *
from qgis.utils import iface
from PyQt5.QtGui import *
from PyQt5.QtCore import *

# initialize the QGIS environment object (second arg suppresses GUI)
qgs = QgsApplication([], False)
qgs.initQgis()
#qgs.exitQgis() <- put this at the end of the script to tidy up

# add QGIS processing module to system path, import and initialize
OSGEO4W_path = Path(os.environ['OSGEO4W_ROOT'])
QGIS_plugins_path = OSGEO4W_path / 'apps/qgis-ltr/python/plugins'
sys.path.append(str(QGIS_plugins_path))
from processing.core.Processing import Processing
Processing.initialize()
import processing

# import QSWATPlus3 modules (located in the user's appdata dir)
appdata_path = Path(os.environ['APPDATA'])
QSWAT_user_relpath = 'QGIS/QGIS3/profiles/default/python/plugins'
QSWAT_user_path = appdata_path / QSWAT_user_relpath
sys.path.append(str(QSWAT_user_path))
from QSWATPlus3_64 import QSWATPlus
from QSWATPlus3_64 import QSWATUtils
from QSWATPlus3_64 import parameters
from QSWATPlus3_64 import delineation
from QSWATPlus3_64 import hrus

# SWAT+ AW helper functions (I may later replace these with my own)
SWATPLUS_AW_PATH = 'C:/SWAT/SWATPlus/Workflow/packages'
sys.path.insert(0, SWATPLUS_AW_PATH)
from helper_functions import read_from, write_to, get_extents, raster_statistics
from projection_lookup import epsg_lookup_dictionary
from qgs_project_template import template

'''----------------- Project configuration data  --------------'''

# define the master project directory (location of all inputs/outputs)
UYRW_path = Path('H:/UYRW_data')

# define the config file path (to do: make this a command line argument)
config_path = UYRW_path / 'data/prepared/qswatplus'

# import the config parameters
sys.path.append(str(config_path))
import config
# will likely to switch to a YAML or JSON settings file at some point

# open the R metadata file to determine filenames
sources_csv_path = config_path / config.R_metadata
sources_csv = pd.read_csv(sources_csv_path, index_col=0)

# copy outlets shapefile and landuse lookup csv paths from metadata CSV
outlet_file = os.path.basename(sources_csv.loc['swat_outlets', 'file'])
land_lookup_file = os.path.basename(sources_csv.loc['swat_landuse_lookup', 'file'])

# copy source raster paths from metadata CSV
landuse_src_file = UYRW_path / sources_csv.loc['swat_landuse_tif', 'file']
soil_src_file = UYRW_path / sources_csv.loc['swat_soils_tif', 'file']

# destination path for project files created in this script
project_name = 'swatplus_aw'
project_path = config_path / project_name

'''----------------- Directories and raster files  --------------'''

# print progress message before creating the output subdirectory
print('\n     >> writing to project directory: ' + str(project_path))
project_path.mkdir(exist_ok=True)

# create the DEM subdirectory and copy the GeoTIFF
dem_dest_reldir = 'Watershed/Rasters/DEM'
dem_dest_file = project_path / dem_dest_reldir / dem_src_file.name
# print('\n       >> copying DEM ' \
#     +  dem_src_file.name + ' to ' + dem_dest_reldir)
# dem_dest_file.parent.mkdir(parents=True, exist_ok=True)
# copyfile(dem_src_file, dem_dest_file)

# create the Landuse subdirectory and copy the GeoTIFF
landuse_dest_dir = 'Watershed/Rasters/Landuse'
landuse_dest_file = project_path / landuse_dest_dir / landuse_src_file.name
# print('\n       >> copying land use raster ' \
#     +  landuse_src_file.name + ' to ' + landuse_dest_dir)
# landuse_dest_file.parent.mkdir(parents=True, exist_ok=True)
# copyfile(landuse_src_file, landuse_dest_file)

# create the soil subdirectory and copy the GeoTIFF
soil_dest_reldir = 'Watershed/Rasters/Soil'
soil_dest_file = project_path / soil_dest_reldir / soil_src_file.name
# print('\n       >> copying soil ' \
#     +  soil_src_file.name + ' to ' + soil_dest_reldir)
# soil_dest_file.parent.mkdir(parents=True, exist_ok=True)
# copyfile(soil_src_file, soil_dest_file)

# create hillshade raster from DEM
# dem_data = gdal.Open(str(dem_dest_file))
# hs_dest_name = dem_dest_file.name.split('.')[0] + '_hillshade.tif'
# hs_dest_file = project_path / dem_dest_reldir / hillshade_dest_name
# hs = gdal.DEMProcessing(str(hs_dest_file), dem_data, 'hillshade', zFactor=30)

# extract DEM projection info and reshape into format needed for qgs file
# dem_projcs = gdal.Info(dem_data).split('Data axis')[0].split('System is:\n')[-1]
# prjcrs = ''
# for line in dem_projcs.split('\n'):
#     while line.startswith(' '):
#         line = line[1:]
#     prjcrs += line.strip('\n')

# formatted projection info
# srs = osr.SpatialReference(wkt=prjcrs)
# srs_string = prjcrs.split('"')[1]
# proj4 = srs.ExportToProj4()
# geogcs = srs.GetAttrValue('geogcs')
# srid, prj_acronym, srsid, ellps_acronym = epsg_lookup_dictionary[srs_string]

# write this projection info to text files accompanying the GeoTIFFs
# dem_dest_basepath = os.path.splitext(dem_dest_file)[0] 
# write_to(dem_dest_basepath + '.prj.txt', dem_projcs) # <- to do: fix write_to
# write_to(dem_dest_basepath + '.prj', prjcrs)
# write_to(dem_dest_basepath + '_hillshade.prj', prjcrs)

# get raster extent values and some statistics on the elevations
# dem_xmin, dem_ymin, dem_xmax, dem_ymax = get_extents(str(dem_dest_file))
# dem_stats = raster_statistics(str(dem_dest_file))
# dem_third_delta = round((dem_stats.maximum - dem_stats.minimum)/3, 0)
# dem_lower_third = dem_stats.minimum + dem_third_delta
# dem_upper_third = dem_stats.maximum - dem_third_delta
# dem_mid_thirds = round((dem_upper_third + dem_lower_third)/2, 0)

'''----------------- HRU settings  --------------'''

# HRU creation flags
hru_land_thres, hru_soil_thres, hru_slope_thres = "", "", ""
area_val = config.Target_Area     
target_val = config.Target_Value
is_area = 1
use_area = 1
is_dominant_hru = 0
is_multiple = 1
is_target = 0

# to do: fill this out with other creation options
# for now this always uses dominant HRU

'''----------------- create project file  --------------'''

# initialize the QSWAT3 plugin using the QGIS environment object created earlier
plugin = QSWATPlus.QSWATPlus(qgs)

# define and initialize the project file
proj_file = str(project_path / project_name) + '.qgs'
proj = QgsProject.instance()
proj.setFileName(proj_file)
proj.write()

# set up a new project, assigning some default parameters
plugin.setupProject(proj, True)

'''----------------- prepare DEM file  --------------'''

# initialize watershed delineation object
delin = delineation.Delineation(plugin._gv, False)
delin.init()

# define source and destination paths 
dem_src_file = UYRW_path / sources_csv.loc['swat_dem_tif', 'file']
dem_dest_file = Path(delin._gv.demDir) / dem_src_file.name

# set the DEM path variables (normally assigned via GUI) 
delin._gv.demFile = str(dem_dest_file)
delin._dlg.selectDem.setText(str(dem_dest_file))

# use GDAL CreateCopy to copy without losing GeoTiff file format
inDs = gdal.Open(str(dem_src_file), gdal.GA_ReadOnly)
tif_driver = gdal.GetDriverByName('GTiff')
outDs = tif_driver.CreateCopy(str(dem_dest_file), inDs, 0, ['COMPRESS=NONE'])
del inDs
del outDs

# create map layer from DEM to add to the project layer tree
root = QgsProject.instance().layerTreeRoot()
lyrs = root.findLayers()
src = str(dem_src_file)
ft = QSWATUtils.FileTypes._DEM
gn = QSWATUtils.QSWATUtils._WATERSHED_GROUP_NAME
dem_layer = QSWATUtils.QSWATUtils.getLayerByFilename(lyrs, src, ft, delin._gv, None, gn)[0]

# write a .prj file containing CRS info, make hillshade
QSWATUtils.QSWATUtils.writePrj(str(dem_dest_file), dem_layer)
delin.setDefaultNumCells(dem_layer)
delin.addHillshade(str(dem_dest_file), root, dem_layer, delin._gv)


'''----------------- run TauDEM (delineation)  --------------'''

# define stream network shapefile to burn in to DEM
lyrs = root.findLayers()
streams_src_file = UYRW_path / sources_csv.loc['swat_streams', 'file']
src = str(streams_src_file)
ft = QSWATUtils.FileTypes._BURN
burn_layer = QSWATUtils.QSWATUtils.getLayerByFilename(lyrs, src, ft, delin._gv, None, gn)[0]
delin._gv.burnFile = src

# run TauDEM to define stream reach network
delin.runTauDEM1()

# add the DEM layer to the project layer tree
# dem_layer_name = os.path.splitext(dem_src_file.name)[0]
# dem_layer = QgsRasterLayer(str(dem_src_file), dem_layer_name)
# proj.addMapLayer(dem_layer)

# TO DO: GO BACK AND OMIT THE DEM COPY STEP
# THIS SEEMS TO DROP SOME PROJECTION INFO
# CODE BELOW LOADS THE SOURCE DEM FILE WITHOUT ERROR
# 
# add the DEM layer to the project layer tree
dem_layer = QgsRasterLayer(str(dem_src_file), 'test')
proj.addMapLayer(dem_layer)

proj_root = proj.layerTreeRoot()
ft = QSWATUtils.FileTypes._DEM
gn = QSWATUtils.QSWATUtils._WATERSHED_GROUP_NAME
QSWATUtils.QSWATUtils.openAndLoadFile(proj_root, ft, None, str(dem_src_file), None, None, gn)


self.delin = Delineation(self._gv, self._demIsProcessed)
        result = self.delin.run()


delin.setDimensions(dem_layer)




root = QgsProject.instance().layerTreeRoot()

root.findLayers()[0]
demLayer = root.findLayers()[0]



delin.setDefaultNumCells(demLayer)

QSWATUtils.QSWATUtils.openAndLoadFile(root.findLayers(), QSWATUtils.FileTypes._DEM, None,
   plugin._gv.demDir, plugin._gv, None, QSWATUtils.QSWATUtils._WATERSHED_GROUP_NAME)

dir(plugin)

delin._dlg.numProcesses()

# assign the DEM
root = QgsProject.instance().layerTreeRoot()
        (demFile, demMapLayer) = QSWATUtils.openAndLoadFile(root, FileTypes._DEM, self._dlg.selectDem,
                                                            self._gv.demDir, self._gv, None, QSWATUtils._WATERSHED_GROUP_NAME)

QSWATUtils.QSWATUtils.printLayers(root, 4)

# assign the DEM path
plugin._gv.demFile = str(dem_dest_file)



dir(delin)

layer = QSWATUtils.getLayerByFilename(root.findLayers(), outFileName, ft, 
                                                  gv, subLayer, groupName, clipToDEM)[0]

proj_root = QgsProject.instance().layerTreeRoot().findLayers()
ft = QSWATUtils.FileTypes._DEM
layer = QSWATUtils.QSWATUtils.getLayerByFilename(proj_root, str(dem_dest_file), ft, None, None, 'test')


plugin._gv.setDimensions()

proj_root = proj.layerTreeRoot()
ft = QSWATUtils.FileTypes._DEM
gn = QSWATUtils.QSWATUtils._WATERSHED_GROUP_NAME
QSWATUtils.QSWATUtils.openAndLoadFile(proj_root, ft, None, str(dem_dest_file), None, None, gn)
ft = QSWATUtils.FileTypes._DEM
QSWATUtils.getLayerByFilename

layer = QSWATUtils.getLayerByFilename(root.findLayers(), outFileName, ft, 
                                                  gv, subLayer, groupName, clipToDEM)[0]

self.setDefaultNumCells(demMapLayer)



(demFile, demMapLayer) = QSWATUtils.openAndLoadFile(root, FileTypes._DEM, self._dlg.selectDem,
                                                            self._gv.demDir, self._gv, None, QSWATUtils._WATERSHED_GROUP_NAME)




delin = delineation.Delineation(plugin._gv, False)
result = delin.run()



#
# CODE BELOW IS UNFINISHED
#
#
#
#

project_string = template.format(

    prjcrs=prjcrs,
    shape_wkt=prjcrs,
    proj4=proj4,
    geogcs=geogcs,
    project_name=project_name,

    snap_threshold=config.Out_Snap_Threshold,
    channel_threshold=config.Channel_Threshold,
    stream_threshold=config.Stream_Threshold,

    hru_land_thres=hru_land_thres,
    hru_slope_thres=hru_slope_thres,
    hru_soil_thres=hru_soil_thres,

    area_val=area_val,
    target_val=target_val,
    is_area=is_area,
    use_area=use_area,
    is_dominant_hru=is_dominant_hru,
    is_multiple=is_multiple,
    is_target=is_target,

    outlet_name=outlet_file.split('.')[0],

    dem_min=dem_stats.minimum,
    dem_max=dem_stats.maximum,
    lower_third=dem_lower_third,
    upper_third=dem_upper_third,
    mid_thirds=dem_mid_thirds,

    dem_name=dem_dest_file.name.split('.')[0],
    landuse_name=landuse_dest_file.name.split('.')[0],
    extension_suffix='.tif',
    soil_name=soil_dest_file.name.split('.')[0],
    dem_file_name=dem_dest_file.name,
    landuse_file_name=landuse_dest_file.name,
    soil_file_name=soil_dest_file.name,
    soil_lookup='SSURGO/STATSGO2',
    land_lookup=land_lookup_file,
    extent_xmin=dem_xmin,
    extent_ymin=dem_ymin,
    extent_xmax=dem_xmax,
    extent_ymax=dem_ymax,
    thresholdCh=config.Channel_Threshold,
    thresholdSt=config.Stream_Threshold,
    slope_classes=config.Slope_Classes,
    srsid=srsid,
    srid=srid,
    srs_description=srs_string,
    projectionacronym=prj_acronym,
    ellipsoidacronym=ellps_acronym,
    geographicflag='false',
)


