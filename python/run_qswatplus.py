# Generate a SWAT+ project from reference database and geometry inputs.
# Based on: SWAT+AW v.1.0.4 (retrieved 09/09/2020).
# Reference: https://celray.github.io/docs/swatplus_aw/introduction.html

# python packages
import os
import sys
import warnings
import re  # regular expressions
import gdal  # spatial libraries
import pandas  # data frames
from pathlib import Path  # Windows path handler

'''----------------- QSWAT+ dependencies  --------------'''

# PyQGIS modules (to set up search path, use vscode_qgis.cmd launcher)
from qgis.core import QgsApplication, QgsProject
from PyQt5.QtCore import QSettings, QFileInfo
from qgis.gui import QgsMapCanvas

# initialize the QGIS environment object (second arg suppresses GUI)
qgs = QgsApplication([], False)
qgs.initQgis()

# add QGIS processing module to system path, import and initialize
OSGEO4W_path = Path(os.environ['OSGEO4W_ROOT'])
QGIS_plugins_path = OSGEO4W_path / 'apps/qgis-ltr/python/plugins'
sys.path.append(str(QGIS_plugins_path))
from processing.core.Processing import Processing
Processing.initialize()

# While it is bad python style to not have all module imports at top of file,
# I have been unable to get the 'Processing' module loaded properly without
# first doing initQgis(). I am also unable to set the Windows Path from my
# launcher, so we need the sys.path calls to make the plugins directories
# visible.

# QSWATPlus3 modules should be located in the user's appdata dir
appdata_path = Path(os.environ['APPDATA'])
QSWAT_user_relpath = 'QGIS/QGIS3/profiles/default/python/plugins'
QSWAT_user_path = appdata_path / QSWAT_user_relpath
sys.path.append(str(QSWAT_user_path))

# hide deprecation warnings from current version of QSWATPlus
with warnings.catch_warnings():
    warnings.simplefilter('ignore')
    from QSWATPlus3_64.QSWATPlus import QSWATPlus

# load a few other required QSWATPlus modules
from QSWATPlus3_64.QSWATUtils import QSWATUtils, FileTypes
from QSWATPlus3_64.delineation import Delineation
from QSWATPlus3_64.hrus import HRUs
from QSWATPlus3_64.landscape import Landscape

# progress message
print('\n     >> PyQGIS and QSWATPlus3_64 loaded')

'''---------- project configuration and directories  --------------'''

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
sources_csv = pandas.read_csv(sources_csv_path, index_col=0)

# destination path for project files created in this script
project_name = 'swatplus_aw'
project_path = config_path / project_name

# create the output subdirectory
print('\n     >> writing to project directory: ' + str(project_path))
project_path.mkdir(exist_ok=True)


'''----------------- create project file  --------------'''

# disable parallel processing (MS MPI) for simplicity
QSettings().setValue('/QSWATPlus/NumProcesses', 0)

# initialize the QSWAT3 plugin using QGIS environment object created earlier
plugin = QSWATPlus(qgs)

# define and initialize the QGIS project file in the new project folder
proj_file = str(project_path / project_name) + '.qgs'
proj = QgsProject.instance()
proj.setFileName(proj_file)
proj.write()

# set up a new QSWAT3 project, setting parameters and creating directories
plugin.setupProject(proj, True)


'''------------ delineation pt1: prepare DEM file  --------------'''

# initialize watershed delineation object
print('\n       >> starting delineation ')
delin = Delineation(plugin._gv, False)
delin.init()

# define source and destination paths, add latter to global vars
dem_src_file = UYRW_path / sources_csv.loc['swat_dem_tif', 'file']
dem_dest_file = Path(delin._gv.demDir) / dem_src_file.name
delin._gv.demFile = str(dem_dest_file)
delin._dlg.selectDem.setText(str(dem_dest_file))

# use GDAL CreateCopy to copy without losing GeoTiff file format
print('\n       >> copying DEM ' + dem_dest_file.name)
tif_driver = gdal.GetDriverByName('GTiff')
dem = gdal.Open(str(dem_src_file), gdal.GA_ReadOnly)
dem = tif_driver.CreateCopy(str(dem_dest_file), dem, 0, ['COMPRESS=NONE'])
del dem

# create map layer from DEM to add to the project layer tree
root = QgsProject.instance().layerTreeRoot()
lyrs = root.findLayers()
src = str(dem_src_file)
ft = FileTypes._DEM
gn = QSWATUtils._WATERSHED_GROUP_NAME
dem_lyr = QSWATUtils.getLayerByFilename(lyrs, src, ft, delin._gv, None, gn)[0]

# write a .prj file containing CRS info and create/write hillshade raster
print('\n       >> creating hillshade raster')
QSWATUtils.writePrj(str(dem_dest_file), dem_lyr)
delin.setDefaultNumCells(dem_lyr)
delin.addHillshade(str(dem_dest_file), root, dem_lyr, delin._gv)


'''--------- delineation pt2: import streams and outlets  --------------'''

# copy stream network shapefile, a "burn-in" shapefile to modify the DEM
print('\n       >> importing streams shapefile')
streams_src_file = UYRW_path / sources_csv.loc['swat_streams', 'file']
streams_dest_file = Path(delin._gv.shapesDir) / streams_src_file.name
QSWATUtils.copyFiles(QFileInfo(str(streams_src_file)), delin._gv.shapesDir)
fn = str(streams_dest_file)
ft = FileTypes._BURN

# add this burn-in shapefile to project layer tree
lyrs = root.findLayers()
burn_lyr = QSWATUtils.getLayerByFilename(lyrs, fn, ft, delin._gv, None, gn)[0]

# add burn-in copy path to global vars, toggle burn-in flag
delin._gv.burnFile = fn
delin._dlg.selectBurn.setText(fn)
delin._dlg.checkBurn.setChecked(True)

# copy inlets/outlets shapefile
print('\n       >> importing inlets/outlets shapefile')
outlets_src_file = UYRW_path / sources_csv.loc['swat_outlets', 'file']
outlets_dest_file = Path(delin._gv.shapesDir) / outlets_src_file.name
QSWATUtils.copyFiles(QFileInfo(str(outlets_src_file)), delin._gv.shapesDir)
fn = str(outlets_dest_file)
ft = FileTypes._OUTLETS

# add outlets shapefile to project layer tree
lyrs = root.findLayers()
pts_lyr = QSWATUtils.getLayerByFilename(lyrs, fn, ft, delin._gv, None, gn)[0]

# add outlets copy path to global vars, toggle "use outlets" flag
delin._gv.outletFile = fn
delin._dlg.selectOutlets.setText(fn)
delin._dlg.useOutlets.setChecked(True)

'''------ delineation pt3: run TauDEM and landscape analysis -------------'''

# monkey-patch the iface method to avoid errors on setActiveLayer() calls
delin._gv.iface.setActiveLayer = lambda *args: True

# run TauDEM to define stream reach network and create subbasins shapefile
print('\n       >> running TauDEM')
delin.runTauDEM2()

# prepare landscape object for flood plain delineation
delin.makeDistancesToOutlets()
nproc = delin._dlg.numProcesses.value()
delin.L = Landscape(delin._gv, delin._dlg.taudemOutput, nproc, delin.progress)

# set threshold to define flood plain (use default value for now)
numCellsSt = int(delin._dlg.numCellsSt.text())

# estimate flood plain (DEM inversion method) - emulates Landscape.run()
print('\n       >> inverting DEM to compute floodplain')
delin.L._dlg.ridgeThresholdCells.setText(str(numCellsSt))
delin.L.clipperFile = delin._gv.subbasinsFile
delin.L._dlg.methodTab.setCurrentIndex(1)
delin.L.changeRidgeArea()
delin.L.methodTabCheck()
delin.L.mustRun = delin.thresholdChanged
delin.L.generate()
# A lakes shapefile could be added here via Delineation.addLakesMap()

# finish up: check/write watershed attributes
print('\n       >> finishing delineation')
delin.finishDelineation()

# update workflow flags, update project config table, tidy up
delin.finishHasRun = True
delin.readProj()
delin.delineationFinishedOK = True
delin._gv.writeProjectConfig(1, 0)
plugin._demIsProcessed = True
plugin.allowCreateHRU()
plugin._gv.db.clearTable('BASINSDATA')


'''------------ HRUs pt 1: assign input layers  --------------'''

# create HRUs object
hrus = HRUs(plugin._gv, plugin._odlg.reportsBox)

# toggle option to use SSURGO/STATSGO2
hrus._dlg.SSURGOButton.setChecked(True)
hrus.init()

# toggle options to use floodplain
hrus._gv.useLandscapes = True
hrus.initFloodplain()

# assign path to land-use lookup CSV (defined in metadata CSV from R workflow)
land_lu_src_file = UYRW_path / sources_csv.loc['swat_landuse_lookup', 'file']

# import land-use lookup table and add to database
names = hrus._db.landuseTableNames
land_lu_table = hrus._db.readCsvFile(str(land_lu_src_file), 'landuse', names)
hrus._dlg.selectLanduseTable.insertItem(0, land_lu_table)
hrus._dlg.selectLanduseTable.setCurrentIndex(0)
hrus.landuseTable = land_lu_table
hrus.setToReadFromMaps()

# define source and destination paths for land-use raster
l_src_file = UYRW_path / sources_csv.loc['swat_landuse_tif', 'file']
l_dest_file = Path(hrus._gv.landuseDir) / l_src_file.name

# use GDAL CreateCopy to copy without losing GeoTiff file format
print('\n       >> copying land use raster ' + l_dest_file.name)
l_ds = gdal.Open(str(l_src_file), gdal.GA_ReadOnly)
l_ds = tif_driver.CreateCopy(str(l_dest_file), l_ds, 0, ['COMPRESS=NONE'])
del l_ds

# assign path to land use raster, add to project layer tree, set global vars
fn = str(l_dest_file)
lyrs = QgsProject.instance().layerTreeRoot().findLayers()
ft = FileTypes._LANDUSES
gn = QSWATUtils._LANDUSE_GROUP_NAME
lyr = QSWATUtils.getLayerByFilename(lyrs, fn, ft, hrus._gv, None, gn, True)[0]
hrus.landuseFile = fn
hrus.landuseLayer = lyr

# define source and destination paths for soils raster
s_src_file = UYRW_path / sources_csv.loc['swat_soils_tif', 'file']
s_dest_file = Path(hrus._gv.soilDir) / s_src_file.name

# use GDAL CreateCopy to copy without losing GeoTiff file format
print('\n       >> copying soils raster ' + s_dest_file.name)
s_ds = gdal.Open(str(s_src_file), gdal.GA_ReadOnly)
s_ds = tif_driver.CreateCopy(str(s_dest_file), s_ds, 0, ['COMPRESS=NONE'])
del s_ds

# assign path to soils raster, add to project layer tree, set global vars
fn = str(s_dest_file)
ft = FileTypes._SOILS
gn = QSWATUtils._SOIL_GROUP_NAME
lyr = QSWATUtils.getLayerByFilename(lyrs, fn, ft, hrus._gv, None, gn, True)[0]
hrus.soilFile = fn
hrus.soilLayer = lyr
hrus.soilTable = 'ssurgo'


'''----- HRUs pt 2: import layers and calculate HRUs -------'''

# monkey-patch the iface method to avoid errors on pushMessage() calls:
# this requires defining a class that does nothing (returns None)...


class dummyClass:
    def __init__(self, dummyFun=lambda msg, level, duration: None):
        self.pushMessage = dummyFun


hrus._iface.messageBar = dummyClass


# import and process land-use and soils rasters
print('\n       >> reading soil and land-use data ')
hrus.readFiles()

# compute HRU geometry and save project
print('\n       >> calculating HRUs ')
hrus.calcHRUs()
proj.write()

'''----- tidy up and finish -------'''

# reload project and its layer tree, making list of all layer IDs
proj.read(proj_file)
root = proj.layerTreeRoot()
lyrs = root.findLayers()
lyr_ids = [lyr.layerId() for lyr in lyrs]

# identify indices of all raster layers
lu_idx = [re.match('^Landuses', Id) is None for Id in lyr_ids].index(False)
soil_idx = [re.match('^Soils', Id) is None for Id in lyr_ids].index(False)
fl_idx = [re.match('^Floodplain', Id) is None for Id in lyr_ids].index(False)
hs_idx = [re.match('^Hillshade', Id) is None for Id in lyr_ids].index(False)
dem_idx = [re.match('^DEM', Id) is None for Id in lyr_ids].index(False)

# (QGIS GUI) reset zoom to the extent of the DEM
dem_lyr = lyrs[dem_idx].layer()
canvas = QgsMapCanvas()
canvas.setExtent(dem_lyr.extent())

# set a custom layer order so the rasters don't hide everything else
idx_pop = [fl_idx, hs_idx, dem_idx, soil_idx, lu_idx]
lyr_ids_pop = [lyr_ids[idx] for idx in idx_pop]
lyr_ids_new = [lyr for lyr in lyr_ids if lyr not in lyr_ids_pop] + lyr_ids_pop
root.setCustomLayerOrderByIds(lyr_ids_new)
root.setHasCustomLayerOrder(True)

# save to project file and quit
proj.write()
qgs.exitQgis()
sys.exit(1)
