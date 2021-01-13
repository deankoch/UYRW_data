rem executes run_qswatplus.py in console 
rem reference: https://docs.qgis.org/2.14/en/docs/pyqgis_developer_cookbook/ide_debugging.html

rem OSGEO4W_ROOT should be set to installation directory of OSGEO4W LTR release of QGIS
@echo off
set OSGEO4W_ROOT=C:\Program Files\QGIS 3.10
call "%OSGEO4W_ROOT%\bin\o4w_env.bat"

rem this part is unchanged from python-qgis-ltr.bat
call qt5_env.bat
call py3_env.bat
@echo off
path %OSGEO4W_ROOT%\apps\qgis-ltr\bin;%PATH%
set QGIS_PREFIX_PATH=%OSGEO4W_ROOT:\=/%/apps/qgis-ltr
set GDAL_FILENAME_IS_UTF8=YES
rem Set VSI cache to be used as buffer, see #6448
set VSI_CACHE=TRUE
set VSI_CACHE_SIZE=1000000
set QT_PLUGIN_PATH=%OSGEO4W_ROOT%\apps\qgis-ltr\qtplugins;%OSGEO4W_ROOT%\apps\qt5\plugins
set PYTHONPATH=%OSGEO4W_ROOT%\apps\qgis-ltr\python;%PYTHONPATH%

rem run the script
set MY_SCRIPT_PATH=H:\UYRW_data\python
cd /D C:\Program Files\QGIS 3.10\apps\Python37
call python.exe %MY_SCRIPT_PATH%\run_qswatplus.py