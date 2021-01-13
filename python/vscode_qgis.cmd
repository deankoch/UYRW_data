rem start python interpreter in VS Code with QGIS dependencies
rem reference: https://docs.qgis.org/2.14/en/docs/pyqgis_developer_cookbook/ide_debugging.html
rem make sure interpreter is set to apps\Python37\python.exe in VS Code

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

rem omit the "VisualStudioCode for QGIS" /B to run in console instead of editor
start "VisualStudioCode for QGIS" /B "C:\Users\deank\AppData\Local\Programs\Microsoft VS Code\Code.exe"