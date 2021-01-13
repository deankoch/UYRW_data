# Generate a SWAT+ project from reference database and geometry inputs

# Based on: prepare_project.py from the SWAT+ AW v.1.0.4 (retrieved Sept 9,
# 2020) references https://celray.github.io/docs/swatplus_aw/introduction.html
# and https://docs.qgis.org/3.4/pdf/en/QGIS-3.4-PyQGISDeveloperCookbook-en.pdf

# This modification is meant to simplify the SWAT+AW codebase by customizing it
# for use with the data products prepared in the UYRW_data project. The main
# differences in our implementation: 
# 
# 1) QSWAT+ methods are imported from the user's QGIS installation directory
# instead of the SWAT+AW bundle. This gives us more control over the version of
# QSWAT+ used to build the model, and the model configuration options (eg it
# should be easy to update this code to keep up with new releases of SWAT+,
# whereas if SWAT+AW is discontinued we may be stuck with old versions).
#
# 2) Source file paths are discovered from the *_metadata.csv file created in
# our R workflow. This avoids unnecessary copies of potentially large data
# files, and directly links the output of the UYRW_data project to the SWAT+
# workflow. In future, we would call this python script from within R (or 
# vice-versa; calling the R scripts from python)
#
# 3) Tabular soil and landuse parameters are copied directly from the SWAT+
# reference database into the project database, instead of being imported
# via CSV intermediaries. This avoids some bugs related to file structure
# changes in the switch from SWAT2012 and SWAT+, and makes it easier for us to
# keep up with any future updates to the source database SQLite files.  


'''----------------- python packages --------------'''

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


'''----------- PyQGIS and SWATEditor dependencies  ----------'''

# progress message
print('\n     >> loading PyQGIS and QSWAT3 modules')

