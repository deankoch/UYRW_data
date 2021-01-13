# reference https://docs.qgis.org/testing/en/docs/pyqgis_developer_cookbook/intro.html
# (using-pyqgis-in-standalone-scripts)

import os
from qgis.core import *

# path to qgis install location
QgsApplication.setPrefixPath(os.getenv('OSGEO4W_ROOT'), True)

# reference to the QgsApplication (second argument disables GUI)
qgs = QgsApplication([], False)

# load providers
qgs.initQgis()

# Write your code here to load some layers, use processing
# algorithms, etc.

# Finally, exitQgis() is called to remove the
# provider and layer registries from memory
qgs.exitQgis()