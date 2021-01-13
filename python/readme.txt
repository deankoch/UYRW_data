This python code uses the data files prepared by the UYRW_data R project
to build a SWAT+ project, including all QSWAT3+ files required to open and
view the project in QGIS3. It is a modification of the SWAT+ Automated
Worflow (SWAT+AW) project by Celray James, but with some adjustments to 
address bugs and simplify everything.

References:
https://celray.github.io/docs/swatplus_aw/introduction.html
https://docs.qgis.org/3.4/pdf/en/QGIS-3.4-PyQGISDeveloperCookbook-en.pdf
https://www.e-education.psu.edu/geog489/l1.html

These scripts are meant to simplify the SWAT+AW codebase by customizing it
for use with the data products prepared in the UYRW_data project. The main
differences in our implementation are:

1) QSWAT+ methods are imported from the user's QGIS installation directory
instead of the SWAT+AW bundle. This gives us more control over the version of
QSWAT+ used to build the model, and the model configuration options (eg it
should be easy to update this code to keep up with new releases of SWAT+).

2) Source file paths are discovered from the *_metadata.csv file created in
our R workflow. This avoids unnecessary copies of potentially large data
files, and directly links the output of the UYRW_data project to the SWAT+
workflow. In future, we would call this python script from within R (or
vice-versa; calling the R scripts from python)

3) Tabular soil and landuse parameters are copied directly from the SWAT+
reference database into the project database, instead of being imported
via CSV intermediaries. This avoids some bugs related to file structure
changes in the switch from SWAT2012 and SWAT+, and makes it easier for us to
keep up with any future updates to the source database SQLite files.