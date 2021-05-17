
## r code

This repository hosts the R codebase for our data analysis and modelling workflows. Most of our scripts (\*.R)
include some [roxygen2-style commenting](https://rmarkdown.rstudio.com/articles_report_from_r_script.html) for
generating markdown report files (\*.knit.md), shared here
[to better document and organize](https://amstat.tandfonline.com/doi/abs/10.1080/00031305.2017.1399928)
our research.

### data

R/get_data contains R scripts to fetch public datasets on the hydrology of UYR:

* [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
defines the study area and loads some hydrology info from
[NHDPlus](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products)
* [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md)
finds climatic sensor station data at the
[GHCN](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html)
* [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
fetches the [USGS National Elevation Dataset](https://catalog.data.gov/dataset/national-elevation-dataset-ned-1-arc-second-downloadable-data-collection-national-geospatial-d)
* [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md)
finds historical daily values from [USGS Water Services](https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd)
* [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
fetches SSURGO/STATSGO2 maps from the [National Cooperative Soil Survey](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053627)
* [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md)
fetches data from [GAP/LANDFIRE](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-overview)
* [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)
fetches
[three](https://daymet.ornl.gov/)
[different](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets)
[gridded](https://ciresgroups.colorado.edu/livneh/data) meteorological reconstructions
* [get_snow](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_snow.md)
finds snow data at [SNOTEL and partner networks](https://wcc.sc.egov.usda.gov)

### SWAT

[The Soil and Water Assessment Tool (SWAT)](https://swat.tamu.edu/) is a widely-used river basin
watershed model with a very long development history dating back to the early 90s. Read more about
it [here](https://swat.tamu.edu/media/90102/azdezasp.pdf). These days SWAT has a
[large and active community of users](https://swat.tamu.edu/support/) running one of the two
public-domain Fortran-based implementations maintained by the USDA and Texas A&M:

* [SWAT2012](https://swat.tamu.edu/software/swat-executables/) is the legacy version. Advantages include: a larger
community, a very long development and testing history, more
[extensions and helper software available](https://swat.tamu.edu/software/), better documentation, and a
larger presence in the hydrology literature.

* [SWAT+](https://swat.tamu.edu/software/plus/) is the newer revised version. Advantages include more flexibility in
[spatial representation of watershed features](https://onlinelibrary.wiley.com/doi/abs/10.1111/1752-1688.12482), 
a modernized file structure. This seems the likely focus of future R&D by the SWAT development team.

Both versions continue to be updated and patched regularly (don't be fooled by the "2012"). However, although
the mathematical models behind the names are largely the same, their project files are not interchangeable as SWAT+ uses different variable names and file structures. Our project will focus on building extensions
of the newest official release of SWAT+.


### analysis

The R/analysis directory (in development) is for building, fitting and analysing SWAT+ models in R.

Using the output from /R/get_data, we plan to fit SWAT+ models sequentially on small catchments in the UYRW.
Check back in the coming weeks as we add to this section to demonstrate the model building code:

* [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md)
partitions the UYRW area into subwatersheds with USGS gages at their outlets
* [helper_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md)
utilities for creating and calibrating SWAT+ models in R



### rswat

The parametrization of a SWAT model is not trivial. A typical use case will involve dozens of config files
containing many thousands of model parameters. Fortunately, SWAT is largely made up of process-based components
whose physically-based parameters can (at least initially) be set using empirical data. 

For example after running [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md) to fetch our DEM, we can run the [TauDEM workflow](https://hydrology.usu.edu/taudem/taudem5/) to define subbasins and construct a routing network connecting them. After running [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
and [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md), we can then delineate
HRUs and assign their plant/soil parameters to survey values. These steps are usually carried out using
[QSWAT](https://swat.tamu.edu/software/qswat/)/[QSWAT+](https://swatplus.gitbook.io/docs/installation)
(GUI-based QGIS plugins). Our code runs them programmatically from within R:

[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md) is a set of tools for building SWAT+ models and managing their many files. This includes R wrappers for running the QSWAT+ workflow
[in PyQGIS](https://gitlab.com/rob-yerc/swat), calling SWATEditor.exe to build model config files; reading, cataloging and modifying these files, executing simulations, and reading the outputs. The following vignettes demonstrate these features on a simple example catchment

* [demo_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md) builds the SWAT+ model
* [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md) loads and modifies it
* [demo_objective](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_objective.md) tunes parameters
* [demo_rswat_docs](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_rswat_docs.md) shows how to search the I/O PDF from within R

We may polish some/all of this code and release it as an R package at some point. Those interested in script-based workflows for SWAT are encouraged to also check out the [SWAT+AW](https://github.com/celray/swatplus-automatic-workflow) and [SWATPlusR](https://github.com/chrisschuerz/SWATplusR) projects. 




## about us

We are a team of mathematicians, statisticians, and ecologists, conducting a multi-year research project to develop an operational forecasting system for streamflow and water quality on the [Upper Yellowstone River](http://fwp.mt.gov/mtoutdoors/images/Storyimages/2017/UpperYellowstoneMap.jpg) (UYR) and its tributaries. Our system will be based on [SWAT+/gflow](https://www.mdpi.com/2306-5338/7/4/75), a hybrid of the [SWAT+](https://swat.tamu.edu/software/plus/) (Soil-Water-Assessment Tool) model for surface water dynamics, and gflow, physically-based groundwater flow module developed by Ryan Bailey to replace the current SWAT+ aquifer module. 

### funding

Our work is funded through a [MITACS](https://www.mitacs.ca/en/about) [Accelerate International](https://www.mitacs.ca/en/programs/accelerate/mitacs-accelerate-international) grant to Dean Koch, partnering the University of Alberta with R2CS LLC in Montana, and the [Yellowstone Ecological Research Center](https://www.yellowstoneresearch.org/yerc-lab). The project began on August 3, 2020.

## gallery

R is a powerful data-retrieval, GIS, and visualization tool. These figures are generated by the scripts in our repo using the [`tmap`](https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html) package:

<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_flowlines.png" width="45%"></img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_basins.png" width="45%"></img> <img 
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/weatherstation_sites.png" width="45%"></img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/streamgage_sites.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/landuse.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_landuse.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/dem.png" width="45%"> </img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/meteo_gridded.png" width="100%"></img>
<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_catchments.png" width="100%"> </img>
<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_upslope_areas.png" width="100%"> </img>
<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png" width="100%"> </img>
<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png" width="100%"> </img>
