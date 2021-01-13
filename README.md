## about us

We are a team of mathematicians, statisticians, and ecologists, conducting a multi-year research project to develop an operational forecasting system for streamflow and water quality on the [Upper Yellowstone River](http://fwp.mt.gov/mtoutdoors/images/Storyimages/2017/UpperYellowstoneMap.jpg) (UYR) and its tributaries. Our system will be based on [SWAT+/gflow](https://www.mdpi.com/2306-5338/7/4/75), a hybrid of the [SWAT+](https://swat.tamu.edu/software/plus/) (Soil-Water-Assessment Tool) model for surface water dynamics, and gflow, physically-based groundwater flow module developed by Ryan Bailey to replace the current SWAT+ aquifer module. 

## R code

The UYRW_data repository is a staging area for R code that can be used to fetch data on the hydrology of UYR. This repository will be active during the early stages of our project (August-November 2020), as we assemble datasets and build documentation for the model. The following scripts download the data:

* [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
defines the study area and loads some hydrology info using
[`nhdplusTools`](https://usgs-r.github.io/nhdplusTools/)
* [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md)
finds climatic sensor station data using
[`snotelr`](https://github.com/bluegreen-labs/snotelr) and
[`rnoaa`](https://github.com/ropensci/rnoaa)
* [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
fetches the National Elevation Dataset from USGS using
[`FedData`](https://cran.r-project.org/web/packages/FedData/index.html)
* [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md)
fetches sensor data from the USGS NWIS using
[`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html)
* [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md)
fetches SSURGO/STATSGO2 data from the Soil Data Mart using
[`FedData`](https://cran.r-project.org/web/packages/FedData/index.html)
* [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md)
fetches GAP/LANDFIRE data from the [USGS ScienceBase catalogue](https://www.sciencebase.gov/catalog/)
* [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)
fetches three different gridded meteorological reconstructions of Northwestern North America

This script does some intermediate steps required to successfully load the data into QSWAT+3 and build a watershed
model:

* [make_qswatplus](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_qswatplus.md) prepares QSWAT+3 inputs

After running the above, the user can load the prepared raster and geometry data files into QSWAT+
(a plugin for QGIS), and construct a SWAT+ model from there. Alternatively, SWAT+ model construction
can be done automatically (and much more quickly) via PyQGIS scripts, such as those provided in the
[SWAT+ Automatic Workflow (AW)](https://celray.github.io/docs/swatplus_aw/introduction.html) by Celray
James. An example that is customized for the UYRW_data project is linked below:

* [run_qswatplus](https://github.com/deankoch/UYRW_data/tree/master/python)

We may open a new repository for this kind of python code, including parameter calibration scripts androutines for splitting large watersheds into smaller ones that can be calibrated sequentially. Check back for more scripts and figures as we add to this list in the coming weeks.

Our R data analysis workflow is structured around git and markdown. Our scripts (\*.R) are documented as dynamic reports -- markdown files of the form \*.knit.md. These document our code and methods in human-readable detail, with console output and figures incorporated automatically using [`rmarkdown` using roxygen2](https://rmarkdown.rstudio.com/articles_report_from_r_script.html). See Jennifer Bryan's
[Am Stat article](https://amstat.tandfonline.com/doi/abs/10.1080/00031305.2017.1399928) and [instructional pages](https://happygitwithr.com/) for more on this.

<!--- These scripts prepare the data structure needed to run the

python codebase from Celray James and Chris George. SWAT+ AW replaces the QSWAT+ GUI as the
main tool for setting up a SWAT+ watershed model, as part of an effort towards reproducible
catchment modelling science
(see also [this talk](https://scholarsarchive.byu.edu/iemssconference/2018/Stream-A/64/)). --->

## funding

Our work is funded through a [MITACS](https://www.mitacs.ca/en/about) [Accelerate International](https://www.mitacs.ca/en/programs/accelerate/mitacs-accelerate-international) grant to Dean Koch, partnering the University of Alberta with R2CS LLC in Montana, and the [Yellowstone Ecological Research Center](https://www.yellowstoneresearch.org/yerc-lab). The project began on August 3, 2020.

## gallery

R is a powerful data-retrieval, GIS, and visualization tool. These figures are generated by the scripts in our repo using the [`tmap`](https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html) package:

<img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_flowlines.png" width="45%"></img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_basins.png" width="45%"></img> <img 
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/weatherstation_sites.png" width="45%"></img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/streamgage_sites.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/landuse.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_landuse.png" width="45%"> <img
src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/dem.png" width="45%"> </img> <img src="https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/meteo_gridded.png" width="100%"></img>
