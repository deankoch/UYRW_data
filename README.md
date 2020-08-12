# URYW_data
links and R scripts for downloading data relevant to modelling streamflow on the Upper Yellowstone River Watershed

![flowlines of the Upper Yellowstone and tributaries](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/UYRW_flowlines.png)

## about us

We are a team of mathematicians, statisticians, and ecologists, conducting a multi-year research project to develop an operational forecasting system for streamflow and water quality on the [Upper Yellowstone River](http://fwp.mt.gov/mtoutdoors/images/Storyimages/2017/UpperYellowstoneMap.jpg) (UYR) and its tributaries. Our system will be based on [SWAT-MODFLOW](https://www.sciencedirect.com/science/article/abs/pii/S136481521930893X?via%3Dihub), a hybrid of the [SWAT+](https://swatplus.gitbook.io/docs/) (Soil-Water-Assessment Tool) model for surface water dynamics and [MODFLOW](https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs?qt-science_center_objects=0#qt-science_center_objects) (Modular Finite-Difference Flow) for groundwater dynamics. 

## R code

The URYW_data repository is a staging area for R code that can be used to fetch data on the hydrology of UYR. This repository will be active during the early stages of our project (August-November 2020), as we assemble datasets and build documentation for the model.

Our scripts (\*.R) are documented in dynamic reports -- markdown files of the form \*.knit.md. These document the R code and data preparation process in human-readable detail. They are compiled automatically by `rmarkdown` using [a roxygen2-style markup in comments](https://rmarkdown.rstudio.com/articles_report_from_r_script.html):

* [NHDPlus.R](https://github.com/deankoch/URYW_data/blob/master/NHDPlus.knit.md) defines the study area and loads some hydrology info
* ... more to follow

## funding

Our work is funded through a [MITACS](https://www.mitacs.ca/en/about) [Accelerate International](https://www.mitacs.ca/en/programs/accelerate/mitacs-accelerate-international) grant to Dean Koch, partnering the University of Alberta with R2CS LLC in Montana, and the [Yellowstone Ecological Research Center](https://www.yellowstoneresearch.org/yerc-lab). The project began on August 3, 2020.