rswat
================
Dean Koch
2021-05-28

**Mitacs UYRW project**

**rswat**: R functions for generating, reading, and editing SWAT+ files

## libraries

Required libraries are now specified in the categorized helper scripts
sourced below, with the exception of the `here` package, which is used
to locate those files.

[`here`](https://cran.r-project.org/web/packages/here/index.html)
simplifies paths involving a project working directory

``` r
library(here)
```

## helper functions

`rswat` is a set of helper functions for managing
[SWAT+](https://swat.tamu.edu/software/plus/) configuration files, and
related tasks. The codebase has gotten very large so it is split into
several files, each containing a number of helper function definitions,
organized by task:

  - [`rswat_qgis`](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_qgis.md)
    runs QSWAT+ and SWAT+Editor to generate the SWAT+ project folder,
    and loads the QSWAT+ shapefiles

  - [`rswat_config`](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_config.md)
    manages model configuration files in the SWAT+ project folder

  - [`rswat_output`](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_output.md)
    assists with model execution and reading outputs. This depends on
    `rswat_config`

  - [`rswat_docs`](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md)
    parses SWAT+ inputs documentation for easier search/access (from
    R\!). This script doesn’t depend on the others,

We wrote this code to better learn/understand the model, get finer
control over its settings, and to develop our own customized extensions
in our implementation of SWAT+ on the Upper Yellowstone watershed. If
there is some interest from the SWAT+ community we will likely turn all
of this into an R package at some point.

``` r
# TODO: make this a package
# run the four helper scripts to define functions
source(here('R/rswat_qgis.R'))
source(here('R/rswat_config.R'))
source(here('R/rswat_output.R'))
source(here('R/rswat_docs.R'))
```
