get\_nomads.R
================
Dean Koch
2021-03-05

**Mitacs UYRW project**

**get\_nomads**: download and import North American Mesoscale (NAM) and
other archived forecasts

## libraries

Start by sourcing two helper scripts (
[helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_get\_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))
```

[`rNOMADS`](https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf)
retrieves datasets from the NOAA archives.

``` r
library(rNOMADS)

# load CRS info list and watershed polygon from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))

# prepare a mask in geographical coordinates
uyrw.poly.geo = st_transform(uyrw.poly, crs=crs.list$epsg.geo)

# define 3km buffer around the perimeter of the study area
uyrw.buff = 3e3 # meters
```

## rNOMADS

``` r
# NOMADSArchiveList(gfsanl)
# x = CheckNOMADSArchive('namanl', '20200101')
# ArchiveGribGrab('namanl', '20050101', 0, 0)
# xx = raster('H:\\UYRW_data/20200101_0000_003.grb2', band=5)
# yy = crop(xx, as(st_transform(uyrw.poly, st_crs(xx)), 'Spatial'))
# plot(yy)
```
