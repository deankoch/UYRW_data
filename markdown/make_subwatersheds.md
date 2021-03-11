make\_subwatersheds.R
================
Dean Koch
2021-03-10

**Mitacs UYRW project**

**make\_subwatersheds.R**: divide the UYRW into subwatersheds to be
fitted with separate SWAT+ models

This script runs the TauDEM workflow on the DEM for the UYRW to
determine the channel network and drainage layout for the entire
watershed. It then partitions this area into subwatersheds based on the
locations of long-term (1e3+ daily records) USGS streamflow gages, and a
minimum area threshold.

Our algorithm delineates subwatersheds along catchment boundaries such
that each one has gets an outlet at the site of a unique long-term USGS
gage record. In some cases a subwatershed located at the headwaters will
represent the full catchment for the gage (ie having no other long-term
gage records upstream); whereas the rest have inlets connecting them via
shared channels to other subwatersheds upstream.

In the no-inlet cases, SWAT+ models can be fitted independently using
the gage data at the outlet. To fit the other cases, we will provide
either simulation output from upstream models, or the observed flow data
from the gage that lies on the connecting channel. This
divide-and-conquer approach should allow us to:

1)  increase the model complexity (number of subbasins) on
    well-instrumented catchments, and vice versa
2)  take a more targeted approach to parameter-fitting, requiring fewer
    simulations
3)  fit SWAT+ to a large watershed sequentially, in managable stages

## libraries

Start by sourcing two helper scripts
([helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_analysis.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
```

load the rswat

``` r
source(here('R/rswat.R'))

# set the minimum number of daily gage records for each subwatershed outlet
obsmin = 3*365

# set the minimum subwatershed area in km^2
areamin = 5
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
subwatersheds.meta = my_metadata('get_subwatersheds', files.towrite, overwrite=TRUE)
```

    ## [1] "> writing metadata to: data/get_subwatersheds_metadata.csv"

``` r
print(subwatersheds.meta[, c('file', 'type')])
```

    ##                                                    file          type
    ## usgs_allcatchments data/prepared/usgs_allcatchments.rds R list object
    ## img_my_catchments            graphics/my_catchments.png   png graphic
    ## taudem                        data/prepared/taudem_uyrw     directory
    ## usgs_catchments       data/prepared/usgs_catchments.rds R list object
    ## metadata            data/get_subwatersheds_metadata.csv           CSV

``` r
# load the DEM and basins info
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
streamgages.meta = my_metadata('get_streamgages')

# load some watershed geometry data
uyrw.dem = raster(here(dem.meta['dem', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))
crs.list = readRDS(here(basins.meta['crs', 'file']))

# load the point representing the main outlet of the UYRW (Carter's Bridge)
uyrw.outlet.geo = readRDS(here(basins.meta['poi', 'file']))$pt$cartersbridge
uyrw.outlet = st_transform(uyrw.outlet.geo, crs=crs.list$epsg)
```

Start by running TauDEM on the full DEM, with only one outlet point (on
main stem). This will delineate the watershed upstream of that outlet,
automatically identifying the lowest (ie most detailed) feasible channel
delineation threshold by drop analysis.

Read about drop analysis
[here](https://hydrology.usu.edu/taudem/taudem5/help53/StreamDropAnalysis.html)

``` r
# run TauDEM on full watershed at max detail, or load files if this is already done
taudem.odir = subwatersheds.meta['taudem', 'file']
if(file.exists(taudem.odir))
{
  # load the list of file paths and descriptions
  taudem_uyrw = my_metadata('taudem', data.dir=subwatersheds.meta['taudem', 'file'])
  
} else {
  
  # run the TauDEM workflow (~10 mins)
  taudem_uyrw = my_taudem(uyrw.dem, taudem.odir, outlet.sf=uyrw.outlet, bsf=uyrw.flowline)
  
}
```

load some of the output

``` r
w = raster(here(taudem_uyrw['w', 'file']))
demnet = st_read(here(taudem_uyrw['demnet', 'file']))
```

    ## Reading layer `taudem_demnet' from data source `H:\UYRW_data\data\prepared\taudem_uyrw\taudem_demnet.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 19477 features and 17 fields
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 491361.9 ymin: 4866855 xmax: 601472.6 ymax: 5049907
    ## projected CRS:  WGS 84 / UTM zone 12N

``` r
subb = st_read(here(taudem_uyrw['subb', 'file']))
```

    ## Reading layer `subbasins' from data source `H:\UYRW_data\data\prepared\taudem_uyrw\subbasins.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 19377 features and 1 field
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 490354.8 ymin: 4866412 xmax: 602291.8 ymax: 5050297
    ## projected CRS:  WGS 84 / UTM zone 12N

The resulting channel and catchment network is too detailed to use in a
single SWAT+ model, but it will be useful right now for finding
catchments whose main outlets have a stream flow record from USGS. SWAT+
models can later be fitted to some of these catchments separately, then
combined together at the end.

The next chunk partitions the UYRW area by delineating gaged
subwatersheds.

``` r
# load USGS stream gage station points and time series
usgs.dat = readRDS(here(streamgages.meta['USGS_data', 'file']))$dat

# these are arranged in a list, where each entry corresponds to a distinct site (n=105)
usgs.pts = do.call(rbind, lapply(usgs.dat, function(site) site$sf[1,]))

# TODO: do this character > integer change in get_streamgages.R
usgs.pts$count_nu = as.integer(usgs.pts$count_nu)

# the number of observations varies a lot - pick out the sites with > 1000 records
usgs.nobs = sapply(usgs.dat, function(site) sum(sapply(site$dat, function(record) nrow(record))))
usgs.pts.long = usgs.pts[usgs.nobs > obsmin,]

# delineate subwatersheds based on these outlet locations, or load results list if this is already done
usgs.catchments.path = here(subwatersheds.meta['usgs_catchments', 'file'])
if(!file.exists(usgs.catchments.path))
{
  # run the catchments workflow (~1-5 mins, depending on thresholds)
  usgs.catchments = my_find_catchments(usgs.pts.long, demnet, subb, areamin)
  saveRDS(usgs.catchments, usgs.catchments.path)
  
} else {
  
  # load the list of file paths and descriptions
  usgs.catchments = readRDS(usgs.catchments.path)
  
}
```

Repeat for all sites (\>0 records total)

``` r
# delineate subwatersheds based on these outlet locations, or load results list if this is already done
usgs.allcatchments.path = here(subwatersheds.meta['usgs_allcatchments', 'file'])
if(!file.exists(usgs.allcatchments.path))
{
  # run the catchments workflow (~1-5 mins, depending on thresholds)
  usgs.allcatchments = my_find_catchments(usgs.pts, demnet, subb, areamin)
  saveRDS(usgs.allcatchments, usgs.allcatchments.path)
  
} else {
  
  # load the list of file paths and descriptions
  usgs.allcatchments = readRDS(usgs.catchments.path)
  
}


# TODO: add a graphic to show the subwatershed delineation results
```

``` r
#my_markdown('make_subwatersheds', 'R/analysis')
```
