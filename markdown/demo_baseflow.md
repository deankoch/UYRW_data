demo\_baseflow.R
================
Dean Koch
2021-04-26

**Mitacs UYRW project**

**demo\_baseflow.R**: (in development) an example of aquifer model
fitting with SWAT+

This script also demonstrates some of the core functionality of the
`rswat` helper functions, including: building a SWAT+ model;
accessing/changing its parameters; running simulations; loading outputs;
and fitting parameters to observed data.

## libraries

[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
[helper\_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md),
and
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
load required libraries, global variables, and some helper functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))
```

[`Evapotranspiration`](https://cran.r-project.org/web/packages/Evapotranspiration/index.html)
package implements several methods to estimate PET

``` r
library('Evapotranspiration')
```

    ## Warning: package 'Evapotranspiration' was built under R version 4.0.4

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

The [`airGR`](https://cran.r-project.org/web/packages/airGR/index.html)
is a collection of methods from INRAE-Antony (HYCAR Research Unit,
France), including the [Oudin et
al. (2005)](https://doi.org/10.1016/j.jhydrol.2004.08.026) PET
estimator

``` r
library(airGR)
```

    ## Warning: package 'airGR' was built under R version 4.0.4

``` r
# TODO: check out the CemaNeige model for snow accumulation and melt (also from this package)
```

The
[`baseflow`](https://cran.r-project.org/web/packages/baseflow/index.html)
R package is an implementation of the method of [Pelletier and
Andréassian (2020)](https://hess.copernicus.org/articles/24/1171/2020/)
for estimating baseflow (and quickflow) from hydrographs of daily
streamflow and precipitation totals.

``` r
library(baseflow)
```

    ## Warning: package 'baseflow' was built under R version 4.0.4

``` r
# numeric optimization for model fitting
library(dfoptim)

# low-level R graphics control
library(grid)
```

## project data

A USGS gage name specifies one of the catchments to use as demo (see
[make\_subwatersheds.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md))

``` r
nm = 'big_c_nr_emigrant'
```

Make a list of the files created by this script

``` r
{
  files.towrite = list(
    
    # USGS gage to use as example
    c(name='example_name',
      file=nm,
      type='string',
      description='catchment/station name of a USGS streamgage used in the demo'),
    
    # directory for SWAT+ model files
    c(name='dir_qswat',
      file=here(file.path(sci.subdir, paste0('baseflow_', nm))),
      type='string',
      description='directory for QSWAT+/SWAT+ files'),
    
    # overview map of the study catchment
    c(name='img_catchment',
      file=file.path(graphics.dir, 'my_baseflow_catchment.png'),
      type='png graphic', 
      description='image of catchment location, channels, and a USGS hydrograph'),
    
    # overview map of the SWAT+ model
    c(name='img_hrus',
      file=file.path(graphics.dir, 'my_baseflow_hrus.png'),
      type='png graphic', 
      description='image showing SWAT+ model HRUs in the catchment')
  )
}
```

write this filename metadata to disk using a [helper
function](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md%5D)

``` r
baseflow.meta = my_metadata('demo_baseflow', files.towrite, overwrite=TRUE, data.dir=sci.subdir)
```

    ## [1] "> writing metadata to: data/analysis/demo_baseflow_metadata.csv"

calls to `my_metadata` will now load the file info from disk (here and
in other R sessions). eg. the following code accesses the file info from
previous scripts
[get\_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
[get\_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
[get\_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md),
and
[make\_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md)

``` r
basins.meta = my_metadata('get_basins')
streamgages.meta = my_metadata('get_streamgages')
meteo.meta = my_metadata('get_meteo')
subwatersheds.meta = my_metadata('make_subwatersheds')

# load PNWNAmet analysis to get weather inputs, USGS data for observed response
meteo = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
usgs.all = readRDS(here(streamgages.meta['USGS_data', 'file']))

# load some geographical features for plotting
uyrw = readRDS(here(basins.meta['boundary', 'file']))
lakes = readRDS(here(basins.meta['waterbody', 'file']))

# load the USGS data for the catchment 
usgs.w = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))
idx = usgs.w$boundary %>% filter(catchment_name == nm) %>% pull(catchment_id)

# extract outlet locations, catchement boundary, channel network
pts = usgs.w$pts[usgs.w$pts$catchment_id==idx,] %>% na.omit
boundary = usgs.w$boundary[usgs.w$boundary$catchment_id==idx,] %>% na.omit
demnet = usgs.w$demnet[usgs.w$demnet$catchment_id==idx,] %>% na.omit
```

## set up simulation times

``` r
# pull gage data from this site
usgs = usgs.all$dat[[pts$site_no]]

# identify contiguous subsets in the time series
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))
```

    ## 1973-1979 1982-1985 
    ##      2250      1095

The gage has two long uninterrupted periods, with a gap of 3 years. For
the demo we’ll look at the longer 1970s period

``` r
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)
```

## overview plot

The chunk below makes a plot of the channel network for the catchment,
its location in the greater upper Yellowstone river watershed (UYRW)
region, and a hydrograph of the selected USGS discharge records

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)

``` r
catchment.png = here(baseflow.meta['img_catchment', 'file'])
if( !file.exists(catchment.png) )
{
  # plot grob for the 1970s gage data
  ggp.usgs = my_tsplot(setNames(gage, c('Date', 'USGS'))) +
    theme(legend.position = 'none', 
          text = element_text(size=26), 
          axis.title = element_text(face='bold'),
          plot.margin = unit(c(1,1,1,1), 'cm'),
          panel.background = element_rect(fill='white', colour=NA)) 
  
  # grab the colour of the line chart to match inset frame
  insetcol = ggp.usgs$scales$scales[[1]]$palette(1)

  # quick plot of the subwatershed
  tmap.w = tm_shape(boundary) + tm_polygons('grey90', border.col=NULL) + 
    tm_shape(demnet) + tm_lines('grey60') +
    tm_shape(pts) + tm_dots(col='white', size=1.2) +
    tm_shape(pts) + tm_dots(col=insetcol, size=1.0) +
    tm_scale_bar(text.size=1) + tm_layout(main.title=nm, main.title.fontface='bold')
  
  # smaller plot showing the location in the greater URYW region
  tmap.uyrw = tm_shape(uyrw) + tm_polygons('grey90', border.col=NULL) +
    tm_shape(boundary) + tm_polygons(insetcol, alpha=0.5 , border.col=NULL) + 
    tm_shape(usgs.w$demnet %>% filter(Order > 2)) + tm_lines('grey80') +
    tm_shape(lakes) + tm_polygons('grey60', border.col=NULL) +
    tm_layout(frame=NA)


  # create plot device
  png(catchment.png, width=1200, height=1200, pointsize=26)
  
    # initialize plot device and left/right pane layout 
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(2,2)))
    
    # draw a background for the left pane
    grid.draw(rectGrob(gp=gpar(fill=insetcol, alpha=0.1), vp=viewport(layout.pos.col=1)))

    # draw the hydrograph and subwatershed plot on the left pane
    print(tmap.w, vp=viewport(layout.pos.col=1, layout.pos.row=1))
    print(ggp.usgs, vp=viewport(layout.pos.col=1, layout.pos.row=2))
    
    # add the location plot on the right
    print(tmap.uyrw, vp=viewport(layout.pos.col=2))
    
    # draw a box around the left pane
    grid.draw(rectGrob(gp=gpar(fill=NA, col=insetcol, lwd=3), vp=viewport(layout.pos.col=1)))
  
  # close the plot device
  dev.off()
}
```

## build a SWAT+ model and visualize it with R

This section builds a SWAT+ model for the selected catchment by calling
[a python script](https://gitlab.com/rob-yerc/swat) that runs
[QSWAT+](https://swatplus.gitbook.io/docs/user/qswat+), then
[SWAT+Editor](https://swatplus.gitbook.io/docs/user/editor). This
creates a large project folder that includes QSWAT+ shapefiles (for
viewing spatial aspects of the model with GIS software), and SWAT+ input
files which configure [the
executable](https://swatplus.gitbook.io/docs/installation) that runs
simulations.

The inputs to QSWAT+ include data layers on topography, soils, plant
communities, meteorology, and general watershed layout parameters. In
our case these have already been prepared for all catchments in the URYW
by the scripts
[get\_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md),
[get\_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md),
[get\_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md),
[get\_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md),
[make\_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md),

The chunk below sets a couple of watershed layout parameters and builds
the model:

``` r
# run only if the QSWAT+ demo directory doesn't exist yet
dir.qswat = baseflow.meta['dir_qswat', 'file']
if( !dir.exists(dir.qswat) )
{
  # set drop levels high for this demo to get a simple (fast) model
  config = list(skip_editor = FALSE, 
                drop_stream = 4e3, 
                drop_channel = (4e3) - 1)
  
  # write QSWAT+ input files using a helper function
  qswat.meta = qswat_setup(idx, usgs.w, projdir=dir.qswat, config=config, wipe=T, quiet=TRUE)
  
  # pass these inputs to QSWAT+ for processing in PyQGIS, then SWAT+ Editor
  qswat_run(qswat.meta)
  
} else {
  
  # load QSWAT+/SWAT+ project from disk when available
  qswat.meta = my_metadata(basename(dir.qswat), data.dir=dir.qswat)
  
}
```

    ## 
    ## >> finished

This markdown report omits a large amount of console output here (mostly
redirected from QSWAT+) that shows a checklist of jobs completing, and
any warnings that come up. Warnings about the deep aquifers shapefile
can be ignored (see
[here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))
as they seem to be a visualization problem that doesn’t impact the SWAT+
input files.

The entire process for this small 50-HRU example takes about 1-2
minutes. Expect it to take longer on examples with more HRUs (ie more
database entries for QSWAT+/SWAT+Editor to process), or more
high-resolution DEM pixels (larger or more detailed DEMs slow down
TauDEM).

The dataframe `qswat.meta` summarizes the QSWAT+ input files and
parameters used here:

``` r
qswat.meta %>% select(type, description) %>% print
```

    ##                          type                                                            description
    ## name                parameter                                                    QSWAT+ project name
    ## wname               parameter                        initial input weather data for SWAT+ simulation
    ## start_yr            parameter                      initial value of start year for SWAT+ simulations
    ## start_day           parameter                       initial value of start day for SWAT+ simulations
    ## end_yr              parameter                        initial value of end year for SWAT+ simulations
    ## end_day             parameter                         initial value of end day for SWAT+ simulations
    ## drop_channel        parameter                 threshold for channel delineation (in number of cells)
    ## drop_stream         parameter                  threshold for stream delineation (in number of cells)
    ## res_thresh          parameter                          threshold for resrevoir delineation (percent)
    ## skip_editor         parameter           whether to skip SWAT+ Editor call to write config text files
    ## proj                directory                                               QSWAT+ project directory
    ## boundary              GeoJSON                   polygon delineating subwatershed for the SWAT+ model
    ## dem                   GeoTIFF                                                             QSWAT+ DEM
    ## landuse               GeoTIFF                                          SWAT+ land use classification
    ## soils                 GeoTIFF        SWAT soils classification, maps to soil table in SWAT+ database
    ## landuse_lookup            CSV integer code for landuse, maps to `plants_plt` table in SWAT+ database
    ## outlets        ESRI Shapefile          outlet point locations, used by QSWAT+ to delineate subbasins
    ## streams        ESRI Shapefile           stream geometries to "burn" into DEM prior to running TauDEM
    ## wdat                directory                    directory for writing SWAT weather input text files
    ## config                   JSON                         configuration file for run_qswatplus.py module
    ## metadata                  CSV            list files of files written by baseflow_big_c_nr_emigrant.R

Helper functions `qswat_read` and `qswat_plot` from
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
can be used to display the QSWAT+ shapefiles. The chunk below makes a
plot of the spatial arrangement of HRUs and saves the results to a file

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png)

``` r
# load the QSWAT+ shapefiles into R
wsh = qswat_read(qswat.meta)

# skip if the file exists already
wsh.png = here(baseflow.meta['img_hrus', 'file'])
if( !file.exists(wsh.png) )
{
  # modify the title but use default settings for everything else
  wsh.titles = list(main=paste('SWAT+ hydrologic response units (HRUS) for', nm))
  wsh.tmap = qswat_plot(wsh, titles=wsh.titles)
  
  # write the file
  tmap_save(tm=wsh.tmap, filename=wsh.png, height=2000, width=2000, pointsize=12)
  
}
```

## managing SWAT+ configuration files in R

The SWAT+ executable runs simulations according to the settings written
in a directory of text files (usually called ‘TxtInOut’). This is a
large and complicated set of files that define all parameters of model.
The `rswat` helper functions include tools for managing and cataloging
these files.

If you are new to SWAT, check out the
[I/O](https://swatplus.gitbook.io/docs/user/io) and
[theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs.
Note that the second link is for a document from 2009 (the most recent,
as far as I am aware), and although core aspects of the the model have
not changed much since then, many variable and parameter names are
different in SWAT+.

The first step is to define the project directory and see what we have:

``` r
# assign the SWAT+ project directory in `rswat`
cio = rswat_cio(dir.qswat)
```

    ## setting `ciopath` to H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/file.cio 
    ## [1] "file.cio: written by SWAT+ editor v2.0.0 on 2021-04-26 16:55"

load all config files into memory except decision tables, which are very
large (and not needed for now). This takes a moment to parse the files,
which are then summarized in the returned dataframe

``` r
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
```

    ## [1] "file.cio: written by SWAT+ editor v2.0.0 on 2021-04-26 16:55"

``` r
print(cio)
```

    ##                 file          group                size            modified nline nskip ntab  nvar
    ## 1           time.sim     simulation   0.168 [kilobytes] 2021-04-26 16:55:28     1     0    1    10
    ## 2          print.prt     simulation   3.389 [kilobytes] 2021-04-26 16:55:28    46     0    5   223
    ## 3         object.cnt     simulation   0.589 [kilobytes] 2021-04-26 16:55:28     1     0    1    42
    ## 4          codes.bsn          basin   0.599 [kilobytes] 2021-04-26 16:55:28     1     0    1    48
    ## 5     parameters.bsn          basin   1.296 [kilobytes] 2021-04-26 16:55:28     1     0    1    88
    ## 6    weather-sta.cli        climate   3.622 [kilobytes] 2021-04-26 16:55:28    16     0    1   153
    ## 7    weather-wgn.cli        climate  42.549 [kilobytes] 2021-04-26 16:55:28   223    16   16  2992
    ## 8            pcp.cli        climate   0.359 [kilobytes] 2021-04-26 16:55:17    16     0    1    17
    ## 9            tmp.cli        climate   0.357 [kilobytes] 2021-04-26 16:55:23    16     0    1    17
    ## 10           hmd.cli        climate   0.363 [kilobytes] 2021-04-26 16:55:14    16     0    1    17
    ## 11           wnd.cli        climate   0.356 [kilobytes] 2021-04-26 16:55:26    16     0    1    17
    ## 12           hru.con        connect   8.425 [kilobytes] 2021-04-26 16:55:28    50     0    1   663
    ## 13     rout_unit.con        connect  16.283 [kilobytes] 2021-04-26 16:55:28    50     0    1  1479
    ## 14       aquifer.con        connect  13.793 [kilobytes] 2021-04-26 16:55:28    51     0    1  1092
    ## 15        recall.con        connect   5.680 [kilobytes] 2021-04-26 16:55:28    25     0    1   442
    ## 16       chandeg.con        connect   5.629 [kilobytes] 2021-04-26 16:55:28    25     0    1   442
    ## 17       initial.cha        channel   0.321 [kilobytes] 2021-04-26 16:55:28     1     0    1    14
    ## 18     nutrients.cha        channel   1.166 [kilobytes] 2021-04-26 16:55:28     1     0    1    80
    ## 19   channel-lte.cha        channel   2.721 [kilobytes] 2021-04-26 16:55:28    25     0    1   156
    ## 20   hyd-sed-lte.cha        channel   8.712 [kilobytes] 2021-04-26 16:55:28    25     0    1   624
    ## 21     rout_unit.def   routing_unit   2.617 [kilobytes] 2021-04-26 16:55:28    50     0    1   204
    ## 22     rout_unit.ele   routing_unit   4.453 [kilobytes] 2021-04-26 16:55:28    50     0    1   306
    ## 23     rout_unit.rtu   routing_unit   5.269 [kilobytes] 2021-04-26 16:55:28    50     0    1   306
    ## 24      hru-data.hru            hru   8.940 [kilobytes] 2021-04-26 16:55:28    50     0    1   510
    ## 25          exco.exc           exco   2.922 [kilobytes] 2021-04-26 16:55:28    25     0    1   156
    ## 26       exco_om.exc           exco   7.137 [kilobytes] 2021-04-26 16:55:28    25     0    1   494
    ## 27        recall.rec         recall   1.572 [kilobytes] 2021-04-26 16:55:28    25     0    1   104
    ## 28       initial.aqu        aquifer   0.321 [kilobytes] 2021-04-26 16:55:28     1     0    1    14
    ## 29       aquifer.aqu        aquifer  13.481 [kilobytes] 2021-04-26 16:55:28    51     0    1   936
    ## 30     hydrology.hyd      hydrology  11.083 [kilobytes] 2021-04-26 16:55:28    50     0    1   765
    ## 31    topography.hyd      hydrology   9.158 [kilobytes] 2021-04-26 16:55:28   100     0    1   606
    ## 32         field.fld      hydrology   3.225 [kilobytes] 2021-04-26 16:55:28    50     0    1   204
    ## 33     tiledrain.str     structural   0.331 [kilobytes] 2021-04-26 16:55:28     1     0    1    18
    ## 34        septic.str     structural   1.211 [kilobytes] 2021-04-26 16:55:28     2     0    1    84
    ## 35   filterstrip.str     structural   0.341 [kilobytes] 2021-04-26 16:55:28     2     0    1    18
    ## 36     grassedww.str     structural   0.560 [kilobytes] 2021-04-26 16:55:28     3     0    1    36
    ## 37       bmpuser.str     structural   0.304 [kilobytes] 2021-04-26 16:55:28     1     0    1    18
    ## 38        plants.plt    hru_parm_db 194.373 [kilobytes] 2021-04-26 16:55:28   256     0    1 13878
    ## 39    fertilizer.frt    hru_parm_db   7.216 [kilobytes] 2021-04-26 16:55:28    59     0    1   480
    ## 40       tillage.til    hru_parm_db   8.311 [kilobytes] 2021-04-26 16:55:28    78     0    1   553
    ## 41     pesticide.pes    hru_parm_db  49.945 [kilobytes] 2021-04-26 16:55:28   233     0    1  3510
    ## 42         urban.urb    hru_parm_db   1.832 [kilobytes] 2021-04-26 16:55:28     9     0    1   130
    ## 43        septic.sep    hru_parm_db   4.644 [kilobytes] 2021-04-26 16:55:28    26     0    1   324
    ## 44          snow.sno    hru_parm_db   0.326 [kilobytes] 2021-04-26 16:55:28     1     0    1    18
    ## 45          harv.ops            ops   1.273 [kilobytes] 2021-04-26 16:55:28    14     0    1    90
    ## 46         graze.ops            ops   1.626 [kilobytes] 2021-04-26 16:55:28    12     0    1    91
    ## 47           irr.ops            ops   0.662 [kilobytes] 2021-04-26 16:55:28     4     0    1    45
    ## 48      chem_app.ops            ops   1.897 [kilobytes] 2021-04-26 16:55:28    12     0    1   130
    ## 49          fire.ops            ops   0.265 [kilobytes] 2021-04-26 16:55:28     3     0    1    16
    ## 50         sweep.ops            ops   0.170 [kilobytes] 2021-04-26 16:55:28     1     0    1     8
    ## 51       landuse.lum            lum   1.613 [kilobytes] 2021-04-26 16:55:28     5     0    1    84
    ## 52       cntable.lum            lum  11.089 [kilobytes] 2021-04-26 16:55:28    52     0    1   424
    ## 53 cons_practice.lum            lum   3.243 [kilobytes] 2021-04-26 16:55:28    38     0    1   156
    ## 54     ovn_table.lum            lum   1.755 [kilobytes] 2021-04-26 16:55:28    20     0    1   105
    ## 55     cal_parms.cal            chg  15.543 [kilobytes] 2021-04-26 16:55:28   184     1    1   921
    ## 56         plant.ini           init   1.151 [kilobytes] 2021-04-26 16:55:28    10     9    1   106
    ## 57    soil_plant.ini           init   0.316 [kilobytes] 2021-04-26 16:55:28     1     0    1    14
    ## 58      om_water.ini           init   0.638 [kilobytes] 2021-04-26 16:55:28     1     0    1    40
    ## 59         soils.sol          soils  41.053 [kilobytes] 2021-04-26 16:55:28   150     0    1  3171
    ## 60     nutrients.sol          soils   0.426 [kilobytes] 2021-04-26 16:55:28     1     0    1    26
    ## 61           lum.dtl decision_table  23.015 [kilobytes] 2021-04-26 16:55:29    NA    NA   NA    NA
    ## 62       res_rel.dtl decision_table 317.488 [kilobytes] 2021-04-26 16:55:30    NA    NA   NA    NA
    ## 63       scen_lu.dtl decision_table   9.514 [kilobytes] 2021-04-26 16:55:30    NA    NA   NA    NA
    ## 64       flo_con.dtl decision_table  10.361 [kilobytes] 2021-04-26 16:55:30    NA    NA   NA    NA
    ## 65       ls_unit.ele        regions   5.063 [kilobytes] 2021-04-26 16:55:30    50     0    1   357
    ## 66       ls_unit.def        regions   3.333 [kilobytes] 2021-04-26 16:55:30    51     1    1   256
    ## 67   aqu_catunit.ele        regions   5.165 [kilobytes] 2021-04-26 16:55:30    51     0    1   364

Each row of `cio` is a file containing a group of model parameters. The
‘nvar’ column indicates how many distinct fields there are in the file
(nrow \* ncol, summed over all of the tables, including headers).

We will be interested in the aquifer model parameters in ‘aquifer.aqu’ -
the chunk below prints the last few lines of the relevant table:

``` r
# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print
```

    ##              file   group               size            modified nline nskip ntab nvar
    ## 1     aquifer.con connect 13.793 [kilobytes] 2021-04-26 16:55:28    51     0    1 1092
    ## 2     initial.aqu aquifer  0.321 [kilobytes] 2021-04-26 16:55:28     1     0    1   14
    ## 3     aquifer.aqu aquifer 13.481 [kilobytes] 2021-04-26 16:55:28    51     0    1  936
    ## 4 aqu_catunit.ele regions  5.165 [kilobytes] 2021-04-26 16:55:30    51     0    1  364

``` r
# this one contains the main process model parameters 
rswat_open('aquifer.aqu') %>% str
```

    ## 'data.frame':    51 obs. of  18 variables:
    ##  $ id       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ name     : chr  "aqu011" "aqu012" "aqu021" "aqu022" ...
    ##  $ init     : chr  "initaqu1" "initaqu1" "initaqu1" "initaqu1" ...
    ##  $ gw_flo   : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ dep_bot  : num  10 10 10 10 10 10 10 10 10 10 ...
    ##  $ dep_wt   : num  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ no3_n    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sol_p    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ carbon   : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ flo_dist : num  50 50 50 50 50 50 50 50 50 50 ...
    ##  $ bf_max   : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ alpha_bf : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ revap    : num  0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 ...
    ##  $ rchg_dp  : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ spec_yld : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ hl_no3n  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_min  : num  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ revap_min: num  5 5 5 5 5 5 5 5 5 5 ...

The helper function `rswat_find` can be useful for tracking down a SWAT+
parameter using keywords or SWAT2012 names. This uses fuzzy
case-insensitive matching (see R’s `?agrep` doc), which catches many of
the name changes in the SWAT2012 -\> SWAT+ updates. eg. the following
code finds the PET estimation method parameter ‘pet’, which was called
‘IPET’ in SWAT2012:

``` r
# fuzzy = 1 to allow inexact matches
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print
```

    ##       name string     class dim          file table  i  j
    ## 1 pet_file   null character   1     codes.bsn     1  1  1
    ## 2      pet      1   integer   1     codes.bsn     1  1  3
    ## 3 harg_pet   <NA>   numeric  50 hydrology.hyd     1 NA 14

The ‘string’ column above shows the plaintext representation of
parameters in the SWAT+ config files listed in column ‘file’. We can see
that ‘pet’ (in file ‘codes.bsn’) set to 1. This codes for the
Penman-Monteith model for potential evapotranspiration (PET). The other
two matches, ‘pet\_file’ and ‘harg\_pet’, are an input file for observed
PET, and a solar radiation coefficient used with the Hargreaves-Samani
model (neither is currently used).

## adjusting a SWAT+ model

To change a parameter, open its container file with `rswat_open`, modify
it in R, then write the change with `rswat_write`. eg. switching to the
Hargreaves-Samani model for PET (coded as ‘pet’ = 2) can be done like
this:

``` r
# open the file and put its contents into an R dataframe called `codes`
codes = rswat_open('codes.bsn')
codes %>% str
```

    ## 'data.frame':    1 obs. of  24 variables:
    ##  $ pet_file  : chr ""
    ##  $ wq_file   : chr ""
    ##  $ pet       : int 1
    ##  $ event     : int 0
    ##  $ crack     : int 0
    ##  $ rtu_wq    : int 1
    ##  $ sed_det   : int 0
    ##  $ rte_cha   : int 0
    ##  $ deg_cha   : int 0
    ##  $ wq_cha    : int 1
    ##  $ nostress  : int 0
    ##  $ cn        : int 0
    ##  $ c_fact    : int 0
    ##  $ carbon    : int 0
    ##  $ baseflo   : int 0
    ##  $ uhyd      : int 1
    ##  $ sed_cha   : int 0
    ##  $ tiledrain : int 0
    ##  $ wtable    : int 0
    ##  $ soil_p    : int 0
    ##  $ abstr_init: int 0
    ##  $ atmo_dep  : chr "a"
    ##  $ stor_max  : int 0
    ##  $ headwater : int 0

By default, SWAT+ estimates PET with Penman-Monteith (‘pet’ = 1), so we
have to change the PET code:

``` r
# change PET method to Hargreaves-Samani 
codes$pet = 2
```

The default behaviour of `rswat_write` is to preview the requested
change:

``` r
# preview changes
rswat_write(codes) %>% str
```

    ## 'data.frame':    1 obs. of  7 variables:
    ##  $ file         : chr "codes.bsn"
    ##  $ table        : num 1
    ##  $ i            : int 1
    ##  $ j            : int 3
    ##  $ name         : chr "pet"
    ##  $ current_value: chr "1"
    ##  $ replacement  : chr "2"

The new ‘string’ field looks fine so we go ahead and overwrite the file
on disk with argument `preview=FALSE`

``` r
# write the changes
rswat_write(codes, preview=FALSE, quiet=TRUE)
```

The SWAT+ executable will now use the Hargreaves-Samani method for
estimation. The Hargreaves-Samani coefficient, ‘harg\_pet’, that turned
up earlier in the search for ‘IPET’ is no longer inactive, so we need to
assign it a sensible value. For now I just use the example value
appearing in the I/O docs - we will tune it later.

``` r
# open the container file for 'harg_pet' and print a summary
hydro = rswat_open('hydrology.hyd')
hydro %>% str
```

    ## 'data.frame':    50 obs. of  15 variables:
    ##  $ name       : chr  "hyd01" "hyd02" "hyd03" "hyd04" ...
    ##  $ lat_ttime  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lat_sed    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ can_max    : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ esco       : num  0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 ...
    ##  $ epco       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ orgn_enrich: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orgp_enrich: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cn3_swf    : num  0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 ...
    ##  $ bio_mix    : num  0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 ...
    ##  $ perco      : num  0.9 0.5 0.9 0.5 0.5 0.9 0.9 0.5 0.9 0.5 ...
    ##  $ lat_orgn   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lat_orgp   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ harg_pet   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ latq_co    : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 ...

The parameters in this file are all length-50 (column) vectors. This is
because there are 50 HRUs in this model, and SWAT+ allows distinct
values for each one in this case. The code below assigns the same
default value for the coefficient to all of them

``` r
# assign the default 'harg_pet' value in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)
```

Hargreaves-Samani may be the best choice for this project since we lack
the detailed data on humidity, wind, and solar energy required with
Penman-Monteith. SWAT+ can generate those missing data using a
stochastic process, but the result is imprecise at the daily scale.

## Viewing simulation data

the SWAT+ executable takes a few seconds to simulate the full seven-year
time series in this example, producing output in the form of .txt tables
containing simulated state variables. There are many such output tables
(100+) and the task of printing any of them to a file can slow down
SWAT+ considerably. To speed things up it is best to request specific
outputs and omit printing the others.

This requires learning what output variables are available and where
they are located. `rswat_output` handles listing and parsing SWAT+ model
outputs

``` r
# get a dataframe with info on the available SWAT+ output files
odf = rswat_output()

# print the total number of rows (files), and the first few lines
print( nrow(odf) )
```

    ## [1] 56

``` r
head(odf)
```

    ##              file     name type step   group oid              size            modified
    ## 1  aquifer_aa.txt  aquifer  prt year aquifer  NA 0.739 [kilobytes] 2021-04-26 16:55:31
    ## 2  aquifer_yr.txt  aquifer  prt year aquifer  NA 0.739 [kilobytes] 2021-04-26 16:55:31
    ## 3 basin_ls_aa.txt basin_ls  prt year   basin  NA 0.442 [kilobytes] 2021-04-26 16:55:31
    ## 4 basin_ls_yr.txt basin_ls  prt year   basin  NA 0.442 [kilobytes] 2021-04-26 16:55:31
    ## 5 basin_nb_aa.txt basin_nb  prt year   basin  NA 0.702 [kilobytes] 2021-04-26 16:55:31
    ## 6 basin_nb_yr.txt basin_nb  prt year   basin  NA 0.702 [kilobytes] 2021-04-26 16:55:31
    ##                                                                                               path
    ## 1  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_aa.txt
    ## 2  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_yr.txt
    ## 3 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_aa.txt
    ## 4 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_yr.txt
    ## 5 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_nb_aa.txt
    ## 6 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_nb_yr.txt

Output files are loaded as R dataframes by specifying `fname`

``` r
# load an example file. 
hydout.yr = rswat_output(fname='hydout_yr.txt')
hydout.yr %>% str
```

    ## 'data.frame':    298 obs. of  25 variables:
    ##  $ date    : Date, format: "1945-01-02" "1945-01-02" "1945-01-02" "1945-01-02" ...
    ##  $ name    : chr  "rtu012" "rtu012" "rtu012" "rtu012" ...
    ##  $ type    : chr  "ru" "ru" "ru" "ru" ...
    ##  $ objtyp  : chr  "sdc" "ru" "ru" "aqu" ...
    ##  $ typ_no  : int  1 1 1 12 2 3 3 22 3 5 ...
    ##  $ hyd_typ : chr  "sur" "sur" "lat" "rhg" ...
    ##  $ fraction: num  0.84 0.16 1 1 0.82 ...
    ##  $ flo     : Units: [m^3/s] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sed     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orgn    : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sedp    : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no3     : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ solp    : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ chla    : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ nh3     : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no2     : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cbod    : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dox     : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ san     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sil     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cla     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sag     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lag     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ grv     : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ null    : Units: [°C] num  0 0 0 0 0 0 0 0 0 0 ...

A subset of columns (output variables) can be specified with `vname`

``` r
# print the first few lines for two particular variables
rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction')) %>% head
```

    ##         date   name type objtyp typ_no hyd_typ fraction       flo
    ## 1 1945-01-02 rtu012   ru    sdc      1     sur     0.84 0 [m^3/s]
    ## 2 1945-01-02 rtu012   ru     ru      1     sur     0.16 0 [m^3/s]
    ## 3 1945-01-02 rtu012   ru     ru      1     lat     1.00 0 [m^3/s]
    ## 4 1945-01-02 rtu012   ru    aqu     12     rhg     1.00 0 [m^3/s]
    ## 5 1945-01-02 rtu022   ru    sdc      2     sur     0.82 0 [m^3/s]
    ## 6 1945-01-02 rtu022   ru     ru      3     sur     0.18 0 [m^3/s]

Notice dates and units are incorporated automatically. Some additional
columns are also loaded by default because they are important
identifiers (eg spatial IDs). This functionality (along with
date-parsing) can be switched off to get faster load times, or for
debugging:

``` r
rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction'), makedates=FALSE, showidx=FALSE) %>% head
```

    ##   fraction       flo
    ## 1     0.84 0 [m^3/s]
    ## 2     0.16 0 [m^3/s]
    ## 3     1.00 0 [m^3/s]
    ## 4     1.00 0 [m^3/s]
    ## 5     0.82 0 [m^3/s]
    ## 6     0.18 0 [m^3/s]

When an output file is scanned by this function, its headers and units
are cached for faster loading in subsequent calls. Variable names in
this database can then be searched by calling `rswat_output` without the
`fname` argument:

``` r
# search for output variables named "fraction". The one loaded above is identified:
rswat_output(vname='fraction') %>% str
```

    ## 'data.frame':    1 obs. of  11 variables:
    ##  $ line_num : int 2
    ##  $ field_num: int 10
    ##  $ start_pos: num 98
    ##  $ end_pos  : num 105
    ##  $ name     : chr "fraction"
    ##  $ units    : chr NA
    ##  $ file     : chr "hydout_yr.txt"
    ##  $ type     : chr "prt"
    ##  $ step     : chr "year"
    ##  $ index    : logi FALSE
    ##  $ class    : chr "numeric"

``` r
# search for 'flow_in'. Only a partial match is found: 
rswat_output(vname='flo_in') %>% str
```

    ## no exact matches for "flo_in", trying partial matches...
    ## 'data.frame':    1 obs. of  11 variables:
    ##  $ line_num : int 2
    ##  $ field_num: int 11
    ##  $ start_pos: num 120
    ##  $ end_pos  : num 122
    ##  $ name     : chr "flo"
    ##  $ units    : chr "m^3/s"
    ##  $ file     : chr "hydout_yr.txt"
    ##  $ type     : chr "prt"
    ##  $ step     : chr "year"
    ##  $ index    : logi FALSE
    ##  $ class    : chr "numeric"

Right now the database only includes the contents of ‘hydout\_yr.txt’,
and `rswat_output()` only reports the files currently in the SWAT+
project folder (“TxtInOut”). To get a more exhaustive list `rswat` can
run a (1-day) simulation, requesting all outputs, then parse the output
files before restoring the original state of the project folder

``` r
# build database of SWAT+ outputs
odf = rswat_output(loadall=TRUE)
```

    ## running SWAT+ to generate all output files...
    ## backing up files... running SWAT+... restoring backup... done
    ## parsing 108 SWAT+ output files...

``` r
odf %>% head
```

    ##               file     name type  step   group oid              size            modified
    ## 1   aquifer_aa.txt  aquifer  prt  year aquifer  NA 0.739 [kilobytes] 2021-04-26 16:55:41
    ## 2  aquifer_day.txt  aquifer  prt   day aquifer  NA    NA [kilobytes]                <NA>
    ## 3  aquifer_mon.txt  aquifer  prt month aquifer  NA    NA [kilobytes]                <NA>
    ## 4   aquifer_yr.txt  aquifer  prt  year aquifer  NA 0.739 [kilobytes] 2021-04-26 16:55:41
    ## 5  basin_ls_aa.txt basin_ls  prt  year   basin  NA 0.442 [kilobytes] 2021-04-26 16:55:41
    ## 6 basin_ls_day.txt basin_ls  prt   day   basin  NA    NA [kilobytes]                <NA>
    ##                                                                                               path
    ## 1  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_aa.txt
    ## 2                                                                                             <NA>
    ## 3                                                                                             <NA>
    ## 4  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_yr.txt
    ## 5 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_aa.txt
    ## 6                                                                                             <NA>

Notice the filenames list now includes entries with NA fields for
‘size’, ‘modified’, and ‘path’. These are files not currently found
in ‘TxtInOut’ but which can be enabled in SWAT+ simulations. Since the
above function call cached their headers (and units), they are now
searchable:

``` r
# repeat the search for 'flo_in' and find many exact matches. 
rswat_output(vname='flo_in') %>% str
```

    ## 'data.frame':    28 obs. of  11 variables:
    ##  $ line_num : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ field_num: int  8 8 8 8 30 30 30 30 8 8 ...
    ##  $ start_pos: num  64 64 64 64 400 400 400 400 67 67 ...
    ##  $ end_pos  : num  69 69 69 69 405 405 405 405 72 72 ...
    ##  $ name     : chr  "flo_in" "flo_in" "flo_in" "flo_in" ...
    ##  $ units    : chr  "ha m" "ha m" "ha m" "ha m" ...
    ##  $ file     : chr  "basin_cha_aa.txt" "basin_cha_day.txt" "basin_cha_mon.txt" "basin_cha_yr.txt" ...
    ##  $ type     : chr  "prt" "prt" "prt" "prt" ...
    ##  $ step     : chr  "year" "day" "month" "year" ...
    ##  $ index    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ class    : chr  "numeric" "numeric" "numeric" "numeric" ...

``` r
# pipes are useful for narrowing the results of a search
rswat_output(vname='flo_in') %>% filter( step == 'day' ) %>% filter( units == 'ha m' ) %>% str
```

    ## 'data.frame':    1 obs. of  11 variables:
    ##  $ line_num : int 2
    ##  $ field_num: int 8
    ##  $ start_pos: num 64
    ##  $ end_pos  : num 69
    ##  $ name     : chr "flo_in"
    ##  $ units    : chr "ha m"
    ##  $ file     : chr "basin_cha_day.txt"
    ##  $ type     : chr "prt"
    ##  $ step     : chr "day"
    ##  $ index    : logi FALSE
    ##  $ class    : chr "numeric"

Searches will return exact matches first, and if nothing is found, the
function reverts to partial (sub-string) matching with increasing
fuzziness until it finds something:

``` r
# print the first few lines of search results showing this behaviour
rswat_output(vname='recharge') %>% head
```

    ## no exact matches for "recharge", trying partial matches...

    ##    line_num field_num start_pos end_pos   name units            file type  step index   class
    ## 11        2        11       115     119  rchrg    mm  aquifer_aa.txt  prt  year FALSE numeric
    ## 18        2        18       219     224 rchrgn kg/ha  aquifer_aa.txt  prt  year FALSE numeric
    ## 35        2        11       115     119  rchrg    mm aquifer_day.txt  prt   day FALSE numeric
    ## 42        2        18       219     224 rchrgn kg/ha aquifer_day.txt  prt   day FALSE numeric
    ## 59        2        11       115     119  rchrg    mm aquifer_mon.txt  prt month FALSE numeric
    ## 66        2        18       219     224 rchrgn kg/ha aquifer_mon.txt  prt month FALSE numeric
