demo\_baseflow.R
================
Dean Koch
2021-05-06

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
[make\_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md)

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

The first step is to define the project directory:

``` r
# assign the SWAT+ project directory in `rswat`
cio = rswat_cio(dir.qswat)
```

    ## setting `ciopath` to D:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/file.cio 
    ## file.cio: written by SWAT+ editor v2.0.0 on 2021-05-06 20:56

Subsequent calls to rswat\_cio() will list all config files in the
project directory

``` r
# print the first few rows of the files dataframe
cio %>% head
```

    ##              file      group              size            modified
    ## 1        time.sim simulation 0.168 [kilobytes] 2021-05-06 20:56:21
    ## 2       print.prt simulation 3.389 [kilobytes] 2021-05-06 20:56:21
    ## 3      object.cnt simulation 0.589 [kilobytes] 2021-05-06 20:56:21
    ## 4       codes.bsn      basin 0.599 [kilobytes] 2021-05-06 20:56:21
    ## 5  parameters.bsn      basin 1.296 [kilobytes] 2021-05-06 20:56:21
    ## 6 weather-sta.cli    climate 3.622 [kilobytes] 2021-05-06 20:56:21

Each row of `cio` is a file containing a group of model parameters.
Before changing anything it’s a good idea to make a backup. `rswat_copy`
with argument `fname='.'` will copy the entire contents of the config
files directory (excluding subdirectories) to a backup subdirectory

``` r
# the return value is a vector of file paths to the backups
path.backup = rswat_copy(fname='.', quiet=TRUE)
dir.backup = dirname(path.backup[1])
print(dir.backup)
```

    ## [1] "D:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/_rswat_backup_file1940642b3147"

to start over later, restore this copy by passing the backup directory
path back to `rswat_copy`

``` r
# restore the backup we just made
opath = rswat_copy(from=dir.backup, overwrite=TRUE, quiet=TRUE)

# To load a config file, pass its filename to `rswat_open` eg. the code below displays the first few rows
# from the SWAT+ aquifer parameters file, 'aquifer.aqu'

# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print
```

    ##              file   group               size            modified
    ## 1     aquifer.con connect 13.793 [kilobytes] 2021-05-06 20:56:21
    ## 2     initial.aqu aquifer  0.321 [kilobytes] 2021-05-06 20:56:21
    ## 3     aquifer.aqu aquifer 13.481 [kilobytes] 2021-05-06 20:56:21
    ## 4 aqu_catunit.ele regions  5.165 [kilobytes] 2021-05-06 20:56:23

``` r
# this one contains the main process model parameters 
rswat_open('aquifer.aqu', quiet=TRUE) %>% str
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

After rswat loads a SWAT+ config file into R, it caches the contents,
and the file becomes searchable and faster to open/write in subsequent
calls. `rswat_cio()` will also return some additional summary info:

``` r
# print summary info about the file 'aquifer.aqu'
rswat_cio() %>% filter( file=='aquifer.aqu' )
```

    ##          file   group               size            modified nline nskip ntab nvar
    ## 1 aquifer.aqu aquifer 13.481 [kilobytes] 2021-05-06 20:56:26    51     0    1  936

The ‘nline’ column counts the total number of data rows in the file;
‘nskip’ indicates lines not understood by the parser (probably
comments, but possibly bugs - best to check this manually); ‘ntab’ is
the number of distinct tables in the file (either having different
headers, or being separated by comments), and ‘nvar’ indicates how many
distinct fields there are in the file (nrow \* ncol, summed over all of
the tables, including headers);

To pre-load all files, use the `loadall` flag with `rswat_cio`. This
will a few seconds to parse all the plaintext and detect headers, types,
and spacing rules. The code below excludes decision table files, which
contain many distinct tables (slow to load) and are not needed for now.

``` r
# load most of the config files into memory for convenience.
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
print(cio)
```

    ##                 file          group                size            modified nline nskip ntab  nvar
    ## 1           time.sim     simulation   0.168 [kilobytes] 2021-05-06 20:56:26     1     0    1    10
    ## 2          print.prt     simulation   3.389 [kilobytes] 2021-05-06 20:56:26    46     0    5   223
    ## 3         object.cnt     simulation   0.589 [kilobytes] 2021-05-06 20:56:26     1     0    1    42
    ## 4          codes.bsn          basin   0.599 [kilobytes] 2021-05-06 20:56:26     1     0    1    48
    ## 5     parameters.bsn          basin   1.296 [kilobytes] 2021-05-06 20:56:26     1     0    1    88
    ## 6    weather-sta.cli        climate   3.622 [kilobytes] 2021-05-06 20:56:26    16     0    1   153
    ## 7    weather-wgn.cli        climate  42.549 [kilobytes] 2021-05-06 20:56:26   223    16   16  2992
    ## 8            pcp.cli        climate   0.359 [kilobytes] 2021-05-06 20:56:26    16     0    1    17
    ## 9            tmp.cli        climate   0.357 [kilobytes] 2021-05-06 20:56:26    16     0    1    17
    ## 10           hmd.cli        climate   0.363 [kilobytes] 2021-05-06 20:56:26    16     0    1    17
    ## 11           wnd.cli        climate   0.356 [kilobytes] 2021-05-06 20:56:26    16     0    1    17
    ## 12           hru.con        connect   8.425 [kilobytes] 2021-05-06 20:56:26    50     0    1   663
    ## 13     rout_unit.con        connect  16.283 [kilobytes] 2021-05-06 20:56:26    50     0    1  1479
    ## 14       aquifer.con        connect  13.793 [kilobytes] 2021-05-06 20:56:26    51     0    1  1092
    ## 15        recall.con        connect   5.680 [kilobytes] 2021-05-06 20:56:26    25     0    1   442
    ## 16       chandeg.con        connect   5.629 [kilobytes] 2021-05-06 20:56:26    25     0    1   442
    ## 17       initial.cha        channel   0.321 [kilobytes] 2021-05-06 20:56:26     1     0    1    14
    ## 18     nutrients.cha        channel   1.166 [kilobytes] 2021-05-06 20:56:26     1     0    1    80
    ## 19   channel-lte.cha        channel   2.721 [kilobytes] 2021-05-06 20:56:26    25     0    1   156
    ## 20   hyd-sed-lte.cha        channel   8.712 [kilobytes] 2021-05-06 20:56:26    25     0    1   624
    ## 21     rout_unit.def   routing_unit   2.617 [kilobytes] 2021-05-06 20:56:26    50     0    1   204
    ## 22     rout_unit.ele   routing_unit   4.453 [kilobytes] 2021-05-06 20:56:26    50     0    1   306
    ## 23     rout_unit.rtu   routing_unit   5.269 [kilobytes] 2021-05-06 20:56:26    50     0    1   306
    ## 24      hru-data.hru            hru   8.940 [kilobytes] 2021-05-06 20:56:26    50     0    1   510
    ## 25          exco.exc           exco   2.922 [kilobytes] 2021-05-06 20:56:26    25     0    1   156
    ## 26       exco_om.exc           exco   7.137 [kilobytes] 2021-05-06 20:56:26    25     0    1   494
    ## 27        recall.rec         recall   1.572 [kilobytes] 2021-05-06 20:56:26    25     0    1   104
    ## 28       initial.aqu        aquifer   0.321 [kilobytes] 2021-05-06 20:56:26     1     0    1    14
    ## 29       aquifer.aqu        aquifer  13.481 [kilobytes] 2021-05-06 20:56:26    51     0    1   936
    ## 30     hydrology.hyd      hydrology  11.083 [kilobytes] 2021-05-06 20:56:26    50     0    1   765
    ## 31    topography.hyd      hydrology   9.158 [kilobytes] 2021-05-06 20:56:26   100     0    1   606
    ## 32         field.fld      hydrology   3.225 [kilobytes] 2021-05-06 20:56:26    50     0    1   204
    ## 33     tiledrain.str     structural   0.331 [kilobytes] 2021-05-06 20:56:26     1     0    1    18
    ## 34        septic.str     structural   1.211 [kilobytes] 2021-05-06 20:56:26     2     0    1    84
    ## 35   filterstrip.str     structural   0.341 [kilobytes] 2021-05-06 20:56:26     2     0    1    18
    ## 36     grassedww.str     structural   0.560 [kilobytes] 2021-05-06 20:56:26     3     0    1    36
    ## 37       bmpuser.str     structural   0.304 [kilobytes] 2021-05-06 20:56:26     1     0    1    18
    ## 38        plants.plt    hru_parm_db 194.373 [kilobytes] 2021-05-06 20:56:26   256     0    1 13878
    ## 39    fertilizer.frt    hru_parm_db   7.216 [kilobytes] 2021-05-06 20:56:26    59     0    1   480
    ## 40       tillage.til    hru_parm_db   8.311 [kilobytes] 2021-05-06 20:56:26    78     0    1   553
    ## 41     pesticide.pes    hru_parm_db  49.945 [kilobytes] 2021-05-06 20:56:26   233     0    1  3510
    ## 42         urban.urb    hru_parm_db   1.832 [kilobytes] 2021-05-06 20:56:26     9     0    1   130
    ## 43        septic.sep    hru_parm_db   4.644 [kilobytes] 2021-05-06 20:56:26    26     0    1   324
    ## 44          snow.sno    hru_parm_db   0.326 [kilobytes] 2021-05-06 20:56:26     1     0    1    18
    ## 45          harv.ops            ops   1.273 [kilobytes] 2021-05-06 20:56:26    14     0    1    90
    ## 46         graze.ops            ops   1.626 [kilobytes] 2021-05-06 20:56:26    12     0    1    91
    ## 47           irr.ops            ops   0.662 [kilobytes] 2021-05-06 20:56:26     4     0    1    45
    ## 48      chem_app.ops            ops   1.897 [kilobytes] 2021-05-06 20:56:26    12     0    1   130
    ## 49          fire.ops            ops   0.265 [kilobytes] 2021-05-06 20:56:26     3     0    1    16
    ## 50         sweep.ops            ops   0.170 [kilobytes] 2021-05-06 20:56:26     1     0    1     8
    ## 51       landuse.lum            lum   1.613 [kilobytes] 2021-05-06 20:56:26     5     0    1    84
    ## 52       cntable.lum            lum  11.089 [kilobytes] 2021-05-06 20:56:26    52     0    1   424
    ## 53 cons_practice.lum            lum   3.243 [kilobytes] 2021-05-06 20:56:26    38     0    1   156
    ## 54     ovn_table.lum            lum   1.755 [kilobytes] 2021-05-06 20:56:26    20     0    1   105
    ## 55     cal_parms.cal            chg  15.543 [kilobytes] 2021-05-06 20:56:26   184     1    1   921
    ## 56         plant.ini           init   1.151 [kilobytes] 2021-05-06 20:56:26    10     9    1   106
    ## 57    soil_plant.ini           init   0.316 [kilobytes] 2021-05-06 20:56:26     1     0    1    14
    ## 58      om_water.ini           init   0.638 [kilobytes] 2021-05-06 20:56:26     1     0    1    40
    ## 59         soils.sol          soils  41.053 [kilobytes] 2021-05-06 20:56:26   150     0    1  3171
    ## 60     nutrients.sol          soils   0.426 [kilobytes] 2021-05-06 20:56:26     1     0    1    26
    ## 61           lum.dtl decision_table  23.015 [kilobytes] 2021-05-06 20:56:26    NA    NA   NA    NA
    ## 62       res_rel.dtl decision_table 317.488 [kilobytes] 2021-05-06 20:56:26    NA    NA   NA    NA
    ## 63       scen_lu.dtl decision_table   9.514 [kilobytes] 2021-05-06 20:56:26    NA    NA   NA    NA
    ## 64       flo_con.dtl decision_table  10.361 [kilobytes] 2021-05-06 20:56:26    NA    NA   NA    NA
    ## 65       ls_unit.ele        regions   5.063 [kilobytes] 2021-05-06 20:56:26    50     0    1   357
    ## 66       ls_unit.def        regions   3.333 [kilobytes] 2021-05-06 20:56:26    51     1    1   256
    ## 67   aqu_catunit.ele        regions   5.165 [kilobytes] 2021-05-06 20:56:26    51     0    1   364

The summary info is now available for most of the files (get even more
detail by toggling `trim=FALSE`) and any variable names appearing among
the table headers of these files now becomes searchable using
`rswat_find`. This can be useful for tracking down a SWAT+ parameter
using keywords or SWAT2012 names. This uses fuzzy case-insensitive
matching (see R’s `?agrep` doc), which catches many of the name changes
in the SWAT2012 -\> SWAT+ updates.

eg. the following code finds the PET estimation method parameter ‘pet’,
which was called ‘IPET’ in SWAT2012:

``` r
# fuzzy > 0 allows inexact matches
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print
```

    ##       name string     class dim          file table  i  j
    ## 1 pet_file   null character   1     codes.bsn     1  1  1
    ## 2      pet      1   integer   1     codes.bsn     1  1  3
    ## 3 harg_pet   <NA>   numeric  50 hydrology.hyd     1 NA 14

We get three matches for ‘IPET’, located in two files. The ‘string’
field (second column) shows the plaintext representation of a parameter
in the SWAT+ config file (fifth column).

We can see that ‘pet’ (in ‘codes.bsn’) is set to 1. This codes for the
Penman-Monteith model for potential evapotranspiration (PET). The other
two matches, ‘pet\_file’ and ‘harg\_pet’, are an input file for observed
PET, and a solar radiation coefficient used with the Hargreaves-Samani
model; Neither is currently used so we don’t worry about them for now.

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

The ‘current\_value’ and ‘replacement’ fields are as expected, so we go
ahead and overwrite the file on disk with argument `preview=FALSE`. We
can reverse this change by the same process, manually specifying the old
value (pet = 1). Be aware that rswat doesn’t keep any kind of changelog,
so it’s always a good idea to have a backup of your “TxtInOut” folder,
or an easy way of regenerating it (eg with `qswat_run`), before
experimenting with the config files.

``` r
# write the changes
rswat_write(codes, preview=FALSE, quiet=TRUE)
```

“codes.bsn” is now configured such that SWAT+ simulations will now use
Hargreaves-Samani for PET. This model has a parameter ‘harg\_pet’, that
turned up earlier in the search for ‘IPET’. Since it’s no longer
inactive, we need to assign it a sensible value.

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

The parameters in this file are all length-50 column vectors. This is
because there are 50 HRUs in this model, and SWAT+ allows distinct PET
parameters for each one. For now we just set all HRUs to the same
default value appearing in the [I/O
docs](https://swatplus.gitbook.io/docs/user/io) - we can tune them
later.

``` r
# assign the default 'harg_pet' value in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)
```

Hargreaves-Samani may be the best choice for this project since we lack
the detailed data on humidity, wind, and solar energy required with
Penman-Monteith. SWAT+ can generate those missing data using a
stochastic process, but the result is imprecise at the daily scale.

## running a SWAT+ simulation

In computational model design and fitting there is a lot of back and
forth between parameter settings and simulations. We adjust parameters,
run a simulation, look for changes in state variables of interest, then
repeat (many times). `rswat` has utilities to streamline this process
from R.

`rswat_exec` runs a simulation by calling the SWAT+ executable, which
reads in parameters from the config files in the current project
directory. This includes the time period to simulate over (specified in
the file ‘time.sim’), and the time period to include in output files (in
‘print.prt’). These can be adjusted manually, or using a helper function
as shown below:

``` r
# `rswat_time` without arguments prints the current settings in 'time.sim'
rswat_time()
```

    ##        start          end 
    ## "1945-01-01" "1945-01-02"

If the model was created with `qswat_run` (as it was here), then these
dates should currently specify a one-day simulation at the very
beginning of the supplied weather time series. The code below changes
them to match the time series in `gage` (adjusting ‘print.prt’ to
match), then calls the SWAT+ executable to run a simulation with daily
timesteps:

``` r
# pass a range of dates or dataframe with a 'date' field to set up simulation start/end dates
rswat_time(gage, daily=TRUE)
```

    ##        start          end 
    ## "1973-09-01" "1979-10-29"

``` r
# run a simulation - the return value is a vector of output files generated
fout = rswat_exec()
```

    ## 
    ## >> finished (18.39 seconds runtime)

``` r
print(fout)
```

    ##  [1] "hru_nb_day.txt"            "hru_ls_day.txt"            "hru_pw_day.txt"            "lsunit_wb_day.txt"        
    ##  [5] "lsunit_nb_day.txt"         "lsunit_ls_day.txt"         "lsunit_pw_day.txt"         "basin_wb_day.txt"         
    ##  [9] "basin_nb_day.txt"          "basin_ls_day.txt"          "basin_pw_day.txt"          "crop_yld_aa.txt"          
    ## [13] "aquifer_day.txt"           "channel_sd_day.txt"        "channel_sdmorph_day.txt"   "basin_crop_yld_yr.txt"    
    ## [17] "basin_crop_yld_aa.txt"     "hydout_day.txt"            "hydin_day.txt"             "deposition_day.txt"       
    ## [21] "wetland_day.txt"           "basin_aqu_day.txt"         "basin_res_day.txt"         "recall_day.txt"           
    ## [25] "basin_cha_day.txt"         "basin_sd_cha_day.txt"      "basin_sd_chamorph_day.txt" "basin_psc_day.txt"        
    ## [29] "ru_day.txt"

## viewing simulation output data

the SWAT+ executable takes about 18-20 seconds on my machine to simulate
the seven-year time series in this example, producing output in the form
of .txt tables containing simulated state variables. There can be many
such output tables (100+) in a given simulation, depending on the
settings in ‘print.prt’ and ‘object.prt’.

The helper function `rswat_output` will catalog available output
variables and filenames, and import them into R as dataframes.

``` r
# get a dataframe with info on the available SWAT+ output files in the project directory
odf = rswat_output()
```

    ## parsing 81 SWAT+ output files...

``` r
# print the first few lines of the dataframe, omitting paths for tidyness
odf %>% select(-path) %>% head
```

    ##                file      name type step activated         group oid                  size            modified
    ## 1   aquifer_day.txt   aquifer  prt  day      TRUE       AQUIFER  NA 36146.989 [kilobytes] 2021-05-06 20:56:52
    ## 2  basin_ls_day.txt  basin_ls  prt  day      TRUE         BASIN  NA   378.442 [kilobytes] 2021-05-06 20:56:52
    ## 3  basin_nb_day.txt  basin_nb  prt  day      TRUE         BASIN  NA   540.702 [kilobytes] 2021-05-06 20:56:52
    ## 4  basin_pw_day.txt  basin_pw  prt  day      TRUE         BASIN  NA   675.708 [kilobytes] 2021-05-06 20:56:52
    ## 5  basin_wb_day.txt  basin_wb  prt  day      TRUE         BASIN  NA   999.996 [kilobytes] 2021-05-06 20:56:52
    ## 6 basin_aqu_day.txt basin_aqu  prt  day      TRUE BASIN_AQUIFER  NA   709.489 [kilobytes] 2021-05-06 20:56:52

Output files can be loaded as R dataframes by specifying their filename

``` r
# load an example file. Dates and units are added automatically 
fname.eg = 'aquifer_day.txt'
aqu.day = rswat_output(fname.eg)
aqu.day %>% str
```

    ## 'data.frame':    114750 obs. of  21 variables:
    ##  $ date   : Date, format: "1973-09-01" "1973-09-01" "1973-09-01" "1973-09-01" ...
    ##  $ unit   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ gis_id : int  11 12 21 22 31 32 41 42 51 52 ...
    ##  $ name   : chr  "aqu011" "aqu012" "aqu021" "aqu022" ...
    ##  $ flo    : Units: [mm] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dep_wt : Units: [m] num  3 3 3 3 3 ...
    ##  $ stor   : Units: [mm] num  350 350 350 350 350 ...
    ##  $ rchrg  : Units: [mm] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ seep   : Units: [mm] num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ revap  : Units: [mm] num  0.03 0.03 0.019 0.019 0.03 0.03 0.019 0.019 0.024 0.024 ...
    ##  $ no3_st : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ minp   : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orgn   : Units: [kg/ha] num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ orgp   : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rchrgn : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ nloss  : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no3gw  : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ seepno3: Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_cha: Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_res: Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_ls : Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...

In daily simulations over many years these output tables can get very
large, so it can be useful to specify a subset of output variables with
argument `vname`. Some columns, like ‘gis\_id’, are always loaded by
default. They are indexing fields needed to identify a row with a
specific spatial object in the model. However, this functionality -
along with date-parsing - can be switched off to get faster load times,
or for debugging purposes:

``` r
# print the first few lines for two particular variables (recharge and lateral flow to/from aquifer)
vname.eg = c('flo', 'rchrg')
rswat_output(fname.eg, vname=vname.eg, makedates=FALSE, showidx=FALSE) %>% head
```

    ##      flo  rchrg
    ## 1 0 [mm] 0 [mm]
    ## 2 0 [mm] 0 [mm]
    ## 3 0 [mm] 0 [mm]
    ## 4 0 [mm] 0 [mm]
    ## 5 0 [mm] 0 [mm]
    ## 6 0 [mm] 0 [mm]

When an output file is scanned in by this function its headers and units
are added to a database of known SWAT+ outputs. Variable names in this
database can then be searched by calling `rswat_output` without the
`fname` argument:

``` r
# search for output variables named "rchrg". The one loaded above is identified, and a few others
rswat_output(vname='rchrg') %>% print
```

    ##    name units type              file step
    ## 1 rchrg    mm  prt    aquifer_aa.txt year
    ## 2 rchrg    mm  prt   aquifer_day.txt  day
    ## 3 rchrg    mm  prt    aquifer_yr.txt year
    ## 4 rchrg    mm  prt  basin_aqu_aa.txt year
    ## 5 rchrg    mm  prt basin_aqu_day.txt  day
    ## 6 rchrg    mm  prt  basin_aqu_yr.txt year

Right now the database only includes the contents of `fname.eg`
(‘aquifer\_day.txt’), and `rswat_output()` only reports the files
currently in the SWAT+ project folder (“TxtInOut”). To get an exhaustive
list of possible outputs, rswat can optionally run a short (1-day)
simulation, requesting all outputs, in order to parse all output file
headers (before restoring the original state of the project folder)

``` r
# build database of SWAT+ outputs using `loadall` flag
odf = rswat_output(loadall=TRUE)
```

    ## running SWAT+ to generate all output files...
    ## parsing 108 SWAT+ output files...

``` r
#  print the first few lines (omit paths for tidiness)
odf %>% select(-path) %>% head
```

    ##               file     name type  step activated   group oid                  size            modified
    ## 1   aquifer_aa.txt  aquifer  prt  year     FALSE AQUIFER  NA     0.739 [kilobytes] 2021-05-06 20:57:01
    ## 2  aquifer_day.txt  aquifer  prt   day      TRUE AQUIFER  NA 36146.989 [kilobytes] 2021-05-06 20:57:01
    ## 3  aquifer_mon.txt  aquifer  prt month     FALSE AQUIFER  NA        NA [kilobytes]                <NA>
    ## 4   aquifer_yr.txt  aquifer  prt  year     FALSE AQUIFER  NA     0.739 [kilobytes] 2021-05-06 20:57:01
    ## 5  basin_ls_aa.txt basin_ls  prt  year     FALSE   BASIN  NA     0.442 [kilobytes] 2021-05-06 20:57:01
    ## 6 basin_ls_day.txt basin_ls  prt   day      TRUE   BASIN  NA   378.442 [kilobytes] 2021-05-06 20:57:01

Notice the filenames list now includes entries with NA fields for
‘size’, ‘modified’ (and ‘path’, though it is not shown here). These
are files not currently found in ‘TxtInOut’ but which can be enabled in
SWAT+ simulations. Since the above function call cached their headers
they are now searchable:

``` r
# repeat the search for "rchrg" and find a new (monthly outputs) matches
rswat_output(vname='rchrg') %>% print
```

    ##    name units type              file  step
    ## 1 rchrg    mm  prt    aquifer_aa.txt  year
    ## 2 rchrg    mm  prt   aquifer_day.txt   day
    ## 3 rchrg    mm  prt   aquifer_mon.txt month
    ## 4 rchrg    mm  prt    aquifer_yr.txt  year
    ## 5 rchrg    mm  prt  basin_aqu_aa.txt  year
    ## 6 rchrg    mm  prt basin_aqu_day.txt   day
    ## 7 rchrg    mm  prt basin_aqu_mon.txt month
    ## 8 rchrg    mm  prt  basin_aqu_yr.txt  year

## Comparing the simulated and observed data

Printing simulation data to the .txt output files is a bottleneck for
SWAT+. To speed things up it is better to request only the outputs you
need, and omit printing the others. These settings are found in
‘print.prt’ (for the normal outputs) and ‘object.prt’ (object
hydrograph outputs). Outputs that are currently toggled on are indicated
by the ‘activated’ field in the dataframe returned by `rswat_output`

``` r
# display the output files that are currently activated in SWAT+
rswat_output() %>% filter(activated) %>% pull(file)
```

    ##  [1] "aquifer_day.txt"           "basin_ls_day.txt"          "basin_nb_day.txt"          "basin_pw_day.txt"         
    ##  [5] "basin_wb_day.txt"          "basin_aqu_day.txt"         "basin_cha_day.txt"         "basin_psc_day.txt"        
    ##  [9] "basin_res_day.txt"         "basin_sd_chamorph_day.txt" "basin_sd_cha_day.txt"      "deposition_day.txt"       
    ## [13] "hru_ls_day.txt"            "hru_nb_day.txt"            "hru_pw_day.txt"            "hydin_day.txt"            
    ## [17] "hydout_day.txt"            "recall_day.txt"            "wetland_day.txt"           "lsunit_ls_day.txt"        
    ## [21] "lsunit_nb_day.txt"         "lsunit_pw_day.txt"         "lsunit_wb_day.txt"         "ru_day.txt"               
    ## [25] "channel_sd_day.txt"        "channel_sdmorph_day.txt"

All daily output files are active (this setting was applied by a call to
`rswat_time` above). There are quite a few of them, so execution is
relatively slow. If we turn off all off the standard output files, the
SWAT+ simulation will still run, and it completes much faster.

``` r
# open fifth table of 'print.prt', disable all output files and write the changes
print.prt = rswat_open('print.prt')[[5]]
print.prt[, names(print.prt) != 'objects'] = 'n'
rswat_write(print.prt, preview=F, quiet=TRUE)

# call the SWAT+ executable
rswat_exec()
```

    ## 
    ## >> finished (8.72 seconds runtime)

    ## [1] "basin_crop_yld_yr.txt" "basin_crop_yld_aa.txt"

On my machine the process completes in less than half the time (7-9
seconds versus 17-20). This time cost reduction may not matter when
running one-off simulations like we do here, but later on when fitting
parameters it becomes very significant, because we will need to run
thousands of simulations.

Note that two files related to crop yields were generated in spite of
the settings in ‘print.prt’. It is not clear how to inactivate them. But
since they are small, yearly tables, they likely don’t impact runtimes
very much.

In parameter fitting we need to generate daily outputs of discharge at
our gaged channel(s) to evaluate errors, ie to evaluate the objective
function. As we can see in the above example, requesting these outputs
via ‘print.prt’ is not ideal because the returned files print the data
for every channel, but we only need it for a few (usually one). SWAT+
has a special type of output file for the purpose of channel-specific
outputs, the object hydrograph (OHG).

``` r
# activate the object hydrograph for the outlet channel (id number 1)
id.outlet = 1
rswat_ohg(overwrite=TRUE, oid=id.outlet)
```

    ##   NUMB OBTYP OBTYPNO HYDTYP      FILENAME
    ## 1    1   sdc       1    tot sdc_1_tot.ohg

this function modifies “file.cio” and “object.prt” so that SWAT+
generates the plaintext output file “sdc\_1\_tot.ohg” in addition to any
others specified in “print.prt”. The `oid` argument specifies that we
only want data on the channel with ID code 1 (AKA ‘cha01’, usually the
main outlet of the catchment).

``` r
# call the SWAT+ executable
fout = rswat_exec()
```

    ## 
    ## >> finished (8.41 seconds runtime)

Note that this new file output seems not to affect runtimes

``` r
# check that the expected file has been generated
rswat_output() %>% filter(type=='ohg') %>% select(-path)
```

    ##            file name type step activated group oid                size            modified
    ## 1 sdc_1_tot.ohg  tot  ohg  day      TRUE   sdc   1 776.591 [kilobytes] 2021-05-06 20:57:25

``` r
# open the output 
rswat_output('sdc_1_tot.ohg') %>% str
```

    ## 'data.frame':    2250 obs. of  21 variables:
    ##  $ date: Date, format: "1973-09-01" "1973-09-02" "1973-09-03" "1973-09-04" ...
    ##  $ name: chr  "chandeg" "chandeg" "chandeg" "chandeg" ...
    ##  $ type: chr  "cha01" "cha01" "cha01" "cha01" ...
    ##  $ flo : Units: [m3/d] num  651416 2481 0 0 0 ...
    ##  $ sed : Units: [tons] num  1.28e+04 9.97e+01 3.04e-02 3.72e-02 2.98e-02 ...
    ##  $ orgn: Units: [kg] num  7.08e+01 1.52e-01 1.37e-06 2.12e-07 1.78e-07 ...
    ##  $ sedp: Units: [kg] num  4.31 9.27e-03 1.73e-07 6.13e-07 5.36e-07 ...
    ##  $ no3 : Units: [kg] num  0.00 1.09e-05 7.87e-07 5.38e-07 4.84e-07 ...
    ##  $ solp: Units: [kg] num  0.00 4.03e-05 1.35e-05 7.07e-06 5.04e-06 ...
    ##  $ chla: Units: [kg] num  0.00 0.00 4.59e-07 7.83e-08 3.93e-08 ...
    ##  $ nh3 : Units: [kg] num  2.11e-02 4.55e-04 5.99e-07 0.00 0.00 ...
    ##  $ no2 : Units: [kg] num  0.00 1.34e-04 2.17e-06 0.00 0.00 ...
    ##  $ cbod: Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dox : Units: [kg] num  4.52e+02 1.02e+01 6.21e-03 1.19e-01 8.85e-02 ...
    ##  $ san : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sil : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cla : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sag : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lag : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ grv : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ null: num  9.13 10.19 11.75 13.59 14.9 ...

This is a subset of the outflow variables that would normally appear in
the “SWAT-DEG\_CHANNEL” group. Note that the variable names in OHG files
omit the suffix "\_out". For example variable ‘flo’ in the OHG file
corresponds to variable ‘flo\_out’ in ‘channel\_sd\_day.txt’.

The units in these files are not necessarily the same - in particular,
the OHG discharge values are in per-day units whereas the normal output
files use per-second units. Different units means we can expect some
post-conversion differences due to numerical imprecision (see also \#32
and \#75
[here](https://infiniteundo.com/post/25509354022/more-falsehoods-programmers-believe-about-time)):

``` r
# load the OHG output file
ohg.out = rswat_output('sdc_1_tot.ohg')

# load the normal output file, trimming to only include values from the main outlet
prt.out = rswat_output('channel_sd_day.txt') %>% filter(gis_id==id.outlet) 

# join them and check the level of discrepancy
left_join(prt.out, ohg.out, by=c('date')) %>% 
  mutate( absdiff = abs( (flo_out - flo) ) ) %>% 
  pull(absdiff) %>% max
```

    ## 0.004930556 [m^3/s]

The errors appear to be small enough here to ignore for the purposes of
parameter fitting.

``` r
# OHG outputs can be switched off for now
rswat_ohg(overwrite=TRUE, delete=TRUE)
```

    ## NULL

The helper function `rswat_flo` is for quickly getting the OHG output
for a simulation. It handles all of the required settings adjustments
laid out above, and by default will restore all config files to their
original state afterwards). Simply pass it a range of dates and get back
the simulated discharge values:

``` r
# `oid=1` is set by default and 'dates' can be range of dates or a dataframe containing a 'date' column
ohg.out = rswat_flo(dates=gage, quiet=TRUE)
ohg.out %>% str
```

    ## 'data.frame':    2250 obs. of  2 variables:
    ##  $ date: Date, format: "1973-09-01" "1973-09-02" "1973-09-03" "1973-09-04" ...
    ##  $ flo : Units: [m3/d] num  651416 2481 0 0 0 ...

If ‘dates’ is a dataframe containing observations (columns names
starting with ‘flo’ or ‘obs’), `rswat_flo` can optionally be passed an
(anonymous) error function `errfn(x,y)`. In that case instead of
returning the discharge simulation values it passes them directly to
`errfn` (as `x`) along with any observed data (as `y`)

eg. this code passes the function `my_nse` (defined in
[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md))
which computes [Nash-Sutcliffe
efficiency](https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient)

``` r
# define `my_nse_2` to get a function of two variables only (`my_nse` has four)
my_nse_2 = function(x, y) {my_nse(x, y, L=2, normalized=TRUE)}

# run simulation and return NSE (normalized , with exponent 2)
rswat_flo(dates=gage, errfn=my_nse_2, quiet=TRUE) %>% print
```

    ## [1] 0.5069249

This is useful for linking up with optimization algorithms offered in
other R packages. For example if we want to maximize NSE for our
simulated hydrograph with respect to the parameter ‘harg\_pet’
(discussed earlier), we can construct an objective function like this:

``` r
# define an anonymous function that modifies parameters then evaluates NSE of resulting simulation 
obj.example = function(x)
{
  # open the container file for 'harg_pet', 
  hydro = rswat_open('hydrology.hyd')

  # assign value `x` to all HRUs then write to disk
  hydro$harg_pet = x
  rswat_write(hydro, preview=FALSE, quiet=TRUE)
  
  # run simulation then return the resulting NSE
  return( rswat_flo(dates=gage, errfn=my_nse_2, quiet=TRUE) )
}
```

A grid search now becomes very simple to program:

``` r
# run the objective function for a range of `harg_pet` values
harg.test = seq(0, 0.01, length=10)
harg.nse = sapply(harg.test, obj.example)

# identify and print the best one
idx.opt = which.max( harg.nse ) 
nse.opt = harg.nse[idx.opt]
harg.opt = harg.test[idx.opt]
print( paste0( 'NSE is maximized (', round(nse.opt, 3),') at harg_pet=', round(harg.opt, 3)) )
```

    ## [1] "NSE is maximized (0.549) at harg_pet=0.001"

`obj.example` can be altered to include multiple parameters in `x` (a
vector). In this way an objective function of any number of SWAT+
parameters can be constructed and passed to an optimizer. However,
before we can run more sophisticated optimization routines we need to
define bounds for the parameters.

TO BE CONTINUED

``` r
#rswat_open('cal_parms.cal') %>% filter( agrepl('harg', name, max.distance=2))


# TODO: 
# - replace rswat_obj, etc
# - swap in the fitted parameter values

# 
# 
# rswat_exec()
# rswat_output()
```
