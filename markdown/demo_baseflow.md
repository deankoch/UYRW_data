demo\_baseflow.R
================
Dean Koch
2021-04-25

**Mitacs UYRW project**

**demo\_baseflow.R**: (in development) example of aquifer model fitting
with SWAT+

## libraries

[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
[helper\_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md),
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

The [`airGR`](https://cran.r-project.org/web/packages/airGR/index.html)
is a collection of methods from INRAE-Antony (HYCAR Research Unit,
France), including an implementation of the [Oudin et
al. (2005)](https://doi.org/10.1016/j.jhydrol.2004.08.026) formula for
PET

``` r
library(airGR)
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

# numeric optimization for model fitting
library(dfoptim)

# low-level customizations for ggplot calls
library(grid)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
baseflow.meta = my_metadata('demo_baseflow', files.towrite, overwrite=TRUE, data.dir=sci.subdir)
```

    ## [1] "> writing metadata to: data/analysis/demo_baseflow_metadata.csv"

``` r
print(baseflow.meta[, c('file', 'type')])
```

    ##                                                                file        type
    ## example_name                                      big_c_nr_emigrant      string
    ## dir_qswat     H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant      string
    ## img_catchment                    graphics/my_baseflow_catchment.png png graphic
    ## img_hrus                              graphics/my_baseflow_hrus.png png graphic
    ## metadata                   data/analysis/demo_baseflow_metadata.csv         CSV

``` r
# metadata from previous R scripts in the workflow
basins.meta = my_metadata('get_basins')
subwatersheds.meta = my_metadata('get_subwatersheds')
streamgages.meta = my_metadata('get_streamgages')
meteo.meta = my_metadata('get_meteo')

# load PNWNAmet analysis to get weather inputs, USGS data for observed response
meteo = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
usgs.all = readRDS(here(streamgages.meta['USGS_data', 'file']))

# load some geographical features for plotting
uyrw = readRDS(here(basins.meta['boundary', 'file']))
lakes = readRDS(here(basins.meta['waterbody', 'file']))

# load the USGS data for an example subwatershed (see make_subwatersheds.R)
usgs.w = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))
idx = usgs.w$boundary %>% filter(catchment_name == nm) %>% pull(catchment_id)

# extract outlet locations, catchement boundary, channel network, 
pts = usgs.w$pts[usgs.w$pts$catchment_id==idx,] %>% na.omit
boundary = usgs.w$boundary[usgs.w$boundary$catchment_id==idx,] %>% na.omit
demnet = usgs.w$demnet[usgs.w$demnet$catchment_id==idx,] %>% na.omit
```

## set up simulation times

``` r
# pull gage data from this site
usgs = usgs.all$dat[[pts$site_no]]

# this gage has two observation periods separated by a few years
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))
```

    ## 1973-1979 1982-1985 
    ##      2250      1095

``` r
# for the demo we'll look at the longer 1970s period
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)
```

## overview plot

This plot shows the channel network for the demo catchment and its
location in the greater UYRW area, along with an inset containing a USGS
hydrograph of streamflow records for the catchment.
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)

``` r
catchment.png = here(baseflow.meta['img_catchment', 'file'])
if( !file.exists(catchment.png) )
{
  # plot grob for the 1980s gage data
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

## Build SWAT+ model and load into R

The data required to build a SWAT+ model for the above catchment are
already prepared in the `out.subdir` directory. We will use a series of
R helper functions (see
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md))
to send these data to QSWAT+, then on to SWAT+Editor, before loading the
results back into R.

This will generate the plaintext SWAT+ model files required to execute a
simulation, as well as the shapefiles normally displayed in QGIS for
visualization of watershed features (eg. HRUs).

``` r
# run only if the QSWAT+ demo directory doesn't exist yet
dir.qswat = baseflow.meta['dir_qswat', 'file']
if( !dir.exists(dir.qswat) )
{
  # set drop levels high for this demo to get a simple (fast) model
  config = list(skip_editor = FALSE, 
                drop_stream = 4e3, 
                drop_channel = (4e3) - 1)
  
  # write QSWAT+ input files using helper function
  qswat = qswat_setup(idx, usgs.w, projdir=dir.qswat, config=config, wipe=T)
  
  # pass these inputs to QSWAT+ for processing in PyQGIS, then SWAT+ Editor
  qswat_run(qswat)
  
} else {
  
  # load QSWAT+/SWAT+ project from disk when available
  qswat = my_metadata(basename(dir.qswat), data.dir=dir.qswat)
  
}
```

Note that the warning about a deep aquifers shapefile can be ignored for
now (see
[here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))

The dataframe `qswat` summarizes the QSWAT+ input files and parameters
used here:

``` r
qswat %>% select(type, description) %>% print
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

A helper function, `qswat_read`, loads the QSWAT+ shapefiles into R

``` r
wsh = qswat_read(qswat)
```

Another helper function, `qswat_plot`, displays the spatial arrangement
of HRUs. The next chunk calls this function and saves the results to a
file
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png)

``` r
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

The SWAT+ Editor step completed above by `qswat_run` writes the SWAT+
I/O config text files. The SWAT+ executable defines the model based on
their contents alone (ie independently of the QSWAT+ shapefiles loaded
above).

These are the files that need to be modified during parameter fitting -
we can read and manipulate them using the `rswat` helper functions. The
first step is to define the project directory (usually “TxtInOut”)

``` r
# assign the SWAT+ project directory in `rswat`
cio = rswat_cio(dir.qswat)
```

    ## setting `ciopath` to H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/file.cio 
    ## [1] "file.cio: written by SWAT+ editor v2.0.0 on 2021-04-24 13:59"

load all config files except decision tables, which are very large (and
not needed for now)

``` r
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
```

    ## [1] "file.cio: written by SWAT+ editor v2.0.0 on 2021-04-24 13:59"

This takes a few seconds to parse the files, which are summarized in the
returned dataframe

``` r
print(cio)
```

    ##                 file          group                size            modified nline nskip ntab  nvar
    ## 1           time.sim     simulation   0.168 [kilobytes] 2021-04-24 21:54:50     1     0    1    10
    ## 2          print.prt     simulation   3.389 [kilobytes] 2021-04-24 21:54:50    46     0    5   223
    ## 3         object.cnt     simulation   0.589 [kilobytes] 2021-04-24 13:59:18     1     0    1    42
    ## 4          codes.bsn          basin   0.599 [kilobytes] 2021-04-24 13:59:28     1     0    1    48
    ## 5     parameters.bsn          basin   1.296 [kilobytes] 2021-04-24 13:59:18     1     0    1    88
    ## 6    weather-sta.cli        climate   3.622 [kilobytes] 2021-04-24 13:59:18    16     0    1   153
    ## 7    weather-wgn.cli        climate  42.549 [kilobytes] 2021-04-24 13:59:18   223    16   16  2992
    ## 8            pcp.cli        climate   0.359 [kilobytes] 2021-04-24 13:59:08    16     0    1    17
    ## 9            tmp.cli        climate   0.357 [kilobytes] 2021-04-24 13:59:13    16     0    1    17
    ## 10           hmd.cli        climate   0.363 [kilobytes] 2021-04-24 13:59:04    16     0    1    17
    ## 11           wnd.cli        climate   0.356 [kilobytes] 2021-04-24 13:59:17    16     0    1    17
    ## 12           hru.con        connect   8.425 [kilobytes] 2021-04-24 13:59:18    50     0    1   663
    ## 13     rout_unit.con        connect  16.283 [kilobytes] 2021-04-24 13:59:18    50     0    1  1479
    ## 14       aquifer.con        connect  13.793 [kilobytes] 2021-04-24 13:59:18    51     0    1  1092
    ## 15        recall.con        connect   5.680 [kilobytes] 2021-04-24 13:59:18    25     0    1   442
    ## 16       chandeg.con        connect   5.629 [kilobytes] 2021-04-24 13:59:18    25     0    1   442
    ## 17       initial.cha        channel   0.321 [kilobytes] 2021-04-24 13:59:18     1     0    1    14
    ## 18     nutrients.cha        channel   1.166 [kilobytes] 2021-04-24 13:59:18     1     0    1    80
    ## 19   channel-lte.cha        channel   2.721 [kilobytes] 2021-04-24 13:59:18    25     0    1   156
    ## 20   hyd-sed-lte.cha        channel   8.712 [kilobytes] 2021-04-24 13:59:18    25     0    1   624
    ## 21     rout_unit.def   routing_unit   2.617 [kilobytes] 2021-04-24 13:59:18    50     0    1   204
    ## 22     rout_unit.ele   routing_unit   4.453 [kilobytes] 2021-04-24 13:59:18    50     0    1   306
    ## 23     rout_unit.rtu   routing_unit   5.269 [kilobytes] 2021-04-24 13:59:18    50     0    1   306
    ## 24      hru-data.hru            hru   8.940 [kilobytes] 2021-04-24 13:59:18    50     0    1   510
    ## 25          exco.exc           exco   2.922 [kilobytes] 2021-04-24 13:59:18    25     0    1   156
    ## 26       exco_om.exc           exco   7.137 [kilobytes] 2021-04-24 13:59:18    25     0    1   494
    ## 27        recall.rec         recall   1.572 [kilobytes] 2021-04-24 13:59:18    25     0    1   104
    ## 28       initial.aqu        aquifer   0.321 [kilobytes] 2021-04-24 13:59:18     1     0    1    14
    ## 29       aquifer.aqu        aquifer  13.481 [kilobytes] 2021-04-24 13:59:18    51     0    1   936
    ## 30     hydrology.hyd      hydrology  11.083 [kilobytes] 2021-04-24 13:59:29    50     0    1   765
    ## 31    topography.hyd      hydrology   9.158 [kilobytes] 2021-04-24 13:59:18   100     0    1   606
    ## 32         field.fld      hydrology   3.225 [kilobytes] 2021-04-24 13:59:18    50     0    1   204
    ## 33     tiledrain.str     structural   0.331 [kilobytes] 2021-04-24 13:59:18     1     0    1    18
    ## 34        septic.str     structural   1.211 [kilobytes] 2021-04-24 13:59:18     2     0    1    84
    ## 35   filterstrip.str     structural   0.341 [kilobytes] 2021-04-24 13:59:18     2     0    1    18
    ## 36     grassedww.str     structural   0.560 [kilobytes] 2021-04-24 13:59:18     3     0    1    36
    ## 37       bmpuser.str     structural   0.304 [kilobytes] 2021-04-24 13:59:18     1     0    1    18
    ## 38        plants.plt    hru_parm_db 194.373 [kilobytes] 2021-04-24 13:59:19   256     0    1 13878
    ## 39    fertilizer.frt    hru_parm_db   7.216 [kilobytes] 2021-04-24 13:59:19    59     0    1   480
    ## 40       tillage.til    hru_parm_db   8.311 [kilobytes] 2021-04-24 13:59:19    78     0    1   553
    ## 41     pesticide.pes    hru_parm_db  49.945 [kilobytes] 2021-04-24 13:59:19   233     0    1  3510
    ## 42         urban.urb    hru_parm_db   1.832 [kilobytes] 2021-04-24 13:59:19     9     0    1   130
    ## 43        septic.sep    hru_parm_db   4.644 [kilobytes] 2021-04-24 13:59:19    26     0    1   324
    ## 44          snow.sno    hru_parm_db   0.326 [kilobytes] 2021-04-24 13:59:19     1     0    1    18
    ## 45          harv.ops            ops   1.273 [kilobytes] 2021-04-24 13:59:19    14     0    1    90
    ## 46         graze.ops            ops   1.626 [kilobytes] 2021-04-24 13:59:19    12     0    1    91
    ## 47           irr.ops            ops   0.662 [kilobytes] 2021-04-24 13:59:19     4     0    1    45
    ## 48      chem_app.ops            ops   1.897 [kilobytes] 2021-04-24 13:59:19    12     0    1   130
    ## 49          fire.ops            ops   0.265 [kilobytes] 2021-04-24 13:59:19     3     0    1    16
    ## 50         sweep.ops            ops   0.170 [kilobytes] 2021-04-24 13:59:19     1     0    1     8
    ## 51       landuse.lum            lum   1.613 [kilobytes] 2021-04-24 13:59:19     5     0    1    84
    ## 52       cntable.lum            lum  11.089 [kilobytes] 2021-04-24 13:59:19    52     0    1   424
    ## 53 cons_practice.lum            lum   3.243 [kilobytes] 2021-04-24 13:59:19    38     0    1   156
    ## 54     ovn_table.lum            lum   1.755 [kilobytes] 2021-04-24 13:59:19    20     0    1   105
    ## 55     cal_parms.cal            chg  15.543 [kilobytes] 2021-04-24 13:59:19   184     1    1   921
    ## 56         plant.ini           init   1.151 [kilobytes] 2021-04-24 13:59:19    10     9    1   106
    ## 57    soil_plant.ini           init   0.316 [kilobytes] 2021-04-24 13:59:19     1     0    1    14
    ## 58      om_water.ini           init   0.638 [kilobytes] 2021-04-24 13:59:19     1     0    1    40
    ## 59         soils.sol          soils  41.053 [kilobytes] 2021-04-24 13:59:19   150     0    1  3171
    ## 60     nutrients.sol          soils   0.426 [kilobytes] 2021-04-24 13:59:19     1     0    1    26
    ## 61           lum.dtl decision_table  23.015 [kilobytes] 2021-04-24 13:59:19    NA    NA   NA    NA
    ## 62       res_rel.dtl decision_table 317.488 [kilobytes] 2021-04-24 13:59:20    NA    NA   NA    NA
    ## 63       scen_lu.dtl decision_table   9.514 [kilobytes] 2021-04-24 13:59:20    NA    NA   NA    NA
    ## 64       flo_con.dtl decision_table  10.361 [kilobytes] 2021-04-24 13:59:20    NA    NA   NA    NA
    ## 65       ls_unit.ele        regions   5.063 [kilobytes] 2021-04-24 13:59:20    50     0    1   357
    ## 66       ls_unit.def        regions   3.333 [kilobytes] 2021-04-24 13:59:20    51     1    1   256
    ## 67   aqu_catunit.ele        regions   5.165 [kilobytes] 2021-04-24 13:59:20    51     0    1   364

Each row of `cio` is a file containing a group of model parameters. The
‘nvar’ column indicates how many distinct fields there are in the file
(nrow \* ncol, summed over all of the tables, including headers). There
are a lot of them, so if you are new to SWAT, check out the
[I/O](https://swatplus.gitbook.io/docs/user/io) and
[theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs.

Note that as of April 2021, I can find no theory guides published for
recent versions of SWAT (including SWAT+), so the link above is for a
document from 2009. Many aspects of the the model have not changed since
then, and the old theory guide remains a good reference. However, most
variable and parameter names have changed in SWAT+.

Parameters can be examined by calling `rswat_open` with a filename. eg
we will be interested in the aquifer model parameters in ‘aquifer.aqu’ -
the code below prints the last few lines of the relevant table:

``` r
# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print
```

    ##              file   group               size            modified nline nskip ntab nvar
    ## 1     aquifer.con connect 13.793 [kilobytes] 2021-04-24 13:59:18    51     0    1 1092
    ## 2     initial.aqu aquifer  0.321 [kilobytes] 2021-04-24 13:59:18     1     0    1   14
    ## 3     aquifer.aqu aquifer 13.481 [kilobytes] 2021-04-24 13:59:18    51     0    1  936
    ## 4 aqu_catunit.ele regions  5.165 [kilobytes] 2021-04-24 13:59:20    51     0    1  364

``` r
# this one contains the main process model parameters
rswat_open('aquifer.aqu') %>% tail %>% print
```

    ##    id        name     init gw_flo dep_bot dep_wt no3_n sol_p carbon flo_dist bf_max alpha_bf revap rchg_dp spec_yld hl_no3n
    ## 46 46      aqu232 initaqu1   0.05      10      3     0     0    0.5       50      1     0.05  0.02    0.05     0.05       0
    ## 47 47      aqu241 initaqu1   0.05      10      3     0     0    0.5       50      1     0.05  0.02    0.05     0.05       0
    ## 48 48      aqu242 initaqu1   0.05      10      3     0     0    0.5       50      1     0.05  0.02    0.05     0.05       0
    ## 49 49      aqu251 initaqu1   0.05      10      3     0     0    0.5       50      1     0.05  0.02    0.05     0.05       0
    ## 50 50      aqu252 initaqu1   0.05      10      3     0     0    0.5       50      1     0.05  0.02    0.05     0.05       0
    ## 51 51 aqu_deep006 initaqu1   0.00     100     20     0     0    0.5       50      1     0.01  0.00    0.00     0.03       0
    ##    flo_min revap_min
    ## 46       3         5
    ## 47       3         5
    ## 48       3         5
    ## 49       3         5
    ## 50       3         5
    ## 51       0         0

The helper function `rswat_find` can be useful for tracking down a SWAT+
parameter using keywords or SWAT2012 names. This uses fuzzy
case-insensitive matching (see R’s `?agrep` doc), which does well to
catch the most common name changes in the SWAT2012 -\> SWAT+ updates

``` r
# eg. find the PET estimation method 'pet', which was called 'IPET' in SWAT2012
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print
```

    ##       name string     class dim          file table  i  j
    ## 1 pet_file   null character   1     codes.bsn     1  1  1
    ## 2      pet      2   integer   1     codes.bsn     1  1  3
    ## 3 harg_pet   <NA>   numeric  50 hydrology.hyd     1 NA 14

The ‘string’ column above shows the plaintext for this parameter in the
SWAT+ config file, and ‘file’ the filename. We can see that ‘pet’ (in
file ‘codes.bsn’) is set to 1. This codes for the Penman-Monteith model.

The other two matches, ‘pet\_file’ and ‘harg\_pet’, are an input file
for observed PET, and a solar radiation coefficient used with the
Hargreaves-Samani model (neither is currently used).

## Adjusting a SWAT+ model

To change a parameter, open its container file with `rswat_open`, modify
it in R, then write the change with `rswat_write`. eg. switching to the
Hargreaves-Samani model for PET (coded as pet=2) can be done like this:

``` r
# open the file and copy to an R dataframe called `codes`
codes = rswat_open('codes.bsn')
print(codes)
```

    ##   pet_file wq_file pet event crack rtu_wq sed_det rte_cha deg_cha wq_cha nostress cn c_fact carbon baseflo uhyd sed_cha
    ## 1                    2     0     0      1       0       0       0      1        0  0      0      0       0    1       0
    ##   tiledrain wtable soil_p abstr_init atmo_dep stor_max headwater
    ## 1         0      0      0          0        a        0         0

``` r
# By default, SWAT+ estimates PET with Penman-Monteith (pet = 0, see I/O docs)

# Hargreaves-Samani is coded as pet = 2: make this change
codes$pet = 2

# default behaviour of `rswat_write` is to preview the requested change. This looks fine...
rswat_write(codes)
```

    ##  [1] string    line_num  field_num start_pos end_pos   class     skipped   header    tabular   short     long      ljust    
    ## [13] rjust     rjust_col start_col end_col   nprec     name      dim       i         j         table     file     
    ## <0 rows> (or 0-length row.names)

``` r
# ... so we overwrite the file on disk with argument `preview=FALSE`
rswat_write(codes, preview=FALSE, quiet=TRUE)
```

The SWAT+ executable will now use the Hargreaves-Samani method for
estimation. The Hargreaves-Samani coefficient that turned up earlier is
no longer inactive, so we need to assign it a nonzero value. For now I
just use the example value appearing in the I/O docs (we can tune it
later).

``` r
# open the container file for 'harg_pet'
hydro = rswat_open('hydrology.hyd')

# The parameters in this file are all length-50 vectors. Distinct values are allowed for each HRU
print( nrow(hydro) )
```

    ## [1] 50

``` r
print( head(hydro) )
```

    ##    name lat_ttime lat_sed can_max esco epco orgn_enrich orgp_enrich cn3_swf bio_mix perco lat_orgn lat_orgp harg_pet latq_co
    ## 1 hyd01         0       0       1 0.95    1           0           0    0.95     0.2   0.9        0        0   0.0023    0.01
    ## 2 hyd02         0       0       1 0.95    1           0           0    0.95     0.2   0.5        0        0   0.0023    0.01
    ## 3 hyd03         0       0       1 0.95    1           0           0    0.95     0.2   0.9        0        0   0.0023    0.01
    ## 4 hyd04         0       0       1 0.95    1           0           0    0.95     0.2   0.5        0        0   0.0023    0.01
    ## 5 hyd05         0       0       1 0.95    1           0           0    0.95     0.2   0.5        0        0   0.0023    0.01
    ## 6 hyd06         0       0       1 0.95    1           0           0    0.95     0.2   0.9        0        0   0.0023    0.01

``` r
# assign the same example value for 'harg_pet' in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)
```

Hargreaves-Samani may be the best choice for this project since we lack
the detailed data on humidity, wind, and solar energy required with
Penman-Monteith. SWAT+ can generate those missing data (this is its
default behaviour) but since their values are generated from a
stochastic process calibrated on long-term historical norms, they are
very imprecise at the daily scale.

## Viewing/running simulations

the SWAT+ executable takes a few seconds to simulate the full seven-year
time series in this example, producing output in the form of .txt tables
containing simulated state variables.

There are many (100+) such output tables and the task of printing any of
them to a file can slow down SWAT+ considerably. To speed things up it
is better to request specific outputs and omit printing the others. This
requires some knowledge of what output variables are available and where
they are located.

The `rswat_output` function returns a dataframe with info on the
available SWAT+ output files:

``` r
# copy the dataframe to variable `odf`
odf = rswat_output()

# print some summary info
head(odf)
```

    ##              file     name type step   group oid              size            modified
    ## 1  aquifer_aa.txt  aquifer  prt year aquifer  NA 0.739 [kilobytes] 2021-04-25 00:33:12
    ## 2  aquifer_yr.txt  aquifer  prt year aquifer  NA 0.739 [kilobytes] 2021-04-25 00:33:12
    ## 3 basin_ls_aa.txt basin_ls  prt year   basin  NA 0.442 [kilobytes] 2021-04-25 00:33:12
    ## 4 basin_ls_yr.txt basin_ls  prt year   basin  NA 0.442 [kilobytes] 2021-04-25 00:33:12
    ## 5 basin_nb_aa.txt basin_nb  prt year   basin  NA 0.702 [kilobytes] 2021-04-25 00:33:12
    ## 6 basin_nb_yr.txt basin_nb  prt year   basin  NA 0.702 [kilobytes] 2021-04-25 00:33:12
    ##                                                                                               path
    ## 1  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_aa.txt
    ## 2  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_yr.txt
    ## 3 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_aa.txt
    ## 4 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_yr.txt
    ## 5 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_nb_aa.txt
    ## 6 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_nb_yr.txt

``` r
print( nrow(odf) )
```

    ## [1] 56

Output files are loaded as R dataframes by specifying `fname` (and,
optionally, `vname` for subsets)

``` r
# load an example file. Dates and units are incorporated automatically
hydout.yr = rswat_output(fname='hydout_yr.txt')
head( hydout.yr )
```

    ##         date   name type objtyp typ_no hyd_typ fraction       flo      sed   orgn   sedp    no3   solp   chla    nh3    no2
    ## 1 1945-01-02 rtu012   ru    sdc      1     sur     0.84 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ## 2 1945-01-02 rtu012   ru     ru      1     sur     0.16 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ## 3 1945-01-02 rtu012   ru     ru      1     lat     1.00 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ## 4 1945-01-02 rtu012   ru    aqu     12     rhg     1.00 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ## 5 1945-01-02 rtu022   ru    sdc      2     sur     0.82 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ## 6 1945-01-02 rtu022   ru     ru      3     sur     0.18 0 [m^3/s] 0 [tons] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg] 0 [kg]
    ##     cbod    dox      san      sil      cla      sag      lag      grv   null
    ## 1 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]
    ## 2 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]
    ## 3 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]
    ## 4 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]
    ## 5 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]
    ## 6 0 [kg] 0 [kg] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [tons] 0 [°C]

``` r
# a subset of columns (output variables) can be specified with `vname`
head( rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction')) )
```

    ##         date   name type objtyp typ_no hyd_typ fraction       flo
    ## 1 1945-01-02 rtu012   ru    sdc      1     sur     0.84 0 [m^3/s]
    ## 2 1945-01-02 rtu012   ru     ru      1     sur     0.16 0 [m^3/s]
    ## 3 1945-01-02 rtu012   ru     ru      1     lat     1.00 0 [m^3/s]
    ## 4 1945-01-02 rtu012   ru    aqu     12     rhg     1.00 0 [m^3/s]
    ## 5 1945-01-02 rtu022   ru    sdc      2     sur     0.82 0 [m^3/s]
    ## 6 1945-01-02 rtu022   ru     ru      3     sur     0.18 0 [m^3/s]

Some columns are loaded by default because they are important
identifiers (eg spatial IDs). This functionality (along with
date-parsing) can be switched off to get faster load times, or for
debugging:

``` r
head( rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction'), makedates=FALSE, showidx=FALSE) )
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
this database can also be searched by calling `rswat_output` without the
`fname` argument:

``` r
# search for output variables named "fraction". The one loaded above is identified:
rswat_output(vname='fraction')
```

    ##    line_num field_num start_pos end_pos     name units          file type step index   class
    ## 10        2        10        98     105 fraction  <NA> hydout_yr.txt  prt year FALSE numeric

``` r
# search for 'flow_in'. Only a partial match is found: 
rswat_output(vname='flo_in')
```

    ## no exact matches for "flo_in", trying partial matches...

    ##    line_num field_num start_pos end_pos name units          file type step index   class
    ## 11        2        11       120     122  flo m^3/s hydout_yr.txt  prt year FALSE numeric

Right now the database only includes the contents of ‘hydout\_yr.txt’,
and `rswat_output()` only reports the files currently in the SWAT+
project folder (“TxtInOut”). To include ALL SWAT+ output variables, we
can run a (short) simulation where all output files are requested, then
parse its outputs:

``` r
# build database of SWAT+ outputs
odf = rswat_output(loadall=TRUE)
```

    ## running SWAT+ to generate all output files...
    ## backing up files... running SWAT+... restoring backup... done
    ## parsing 108 SWAT+ output files...

``` r
head( odf )
```

    ##               file     name type  step   group oid              size            modified
    ## 1   aquifer_aa.txt  aquifer  prt  year aquifer  NA 0.739 [kilobytes] 2021-04-25 00:36:10
    ## 2  aquifer_day.txt  aquifer  prt   day aquifer  NA    NA [kilobytes]                <NA>
    ## 3  aquifer_mon.txt  aquifer  prt month aquifer  NA    NA [kilobytes]                <NA>
    ## 4   aquifer_yr.txt  aquifer  prt  year aquifer  NA 0.739 [kilobytes] 2021-04-25 00:36:10
    ## 5  basin_ls_aa.txt basin_ls  prt  year   basin  NA 0.442 [kilobytes] 2021-04-25 00:36:10
    ## 6 basin_ls_day.txt basin_ls  prt   day   basin  NA    NA [kilobytes]                <NA>
    ##                                                                                               path
    ## 1  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_aa.txt
    ## 2                                                                                             <NA>
    ## 3                                                                                             <NA>
    ## 4  H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/aquifer_yr.txt
    ## 5 H:/UYRW_data/data/analysis/baseflow_big_c_nr_emigrant/Scenarios/Default/TxtInOut/basin_ls_aa.txt
    ## 6                                                                                             <NA>

This only takes a few seconds, and only temporarily alters the files in
‘TxtInOut’ (a backup is restored after it completes). Notice the
filenames list now includes entries with NA size - these are files not
currently found in ‘TxtInOut’, but which can be enabled in SWAT+
simulations. Their headers (and units) have now been cached, and are
searchable:

``` r
# repeat the search for 'flo_in' and find many exact matches. 
nrow( rswat_output(vname='flo_in') ) 
```

    ## [1] 28

``` r
# pipes are useful for narrowing the results of a search
rswat_output(vname='flo_in') %>% filter( step == 'day' ) %>% filter( units == 'ha m' )
```

    ##   line_num field_num start_pos end_pos   name units              file type step index   class
    ## 1        2         8        64      69 flo_in  ha m basin_cha_day.txt  prt  day FALSE numeric

Searches will return exact matches first, and if nothing is found, the
function reverts to partial (sub-string) matching with increasing
fuzziness until it finds something:

``` r
# an example of fuzzy partial matching
rswat_output(vname='recharge')
```

    ## no exact matches for "recharge", trying partial matches...

    ##     line_num field_num start_pos end_pos   name units              file type  step index   class
    ## 11         2        11       115     119  rchrg    mm    aquifer_aa.txt  prt  year FALSE numeric
    ## 18         2        18       219     224 rchrgn kg/ha    aquifer_aa.txt  prt  year FALSE numeric
    ## 35         2        11       115     119  rchrg    mm   aquifer_day.txt  prt   day FALSE numeric
    ## 42         2        18       219     224 rchrgn kg/ha   aquifer_day.txt  prt   day FALSE numeric
    ## 59         2        11       115     119  rchrg    mm   aquifer_mon.txt  prt month FALSE numeric
    ## 66         2        18       219     224 rchrgn kg/ha   aquifer_mon.txt  prt month FALSE numeric
    ## 83         2        11       115     119  rchrg    mm    aquifer_yr.txt  prt  year FALSE numeric
    ## 90         2        18       219     224 rchrgn kg/ha    aquifer_yr.txt  prt  year FALSE numeric
    ## 523        2        11       115     119  rchrg    mm  basin_aqu_aa.txt  prt  year FALSE numeric
    ## 530        2        18       219     224 rchrgn kg/ha  basin_aqu_aa.txt  prt  year FALSE numeric
    ## 547        2        11       115     119  rchrg    mm basin_aqu_day.txt  prt   day FALSE numeric
    ## 554        2        18       219     224 rchrgn kg/ha basin_aqu_day.txt  prt   day FALSE numeric
    ## 571        2        11       115     119  rchrg    mm basin_aqu_mon.txt  prt month FALSE numeric
    ## 578        2        18       219     224 rchrgn kg/ha basin_aqu_mon.txt  prt month FALSE numeric
    ## 595        2        11       115     119  rchrg    mm  basin_aqu_yr.txt  prt  year FALSE numeric
    ## 602        2        18       219     224 rchrgn kg/ha  basin_aqu_yr.txt  prt  year FALSE numeric
