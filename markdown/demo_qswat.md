demo\_qswat.R
================
Dean Koch
2021-05-08

**Mitacs UYRW project**

**demo\_qswat.R**: Demonstration of SWAT+ model-building on Big Creek

This script demonstrates some of the `rswat` helper functions related
running and interpreting the output of
[QSWAT+](https://swatplus.gitbook.io/docs/user/qswat+), the QGIS3-based
SWAT+ model builder.

It uses the example of Big Creek, near Emigrant Montana. The data for
this catchment was prepared earlier by running
[make\_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md),
which processed data collected by the "get\_\*.R" functions (namely,
[get\_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
[get\_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
[get\_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
[get\_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
[get\_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md))
for the full URYW region.

The output is a large set of SWAT+ model project files with default
parameters, along with shapefiles created by QSWAT+ for visualization
(and a few graphics, see below). These project files will be used in the
sequel to this script,
[demo\_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md)
where we show how manage model parameters and run SWAT+ simulations.

## libraries and helper functions

``` r
# `here` manages working directories
library(here)
```

[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
[helper\_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md),
and
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
load some other required libraries, global variables, and helper
functions.

``` r
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))

# low-level R graphics control
library(grid)
```

## project data

This USGS gage name specifies one of the catchments to use as demo (see
[make\_subwatersheds.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md))

``` r
# the catchment name in the datasets loaded below
nm = 'big_c_nr_emigrant'

# name for output directory created below
nmo = paste0('demo_', nm)
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
    
    # gage data for the catchment outlet
    c(name='gage',
      file=file.path(demo.subdir, paste0(nmo, '_gage.rds')), 
      type='R dataframe',
      description='USGS gage data for the catchment outlet'),
    
    # directory for SWAT+ model files
    c(name='dir_qswat',
      file=here(file.path(demo.subdir, nmo)),
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
      description='image showing SWAT+ model HRUs in the catchment'),
    
    # copy of the TxtInOut directory of the SWAT+ model
    c(name='txtinout',
      file=file.path(demo.subdir, paste0(nmo, '_txtinout.zip')),
      type='zip archive', 
      description='copy of the TxtInOut directory of the SWAT+ model generated by qswat_run')
  )
}
```

write this filename metadata to disk using a function from
[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md%5D)

``` r
baseflow.meta = my_metadata('demo_qswat', files.towrite, overwrite=TRUE, data.dir=demo.subdir)
```

    ## [1] "> writing metadata to: data/demo/demo_qswat_metadata.csv"

calls to `my_metadata` will now load the file info from disk (here and
in other R sessions). In the same way, we locate some required output
files created in earlier scripts,
[get\_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
[get\_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
[get\_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)),
and
[make\_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md),
in the code below:

``` r
# load file metadata from four earlier scripts 
basins.meta = my_metadata('get_basins')
streamgages.meta = my_metadata('get_streamgages')
subwatersheds.meta = my_metadata('make_subwatersheds')
meteo.meta = my_metadata('get_meteo')

# load some geographical features for plotting
uyrw = readRDS(here(basins.meta['boundary', 'file']))
lakes = readRDS(here(basins.meta['waterbody', 'file']))

# load PNWNAmet analysis (weather inputs used in "make_subwatersheds.R")
meteo = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))

# load USGS observed discharge time series (large list of dataframes and point geometries)
usgs.ts = readRDS(here(streamgages.meta['USGS_data', 'file']))

# load outlet point, catchement boundary, channel network for the Big Creek USGS site
usgs.w = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))
idx = usgs.w$boundary %>% filter(catchment_name == nm) %>% pull(catchment_id)
pts = usgs.w$pts[usgs.w$pts$catchment_id==idx,] %>% na.omit
boundary = usgs.w$boundary[usgs.w$boundary$catchment_id==idx,] %>% na.omit
demnet = usgs.w$demnet[usgs.w$demnet$catchment_id==idx,] %>% na.omit
```

## set time period of interest

``` r
# pull gage data from this site
usgs = usgs.ts$dat[[pts$site_no]]

# identify contiguous subsets in the time series
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))
```

    ## 1973-1979 1982-1985 
    ##      2250      1095

The gage has two long uninterrupted periods, with a gap of 3 years. For
the demos we’ll look at the longer 1970s period

``` r
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)

# save a copy for easy loading later
gage.path = here(baseflow.meta['gage', 'file'])
saveRDS(gage, gage.path)
```

## overview plot

The chunk below makes a plot of the channel network for the catchment,
its location in the greater upper Yellowstone river watershed (UYRW)
region, and a hydrograph of the selected USGS discharge records

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)

``` r
# skip if the file exists already
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
[SWAT+Editor](https://swatplus.gitbook.io/docs/user/editor). It creates
a large project folder that includes QSWAT+ shapefiles for viewing
spatial aspects of the model with GIS software, and the plaintext SWAT+
input files which are passed to [the
executable](https://swatplus.gitbook.io/docs/installation) that runs
simulations.

The inputs to QSWAT+ include data layers on topography, soils, plant
communities, meteorology, and general watershed layout parameters. In
our case these have already been prepared for all catchments in the URYW
by earlier scripts (see above). The chunk below sets a couple of
watershed layout parameters and prepares input data for QSWAT+ using
helper functions from
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)

``` r
# pick a QSWAT+ project directory root where `qswat_setup` can write project files
dir.qswat = baseflow.meta['dir_qswat', 'file']

# set drop levels high for this demo to get a simple (fast) model
config = list(skip_editor = FALSE, 
              drop_stream = 4e3, 
              drop_channel = (4e3) - 1)

# write QSWAT+ input files using a helper function (NOTE: `wipe=TRUE` erases directory `dir.qswat`!)
qswat.meta = qswat_setup(idx, usgs.w, projdir=dir.qswat, config=config, wipe=TRUE, quiet=TRUE)
```

The dataframe `qswat.meta` logs the QSWAT+ input files and parameters
used here:

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
    ## metadata                  CSV                list files of files written by demo_big_c_nr_emigrant.R

At this point you could open up QGIS3 and specify these files in the
QSWAT+ GUI to build the model manually. The R function `qswat_run` will
do this automatically via PyQGIS. Note that some model-layout settings
(eg. floodplain estimation method) are hard-coded into these automated
routines for now. We plan to expand the options list `config` in future
to allow users to control all model-layout settings.

For this small 50-channel example it takes about 30 seconds to build
input files and run QGIS+, and another minute or so to run SWAT+Editor
and finish the job. Expect it to take longer on examples with lower drop
thresholds (ie. more HRUs QSWAT+/SWAT+Editor to process),
higher-resolution DEMs (slowing down TauDEM), or larger geographical
extents (both problems apply).

``` r
# pass these inputs to QSWAT+ for processing in PyQGIS, then SWAT+ Editor
qswat_run(qswat.meta)
```

    ## 
    ## >> finished

This markdown report omits a large amount of console output here, mostly
redirected from QSWAT+, that shows a checklist of jobs completing along
with any warnings that may come up. Warnings about the deep aquifers
shapefile can be ignored (see
[here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))
as they are only a visualization issue, not impacting the SWAT+ model
input files.

The function `qswat_read` will load the QSWAT+ shapefiles created by
`qswat_run` as R `sf` objects

``` r
# load the QSWAT+ shapefiles into R by passing the dataframe returned by `qswat_setup`  
wsh = qswat_read(qswat.meta)
wsh %>% summary
```

    ##     Length Class       Mode
    ## sta      7 -none-      list
    ## dem 516637 RasterLayer S4  
    ## out      6 sf          list
    ## cha     17 sf          list
    ## hru     21 sf          list
    ## lsu     16 sf          list
    ## sub     13 sf          list
    ## bou      1 sfc_POLYGON list

The shapefiles are mostly for visualization, but they are also useful
for matching up locations to the spatial ID keys in SWAT+ input/output
tables. For example we will be interested in the outlet (the USGS gage
site): the following code prints info about the associated channel and
HRUs

``` r
# compute distances between `pts` (the outlet point geometry) and SWAT+ channels, identifying the nearest one
outlet.distances = st_distance(wsh$cha, pts)
print(outlet.distances)
```

    ## Units: [m]
    ##              [,1]
    ##  [1,]    11.22867
    ##  [2,]  1615.15795
    ##  [3,]  3220.78761
    ##  [4,]  5749.15921
    ##  [5,]  7839.16973
    ##  [6,] 10968.93163
    ##  [7,]  7839.16973
    ##  [8,] 11251.17236
    ##  [9,] 12455.40564
    ## [10,]  1573.87429
    ## [11,] 12779.60358
    ## [12,]  8375.11448
    ## [13,] 13502.13870
    ## [14,] 12765.21539
    ## [15,] 11164.62729
    ## [16,] 12455.01195
    ## [17,] 13502.13870
    ## [18,] 10960.33444
    ## [19,]  9593.07252
    ## [20,]  9476.49628
    ## [21,]  8414.02451
    ## [22,]  5749.15921
    ## [23,]  2218.06429
    ## [24,]  3184.95246
    ## [25,]  2220.65803

``` r
idx.min = which.min(outlet.distances)
id.outlet = wsh$cha$Channel[idx.min]

# print attributes for the associated SWAT+ spatial objects
wsh$cha %>% filter(Channel == id.outlet)
```

    ## Simple feature collection with 1 feature and 16 fields
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 508039.6 ymin: 5016874 xmax: 510188.1 ymax: 5017546
    ## projected CRS:  WGS 84 / UTM zone 12N
    ## # A tibble: 1 x 17
    ##   LINKNO Channel ChannelR Subbasin AreaC Order  Len2  Slo2  Wid2  Dep2 MinEl MaxEl Reservoir  Pond LakeIn LakeOut
    ## *  <int>   <int>    <int>    <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <int> <int>  <int>   <int>
    ## 1     24       1        0        6 17106     3 2632.  2.63  28.2  1.02 1594. 1664.         0     0      0       0
    ## # ... with 1 more variable: geometry <LINESTRING [m]>

``` r
wsh$hru %>% filter(Channel == id.outlet)
```

    ## Simple feature collection with 2 features and 20 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 506656.5 ymin: 5015491 xmax: 510255.3 ymax: 5019546
    ## projected CRS:  WGS 84 / UTM zone 12N
    ##   id Channel LINKNO LSUID Subbasin  Landscape Category Slope    Len1   Csl Wid1 Dep1   Landuse   Soil SlopeBand          long
    ## 1 11       1     24    12        6    Upslope        2 30.00 3337.02 23.26 2.74 0.21      migs 697292    0-9999 -110.8908 [°]
    ## 2 12       1     24    11        6 Floodplain        1 22.16  539.03 12.32 0.99 0.11 frse_tems 696034    0-9999 -110.8865 [°]
    ##            lat         elev            area frac                       geometry
    ## 1 45.30914 [°] 1677.417 [m] 3512535.9 [m^2] 84.4 MULTIPOLYGON (((508267.9 50...
    ## 2 45.30847 [°] 1668.767 [m]  647691.4 [m^2] 15.6 MULTIPOLYGON (((508831.9 50...

From the first table we see the channel of interest has ID 1 (column
‘Channel’), and from the second we see that this channel is associated
with LSUs 11 and 12 (floodplain and upslope, respectively), which are
assigned HRU IDs 12 and 11 respectively.

`qswat_plot` returns a quick plot of the QSWAT+ shapefiles. The chunk
below makes a graphic showing the spatial arrangement of HRUs and saves
the results to a file

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
  tmap_save(tm=wsh.tmap, filename=wsh.png, height=1400, width=2000, pointsize=12)
}
```

These shapefiles are helpful for understanding the model, but not
required by SWAT+ to run a simulation. Instead, the SWAT+ executable
will read in routing tables derived from these shapefiles (along many
other parameters) which are stored in a directory of plaintext
configuration files, usually called ‘TxtInOut’.

It is useful to make a backup of this directory before changing
anything, so we zip a copy now

``` r
# set up filenames and directories
dir.txtinout = wsh$sta$paths[['txtio']]
fname.txtinout = list.files(dir.txtinout)
zip.path = here(baseflow.meta['txtinout', 'file'])

# zip all files in TxtInOut (j = junk paths, 9 = compress better, X = drop extended attribuets)
zip(zip.path, files=file.path(wsh$sta$paths[['txtio']], fname.txtinout), flags='-j9X')
```

The next demo script,
[demo\_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md),
will show how to access and change these files using `rswat`.