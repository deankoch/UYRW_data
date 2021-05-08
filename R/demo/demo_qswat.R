#' ---
#' title: "demo_qswat.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_qswat.R**: Demonstration of SWAT+ model-building on Big Creek
#' 
#' This script demonstrates some of the `rswat` helper functions related running and interpreting the output
#' of [QSWAT+](https://swatplus.gitbook.io/docs/user/qswat+), the QGIS3-based SWAT+ model builder.
#' 
#' It uses the example of Big Creek, near Emigrant Montana. The data for this catchment was prepared earlier
#' by running [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md),
#' which processed data collected by the "get_*.R" functions (namely,
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md))
#' for the full URYW region.
#' 
#' The output is a large set of SWAT+ model project files with default parameters, along with shapefiles created
#' by QSWAT+ for visualization (and a few graphics, see below). These project files will be used in
#' the sequel to this script, 
#' [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md)
#' where we show how manage model parameters and run SWAT+ simulations.  
#' 

#'
#' ## libraries and helper functions

# `here` manages working directories
library(here)

#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
#' [helper_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md), and
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
#' load some other required libraries, global variables, and helper functions.
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))

# low-level R graphics control
library(grid)

#'
#' ## project data

#' This USGS gage name specifies one of the catchments to use as demo (see
#' [make_subwatersheds.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md))
nm = 'big_c_nr_emigrant'

#' Make a list of the files created by this script
{
  files.towrite = list(
    
    # USGS gage to use as example
    c(name='example_name',
      file=nm,
      type='string',
      description='catchment/station name of a USGS streamgage used in the demo'),
    
    # directory for SWAT+ model files
    c(name='dir_qswat',
      file=here(file.path(demo.subdir, paste0('baseflow_', nm))),
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

#' write this filename metadata to disk using a function from
#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md])
baseflow.meta = my_metadata('demo_qswat', files.towrite, overwrite=TRUE, data.dir=demo.subdir)

#' calls to `my_metadata` will now load the file info from disk (here and in other R sessions).
#' In the same way, we locate some required output files created in earlier scripts, 
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)), and
#' [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md),
#' in the code below:

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


#' 
#' ## set time period of interest

# pull gage data from this site
usgs = usgs.ts$dat[[pts$site_no]]

# identify contiguous subsets in the time series
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))

#' The gage has two long uninterrupted periods, with a gap of 3 years. For the demos
#' we'll look at the longer 1970s period
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)

#' ## overview plot
#' 
#' The chunk below makes a plot of the channel network for the catchment, its location in the greater
#' upper Yellowstone river watershed (UYRW) region, and a hydrograph of the selected USGS discharge records
#' 
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)
#' 

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


#' ## build a SWAT+ model and visualize it with R
#' 
#' This section builds a SWAT+ model for the selected catchment by calling
#' [a python script](https://gitlab.com/rob-yerc/swat)
#' that runs [QSWAT+](https://swatplus.gitbook.io/docs/user/qswat+), then
#' [SWAT+Editor](https://swatplus.gitbook.io/docs/user/editor). It creates a large project
#' folder that includes QSWAT+ shapefiles for viewing spatial aspects of the model with GIS
#' software, and the plaintext SWAT+ input files which are passed to 
#' [the executable](https://swatplus.gitbook.io/docs/installation) that runs simulations.
#' 
#' The inputs to QSWAT+ include data layers on topography, soils, plant communities, 
#' meteorology, and general watershed layout parameters. In our case these have already been
#' prepared for all catchments in the URYW by earlier scripts (see above). The chunk below sets
#' a couple of watershed layout parameters and prepares input data for QSWAT+ using helper
#' functions from [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md) 

# pick a QSWAT+ project directory root. NOTE: This directory gets overwritten by `qswat_setup`!!
dir.qswat = baseflow.meta['dir_qswat', 'file']

# set drop levels high for this demo to get a simple (fast) model
config = list(skip_editor = FALSE, 
              drop_stream = 4e3, 
              drop_channel = (4e3) - 1)

# write QSWAT+ input files using a helper function
qswat.meta = qswat_setup(idx, usgs.w, projdir=dir.qswat, config=config, wipe=TRUE, quiet=TRUE)

#' The dataframe `qswat.meta` logs the QSWAT+ input files and parameters used here:
qswat.meta %>% select(type, description) %>% print

#' At this point you could open up QGIS3 and specify these files in the QSWAT+ GUI to build the model
#' manually. The R function `qswat_run` will do this automatically via PyQGIS. Note that some model-layout
#' settings (eg. floodplain estimation method) are hard-coded into these automated routines for now. We
#' plan to expand the options list `config` in future to allow users to control all model-layout settings.  
#' 
#' For this small 50-channel example it takes about 30 seconds to build input files and run QGIS+, and another
#' minute or so to run SWAT+Editor and finish the job. Expect it to take longer on examples with lower drop
#' thresholds (ie. more HRUs QSWAT+/SWAT+Editor to process), higher-resolution DEMs (slowing down TauDEM),
#' or larger geographical extents (both problems apply).
#' 

# pass these inputs to QSWAT+ for processing in PyQGIS, then SWAT+ Editor
qswat_run(qswat.meta)

#' This markdown report omits a large amount of console output here, mostly redirected from QSWAT+,
#' that shows a checklist of jobs completing along with any warnings that may come up. Warnings about the deep
#' aquifers shapefile can be ignored (see
#' [here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))
#' as they are only a visualization issue, not impacting the SWAT+ model input files.
#' 
#' The function `qswat_read` will load the QSWAT+ shapefiles created by `qswat_run` as R `sf` objects

# load the QSWAT+ shapefiles into R by passing the dataframe returned by `qswat_setup`  
wsh = qswat_read(qswat.meta)
wsh %>% summary

#' These shapefiles are mostly for visualization, but they are also useful for matching up locations to
#' the spatial ID keys in SWAT+ input/output tables. For example we will be interested in the outlet
#' (the USGS gage site): the following code prints info about the associated channel and HRUs

# compute distances between `pts` (the outlet point geometry) and SWAT+ channels, identifying the nearest one
outlet.distances = st_distance(wsh$cha, pts)
print(outlet.distances)
idx.min = which.min(outlet.distances)
id.outlet = wsh$cha$Channel[idx.min]

# print attributes for the associated SWAT+ spatial objects
wsh$cha %>% filter(Channel == id.outlet)
wsh$hru %>% filter(Channel == id.outlet)

#' From the first table we see the channel of interest has ID 1 (column 'Channel'), and from the second
#' we see that this channel is associated with LSUs 11 and 12 (floodplain and upslope, respectively), which
#' are assigned HRU IDs 12 and 11 respectively.
#'
#'  `qswat_plot` returns a quick plot of the QSWAT+ shapefiles. The chunk below makes a graphic showing
#' the spatial arrangement of HRUs and saves the results to a file
#' 
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png)
#' 

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

#' These shapefiles are helpful for understanding the model, but not required by SWAT+ to run a simulation.
#' Instead, the SWAT+ executable will read in routing tables derived from these shapefiles (along many other
#' parameters) which are stored in a directory of plaintext configuration files, usually called 'TxtInOut'.
#' The next demo script,
#' [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md),
#' will show how to access and change these files using `rswat`.

#+ include=FALSE
# Development code
#my_markdown('demo_qswat', 'R/demo')


