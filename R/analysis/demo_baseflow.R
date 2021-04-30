#' ---
#' title: "demo_baseflow.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_baseflow.R**: (in development) an example of aquifer model fitting with SWAT+
#' 
#' This script also demonstrates some of the core functionality of the `rswat` helper functions,
#' including: building a SWAT+ model; accessing/changing its parameters; running simulations;
#' loading outputs; and fitting parameters to observed data.
#' 

#'
#' ## libraries
#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
#' [helper_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md), and
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
#' load required libraries, global variables, and some helper functions.

library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))

#' [`Evapotranspiration`](https://cran.r-project.org/web/packages/Evapotranspiration/index.html) package
#' implements several methods to estimate PET
library('Evapotranspiration')

#' The [`airGR`](https://cran.r-project.org/web/packages/airGR/index.html) is a collection of methods
#' from INRAE-Antony (HYCAR Research Unit, France), including the
#' [Oudin et al. (2005)](https://doi.org/10.1016/j.jhydrol.2004.08.026) PET estimator
library(airGR)
# TODO: check out the CemaNeige model for snow accumulation and melt (also from this package)

#' The [`baseflow`](https://cran.r-project.org/web/packages/baseflow/index.html) R package is an
#' implementation of the method of
#' [Pelletier and Andréassian (2020)](https://hess.copernicus.org/articles/24/1171/2020/)
#' for estimating baseflow (and quickflow) from hydrographs of daily streamflow and precipitation totals.
library(baseflow)

# numeric optimization for model fitting
library(dfoptim)

# low-level R graphics control
library(grid)


#'
#' ## project data

#' A USGS gage name specifies one of the catchments to use as demo (see
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

#' write this filename metadata to disk using a
#' [helper function](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md])
baseflow.meta = my_metadata('demo_baseflow', files.towrite, overwrite=TRUE, data.dir=sci.subdir)

#' calls to `my_metadata` will now load the file info from disk (here and in other R sessions).
#' eg. the following code accesses the file info from previous scripts 
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md), and
#' [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md)
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


#' 
#' ## set up simulation times
#' 

# pull gage data from this site
usgs = usgs.all$dat[[pts$site_no]]

# identify contiguous subsets in the time series
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))

#' The gage has two long uninterrupted periods, with a gap of 3 years. For the demo we'll look at the longer 1970s period
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)

#' ## overview plot
#' 
#' The chunk below makes a plot of the channel network for the catchment, its location in the greater
#' upper Yellowstone river watershed (UYRW) region, and a hydrograph of the selected USGS discharge records
#' 
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)
#' 
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
#' [SWAT+Editor](https://swatplus.gitbook.io/docs/user/editor). This creates a large project
#' folder that includes QSWAT+ shapefiles (for viewing spatial aspects of the model with GIS
#' software), and SWAT+ input files which configure
#' [the executable](https://swatplus.gitbook.io/docs/installation) that runs simulations.
#' 
#' The inputs to QSWAT+ include data layers on topography, soils, plant communities, 
#' meteorology, and general watershed layout parameters. In our case these have already been
#' prepared for all catchments in the URYW by the scripts
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md),
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md),
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md),
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md),
#' [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md)
#'
#'  The chunk below sets a couple of watershed layout parameters and builds the model:

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

#' This markdown report omits a large amount of console output here (mostly redirected from QSWAT+)
#' that shows a checklist of jobs completing, and any warnings that come up.  Warnings about the deep
#' aquifers shapefile can be ignored (see
#' [here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))
#' as they seem to be a visualization problem that doesn't impact the SWAT+ input files.
#' 
#' The entire process for this small 50-HRU example takes about 1-2 minutes. Expect it to take longer
#' on examples with more HRUs (ie more database entries for QSWAT+/SWAT+Editor to process),
#' or more high-resolution DEM pixels (larger or more detailed DEMs slow down TauDEM).
#' 
#' The dataframe `qswat.meta` summarizes the QSWAT+ input files and parameters used here:
qswat.meta %>% select(type, description) %>% print

#' Helper functions `qswat_read` and `qswat_plot` from
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md) can be used to
#' display the QSWAT+ shapefiles. The chunk below makes a plot of the spatial arrangement of HRUs
#' and saves the results to a file
#' 
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png)
#' 

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

#' 
#' ## managing SWAT+ configuration files in R
#' 
#' The SWAT+ executable runs simulations according to the settings written
#' in a directory of text files (usually called 'TxtInOut'). This is a large and complicated
#' set of files that define all parameters of model. The `rswat` helper functions include tools
#' for managing and cataloging these files. 
#' 
#' If you are new to SWAT, check out the [I/O](https://swatplus.gitbook.io/docs/user/io)
#' and [theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs. Note that the second link
#' is for a document from 2009 (the most recent, as far as I am aware), and although core aspects of the the
#' model have not changed much since then, many variable and parameter names are different in SWAT+.
#' 
#' The first step is to define the project directory and see what we have:

# assign the SWAT+ project directory in `rswat`
cio = rswat_cio(dir.qswat)

#' load all config files into memory except decision tables, which are very large (and not
#' needed for now). This takes a moment to parse the files, which are then summarized in the
#' returned dataframe
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
print(cio)

#' Each row of `cio` is a file containing a group of model parameters. The 'nvar' column indicates
#' how many distinct fields there are in the file (nrow * ncol, summed over all of the tables,
#' including headers). 
#' 
#' We will be interested in the aquifer model parameters in 'aquifer.aqu' - the chunk below prints the
#' last few lines of the relevant table:

# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print

# this one contains the main process model parameters 
rswat_open('aquifer.aqu') %>% str

#' The helper function `rswat_find` can be useful for tracking down a SWAT+ parameter using keywords or
#' SWAT2012 names. This uses fuzzy case-insensitive matching (see R's `?agrep` doc), which catches many
#' of the name changes in the SWAT2012 -> SWAT+ updates. eg. the following code finds the PET estimation
#' method parameter 'pet', which was called 'IPET' in SWAT2012:

# fuzzy = 1 to allow inexact matches
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print

#' The 'string' column above shows the plaintext representation of parameters in the SWAT+ config files
#' listed in column 'file'. We can see that 'pet' (in file 'codes.bsn') set to 1. This codes for the
#' Penman-Monteith model for potential evapotranspiration (PET). The other two matches, 'pet_file' and
#' 'harg_pet', are an input file for observed PET, and a solar radiation coefficient used with the
#' Hargreaves-Samani model (neither is currently used).  
#' 
#' ## adjusting a SWAT+ model
#' 
#' To change a parameter, open its container file with `rswat_open`, modify it in R, then write the change with
#' `rswat_write`. eg. switching to the Hargreaves-Samani model for PET (coded as 'pet' = 2) can be done like this:
 
# open the file and put its contents into an R dataframe called `codes`
codes = rswat_open('codes.bsn')
codes %>% str

#' By default, SWAT+ estimates PET with Penman-Monteith ('pet' = 1), so we have to change the PET code:

# change PET method to Hargreaves-Samani 
codes$pet = 2

#' The default behaviour of `rswat_write` is to preview the requested change:

# preview changes
rswat_write(codes) %>% str

#' The new 'string' field looks fine so we go ahead and overwrite the file on disk with argument `preview=FALSE` 

# write the changes
rswat_write(codes, preview=FALSE, quiet=TRUE)

#' The SWAT+ executable will now use the Hargreaves-Samani method for estimation.
#' The Hargreaves-Samani coefficient, 'harg_pet', that turned up earlier in the search for 'IPET' is no longer
#' inactive, so we need to assign it a sensible value. For now I just use the example value appearing in the
#' I/O docs - we will tune it later.

# open the container file for 'harg_pet' and print a summary
hydro = rswat_open('hydrology.hyd')
hydro %>% str

#' The parameters in this file are all length-50 (column) vectors. This is because there are 50 HRUs in this
#' model, and SWAT+ allows distinct values for each one in this case. The code below assigns the same default
#' value for the coefficient to all of them

# assign the default 'harg_pet' value in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)

#' Hargreaves-Samani may be the best choice for this project since we lack the detailed data on humidity,
#' wind, and solar energy required with Penman-Monteith. SWAT+ can generate those missing data using a
#' stochastic process, but the result is imprecise at the daily scale.
#' 

#' 
#' ## running a SWAT+ simulation
#' 
#' In model design and fitting there is a lot of back and forth between these configuration files and the
#' SWAT+ executable - we adjust a parameter, run a simulation, look for changes in state variables
#' of interest, then repeat. `rswat` has utilities to streamline this process from R.
#'
#' `rswat_exec` runs a simulation by calling the SWAT+ executable, with parameters loaded from the config
#' files in the current project directory. This includes the time period to simulate over (specified in
#' the file 'time.sim'), and the time period to include in output files (in 'print.prt'). These can be
#' adjusted manually, or using a helper function as shown here:
#'  

# `rswat_tinit` without arguments prints the current settings in 'time.sim'
rswat_tinit()

#' If the model was created with `qswat_run` (as it was here), then these dates should currently specify
#' a one-day simulation at the very beginning of the supplied weather time series. The code below changes
#' them to match the time series in `gage` (adjusting 'print.prt' to match), then calls the SWAT+ executable
#' to run a simulation with daily timesteps:

# pass a range of dates to set up simulation start/end dates
rswat_tinit(range(gage$date), daily=TRUE)

# run a simulation - the return value is a vector of output files generated
fout = rswat_exec()
print(fout)

#' 
#' ## viewing simulation output data
#' 
#' the SWAT+ executable takes about ten seconds to simulate the seven-year time series in this example,
#' producing output in the form of .txt tables containing simulated state variables. There can be many such
#' output tables (100+) in a given simulation, depending on the settings in 'print.prt' and 'object.prt'.
#' 
#' The helper function `rswat_output` will catalog available output variables and filenames, and import
#' them into R as dataframes.
#' 

# get a dataframe with info on the available SWAT+ output files in the project directory
odf = rswat_output()

# print the total number of rows (files), and the first few lines
print( nrow(odf) )
head(odf)

#' Output files are loaded as R dataframes by specifying `fname`

# load an example file. 
fname.eg = 'aquifer_day.txt'
aqu.day = rswat_output(fname=fname.eg)
aqu.day %>% str

#' A subset of columns (output variables) can be specified with `vname`

# print the first few lines for two particular variables (recharge and lateral flow to/from aquifer)
vname.eg = c('flo', 'rchrg')
rswat_output(fname=fname.eg, vname=vname.eg) %>% head

#' Notice dates and units are incorporated automatically. Some additional columns are also loaded
#' by default because they are important identifiers (eg spatial IDs). This functionality (along
#' with date-parsing) can be switched off to get faster load times, or for debugging:
rswat_output(fname=fname.eg, vname=vname.eg, makedates=FALSE, showidx=FALSE) %>% head

#' When an output file is scanned by this function, its headers and units are cached for faster
#' loading in subsequent calls. Variable names in this database can then be searched by calling
#' `rswat_output` without the `fname` argument:
#' 

# search for output variables named "water_temp". The one loaded above is identified:
rswat_output(vname='rchrg') %>% str

# search for 'water_temp'. No matches among the cached headers... 
rswat_output(vname='water_temp') %>% str

#' 
#' Right now the database only includes the contents of `fname.eg` ('aquifer_day.txt'), and
#' `rswat_output()` only reports the files currently in the SWAT+ project folder ("TxtInOut").
#' To get a more exhaustive list `rswat` can run a (1-day) simulation, requesting all outputs,
#' then parse the output files before restoring the original state of the project folder

# build database of SWAT+ outputs
odf = rswat_output(loadall=TRUE)
odf %>% head

#' Notice the filenames list now includes entries with NA fields for 'size', 'modified', and 'path'.
#' These are files not currently found in 'TxtInOut' but which can be enabled in SWAT+ simulations.
#' Since the above function call cached their headers (and units) they are now searchable:

# repeat the search for 'water_temp' and find several exact matches now:
rswat_output(vname='water_temp') %>% pull(file)

#' 
#' ## Comparing the simulated and observed data
#' 
#' Printing simulation data to plaintext output files can slow down SWAT+. To speed things up it is
#' better to request specific outputs and omit printing the others. These settings are found in
#' 'print.prt' (for the normal outputs) and 'object.prt' (object hydrograph outputs).
#' 
#' Outputs that are currently toggled on are indicated by the 'activated' field in the dataframe
#' returned by `rswat_output`
#' 
# display the output files that are currently activated in SWAT+
odf %>% filter(activated) %>% pull(file)

#' If we turn them all off the SWAT+ simulation will still run, and it completes much faster
#' 
#' 

# call the SWAT+ executable
print( Sys.time() )
rswat_exec()
print( Sys.time() )

# open 'print.prt' and disable all output files, then write the changes
print.prt = rswat_open('print.prt')
print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
rswat_write(print.prt[[5]], preview=F, quiet=TRUE)

# call the SWAT+ executable
print( Sys.time() )
rswat_exec()
print( Sys.time() )


#' TODO: continue this

# TODO: 
# - replace rswat_daily, rswat_obj, etc
# - swap in the fitted parameter values


#'

#+ include=FALSE
# TODO: tidy up this older code
if(0)
{

  
  
  # ohg = rswat_output() %>% filter(type=='ohg')
  # 
  # for(idx in 1:nrow(ohg))
  # {
  #   cat('--------')
  #   cat(ohg$file[idx])
  #   cat(' : ')
  #   cat(names(rswat_output(ohg$file[idx])))
  #   cat('\n')
  #   
  # }
  # 
  # 
  # 
  # 
  # rswat_ohg(overwrite=TRUE, otype='out', htype='tot')
  # rswat_exec()
  # 
  # 
  # wsh
  # 
  # 
  # head()
  # head(rswat_output('ohg_hru_1_tot.ohg'))
  # n.sec = 60 * 60 * 24
  # 
  # flow.chan = rswat_output('channel_sd_day.txt') %>% filter(gis_id == 1) %>% select(date, flo_in, flo_out)
  # flow.ohg = rswat_output('ohg_sdc_1_tot.ohg') %>% select(flo)
  # 
  # #flow.ohg = rswat_output('ohg_hru_1_tot.ohg') 
  # flow = cbind(flow.chan, flow.ohg)
  # my_tsplot(flow)
  # 
  # (flow[['flo']] / n.sec)  drop_units(flow[['flo_out']])
  # 
  # rswat_fout()
  # 
  # rswat_open('print.prt')
  # 
  # 
  # dean = c(1275, 241, 98)
  # tess = c(227, 37, 46, 133)
  # 
  # ( sum(dean) + sum(tess) ) / 2
  
  
  
  ## DEVELOPMENT: OHG file handling
  
  # set up a short 25-day run
  rswat_tinit('1995-01-05')
  rswat_tinit(25)
  
  # test some combinations
  rswat_ohg(otype=c('hru'), htype=c('sur'), overwrite=T)
  rswat_exec()
  
  
  rswat_oscan()
  .rswat$stor$output$fname 
  
  
  odf %>% filter(step=='day') %>% filter( grepl('flo', name) )
  
  odf[grepl('flo', odf$name),]
  
  
  # To run a simulation, pass the observed gage data (or just the dates) to `rswat_daily`
  daily.default = rswat_daily(gage.subset)
  
  
  # To run a simulation, pass the observed gage data (or just the dates) to `rswat_daily`
  gage.subset = gage #%>% filter(date < as.Date('1976-01-01'))
  daily.default = rswat_daily(gage.subset, quiet=TRUE)
  str(daily.default)
  
  # plot the simulated discharge at the outlet of the catchment and to compare with USGS observations
  flow.default = cbind(gage.subset, daily.default$flo_out)
  names(flow.default) = c('date', 'USGS_main_outlet', 'SWAT_main_outlet')
  my_tsplot(flow.default)
  
  #' SWAT+ outputs many different physical variables and groups them into output text files according
  #' to the the type of watershed feature they belong to. The function `rswat_output` can be used to
  #' access these files directly
  
  # When called without arguments, `rswat_output` prints the available output files:
  print(rswat_output())
  
  #' "channel_sd_day.txt" and "channel_sdmorph_day.txt" are particularly important, as they contain the
  #' daily streamflow simulations that we need for model fitting. The latter is loaded by default in calls
  #' to `rswat_daily`, which return the discharge values from the main outlet channel after running a
  #' simulation. To access the data for other channels, or other physical variables associated with channel
  #' flow, we can use `rswat_output`
  
  # print a summary of the data in the default output file
  chan.default = rswat_output('channel_sdmorph_day')
  str(chan.default)
  
  # extract flow for a different channel (id #10) and plot the time series
  upstream.default = chan.default %>% filter(gis_id==10) %>% select(flo_in)
  flow.default2 = cbind(flow.default, setNames(upstream.default, 'SWAT_upstream_channel'))
  my_tsplot(flow.default2)
  
  #' A more detailed set of variables, including water temperature and a sediment budget can be found in
  #' the second channel output file, "channel_sd_day":
  
  names(rswat_output('channel_sd_day'))
  
  
  #'  
  rswat_cio(wipe=T)
  rswat_ohg(overwrite=TRUE)
  rswat_open('object.prt')
  
  daily.default = rswat_daily(gage.subset, quiet=F)
  
  
  
  overwrite=FALSE
  otype='sdc'
  oid=1
  htype='tot'
  fname=NULL 
  ciopath=NULL
  
  
  rswat_cio(.rswat$ciopath)
  
  # only load 'print.prt' as needed
  print.prt = rswat_open('print.prt', quiet=TRUE)
  
  # grab the full list of valid object names
  object.all = print.prt[[5]]$objects
  
  # reset all values to no-print, then toggle requested files
  print.prt[[5]][] = 'n'
  
  
  # write the changes to print.prt
  rswat_write(print.prt[[5]], preview=F, quiet=TRUE)
  
  
  # run the simulation and return output data as R dataframe
  
  rswat_exec(quiet=FALSE)
  
  
  
  chan.default = rswat_output('channel_sdmorph_aa')
  rswat_output('basin_nb_aa.txt')
  rswat_output('flow_test')
  fname='flow_test' 
  
  
  str( rswat_output('channel_sdmorph_aa') )
  rswat_output('channel_sdmorph_day')
  rswat_output('channel_sdmorph_day', vname=c('flo_in', 'flo_out'))
  
  fname = 'basin_nb_aa.txt'
  fname = 'channel_sdmorph_day'
  vname='flo_in'
  vname=c('flo_in', 'flo_out')
  vname=NULL
  add_units=TRUE
  add_dates=TRUE
  
  
  #' By default  `rswat_daily` only requests "channel_sdmorph_day.txt". As we are interested in baseflow, we should
  #' toggle the option to print "aquifer_day.txt", which contains simulations of geologic storage under
  #' soil, channels and reservoirs
  #' 
  
  
  
  # # try changing some of the parameters to values fitted earlier
  # bpath = 'H:/UYRW_data/data/analysis/big_creek_fitted'
  # fname = c('snow.sno', 'parameters.bsn', 'aquifer.aqu')
  # rswat_backup(bpath, fname, bmode='restore')
  # 
  # 
  
  
  
  # hydro$esco = 0.5
  # hydro$harg_pet = 0.0010
  # rswat_write(hydro, preview=FALSE)
  # 
  # sim.calib = rswat_daily(gage.subset, quiet=TRUE)
  # df.plot = data.frame(gage.subset, SWAT_default=sim.default$flo_out, SWAT_calib=sim.calib$flo_out)
  # my_tsplot(df.plot)
  
  
##################

# change the simulation time to match overlap of weather and gage time series
range(usgs.eg$dat[[1]]$date)
plot(flow~date, data=usgs.eg$dat[[1]])
range(meteo.eg$dates)

# # this gage has two observation periods, separated by decades. There's a helper function for this
# dates.all = my_split_series(usgs.eg$dat[[1]]$date, meteo.eg$dates)
# print(sapply(dates.all, length))
# idx.dates = which.max( sapply(dates.all, length) )
# dates = dates.all[[idx.dates]]
# range(dates)
# 
# # pull a copy of a subset of gage data to focus on
# dates.crop = dates[dates > as.Date('1974-01-01')]
# gage = usgs.eg$dat[[1]] %>% filter(date %in% dates.crop)

# run a simulation
textio = dirname(.rswat$ciopath)
my_gage_objective(gage, textio)

## plot the simulation streamflow output

# find the weather data associated with this catchment
idx.dates = meteo.eg$dates %in% dates.crop
nm.grid = gsub('.pcp', '', gsub('pcp_', '', rswat_open('weather-sta.cli')$pcp))
wdat = meteo.eg$tables$pcp[idx.dates, nm.grid]



# load the simulation output and merge with observed flow, mean precip
object = c(channel_sd='daily')
fname = paste0(names(object), '_day')
vname = 'flo_out'
sim = rswat_output(fname, vname) %>% 
  filter( gis_id == 1 ) %>%
  mutate( obs = gage$flow[match(date, gage$date)] ) %>%
  mutate( pcp = rowMeans(wdat)[match(date, dates.crop)] ) %>%
  mutate( pcp_plot = -scales::rescale(pcp, c(0, max(obs)/10)) )

# set up colours 
colors = c('USGS record'='firebrick', 'SWAT+ simulation'='navy', 'precipitation (scaled)'='lightblue')

# plot the calibrated version
nse.text = paste('Nash-Sutcliffe coefficient:', round(my_nse(sim$flo_out, sim$obs, L=2), 2))

ggp.flow.cal = ggplot(data=sim, aes(date)) +
  geom_line(aes(y=0),  col='orange') +
  geom_line(aes(y=drop_units(flo_out), color='SWAT+ simulation'), alpha=0.5) + 
  geom_line(aes(y=drop_units(obs), color='USGS record')) +
  geom_line(aes(y=pcp_plot, color='precipitation (scaled)')) +
  theme_minimal() +
  labs(x = 'date', y='flow (m3/sec)', color='') +
  ggtitle(paste(gsub('_', ' ', nm), paste0('(', nse.text, ')'))) +
  scale_color_manual(values=setNames(adjustcolor(colors, alpha.f=0.8), names(colors))) +
  theme(legend.position = c(8,9)/10, legend.key.width = unit(2, 'cm')) +
  guides(color = guide_legend(override.aes = list(size=1, alpha=1)))

ggp.flow.cal
ggp.flow.new = ggp.flow.cal

## add to this plot by varying parameters and rerunning simulations
rswat_open('aquifer.aqu', reload=TRUE)
rswat_open('aquifer.aqu')

#rswat_open(reload=TRUE)

# change CN calculation method
# codes.bsn = rswat_open('codes.bsn')
# codes.bsn$cn = 1
# rswat_write(codes.bsn, preview=F)

# define the parameters to calibrate in a dataframe (based on Grusson et al 2015)
cal.snow = rswat_find(include='snow.sno') %>% filter(name != 'snow_init')
cal.misc = rswat_find('esco|can_max|surq_lag')
cal.gw = rswat_find('gw|revap|revap_min|alpha_bf|rchg_dp|dep_bot|bf_max|dep_wt|flo_dist')

# we will calibrate the deep aquifer separately 
idx.deep = grepl('deep', rswat_open('aquifer.aqu')$name)
cal.gw.shal = cal.gw %>% mutate( i = -which(idx.deep) )

# rbind everything and send to objective function maker
cal = rbind(cal.misc, cal.gw.shal, cal.snow)
obj = my_objective(cal, gage, dirname(.rswat$ciopath))


obj(refresh=TRUE)
vals = obj()$value
vals[which( obj()$name == 'adj_cn' )] = 1 # inactive?
vals[which( obj()$name == 'bf_max' )] = 25  # inactive?
vals[which( obj()$name == 'flo_dist' )] = 1  # inactive?
vals[which( obj()$name == 'dep_bot' )] = 6
vals[which( obj()$name == 'dep_wt' )] = 1
vals[which( obj()$name == 'rchg_dp' )] = 0.5 # important for setting level of baseflow
vals[which( obj()$name == 'revap_min' )] = 0.001 # alpha_bf only becomes sensitive when this is low
vals[which( obj()$name == 'alpha_bf' )] = 0.003
vals[which( obj()$name == 'can_max' )] = 1
vals[which( obj()$name == 'esco' )] = 0.5
vals[obj()$name == 'surq_lag'] = 0.025 # important for end of melt season
vals[obj()$name == 'melt_tmp'] = 2

obj(vals)

sim.new = rswat_output(fname, vname) %>% filter( gis_id == 1 ) 
ggp.flow.new = ggp.flow.new + geom_line(aes(color='SWAT+ simulation'), y=drop_units(sim.new$flo_out), alpha=0.5)
ggp.flow.new



ggp.flow.new = ggp.flow.cal

}

#+ include=FALSE
# TODO: Water budget
if(0)
{
  #' ## meteorological forcing
  
  # get daily total precipitation estimate from average of within-catchment gridpoints  
  meteo.nm = meteo$coords_sf$name[ st_intersects(meteo$coords_sf, boundary, sparse=F) ]
  pcp.mat = as.matrix(meteo$tables$pcp[match(dates, meteo$dates), meteo.nm])
  pcp.avg = rowMeans(pcp.mat) %>% set_units('mm/day')
  
  # (average) daily wind speed at 2m
  wnd.mat = as.matrix(meteo$tables$wnd[match(dates, meteo$dates), meteo.nm])
  wnd.avg = rowMeans(wnd.mat) %>% set_units('m/s')
  
  # (average) catchment-wide daily temperature min/max, and their daily averages
  tmin.mat = as.matrix(meteo$tables$tmin[match(dates, meteo$dates), meteo.nm])
  tmax.mat = as.matrix(meteo$tables$tmax[match(dates, meteo$dates), meteo.nm])
  tmin.avg = rowMeans(tmin.mat) %>% set_units('degC')
  tmax.avg = rowMeans(tmax.mat) %>% set_units('degC')
  tavg.avg = (tmin.avg + tmax.avg) / 2
  
  # catchment-wide elevation and latitude (averages) for PET calculations
  elev.avg = mean(meteo$elevation[meteo.nm])
  lat.avg = mean(meteo$coords[meteo.nm, 'lat']) %>% set_units('degrees')
  
  #' compute PET using Oudin's formula in `airGR` package
  pet.oudin = PE_Oudin(JD = as.integer(format(gage$date, '%j')),
                       Temp = drop_units(tavg.avg),
                       Lat = drop_units(lat.avg),
                       LatUnit = 'deg') %>% set_units('mm/day')
  
  #' Prepare inputs to methods from the `Evapotranspiration` package
  
  # model constants: elevation, latent heat of vaporisation, latitude in radians, solar constant
  ET.const = list(Elev = drop_units(elev.avg), 
                  lambda = drop_units(set_units(2.45, 'MJ / kg')), 
                  lat_rad = drop_units(set_units(lat.avg, 'radians')), 
                  Gsc = drop_units(set_units(0.082, 'MJ / min / m^2')))
  
  # convert input data to `zoo` format
  ET.inputs = ReadInputs(varnames = c('Tmin','Tmax','u2'), 
                         stopmissing = rep(99,3),
                         climatedata = data.frame(Year = format(gage$date, '%y'),
                                                  Month = format(gage$date, '%m'),
                                                  Day = format(gage$date, '%d'),
                                                  Tmin = drop_units(tmin.avg),
                                                  Tmax = drop_units(tmax.avg),
                                                  u2 = drop_units(wnd.avg)))
  
  #' For comparison, compute PET by two older methods:
  #' [Hargreaves-Samani (1985)](https://elibrary.asabe.org/abstract.asp?aid=26773)
  #' and [McGuinness-Bordne (1972)](https://agris.fao.org/agris-search/search.do?recordID=US201300408590)
  pet.hs = ET.HargreavesSamani(ET.inputs, ET.const)$ET.Daily %>% as.numeric %>% set_units('mm/day')
  pet.mb = ET.McGuinnessBordne(ET.inputs, ET.const)$ET.Daily %>% as.numeric %>% set_units('mm/day')
  
  #' 
  
  my_tsplot(data.frame(date=gage$date,
                       precip=pcp.avg,
                       Oudin=my_effp(pet.oudin, pcp.avg), 
                       Hargreaves=my_effp(pet.hs, pcp.avg), 
                       McGuiness_Bordne=my_effp(pet.mb, pcp.avg))[1000 + c(1:100),])
  
  
  #' ## baseflow separation
  
  #' The Pelletier and Andréassian (2020) model uses a simple ordinary differential equation to
  #' relate baseflow to a conceptual reservoir that continually fills and drains. The reservoir does not
  #' represent the aquifer, but rather attempts to mimic its empirical behaviour via easy-to-simulate
  #' dynamical systems that exhibit the same smoothing and memory effects seen in real aquifers.
  #' 
  #' Its two model parameters are fitted by maximizing the correlation coefficient of baseflow with a
  #' lagged sum of (catchment-wide) precipitation levels.
  
  #' `baseflow` inputs are daily PET, precip and discharge
  
  # approximate catchment-wide runoff (in mm/day) from streamflow at outlet (in m^3/s)
  loss = set_units(gage$flow / boundary$area, 'mm/day')
  
  # the package defines its own basin data objects
  bd = BasinData(Name = 'test',
                 startDate = as.POSIXct( as.character(min(gage$date)), tz='UTC'), 
                 endDate = as.POSIXct( as.character(max(gage$date)), tz='UTC' ), 
                 P = drop_units( my_effp(pet.oudin, pcp.avg) ),
                 PET = drop_units( pet.hs ),
                 Qobs = drop_units( loss ))
  
  #' Note: I have experimented with various levels of PET (see below), and it appears to have no effect
  #' on the output of baseflow. eg. setting it to zero produces no change in estimated parameters or
  #' baseflow levels. However we need to supply this input for the basin data function call to work.
  #' 
  #' Next we fit the model parameters by a simple grid search to optimize the lagged correlation.
  
  # this controls the effective reservoir capacity - set roughly the same range as described in the paper
  alphas = seq(from=2, to=2e6, length.out=1e3)
  
  # this parameter controls transit lag times - grid search will cover entire range of testable delays
  taus = 2:min(1*365, length(loss))
  #' Note: the recommended range for this parameter (following eq.8 in the paper) is unclear - the
  #' authors use [5,(5*365)] (days) but mention that typical values are in the 3-12 month range.
  #' However, their dataset is much longer ()
  #' 
  #' I have found that the objective function favours longer delays
  
  # ~1.8M parameter combinations, but it takes only a few seconds to evaluate and pick the optimum
  print( length(alphas) * length(taus) )
  gsearch = corr_crit_vect(bd, alphas, taus, updateFunction='quadr')
  gsearch.opt = gsearch[which.max(gsearch$crit), ]
  print(gsearch.opt)
  
  #' The vectorized objective function is implemented in Rust and is very fast, making a grid search a 
  #' good option. However I find the optimization is very sensitive to boundary conditions (optima
  #' often lie on parameter space boundary), which could be a problem for automation.
  
  # convert these results to raster for easy plotting
  gsearch.tif = flip(raster(matrix(gsearch$crit, length(alphas))), 'y')
  plot(gsearch.tif)
  
  #' The fitted model predicts a baseflow index of 47% (a bit high?) with a max-correlation delay
  #' of 1741 days.
  #' 
  
  #' 
  #' 
  
  # run the filter with this optimal alpha value
  bd.filter = BaseflowFilter(bd, gsearch.opt$alpha, updateFunction='quadr')
  which(bd.filter@update)
  
  # TODO: Try another method: Nathon And McMahon (1990)
  
}

#+ include=FALSE
# Development code
#my_markdown('demo_baseflow', 'R/analysis')


