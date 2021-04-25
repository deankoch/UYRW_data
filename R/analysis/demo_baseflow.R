#' ---
#' title: "demo_baseflow.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_baseflow.R**: (in development) example of aquifer model fitting with SWAT+
#' 

#'
#' ## libraries
#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md),
#' [helper_analysis](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md),
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
#' from INRAE-Antony (HYCAR Research Unit, France), including an implementation of the
#' [Oudin et al. (2005)](https://doi.org/10.1016/j.jhydrol.2004.08.026) formula for PET
library(airGR)
# TODO: check out the CemaNeige model for snow accumulation and melt (also from this package)

#' The [`baseflow`](https://cran.r-project.org/web/packages/baseflow/index.html) R package is an
#' implementation of the method of
#' [Pelletier and Andréassian (2020)](https://hess.copernicus.org/articles/24/1171/2020/)
#' for estimating baseflow (and quickflow) from hydrographs of daily streamflow and precipitation totals.
library(baseflow)

# numeric optimization for model fitting
library(dfoptim)

# low-level customizations for ggplot calls
library(grid)


#'
#' ## project data
#+ echo=FALSE

# USGS gage name specifies the catchment to use as demo 
nm = 'big_c_nr_emigrant'
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

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
baseflow.meta = my_metadata('demo_baseflow', files.towrite, overwrite=TRUE, data.dir=sci.subdir)
print(baseflow.meta[, c('file', 'type')])

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


#' ## set up simulation times
#' 
# pull gage data from this site
usgs = usgs.all$dat[[pts$site_no]]

# this gage has two observation periods separated by a few years
dates.src = my_split_series(usgs$dat[[1]]$date, meteo$dates)
print(sapply(dates.src, length))

# for the demo we'll look at the longer 1970s period
dates = dates.src[[1]]
gage = usgs$dat[[1]] %>% filter(date %in% dates)

#' ## overview plot
#' 
#' This plot shows the channel network for the demo catchment and its location in the greater UYRW area,
#' along with an inset containing a USGS hydrograph of streamflow records for the catchment.
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_catchment.png)
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


#' ## Build SWAT+ model and load into R
#' 
#' The data required to build a SWAT+ model for the above catchment are already prepared in 
#' the `out.subdir` directory. We will use a series of R helper functions (see
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)) to send these
#' data to QSWAT+, then on to SWAT+Editor, before loading the results back into R.
#' 
#' This will generate the plaintext SWAT+ model files required to execute a simulation, as well as
#' the shapefiles normally displayed in QGIS for visualization of watershed features (eg. HRUs).
#' 

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

#' Note that the warning about a deep aquifers shapefile can be ignored for now
#' (see [here](https://groups.google.com/g/qswatplus/c/Z5AGrC_Wfq0/m/1TeG9bQFCgAJ))
#' 
#' The dataframe `qswat` summarizes the QSWAT+ input files and parameters used here:
qswat %>% select(type, description) %>% print

#' A helper function, `qswat_read`, loads the QSWAT+ shapefiles into R
wsh = qswat_read(qswat)

#' Another helper function, `qswat_plot`, displays the spatial arrangement of HRUs. The next
#' chunk calls this function and saves the results to a file
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_baseflow_hrus.png)

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

#' The SWAT+ Editor step completed above by `qswat_run` writes the SWAT+ I/O config text files.
#' The SWAT+ executable defines the model based on their contents alone (ie independently of the
#' QSWAT+ shapefiles loaded above).
#' 
#' These are the files that need to be modified during parameter fitting - we can read and
#' manipulate them using the `rswat` helper functions. The first step is to define the project
#' directory (usually "TxtInOut")

# assign the SWAT+ project directory in `rswat`
cio = rswat_cio(dir.qswat)

#' load all config files except decision tables, which are very large (and not needed for now)
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)

#' This takes a few seconds to parse the files, which are summarized in the returned dataframe
print(cio)

#' Each row of `cio` is a file containing a group of model parameters. The 'nvar' column indicates
#' how many distinct fields there are in the file (nrow * ncol, summed over all of the tables,
#' including headers). There are a lot of them, so if you are new to SWAT, check out the
#' [I/O](https://swatplus.gitbook.io/docs/user/io) and 
#' [theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs.
#' 
#' Note that as of April 2021, I can  find no theory guides published for recent versions of SWAT
#' (including SWAT+), so the link above is for a document from 2009. Many aspects of the the model
#' have not changed since then, and the old theory guide remains a good reference. However, most
#' variable and parameter names have changed in SWAT+.
#' 
#' Parameters can be examined by calling `rswat_open` with a filename. eg we will be interested in the
#' aquifer model parameters in 'aquifer.aqu' - the code below prints the last few lines of the relevant
#' table:

# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print

# this one contains the main process model parameters
rswat_open('aquifer.aqu') %>% tail %>% print

#' The helper function `rswat_find` can be useful for tracking down a SWAT+ parameter using keywords or
#' SWAT2012 names. This uses fuzzy case-insensitive matching (see R's `?agrep` doc), which does well to catch
#' the most common name changes in the SWAT2012 -> SWAT+ updates

# eg. find the PET estimation method 'pet', which was called 'IPET' in SWAT2012
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print

#' The 'string' column above shows the plaintext for this parameter in the SWAT+ config file, and 'file'
#' the filename. We can see that 'pet' (in file 'codes.bsn') is set to 1. This codes for the Penman-Monteith
#' model.
#' 
#' The other two matches, 'pet_file' and 'harg_pet', are an input file for observed PET, and
#' a solar radiation coefficient used with the Hargreaves-Samani model (neither is currently used).  

#' 
#' ## Adjusting a SWAT+ model
#' 
#' To change a parameter, open its container file with `rswat_open`, modify it in R, then write the change with
#' `rswat_write`. eg. switching to the Hargreaves-Samani model for PET (coded as pet=2) can be done like this:
 
# open the file and copy to an R dataframe called `codes`
codes = rswat_open('codes.bsn')
print(codes)

# By default, SWAT+ estimates PET with Penman-Monteith (pet = 0, see I/O docs)

# Hargreaves-Samani is coded as pet = 2: make this change
codes$pet = 2

# default behaviour of `rswat_write` is to preview the requested change. This looks fine...
rswat_write(codes)

# ... so we overwrite the file on disk with argument `preview=FALSE`
rswat_write(codes, preview=FALSE, quiet=TRUE)

#' The SWAT+ executable will now use the Hargreaves-Samani method for estimation.
#' The Hargreaves-Samani coefficient that turned up earlier is no longer inactive, so we need to assign it
#' a nonzero value. For now I just use the example value appearing in the I/O docs (we can tune it later).

# open the container file for 'harg_pet'
hydro = rswat_open('hydrology.hyd')

# The parameters in this file are all length-50 vectors. Distinct values are allowed for each HRU
print( nrow(hydro) )
print( head(hydro) )

# assign the same example value for 'harg_pet' in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)

#' Hargreaves-Samani may be the best choice for this project since we lack the detailed data on humidity,
#' wind, and solar energy required with Penman-Monteith. SWAT+ can generate those missing data (this is
#' its default behaviour) but since their values are generated from a stochastic process calibrated on
#' long-term historical norms, they are very imprecise at the daily scale.

#' 
#' ## Viewing/running simulations
#' 
#' the SWAT+ executable takes a few seconds to simulate the full seven-year time series in this example,
#' producing output in the form of .txt tables containing simulated state variables.
#' 
#' There are many (100+) such output tables and the task of printing any of them to a file can slow down SWAT+
#' considerably. To speed things up it is better to request specific outputs and omit printing the others. This
#' requires some knowledge of what output variables are available and where they are located.
#' 
#' The `rswat_output` function returns a dataframe with info on the available SWAT+ output files:

# copy the dataframe to variable `odf`
odf = rswat_output()

# print some summary info
head(odf)
print( nrow(odf) )

#' Output files are loaded as R dataframes by specifying `fname` (and, optionally, `vname` for subsets)

# load an example file. Dates and units are incorporated automatically
hydout.yr = rswat_output(fname='hydout_yr.txt')
head( hydout.yr )

# a subset of columns (output variables) can be specified with `vname`
head( rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction')) )

#' Some columns are loaded by default because they are important identifiers (eg spatial IDs).
#' This functionality (along with date-parsing) can be switched off to get faster load times,
#' or for debugging:

head( rswat_output(fname='hydout_yr.txt', vname=c('flo', 'fraction'), makedates=FALSE, showidx=FALSE) )

#' When an output file is scanned by this function, its headers and units are cached for faster
#' loading in subsequent calls. Variable names in this database can also be searched by calling
#' `rswat_output` without the `fname` argument:
#' 

# search for output variables named "fraction". The one loaded above is identified:
rswat_output(vname='fraction')

# search for 'flow_in'. Only a partial match is found: 
rswat_output(vname='flo_in')

#' Right now the database only includes the contents of 'hydout_yr.txt', and `rswat_output()` only
#' reports the files currently in the SWAT+ project folder ("TxtInOut"). To include ALL SWAT+ output
#' variables, we can run a (short) simulation where all output files are requested, then parse its
#' outputs:

# build database of SWAT+ outputs
odf = rswat_output(loadall=TRUE)
head( odf )

#' This only takes a few seconds, and only temporarily alters the files in 'TxtInOut' (a backup is
#' restored after it completes). Notice the filenames list now includes entries with NA size - these
#' are files not currently found in 'TxtInOut', but which can be enabled in SWAT+ simulations. Their
#' headers (and units) have now been cached, and are searchable:

# repeat the search for 'flo_in' and find many exact matches. 
nrow( rswat_output(vname='flo_in') ) 

# pipes are useful for narrowing the results of a search
rswat_output(vname='flo_in') %>% filter( step == 'day' ) %>% filter( units == 'ha m' )

#' Searches will return exact matches first, and if nothing is found, the function reverts to
#' partial (sub-string) matching with increasing fuzziness until it finds something:

# an example of fuzzy partial matching
rswat_output(vname='recharge')

#'

#+ include=FALSE
# TODO: tidy up this older code
if(0)
{

  
  
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


