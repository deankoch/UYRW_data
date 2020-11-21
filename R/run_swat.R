#' ---
#' title: "run_swat.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **run_swat.R**: opens Mill Creek QSWAT3 model, runs SWAT2012 simulation, displays results (WORK IN PROGRESS)
#' 
#' After running `make_qswat`, users should be able to complete the 'Delineate Watershed' and 'Create HRUs'
#' steps in QSWAT3. 
#' The following scripts should be run first to fetch and process data inputs:
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md),
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md),
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md), and
#' [make_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_qswat.md).

#' ## libraries
#' [`measurements`](https://cran.r-project.org/web/packages/measurements/index.html) simplifies unit
#' conversions. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(here)
source(here('R/get_helperfun.R'))
library(SWATplusR)
library(measurements)
#library(xml2)


#'
#' ## project data

# load file paths info about data and parameters
meteo.meta = my_metadata('get_meteo')
dem.meta = my_metadata('get_dem')
makeqswat.meta = my_metadata('make_qswat')
streamgages.meta = my_metadata('get_streamgages')

# Load study area polygon, outlets geometry (input to SWAT), and discharge data
outlets.swat = read_sf(here(makeqswat.meta['swat_outlets', 'file']))
bd.poly = readRDS(makeqswat.meta['swat_boundary', 'file'])
usgs.sf = st_intersection(readRDS(here(streamgages.meta['USGS_sites', 'file'])), bd.poly) 

# path to QSWAT source and I/O files
swat.dir = here(makeqswat.meta['swat_source', 'file'])
swat.projdir = makeqswat.meta['swat_proj', 'file']
swat.name = basename(swat.projdir)

#+ echo=FALSE
# define the files to write
{
  files.towrite = list(
    
    # directory to write SWAT wdat input text files
    c(name='swat_weatherstn_TEMP',
      file=file.path(swat.projdir, 'UYRW_weather_data'),
      type='directory',
      description='directory for writing SWAT weather input text files')
  )
}

# store as metadata table csv
runqswat.meta = my_metadata('run_qswat', files.towrite, overwrite=TRUE)
print(runqswat.meta[, c('file', 'type')])



#'
#' ## load some discharge observations
#' 

library(dataRetrieval)

# load observed discharge (dc) data at SWAT outlets. We will compare this against simulation
dc = usgs.sf[st_distance(outlets.swat, usgs.sf) < units::set_units(1, m),]

# this case study only has one time series of daily values, in the 1950s
idx.dv = dc$data_type_cd=='dv'
dv.pt = st_geometry(dc[idx.dv,])

# download it from NWIS (use `attr(dcdv, 'variableInfo')` to view metadata)
dcdv = renameNWISColumns(readNWISdv(siteNumber=dc$site_no[idx.dv], 
                                    parameterCd=dc$parm_cd[idx.dv], 
                                    startDate=dc$begin_date[idx.dv] , 
                                    endDate=dc$end_date[idx.dv], 
                                    statCd=paste0('0000', 3)))

# convert from imperial (above time series) to metric (SWAT2012)
dcdv$Flow = conv_unit(dcdv$Flow, "ft3", "m3")

# load the SWAT reaches info and find the reach ID to analyze
shp.subdir = 'Watershed/Shapes'
shp.dir = here(swat.projdir, shp.subdir)
swat.riv = read_sf(file.path(shp.dir, 'riv1.shp'))
dv.reachID = swat.riv$Subbasin[st_distance(dv.pt, swat.riv) < units::set_units(100, m)]
#dv.reachID = swat.riv$Subbasin



#'
#' ## reload the weather dataset that has been imported into the SWAT directory
#' eventually we will write the weather data directly, and store information about the 
#' weather source data in a SWAT parameters dataframe that will function as an interface
#' to all of the many settings text files for SWAT 
#' 
wdat.in = readRDS(here(meteo.meta['livneh_uyrw', 'file']))

# set start/end dates to coincide with climatic and discharge time series
startdate = as.character(min(wdat.in$dates))
enddate = as.character(max(as.Date(dcdv$Date)))
sim.dates = seq.Date(as.Date(startdate), as.Date(enddate), by='day')
n.days = length(sim.dates)

# compute mean precip as a rough guide to weather conditions
idx.wdat = wdat.in$dates %in% sim.dates
prec.ts = rowMeans(wdat.in$tables$prec[idx.wdat,])
prec.ts.rescaled = -scales::rescale(prec.ts, c(0,10))

########################
# Let's see if we can write some custom code to call the SWAT executable and import results


# set the "project directory" to look for text input/output files
txtio.subdir = 'Scenarios/Default/TxtInOut'
txtio.dir =  here(swat.projdir, txtio.subdir)

# copy the SWAT executable to that directory
swat.editor.dir = 'C:/SWAT/SWATEditor'
swat.exec.fn = 'SWAT_64rel.exe'
swat.exec.path = file.path(swat.editor.dir, swat.exec.fn)

# paths to config text files
all.path = list.files(txtio.dir, full.names=TRUE)
cio.path = file.path(txtio.dir, 'file.cio')
bsn.path = file.path(txtio.dir, 'basins.bsn')
hru.path = all.path[endsWith(all.path, '1.hru')]
gw.path = all.path[endsWith(all.path, '1.gw')]
mgt.path = all.path[endsWith(all.path, '1.mgt')]

# set up a simple grid search for one of the parameters
np.test = 25
param.bounds = list(SURLAG = c(0, 1, 30))
param.test = sapply(param.bounds, function(param) seq(param[1], param[3], length.out=np.test))
param.values = c(param.test)

# output storage
n.test = length(param.values)
nse.out = rep(NA, n.test)

for(idx.test in 1:n.test)
{
  # to set up model parameters for a simulation, we have to write parameter values to the SWAT input
  # text files with a very specific syntax. For example integer-valued parameters cannot have a decimal
  # point (even if followed by zeros). In the following, we handle each parameter input file separately,
  # and further separate parameters by type (integer or floating point)
  
  # changes required in "file.cio" to set this simulation time period and select daily output
  file.cio.int = c(NBYR = 1 + diff(as.integer(format(as.Date(c(startdate, enddate)), '%Y'))), 
                   IYR = as.integer(format(as.Date(startdate), '%Y')), 
                   IDAF = as.numeric(format(as.Date(startdate), '%j')), 
                   IDAL = as.numeric(format(as.Date(enddate), '%j')),
                   IPRINT = 1)

  # Note: toggling IA_B = 1 first writes output data as binary (in addition to the normal text files).
  # Since writing output to disk appears to be a major slowdown, we might want to look into this option
  # if we want to optimize execution speed in the future.

  # define subset of variables to print to output files (this speeds execution considerably)
  file.cio.vec = list('Reach output variables' = 1,
                      'Subbasin output variables' = 1,
                      'HRU output variables' = 1,
                      'HRU data to be printed' = 1)
  
  # changes to the basin-level process model numeric parameters
  basin.bsn.num = c(SFTMP = 1.2,
                    SMTMP = 2.5,
                    SMFMN = 2,
                    SMFMX = 2.3,
                    SNOCOVMX = 2,
                    TIMP = 0.71,
                    SNO50COV = 0.5,
                    CNCOEF = 0.5)
  
  # changes to the basin=level process model integer parameters
  basin.bsn.int = c(ICN = 1)
  
  # global changes to the (numerous) HRU files
  hru.num = c(SURLAG = 0.15,
              SLSUBBSN = 18,
              EPCO = 1,
              ESCO = 0.01) 
  
  # global changes to the (numerous) GW files
  gw.num = c(GW_DELAY = 1)
  
  # global changes to the (numerous) MGT files
  #mgt.num = c(CN2 = param.values[idx.test])
  
  # write changes to the global parameters files
  my_swat_rwnum(cio.path, newval=file.cio.int, type='i')
  my_swat_rwvec(cio.path, newval=file.cio.vec)
  my_swat_rwnum(bsn.path, newval=basin.bsn.num)
  my_swat_rwnum(bsn.path, newval=basin.bsn.int, type='i')
  
  # write changes to the numeroud HRU, GW, and MGT files
  invisible(sapply(hru.path, function(hru) my_swat_rwnum(hru, newval=hru.num)))
  invisible(sapply(gw.path, function(gw) my_swat_rwnum(gw, newval=gw.num)))
  invisible(sapply(mgt.path, function(mgt) my_swat_rwnum(mgt, newval=mgt.num)))
  #unname(sapply(hru.path, function(hru) my_swat_rwnum(hru, 'EPCO')))
  #unname(sapply(gw.path, function(gw) my_swat_rwnum(gw, 'GW_DELAY')))
  
  # run the simulation
  my_call_swat(txtio.dir, swat.exec.path)
  

  # identify the main channel output file and load a subset of variables
  rch.path = file.path(txtio.dir, 'output.rch')
  varname = 'FLOW_IN'
  rch = my_read_rch(rch.path, varname, origin.date=startdate)
  
  # extract reach #1 (at main outlet)
  rch.r1 = rch[rch$RCH==1,]
  
  # identify the period of overlap and extract response vectors
  dates.overlap = intersect(as.Date(dcdv$Date), rch.r1$date[rch.r1$RCH==1])
  qobs = dcdv$Flow[match(dates.overlap, dcdv$Date)]
  qsim = rch.r1[[varname]][match(dates.overlap, rch.r1$date)]
  
  # store NSE
  nse.out[idx.test] = my_nse(qobs, qsim, L=1)

  # plot the simulated output flow against observed, with precipitation underneath
  ggp = ggplot(data=rch.r1) +
    geom_line(aes(x=date, y=0),  col='red') +
    annotate(geom='line', x=rch.r1[['date']], y=rch.r1[[varname]], col='grey') +
    annotate(geom='line', x=dcdv$Date, y=dcdv$Flow, col='darkblue') +
    annotate(geom='line', x=wdat.in$dates[idx.wdat], y=prec.ts.rescaled, col='lightblue') +
    theme_bw()

  print(ggp)
  print(nse.out[idx.test])
  
  
}

plot(nse.out~param.values)



################
# the code in the next chunk is now failing after I re-ran the qswat model setup with elevation bands
# (and likely a different time period entered for the initial SWAT run from SWATEditor). It appears
# to once again be issues of parsing the input after running a simulation successfully. 
######################
#'
#' ## prepare a SWAT simulation
#' 
#' Steps for completing the 'Edit Inputs and Run SWAT' step in QSWAT3:
#' (to do: flesh these out)
#' 
#' (1) change the soils database path to point same SQL file as the reference database
#' (2) Define weather generator database (I use the 1960-2010 version)
#' (3) Select any meteorological data (ie the txt files written by make_qswat) as observations
#' (4) write all SWAT input tables
#' (5) click 'cancel' or 'exit' until back to the main QSWAT dialogue (closes SQL)
#' 
#' At this point the model is all set up to run simulations. To ensure that a simulation
#' is driven by the weather inputs generated above, we just need to set the simulation
#' years to match the period of the weather time series. Otherwise (and for all missing
#' variables, eg humidity), SWAT will use the weather generators to fill in any missing
#' information.
#' 
#' Simulations (ie SWAT executables) can be run from within QSWAT3, and the results can
#' be viewed using some cool visualization features built in to the plugin. However since
#' we will need to be able to automate these simulations, we will call SWAT from R using
#' the SWATPlusR pacakge (note that this package supports both SWAT+ and SWAT2012).
#' 

#' # set the "project directory" to look for text input/output files
#' txtio.subdir = 'Scenarios/Default/TxtInOut'
#' txtio.dir =  here(swat.projdir, txtio.subdir)
#' 
#' # copy the SWAT executable to that directory
#' swat.editor.dir = 'C:/SWAT/SWATEditor'
#' swat.exec.fn = 'SWAT_64rel.exe'
#' src.path = file.path(swat.editor.dir, swat.exec.fn)
#' dest.path = file.path(txtio.dir, swat.exec.fn)
#' file.copy(src.path, dest.path)
#' 
#' # set start/end dates to coincide with climatic and discharge time series
#' startdate = as.character(min(wdat.in$dates))
#' enddate = as.character(max(as.Date(dcdv$Date)))
#' #test.enddate = '1953-01-01'
#' n.days = as.integer(as.Date(enddate) - as.Date(startdate)) + 1
#' 
#' # define output object for SWATPlusR
#' test.output = define_output(file='rch', variable='FLOW_OUT', unit=dv.reachID)
#' 
#' # run the simulation
#' q_sim = run_swat2012(project_path=txtio.dir,
#'                      output=test.output,
#'                      start_date=startdate,
#'                      end_date=enddate)
#' 
#' 
#' 
#' 
#' 
#' # compute mean precip as a rough guide to weather conditions
#' idx.wdat = wdat.in$dates %in% q_sim$date
#' prec.ts = rowMeans(wdat.in$tables$prec[idx.wdat,])
#' prec.ts.rescaled = -scales::rescale(prec.ts, c(0,10))
#' 
#' 
#' # plot the simulated output flow against observed, with precipitation underneath
#' ggplot(data=q_sim) +
#'   geom_line(aes(x=date, y=FLOW_OUT), col='red') +
#'   annotate(geom='line', x=dcdv$Date, y=dcdv$Flow) +
#'   annotate(geom='line', x=wdat.in$dates[idx.wdat], y=prec.ts.rescaled, col='blue') +
#'   theme_bw()
#' 
#' #' This isn't terrible, considering we have made no adjustment of the default parameters.
#' #' The most obvious problem is that the observed spring melt (in black) substantially lags
#' #' the simulated values (in red). We can fix this by tweaking some of the parameters related
#' #' to snowmelt and groundwater propegation delays. 
#' 
#' # try re-running with a modified delay parameter
#' par_mod = c('TIMP.bsn|change = absval' = 0.1,
#'             'SMTMP.bsn|change = absval' = 2,
#'             'SMFMN.bsn|change = absval' = 1,
#'             'SMFMX.bsn|change = absval' = 2,
#'             'SNOCOVMX.bsn|change = absval' = 5,
#'             'GW_DELAY.gw|change = absval' = 1)
#' 
#' # run the simulation and plot the result like before
#' q_sim2 = run_swat2012(project_path=txtio.dir,
#'                       output=test.output,
#'                       start_date=startdate,
#'                       end_date=enddate,
#'                       parameter=par_mod)
#' q_sim2 = q_sim2$simulation
#' ggplot(data=q_sim2) +
#'   geom_line(aes(x=date, y=FLOW_OUT), col='red') +
#'   annotate(geom='line', x=dcdv$Date, y=dcdv$Flow) +
#'   annotate(geom='line', x=wdat.in$dates[idx.wdat], y=prec.ts.rescaled, col='blue') +
#'   theme_bw()
#' 
#' #' Much better in terms of seasonal timing, but still not matching well in terms of 
#' #' magnitude, small scale fluctuations, or baseflow. These problems are more obvious
#' #' if we zoom in:
#' 
#' # plot a narrower time period
#' startdate.sub = '1954-01-01'
#' enddate.sub = '1956-01-01'
#' ggplot(data=q_sim2) +
#'   geom_line(aes(x=date, y=FLOW_OUT), col='red') +
#'   annotate(geom='line', x=dcdv$Date, y=dcdv$Flow) +
#'   annotate(geom='line', x=wdat.in$dates[idx.wdat], y=prec.ts.rescaled, col='blue') +
#'   scale_x_date(limits = as.Date(c(startdate.sub, enddate.sub))) +
#'   theme_bw()
#' 
#' #' There appears to be a bug in SWATPlusR that prevents us from setting custom simulation start
#' #' and end dates after the weather files have been written. See below:  
#' # try an adjustment of the time period in simulation calls with SWATPlusR::run_swat2012
#' startdate.sub = '1954-01-01'
#' enddate.sub = '1956-01-01'
#' n.days = as.Date(enddate.sub) - as.Date(startdate.sub) + 1
#' q_sim3 = run_swat2012(project_path=txtio.dir,
#'                       output=test.output,
#'                       start_date=startdate.sub,
#'                       end_date=enddate.sub)
#' 
#' #' The simulation completes successfully, but we get a strange error message at the end:
#' #'
#' #' "Error: Can't recycle `..1` (size 731) to match `..2` (size 366)."
#' #' 
#' #' It seems that a vector of size 1097 (the duration of the entire requested simulation, ie
#' #' `n.days`) is getting crammed into a vector of size 732 (duration of all but the final year).
#' #' I wonder if this is a problem with SWATPlusR's handling of leap years (like 1952)?
#' #' 
#' #' After trying various start/end date combinations, it's looking like any requests for
#' #' simulation periods that end on Jan 01 are doomed to fail. This problem persists with the
#' #' `save_file` and `return_output` arguments set to write output to disk instead of loading it
#' #' into R. It also occurs in the development version of the package (as of Nov 10, 2020)
#' 
#' #' Maybe we can fix this by modifying file.cio? Check this by monitoring the temporary "TxtInOut"
#' #' project folder (".mode_run") that SWATPlusR creates when it does a simulation
#' 
#' # check what happens to file.cio in simulations that occur entirely within a single calendar year
#' startdate.sub = '1954-01-01'
#' enddate.sub = '1954-12-31'
#' n.days = as.Date(enddate.sub) - as.Date(startdate.sub) + 1
#' q_sim3 = run_swat2012(project_path=txtio.dir,
#'                       output=test.output,
#'                       start_date=startdate.sub,
#'                       end_date=enddate.sub,
#'                       keep_folder=TRUE)
#' q_sim3.cio = readLines(file.path(txtio.dir, '.model_run/thread_1/file.cio'))
#' print(q_sim3.cio[1:11])
#' 
#' #' From this we can get a pretty good idea of how things are coded. We have NBYR=1 because the
#' #' simulation takes place within a single calendar year; IDAF=1 because we start on Jan 1st; and
#' #' IDAL=365 because we end on Dec 31.
#' #' 
#' #' The trouble is when we roll into the new year 
#' 
#' # include the first day of the new year
#' startdate.sub = '1954-01-01'
#' enddate.sub = '1955-01-01'
#' n.days = as.Date(enddate.sub) - as.Date(startdate.sub) + 1
#' q_sim4 = run_swat2012(project_path=txtio.dir,
#'                       output=test.output,
#'                       start_date=startdate.sub,
#'                       end_date=enddate.sub,
#'                       keep_folder=TRUE)
#' q_sim4.cio = readLines(file.path(txtio.dir, '.model_run/thread_1/file.cio'))
#' print(q_sim4.cio[1:11])
#' 
#' startdate.sub = '1954-01-01'
#' enddate.sub = '1954-12-09'
#' q_sim4 = run_swat2012(project_path=txtio.dir,
#'                       output=test.output,
#'                       start_date=startdate.sub,
#'                       end_date=enddate.sub,
#'                       keep_folder=TRUE)
#' 
#' 
#' 
#' 
#' #' We have NBYR=1, with IDAF = IDAL = 1, or a period of 1 day. At first glance the output seems okay
#' #' because we are returned a simulation with the correct length (366 days), however this output is
#' #' just the data from Jan 1st repeated over and over (zero flow, in this case)
#' nrow(q_sim4) == n.days
#' all(q_sim4$FLOW_OUT==0)


#my_markdown('run_swat')
