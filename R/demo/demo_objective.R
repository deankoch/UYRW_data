#' ---
#' title: "demo_objective.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_objective.R**: Parameter fitting for a SWAT+ model with `rswat` 
#' 
#' This script demonstrates the use of OHG files to quickly get simulated SWAT+ channel flow values,
#' and methods for building objective functions to optimize in parameter-fitting.

#'
#' ## libraries
#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md) and
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
#' load required libraries, global variables, and some helper functions.

library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))


#'
#' ## project data


#' load the SWAT+ project info and gage data fro previous scripts
#' ([demo_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md), and
#' [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md))
#' 
# load some info about the SWAT+ model
qswat.meta = my_metadata('demo_qswat', data.dir=demo.subdir)
txtinout.meta = my_metadata('demo_txtinout', data.dir=demo.subdir)
nm = qswat.meta['example_name', 'file']
print(nm)

# load the gage data for this catchment 
gage = readRDS(here(qswat.meta['gage', 'file']))
head(gage)

# copy the directory of the project folder and its backup
demo.dir = here( txtinout.meta['txtinout', 'file'] )
demobak.dir = here( txtinout.meta['txtinout_bak', 'file'] )

#' ## load the SWAT+ project
#' 
#' Start by loading a SWAT+ project folder

# point `rswat` to the demo "TxtInOut" directory used earlier 
demo.dir = here( txtinout.meta['txtinout', 'file'] )
print(demo.dir)
rswat_cio(demo.dir)

#' restore the backup that was created after running the previous demo script

# restore model backup, which has Hargreaves-Samani PET method activated
print(demobak.dir)
fout = rswat_copy(from=demobak.dir, fname='.', overwrite=TRUE, quiet=TRUE)

# pre-load all output and config files (except decision tables)
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
odf = rswat_output(loadall=TRUE)

## TEMPORARILY DISABLED
if(0)
{
  
#' The helper functions `rswat_copy`, `rswat_cio`, and `rswat_output` are described in the previous
#' demo script [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md).
#' 
#' ## Comparing the simulated and observed data
#' 
#' Printing simulation data to the .txt output files is a bottleneck for SWAT+. To speed things up it is
#' better to request only the outputs you need, and omit printing the others. These settings are found in
#' 'print.prt' (for the normal outputs) and 'object.prt' (object hydrograph outputs). Outputs that are
#' currently toggled on are indicated by the 'activated' field in the dataframe returned by `rswat_output`
#' 
# display the output files that are currently activated in SWAT+
rswat_output() %>% filter(activated) %>% pull(file)

#' All daily output files are active (this setting was applied by a call to `rswat_time` above).
#' There are quite a few of them, so execution is relatively slow. If we turn off all off the standard
#' output files, the SWAT+ simulation will still run, and it completes much faster.

# open fifth table of 'print.prt', disable all output files and write the changes
print.prt = rswat_open('print.prt')[[5]]
print.prt[, names(print.prt) != 'objects'] = 'n'
rswat_write(print.prt, preview=F, quiet=TRUE)

# call the SWAT+ executable
fout = rswat_exec()

#' On my machine the process completes in less than half the time (7-9 seconds versus 17-20). This time cost
#' reduction may not matter when running one-off simulations like we do here, but later on when fitting
#' parameters it becomes very significant, because we will need to run thousands of simulations.
#' 
#' Note that two files related to crop yields were generated in spite of the settings in 'print.prt'.
#' It is not clear how to inactivate them. But since they are small, yearly tables, they likely don't impact
#' runtimes very much.
#' 
#' In parameter fitting we need to generate daily outputs of discharge at our gaged channel(s) to evaluate
#' errors, ie to evaluate the objective function. As we can see in the above example, requesting these
#' outputs via 'print.prt' is not ideal because the returned files print the data for every channel, but we
#' only need it for a few (usually one). SWAT+ has a special type of output file for the purpose of
#' channel-specific outputs, the object hydrograph (OHG).

# activate the object hydrograph for the outlet channel (id number 1)
id.outlet = 1
rswat_ohg(overwrite=TRUE, oid=id.outlet)

#' this function modifies "file.cio" and "object.prt" so that SWAT+ generates the plaintext output file
#' "sdc_1_tot.ohg" in addition to any others specified in "print.prt". The `oid` argument specifies that we
#' only want data on the channel with ID code 1 (AKA 'cha01', usually the main outlet of the catchment). 

# call the SWAT+ executable
fout = rswat_exec()

#' Note that this new file output seems not to affect runtimes

# check that the expected file has been generated
rswat_output() %>% filter(type=='ohg') %>% select(-path)

# open the output 
rswat_output('sdc_1_tot.ohg') %>% str

#' This is a subset of the outflow variables that would normally appear in the "SWAT-DEG_CHANNEL" group.
#' Note that the variable names in OHG files omit the suffix "_out". For example variable 'flo' in the OHG file
#' corresponds to variable 'flo_out' in 'channel_sd_day.txt'. 
#' 
#' The units in these files are not necessarily the same - in particular, the OHG discharge values are in per-day
#' units whereas the normal output files use per-second units. Different units means we can expect some
#' post-conversion differences due to numerical imprecision (see also #32 and #75
#' [here](https://infiniteundo.com/post/25509354022/more-falsehoods-programmers-believe-about-time)):

# load the OHG output file
ohg.out = rswat_output('sdc_1_tot.ohg')

# load the normal output file, trimming to only include values from the main outlet
prt.out = rswat_output('channel_sd_day.txt') %>% filter(gis_id==id.outlet) 

# join them and check the level of discrepancy
left_join(prt.out, ohg.out, by=c('date')) %>% 
  mutate( absdiff = abs( (flo_out - flo) ) ) %>% 
  pull(absdiff) %>% max

#' The errors appear to be small enough here to ignore for the purposes of parameter fitting.

# OHG outputs can be switched off for now
rswat_ohg(overwrite=TRUE, delete=TRUE)

#' The helper function `rswat_flo` is for quickly getting the OHG output for a simulation. It handles all
#' of the required settings adjustments laid out above, and by default will restore all config files to their
#' original state afterwards). Simply pass it a range of dates and get back the simulated discharge values:

# `oid=1` is set by default and 'dates' can be range of dates or a dataframe containing a 'date' column
ohg.out = rswat_flo(dates=gage, quiet=TRUE)
ohg.out %>% str

#' If 'dates' is a dataframe containing observations (columns names starting with 'flo' or 'obs'), `rswat_flo`
#' can optionally be passed an (anonymous) error function `errfn(x,y)`. In that case instead of returning the
#' discharge simulation values it passes them directly to `errfn` (as `x`) along with the observed data (as `y`)
#' 
#' eg. this code passes the function `my_nse`
#' (defined in [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md))
#' which computes
#' [Nash-Sutcliffe efficiency](https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient)

# define `my_nse_2` to get a function of two variables only (`my_nse` has four)
my_nse_2 = function(x, y) {my_nse(x, y, L=2, normalized=TRUE)}

# run simulation and return NSE (normalized , with exponent 2)
rswat_flo(dates=gage, errfn=my_nse_2, quiet=TRUE) %>% print

#' This is useful for linking up with optimization algorithms offered in other R packages. For example if we
#' want to maximize NSE for our simulated hydrograph with respect to the parameter 'harg_pet' (discussed earlier),
#' we can construct an objective function like this:

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

#' A grid search now becomes very simple to program:

# run the objective function for a range of `harg_pet` values
harg.test = seq(0, 0.01, length=10)
harg.nse = sapply(harg.test, obj.example)

# identify and print the best one
idx.opt = which.max( harg.nse ) 
nse.opt = harg.nse[idx.opt]
harg.opt = harg.test[idx.opt]
print( paste0( 'NSE is maximized (', round(nse.opt, 3),') at harg_pet=', round(harg.opt, 3)) )
}

#' `obj.example` can be altered to include multiple parameters in `x` (a vector). In this way
#' an objective function of any number of SWAT+ parameters can be constructed and passed to an
#' optimizer. 
#' 
#' 
#' However, before we can run more sophisticated optimization routines we need to define
#' bounds for the parameters.

## TEMPORARY: fit the model with hardcoded parameter bounds
## Later this will be replaced with calls to the appropriate helper functions

library(dfoptim)

# load some documentation
rswat_pdf_open(reload=T, desc.maxlen=75)
calparms = rswat_open('cal_parms.cal')

# snow parameters and their bounds
rswat_docs('snow.sno')
parms.snow = rswat_find(include='snow.sno') %>% slice(1:7)
bds.snow = calparms %>% slice(26:30) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias, min, max)
bds.snow = rbind(bds.snow, data.frame(alias=c('snow_h2o', 'cov50'), min=c(0,0), max=c(1,1)))
cal.snow = cbind(parms.snow, bds.snow)

# aquifer parameters and their bounds
rswat_docs('aquifer.aqu')
parms.aqu = rswat_find(include='aquifer.aqu') %>% slice( c(11, 16, 12, 17, 13, 4) )
bds.aqu = calparms %>% slice(147, 149:151) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias, min, max)
bds.aqu = rbind(bds.aqu, data.frame(alias=c('rchg_dp', 'dep_bot'), min=c(0, 20), max=c(0.5, 25)))
cal.aqu = cbind(parms.aqu, bds.aqu)

# make an adjustment to some of the bounds
cal.aqu$max[ cal.aqu$name == 'alpha_bf' ] = 1/150 # make sure we get baseflow through the winter
cal.aqu$max[ cal.aqu$name == 'flo_min' ] = 23 # typo in description?? I think this should be in mm not m
cal.aqu$max[ cal.aqu$name == 'revap_min' ] = 23 # important for getting baseflow
cal.aqu$i = -51 # specify fitting to shallow aquifer only

# general hydrology parameters and their bounds (TODO: look into perco)
rswat_docs('hydrology.hyd')
parms.hyd = rswat_find(include='hydrology.hyd') %>% slice( c(3:5, 13) )
bds.hyd = calparms %>% slice(c(12:14,20)) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias,min,max)
cal.hyd = cbind(parms.hyd, bds.hyd)

# basin-wide hydrology parameters and bounds
rswat_docs('parameters.bsn')
parms.bsn = rswat_find(include='parameters.bsn') %>% slice(3)
bds.bsn = calparms %>% slice(55) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias,min,max)
cal.bsn = cbind(parms.bsn, bds.bsn)

# set some initial values
parms.ini = c(fall_tmp = 2,
              melt_tmp = 3,
              melt_max = 2,
              melt_min = 1,
              tmp_lag = 0.5,
              snow_h2o = 0.5,
              cov50 = 0.5,
              alpha_bf = 1/300,
              flo_min = 20,
              revap = 0.1,
              revap_min = 23,
              rchg_dp = 0.1,
              dep_bot = 22,
              can_max = 44,
              esco = 0.5,
              epco = 0.5,
              harg_pet = 0.0023,
              surq_lag = 11)

# combine everything into a single dataframe and add initial values
cal.all = rbind(cal.snow, cal.aqu, cal.hyd, cal.bsn) %>% mutate( ini = parms.ini )

##########


# construct an objective function
my_nse_2 = function(x, y) {my_nse(x, y, L=2, normalized=TRUE)}
cal.obj = my_objective(cal.all, gage, errfn=my_nse_2, quiet=TRUE)
cal.obj(refresh=TRUE)

# cal.obj(cal.all$ini)
# cal.gage = gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_0' 
# my_tsplot(cal.gage)


# initialize hydrograph comparison with simulations
parms.stor = cal.all %>% select(name, file, min, max) %>% mutate(fit_0=cal.all$ini)
score.stor = rep(NA, n.stages)
score.stor[1] = cal.obj(parms.stor$fit_0)
sim.stor = gage %>% left_join(rswat_flo(quiet=T), by='date')
names(sim.stor)[names(sim.stor) == 'flo'] = 'fit_0'

# initialize long sequence of model fitting stages with several iterations apiece
n.stages = 25
n.each = 250
control = list(maxfeval=n.each, maximize=TRUE)
for( idx in 1:n.stages)
{
  # stage startup message
  iternm = paste0('fit_', idx)
  cat(paste('\nrunning stage', idx, 'of', n.stages, '...\n'))
  timer.start = Sys.time()
  
  # run the fitting algorithm and copy score and parameter values
  hjkb.res = hjkb(parms.stor[[4+idx]], cal.obj, cal.all$min, cal.all$max, control)
  score.stor[idx] = hjkb.res$value
  parms.stor[[iternm]] = hjkb.res$par
  
  # stage end message
  timer.end = Sys.time()
  minutes.msg = round(difftime(timer.end, timer.start, units='mins')[[1]], 2)
  iternm.msg = paste0('"', iternm, '" (', hjkb.res$niter, ' iteration(s), ', hjkb.res$feval, ' simulations)')
  cat(paste('finished', iternm.msg, 'in', minutes.msg, 'minutes with score', round(score.stor[idx], 3), '\n'))
  
  # print subset of the history of parameter values
  parms.diff = parms.stor[[5+idx]] - parms.stor[[4+idx]]
  parms.display = parms.stor[,c(1:4, 4:5 + idx)] %>% mutate( diff = parms.diff )
  print(parms.display)
  
  # run the simulation with current parameters
  cal.obj(parms.stor[[5+idx]])
  sim.current = rswat_flo(quiet=T)
  sim.stor = sim.stor %>% left_join(sim.current, by='date')
  names(sim.stor)[names(sim.stor) == 'flo'] = iternm
  
  # plot most recent 3 simulations values against observed (flow) and initial
  sim.plot = sim.stor[, unique(c(1, 2, idx + 1:3))] %>% slice(500:1600)
  print(my_tsplot(sim.plot))
}


sim.plot = sim.stor %>% select(date, flow, fit_0, fit_14)
print(my_tsplot(sim.plot))

# temporary save
#saveRDS(list(sim.stor=sim.stor, score.stor=score.stor, parms.stor=parms.stor), 'hjkb_testing.rds')
#saveRDS(list(sim.stor=sim.stor, score.stor=score.stor, parms.stor=parms.stor), 'hjkb_testing2.rds')
xx = readRDS('hjkb_testing.rds')


saveRDS(list(sim.stor=sim.stor, score.stor=score.stor, parms.stor=parms.stor, xx=xx), 'hjkb_testing_finished.rds')


yy = sim.stor %>% 
  mutate(USGS_obs=flow, SWAT_with_bf=fit_14) %>%
  select(date, USGS_obs, SWAT_with_bf) %>%
  mutate(SWAT_no_bf = xx$sim.stor$fit_18) 

png('test.png', height=1200, width=1600)
my_tsplot( yy %>% slice(1200:2250) ) 
dev.off()

zz = yy %>% mutate(SWAT_avg = ( SWAT_no_bf + SWAT_with_bf ) / 2) %>% select(date, USGS_obs, SWAT_avg)

colors = c('deepskyblue', 'firebrick3')
my_tsplot( zz %>% slice(1200:2250), colors=colors) 

png('test.png', height=600, width=1200, pointsize=25)
ggp.out = my_tsplot( zz %>% slice(700:2250), colors=colors) 
ggp.out + theme(text = element_text(size = 20))
dev.off()



###########
#library(DEoptimR)
# now try again with DEoptimR

# first try with aquifer parameters fixed
cal.obj(refresh=TRUE)

aquifer.aqu = rswat_open('aquifer.aqu')
aquifer.aqu %>% head
# deep 
#aquifer.aqu$alpha_bf[51] = 1 this doesn't appear to be active
#aquifer.aqu$dep_wt[51] = 1000 this doesn't appear to be active
#aquifer.aqu$dep_bot[51] = 15 this doesn't appear to be active
# shallow
aquifer.aqu$dep_wt[-51] = 18
aquifer.aqu$dep_bot[-51] = 25
aquifer.aqu$gw_flo[-51] = 5
aquifer.aqu$alpha_bf[-51] = 1/300
aquifer.aqu$revap_min[-51] = 25
aquifer.aqu$rchg_dp[-51] = 0.1
aquifer.aqu$flo_min[-51] = 20


rswat_write(aquifer.aqu, preview=F)
sim.stor2 = gage %>% left_join(rswat_flo(quiet=T), by='date')
names(sim.stor2)[names(sim.stor2) == 'flo'] = 'fit_0'
my_tsplot(sim.stor2)
## THESE VALUES FOUDN BY MANUAL CALIBRATION



# new calibration dataframe without aquifer
parms.ini2 = cal.obj(refresh=TRUE) %>% filter(file != 'aquifer.aqu') %>% pull(value)
cal.all2 = rbind(cal.snow, cal.hyd, cal.bsn) %>% mutate( ini = parms.ini2 )

# construct an objective function
cal.obj2 = my_objective(cal.all2, gage, errfn=my_nse_2, quiet=TRUE)
cal.obj2(refresh=TRUE)

# initialize hydrograph comparison with simulations
parms.stor2 = cal.all2 %>% select(name, file, min, max) %>% mutate(fit_0=parms.ini2)

# set up storage
sim.stor2 = gage %>% left_join(rswat_flo(quiet=T), by='date')
names(sim.stor2)[names(sim.stor2) == 'flo'] = 'fit_0'
my_tsplot(sim.stor2)

# initialize long sequence of model fitting stages with 10 iterations apiece
n.stages = 25
n.each = 50
control = list(maxfeval=n.each, maximize=TRUE)
for( idx in 1:n.stages)
{
  # stage startup message
  iternm = paste0('fit_', idx)
  cat(paste('\nrunning stage', idx, 'of', n.stages, '...\n'))
  timer.start = Sys.time()
  
  # run the fitting algorithm and copy score and parameter values
  hjkb.res = hjkb(parms.stor2[[4+idx]], cal.obj2, cal.all2$min, cal.all2$max, control)
  score.stor2[idx] = hjkb.res$value
  parms.stor2[[iternm]] = hjkb.res$par
  
  # stage end message
  timer.end = Sys.time()
  minutes.msg = round(difftime(timer.end, timer.start, units='mins')[[1]], 2)
  iternm.msg = paste0('"', iternm, '" (', hjkb.res$niter, ' iteration(s), ', hjkb.res$feval, ' simulations)')
  cat(paste('finished', iternm.msg, 'in', minutes.msg, 'minutes with score', round(score.stor2[idx], 3), '\n'))
  
  # print subset of the history of parameter values
  parms.diff = parms.stor2[[5+idx]] - parms.stor2[[4+idx]]
  parms.display = parms.stor2[,c(1:4, 4:5 + idx)] %>% mutate( diff = parms.diff )
  print(parms.display)
  
  # run the simulation with current parameters
  cal.obj2(parms.stor2[[5+idx]])
  sim.current = rswat_flo(quiet=T)
  sim.stor2 = sim.stor2 %>% left_join(sim.current, by='date')
  names(sim.stor2)[names(sim.stor2) == 'flo'] = iternm
  
  # plot most recent 3 simulations values against observed (flow) and initial
  sim.plot = sim.stor2[, unique(c(1, 2, idx + 1:3))] %>% slice(500:1600)
  print(my_tsplot(sim.plot))
}

my_tsplot(sim.stor2 %>% select(date, flow, fit_0, fit_10))






# 
# # note the initial objective score and plot the initial hydrograph
# score.seqcal.obj(parms.seq$iter_0)
# cal.gage = gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_0' 
# my_tsplot(cal.gage)
# 
# # try a short hooke-jeeves optimization and copy the fitted parameters
# control = list(maxfeval=10, maximize=TRUE)
# hjkb.res = hjkb(cal.all$ini, cal.obj, cal.all$min, cal.all$max, control)
# cal.all$sim_10 = cal.obj(refresh=T)$value
# 
# # check how the score and hydrograph are looking
# cal.scores['sim_10'] = cal.obj(cal.all$sim_10)
# cal.gage = cal.gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_10' 
# my_tsplot(cal.gage)
# 
# # try a longer hooke-jeeves optimization - setting initial values to where we left off
# control = list(maxfeval=25, maximize=TRUE)
# hjkb.res = hjkb(cal.all$sim_10, cal.obj, cal.all$min, cal.all$max, control)
# cal.all$sim_25 = cal.obj(refresh=T)$value
# 
# # check how the score and hydrograph are looking
# cal.scores['sim_25'] = cal.obj(cal.all$sim_25)
# cal.gage = cal.gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_25' 
# my_tsplot(cal.gage)
# 
# # try a much longer hooke-jeeves optimization - setting initial values to where we left off
# control = list(maxfeval=100, maximize=TRUE)
# hjkb.res = hjkb(cal.all$sim_25, cal.obj, cal.all$min, cal.all$max, control)
# cal.all$sim_100 = cal.obj(refresh=T)$value
# 
# # check how the score and hydrograph are looking
# cal.scores['sim_100'] = cal.obj(cal.all$sim_100)
# cal.gage = cal.gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_100' 
# my_tsplot(cal.gage)
# 

# # try an overnight hooke-jeeves optimization - setting initial values to where we left off
# control = list(maxfeval=1000, maximize=TRUE)
# hjkb.res = hjkb(cal.all$sim_100, cal.obj, cal.all$min, cal.all$max, control)
# cal.all$sim_1000 = cal.obj(refresh=T)$value
# 
# # check how the score and hydrograph are looking
# cal.score.1000 = cal.obj(cal.ini.1000)
# cal.gage = cal.gage %>% left_join(rswat_flo(quiet=T), by='date')
# names(cal.gage)[names(cal.gage) == 'flo'] = 'sim_1000' 
# my_tsplot(cal.gage)



# TODO: configure the parameters in rswat_open('topography.hyd')
# TODO: switch codes.bsn::cn to 1, calibrate (see rswat_docs('cncoef'))
# TODO: check out other parameters in "parameters.bsn" such as 'scoef'
# TODO: look into bf_max - is it inactive or what is the deal?


rswat_open('snow.sno')
rswat_open('aquifer.aqu')
rswat_open('hydrology.hyd')

vals[which( obj()$name == 'flo_dist' )] = 1  # inactive?
vals[which( obj()$name == 'dep_bot' )] = 6
vals[which( obj()$name == 'dep_wt' )] = 1



rswat_docs('spec_yld')
rswat_docs('revap_min')
rswat_docs('revap', fuzzy=-1, descw=0)


cal.hyd = rswat_find(include='hydrology.hyd')
cal.bsn = rswat_find(include='parameters.bsn')


rswat_find('esco|can_max|surq_lag')

rswat_docs('timp')

rswat_find(include='hydrology.hyd')
rswat_find(include='ovn_table.lum')
rswat_find(include='hyd-sed-lte.cha')
rswat_find(include='codes.bsn')




#' 
#' Code below is in development
#' 
#' 
#' 
#' 
#' 
#' 
#' 

cal.par[grep('awc', cal.nm),]


cal.par = rswat_open('cal_parms.cal')
cal.nm = cal.par$name
swat.par = rswat_find() %>% 
  filter(class=='numeric') %>% 
  select(-string, -class)
swat.nm = swat.par$name

charvec = rswat_find(trim=f)

xx = sapply(cal.nm, function(x) rswat_match( x, swat.nm, fuzzy=2 )[1] )
data.frame(swat=swat.nm[xx], cal=cal.nm)


x = 'canmx'
x = cal.nm[1]
rswat_match(x, swat.nm, fuzzy=2, index=F)

charvec[  ] %>% head

cal.par %>% filter(obj_typ=='hru')
rswat_cio()
rswat_open()

rswat_open('snow.sno')
pattern = 'snofall_tmp'
pattern = 'IPET'
pattern = 'sed'

rswat_find(pattern)

rswat_match(index=F)

pattern = c('gw', 'revap', 'revap_min', 'alpha_bf', 'rchg_dp', 'dep_bot', 'bf_max', 'dep_wt', 'flo_dist')
rswat_find(pattern)



# test cases
cal.snow = rswat_find(include='snow.sno')
cal.hyd = rswat_find(include='hydrology.hyd')
cal.bsn = rswat_find(include='parameters.bsn')

bds.all = rswat_open('cal_parms.cal')




# 
rswat_find(ignore=) %>% 
  filter(class == 'numeric') %>% 
  group_by(file) %>% 
  summarize(nvar=n())


rswat_cio()

fname.include = c('parameters.bsn', 'nutrients.cha')
vname.ignore = c('name', 'description')

rswat_open(fname.include)

group.include = c('simulation', 'connect', 'climate', 'routing_unit', 'hru', 'exco')
rswat_cio() %>% filter(group=='simulation') %>% 
  pull(file) %>% rswat_open

rswat_open('bmpuser.str')

rswat_open()
rswat_open('exco_om.exc')


pattern = cal.hyd$name

rswat_match(cal.hyd$name, bds.all$name, fuzzy=-2, index=FALSE)
rswat_match(cal.snow$name, bds.all$name, fuzzy=-Inf, index=FALSE)
rswat_match(cal.bsn$name, bds.all$name, fuzzy=-1, index=FALSE)


rswat_match(pattern, bds.all$name) %>% c

pattern = 'can_max'







#rswat_open('cal_parms.cal') %>% filter( agrepl('harg', name, max.distance=2))


##
##










#+ include=FALSE
# Development code
#my_markdown('demo_objective', 'R/demo')


