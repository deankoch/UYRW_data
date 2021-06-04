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

# grab directory of the project folder and its backup
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
rswat_ohg_toggle(overwrite=TRUE, oid=id.outlet)

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
rswat_ohg_toggle(overwrite=TRUE, delete=TRUE)

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
harg.test = seq(0, 0.005, length=8)

# time the execution to compare with parellelized version below:
timer.start = Sys.time()
harg.nse = sapply(harg.test, obj.example)
timer.end = Sys.time()

# print runtime in seconds
rsec.msg = round(difftime(timer.end, timer.start, units='secs')[[1]], 2)
cat( paste(rsec.msg, ' seconds\n') )

# identify and print the best one
idx.opt = which.max( harg.nse ) 
nse.opt = harg.nse[idx.opt]
harg.opt = harg.test[idx.opt]
print( paste0( 'NSE is maximized (', round(nse.opt, 3),') at harg_pet=', round(harg.opt, 3)) )

#' `obj.example` could be altered to include multiple parameters in `x` (a vector). In this way
#' an objective function of any number of SWAT+ parameters can be constructed and passed to an
#' optimizer. The new function would do a series of `rswat_open` calls, new parameter assignments,
#' then a series of `rswat_write` calls - one for each file corresponding to one of the parameters
#' in `x` 
#' 
#' To simplify this type of code, we have the function `rswat_amod`. It returns an anonymous
#' function, customized for quickly modifying a selection of model parameters. This provides a shortcut
#' for when we need to repeatedly read/write certain parameters, such as when defining an objective
#' function like `obj.example`.
#' 
#' To specify the desired selection of parameters, simply pass the output of `rswat_find` to
#' `rswat_amod`. This can be done with (`magittr`) pipes, if you're into that sort of thing:
#' 

# make sure that we get the variable of interest from the `rswat_find` call
rswat_find('harg_pet')

# pipe it to `rswat_amod` to define the function, which we are calling "harg_pet"
harg_pet = rswat_find('harg_pet') %>% rswat_amod

# calling the function without arguments produces the current value and some metadata
harg_pet()

# modify the parameter on disk with a one-liner (overwrites "hydrology.hyd"!)
harg_pet(0.0023)

#' This example has one parameter, but any number of parameters (in any number of files) can
#' be specified. For example the code below shows how to modify all of the (numeric) parameters
#' in "snow.sno":

# define a function for quickly querying/modifying parameters in the file "snow.sno" 
snow.sno = rswat_find(include='snow.sno') %>% rswat_amod

# print info about the current values 
snow.sno()

# make a small adjustment and write the change
snow.newvals = snow.sno()$value + 1e-2 
snow.sno(snow.newvals)

# restore the old values
snow.sno(snow.newvals - 1e-2)

#' `rswat_amod` becomes very useful when we move beyond these toy examples and starting selecting
#' larger subsets of parameters for tuning. This is introduced along with parallel processing in the
#' next demo script 
#' 


#+ include=FALSE
# Development code
#my_markdown('demo_objective', 'R/demo')


