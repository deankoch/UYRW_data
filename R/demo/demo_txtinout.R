#' ---
#' title: "demo_txtinout.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_txtinout.R**: managing SWAT+ config and output files with `rswat` 
#' 
#' This script demonstrates some of the core functionality of the `rswat` helper functions:
#' accessing/changing parameters and settings for the SWAT+ simulator, running simulations,
#' and loading outputs.

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


#' load the SWAT+ project info and gage data from the previous script
#' ([demo_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md))

# load some info about the SWAT+ model
qswat.meta = my_metadata('demo_qswat', data.dir=demo.subdir)
dir.qswat = qswat.meta['dir_qswat', 'file']
nm = qswat.meta['example_name', 'file']
print(nm)

# load the gage data for this catchment
gage = readRDS(here(qswat.meta['gage', 'file']))
head(gage)

# set up the SWAT+ project config folder location to use in the demo
zip.path = here( qswat.meta['txtinout', 'file'] )
demo.dir = file.path( demo.subdir, gsub( '.zip', '', basename(zip.path) ) )

# store filenames created by this script
files.towrite = list(
  
  # directory for SWAT+ model files
  c(name='txtinout',
    file=demo.dir,
    type='string',
    description='directory for SWAT+ config files to use in demo'),
  
  # directory for SWAT+ model files
  c(name='txtinout_bak',
    file=file.path(demo.dir, 'backup_harg'),
    type='string',
    description='directory for SWAT+ config files to use in demo')
)
  
# write this filename metadata to disk
txtinout.meta = my_metadata('demo_txtinout', files.towrite, overwrite=TRUE, data.dir=demo.subdir)

# unzip a fresh copy of "TxtInOut" to the demo folder
print(demo.dir)
unlink(here(demo.dir), recursive=TRUE)
unzip(zip.path, exdir=here(demo.dir), overwrite=TRUE)

#' 
#' ## managing SWAT+ configuration files in R
#' 
#' The SWAT+ executable runs simulations according to the settings written
#' in a directory of text files, usually called 'TxtInOut'. This is a large and complicated
#' set of files that define all parameters of model. `rswat` is a set of tools for managing and cataloging
#' these files. 
#' 
#' If you are new to SWAT, check out the [I/O](https://swatplus.gitbook.io/docs/user/io)
#' and [theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs. The I/O descriptions are relatively new
#' but the theory document is older, from 2009. This is the most recent, as far as I am aware, and although core aspects
#' of the the model have not changed much since then, note that many variable and parameter names are slightly
#' different in SWAT+.
#' 
#' Once a SWAT+ model has been created (see
#' [demo_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md)), make it known to
#' `rswat` by passing the 'TxtInOut' path to `rswat_cio`. This will set the project directory and read in the
#' master watershed file "file.cio". This should work with any up-to-date SWAT+ project regardless of how it was
#' created (ie. you don't have to use `qswat_run`)

# assign the SWAT+ project directory. This builds a list of files in memory
cio = rswat_cio( here(demo.dir) )

#' Subsequent calls to rswat_cio() will list all config files in the project directory. eg here are the first
#' few entries:
# print the first few rows of the files dataframe
cio %>% head

#' Each row of `cio` is a file containing a group of model parameters. Before changing anything it's a good
#' idea to have a backup. `rswat_copy` with argument `fname='.'` will copy the entire contents of the config
#' files directory (excluding subdirectories) to a backup subdirectory

# the return value is a vector of file paths to the backups
path.backup = rswat_copy(fname='.', quiet=TRUE)
dir.backup = dirname(path.backup[1])
print(dir.backup)

#' to start over later, restore this copy by passing the backup directory path back to `rswat_copy`

# restore the backup we just made
opath = rswat_copy(from=dir.backup, overwrite=TRUE, quiet=TRUE)

# delete the backup (we already have a zipped copy elsewhere)
unlink(dir.backup, recursive=TRUE)

#' To load a config file, pass its filename to `rswat_open` eg. the code below displays the first few rows
#' from the SWAT+ aquifer parameters file, "aquifer.aqu"

# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print

# this one contains the main process model parameters 
rswat_open('aquifer.aqu', quiet=TRUE) %>% str

#' After rswat loads a SWAT+ config file into R, it caches the contents, and the file becomes searchable and
#' faster to open/write in subsequent calls. `rswat_cio()` will also return some additional summary info:

# print summary info about the file 'aquifer.aqu'
rswat_cio() %>% filter( file=='aquifer.aqu' )

#' The 'nline' column counts the total number of data rows in the file; 'nskip' indicates lines not understood
#' by the parser (probably comments, but possibly bugs - best to check this manually); 'ntab' is the number of
#' distinct tables in the file (either having different headers, or being separated by comments), and 'nvar'
#' indicates how many distinct fields there are in the file (nrow * ncol, summed over all of the tables,
#' including headers); 
#' 
#' To pre-load all files, use the `loadall` flag with `rswat_cio`. This will take a few seconds to parse all the
#' plaintext and detect headers, types, and spacing rules. The code below excludes decision table files, which
#' contain many distinct tables (slow to load) and are not needed for now.

# load most of the config files into memory for convenience.
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
print(cio)

#' The summary info is now available for most of the files (get even more detail by toggling `trim=FALSE`)
#' and any variable names appearing among the table headers of these files now becomes searchable using
#' `rswat_find`. This can be useful for tracking down a SWAT+ parameter using keywords or SWAT2012 names.
#' This uses fuzzy case-insensitive matching (see R's `?agrep` doc), which catches many of the name changes
#' in the SWAT2012 -> SWAT+ updates.
#' 
#' eg. the following code finds the PET estimation method parameter 'pet', which was called 'IPET' in SWAT2012:

# fuzzy > 0 allows inexact matches
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print

#' We get three matches for 'IPET', located in two files. The 'string' field (second column) shows the
#' plaintext representation of a parameter in the SWAT+ config file (fifth column).
#' 
#' We can see that 'pet' (in 'codes.bsn') is set to 1. This codes for the Penman-Monteith model for
#' potential evapotranspiration (PET). The other two matches, 'pet_file' and 'harg_pet', are an input file
#' for observed PET, and a solar radiation coefficient used with the Hargreaves-Samani model; Neither is
#' currently used so we don't worry about them for now.  
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

#' The default behaviour of `rswat_write` is to preview the requested change, so the following code doesn't 
#' overwrite anything on disk:

# preview changes
rswat_write(codes) %>% str

#' The 'current_value' and 'replacement' fields are as expected, so we can go ahead and overwrite the file on disk
#' with argument `preview=FALSE`. 

# write the changes
rswat_write(codes, preview=FALSE, quiet=TRUE)

#' "codes.bsn" is now configured such that SWAT+ simulations will now use Hargreaves-Samani for PET. This model
#' has a parameter 'harg_pet', that turned up earlier in the search for 'IPET'. Since it's no longer inactive,
#' we need to assign it a sensible value.

# open the container file for 'harg_pet' and print a summary
hydro = rswat_open('hydrology.hyd')
hydro %>% str

#' The parameters in this file are all length-50 column vectors. This is because there are 50 channels in this
#' model, and SWAT+ allows distinct PET parameters for the HRUs associated with each of them. From the table above
#' we can see that SWAT+Editor has assigned them all the default value 0, which means the model currently assumes
#' no evapotranspiration is happening. 
#' 
#' For now we just set all HRUs to the same default value appearing in the
#' [I/O docs](https://swatplus.gitbook.io/docs/user/io), which is an empirical estimate from Hargreaves and
#' Samani [(1985)](https://elibrary.asabe.org/abstract.asp?aid=26773) based on Alta Fescue grass in California.
#' Local calibration (if needed) can be done later.

# assign the default 'harg_pet' value in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)

#' Hargreaves-Samani may be the best choice for this project since we lack the detailed data on humidity,
#' wind, and solar energy required with Penman-Monteith. SWAT+ can generate those missing data using a
#' stochastic process, but the result is imprecise at the daily scale.
#' 
#' ## running a SWAT+ simulation
#' 
#' In computational model design and fitting there is a lot of back and forth between parameter settings
#' and simulations. We adjust parameters, run a simulation, look for changes in state variables of interest,
#' then repeat (many times). `rswat` has utilities to streamline this process from R.
#'
#' `rswat_exec` runs a simulation by making a system call to run the SWAT+ executable. This reads in parameters
#' from the config files in the current project directory. These include the time period to simulate over
#' (specified in the file 'time.sim'), and the time period to include in output files (fifth table of 'print.prt').
#' These can be adjusted manually, or using a helper function as shown below:
#'  

# `rswat_time` without arguments prints the current settings in 'time.sim'
rswat_time()

#' If the model was created with `qswat_run` (as it was here), then these dates currently specify
#' a one-day simulation at the very beginning of the supplied weather time series. The code below changes
#' them to match the time series in `gage` (adjusting 'print.prt' to match), then calls the SWAT+ executable
#' to run a simulation with daily timesteps:

# pass a range of dates or dataframe with a 'date' field to set up simulation start/end dates
rswat_time(gage, daily=TRUE)

#' The time series in `gage` is around seven years long, and the daily simulation takes about 18-20 seconds
#' to complete on my PC

# run a simulation
fout = rswat_exec()
print(fout)

#' the return value is a vector of output files generated. The next section shows how to read these files
#' with R.

#' 
#' ## viewing simulation output data
#' 
#' The SWAT+ executable produces output in the form of .txt tables containing simulated state variables. There
#' can be many such output tables (100+) in a given simulation, depending on the settings in 'print.prt' and
#' 'object.prt'.
#' 
#' The helper function `rswat_output` will catalog available output variables and filenames, and import
#' them into R as dataframes.

# get a dataframe with info on the available SWAT+ output files in the project directory
odf = rswat_output()

# print the first few lines of the dataframe, omitting paths for tidyness
odf %>% select(-path) %>% head

#' Output files can be loaded as R dataframes by specifying their filename

# load an example file. Dates and units are added automatically 
fname.eg = 'aquifer_day.txt'
aqu.day = rswat_output(fname.eg)
aqu.day %>% str

#' Specify a subset of output variables with argument `vname`. Some columns, like 'gis_id', are always
#' loaded by default. They are indexing fields needed to identify a row with a specific spatial
#' object in the model. However, this functionality - along with date-parsing - can be switched off to get faster
#' load times, or for debugging purposes:

# print the first few lines for two particular variables (recharge and lateral flow to/from aquifer)
vname.eg = c('flo', 'rchrg')
rswat_output(fname.eg, vname=vname.eg, makedates=FALSE, showidx=FALSE) %>% head

#' When an output file is scanned in by `rswat` its headers and units are added to a database of
#' known SWAT+ outputs. Variable names in this database can then be searched by calling `rswat_output`
#' without the `fname` argument:
#' 

# search for output variables named "rchrg". The one loaded above is identified, and a few others
rswat_output(vname='rchrg') %>% print

#' 
#' Right now the database only includes the files currently found in the SWAT+ project folder ("TxtInOut").
#' To get an more exhaustive list of possible outputs, rswat can optionally run a short (1-day) simulation,
#' requesting all outputs, in order to parse all output file headers (before restoring the original
#' state of the project folder)

# build database of SWAT+ outputs using `loadall` flag
odf = rswat_output(loadall=TRUE)

#  print the first few lines (omit paths for tidiness)
odf %>% select(-path) %>% head

#' Notice the filenames list now includes entries with NA fields for 'size', 'modified' (and 'path',
#' though it is not shown here). These are files not currently found in 'TxtInOut' but which can be enabled
#' in SWAT+ simulations. Since the above function call cached their headers they are now searchable:

# repeat the search for "rchrg" and find a new (monthly outputs) matches
rswat_output(vname='rchrg') %>% print

#' SWAT+ potentially generates lot of data in a given simulation depending on the output settings. This slows
#' things down, and we need to avoid that in model fitting. The next script in this series,
#' [demo_objective](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_objective.md), uses an
#' alternative output mode in SWAT+ to speed up simulations, then demonstrates some basics of SWAT+ model
#' fitting in R.
#' 
#' Make a backup of the "TxtInOut" folder to pick up from later
fout = rswat_copy(to=here( txtinout.meta['txtinout_bak', 'file'] ), fname='.', quiet=TRUE)


#+ include=FALSE
# Development code
#my_markdown('demo_txtinout', 'R/demo')


