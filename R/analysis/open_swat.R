
# DEVELOPMENT
# demo some functions for opening and writing to SWAT+ text files

library(here)
source(here('R/swat/get_swathelperfun_develop.R'))

# define the SWAT+ text IO directory
swat.name = 'millcreek_sandbox'
swat.projdir = file.path('data/prepared/qswatplus', swat.name)
textio.subdir = 'Scenarios/Default/TxtInOut'
textio = here(file.path(swat.projdir, textio.subdir))

## DEVELOP

# some vignette code:

#' define the path to your 'file.cio' and pass it to `rswat_cio` to get a dataframe of filepaths in the project

ciopath = file.path(textio, 'file.cio')
rswat_cio(ciopath)


#' subsequent calls can drop the `ciopath` argument

# eg. get a list of groups
unique(rswat_cio()$group)

# eg. using dplyr pipes: get a list of files in the 'simulation' group
rswat_cio() %>% filter(group=='simulation') %>% pull(file)


#' after the `rswat_cio(ciopath)` call, config files are accessed using `rswat_open`. By
#' default they are only loaded once, with subsequent calls to `rswat_open` loading from memory

# pass a name from the 'file' column of `rswat_cio()` to open the file. Copy it to variable `x`
x =  rswat_open('time.sim')

# the data are in the form of a list of dataframes, named by source file
summary(x)
print(head(x))

# delete `x` and start over - no load message this time because it's already in memory
rm(x)
x = rswat_open('time.sim') 
print(head(x))


#' use `reload=TRUE` to force a reload (eg after writing changes with an external program)

# nothing is returned, but this function call has the side effect of loading the file into memory 
rm(x)
rswat_open('time.sim', reload=TRUE) 
# future `rswat_open('time.sim')` calls will be much faster

#' multiple files can be requested in one call

# vectors of file names are accepted
y = rswat_open(c('time.sim', 'recall.rec', 'harv.ops'))
summary(y)

# group names are also accepted
z = rswat_open('simulation')
summary(z)


#' after a file has been loaded, `rswat_cio` will return some additional info 

# info on number of tables, fields, and number of lines skipped by the file parser (should be zero!)
rswat_cio(ciopath) %>% filter(group == 'simulation')

#' to load everything, just do `rswat_open` without the name argument

# import all files not already loaded into memory
a_very_big_list = rswat_open()
summary(a_very_big_list)
rm(a_very_big_list)

# reload the entire project
rswat_open(reload=TRUE)

# `rswat_cio` now returns table and variable counts for all files
rswat_cio() %>% arrange(desc(nvar)) %>% head


#' the decision table files tend to be large and complicated (many distinct tables), so I exclude them by default
#' from the "open everything" `rswat_open()` calls. But they can still be accessed like any other file:

# files specified in `exclude` argument to `rswat_cio` are hidden by default. Make them visible with `trim=FALSE`
rswat_cio(trim=FALSE) %>% filter(group=='decision_table')

# load them by providing their file or group name to `rswat_open`
a_decision_table = rswat_open('scen_lu.dtl')
summary(a_decision_table)


#' To flush a project (from R) and start over, use the `wipe` argument

# after flushing, you will need to enter the file.cio path again before you can load anything
rswat_cio(wipe=TRUE)
rswat_cio()


#' Note that switching to a new `file.cio` path will automatically flush whatever is currently loaded
#' in the package. 
#' TODO: maybe put a demo here of comparing data copied from two projects

#' the `reload` option in will import everything in one call

rswat_cio(ciopath, reload=TRUE)


#' `rswat` compiles a big list of SWAT+ parameters, which is searchable using `rswat_find`

# the default behaviour is to treat the input as a regular expression (using R's `grepl`)
rswat_find('lai')

# it can also be set to return exact matches only
rswat_find('lai_pot', exact=TRUE)

# with no arguments, `rswat_find` returns info about all parameters. This can be piped into more nuanced queries
rswat_find() %>% 
  filter(class=='integer') %>% 
  filter(grepl('^aqu', file)) %>%
  filter(tabular) 

# the `trim=FALSE` option shows information about the text in the file representing the parameter 
rswat_find('lai', trim=FALSE) %>% arrange(file, line_num, field_num)









