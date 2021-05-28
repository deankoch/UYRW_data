#' ---
#' title: "rswat_output"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **rswat_output**: R functions for executing SWAT+ simulations and reading their outputs
#' 
#' See the vignettes
#' [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md) and
#' [demo_objective](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_objective.md)
#' for examples of basic usage without parallelization, and
#' [demo_parallel](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_parallel.md)
#' for a demonstration of how to use `rswat_pexec` and related functions.
#' 
#' ## libraries
#' 
#' The following libraries  are only needed if you're using the parallel execution functions
#' `rswat_cluster`, `rswat_pexec`, or `rswat_rswat_pardevol`:
#'
#' [`parallel`](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf) is an R
#' core package that adds support for parallel computing
library(parallel)

#' [`DEOptimR`](https://cran.r-project.org/web/packages/DEoptimR) is an implementation of
#' differential evolution algorithms for nonlinear gradient-free optimization problems
library(DEoptimR)

#'
#' ## dependencies
#' 
#' "rswat_output.R" is an extension of
#' "[rswat_config.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_config.md)",
#' not a standalone library of helper functions (like "rswat_docs.R"). Most of the function
#' definitions below make use of other functions defined in "rswat_config", so it needs to
#' be sourced for them to work.
#' 

#' 
#' ## output readers
#' 
#' functions for opening and cataloguing SWAT+ output files/variables
#' 

#' read a SWAT+ output file as dataframe, or return a list of files or output variables
rswat_output = function(fname=NULL, vname=NULL, makedates=TRUE, showidx=TRUE, loadall=FALSE)
{
  # ARGUMENTS:
  #
  # `fname`: character, the SWAT+ output filename with extension
  # `vname`: character vector, (optional) names of the columns to read
  # `makedates`: boolean, whether to add Date class column, replacing 'day', 'jday', etc
  # `showidx`: boolean, whether to include indexing variables
  # `loadall`: boolean, whether to run a dummy simulation to discover all possible outputs
  #
  # RETURN VALUE:
  #
  # in read mode (`fname` supplied):
  # 
  # Returns a dataframe containing the requested variable(s) `vname` from file `fname`.
  # This matches the table in the file, with headers assigned to column names, appropriate
  # classes for the data vectors, and any available units (on header lines) assigned. 
  #
  # in list mode (`fname` not supplied):
  # 
  # If `fname` and `vname` are both NULL (the default), the function returns a dataframe of
  # info about output files detected in the textio directory. If `vname` is non-NULL the
  # function does a text search for `vname` among all known output variable names, and returns
  # a dataframe of info on exact matches. 
  #
  #
  # DETAILS
  #
  # Arguments 'makedates' and 'showidx' are ignored in list mode
  #
  # Get a list of valid arguments for `fname` by running `rswat_output()` or `rswat_init()`.
  #
  # Files that don't exist in the project folder (but are available from the SWAT+ executable)
  # can be browsed with `rswat_output(vname='')`, which returns the full database on known SWAT+
  # outputs. 
  #
  # `showidx=TRUE` specifies to include all indexing columns (GIS ids, names, date, etc)
  # in the result, even if they are left out of `vname`. `makedates=TRUE` adds Date object
  # column to match the dates in the file, and omits the 'jday', 'day', 'mon', and 'yr'
  # columns (regardless of `showidx` and `vname`). 
  #
  # If the function runs into an error while parsing, `fread` is attempted on the file
  # (and the result returned, if successful). 
  # 
  # Note: Date parsing is currently disabled for the year 1900.
  # see: https://groups.google.com/g/swatplus/c/gJ5Mvr_SYo8/m/aiHKeHKnDgAJ
  
  
  # magic line numbers for the first data rows in 'ohg' and 'prt' type output files
  ln.magic = list(prt=4, ohg=2, unknown=4)
  
  # set default path for 'textio' when `rswat_cio` has been called
  if( exists('.rswat') )
  {
    # copy the path from the package environment
    ciopath = .rswat$ciopath
    if( is.null(ciopath) ) stop('`textio` not found. Try setting `ciopath` with `rswat_cio`')
    
    # copy parent directory
    textio = dirname(ciopath)
    
  } else stop('`textio` not found. Either call `rswat_cio` or provide `textio`')
  
  # run dummy simulation to discover output filenames and headers, if requested
  if( loadall )
  {
    # run a simulation to generate dummy output files to populate databases
    cat('running SWAT+ to generate all output files...\n')
    fpath.dummy = rswat_odummy(quiet=TRUE)
    textio.dummy = dirname(fpath.dummy[1])
    
    # parse the dummy files (creates/modifies .rswat$stor$output)
    rswat_oparse(textio=textio.dummy)
    
    # remove the dummy simulation files
    unlink(textio.dummy, recursive=TRUE)
    
    # force re-scan of project directory for existing output files
    fname.ref = rswat_oscan(textio=textio, fast=FALSE)
  }
  
  # get a copy of output variable and file names
  vname.ref = rswat_ofind(trim=FALSE)
  fname.ref = rswat_oscan(fast=FALSE)
  
  # LIST MODE:
  if( length(fname) == 0 )
  {
    # when no variable name is supplied, return output filenames list
    if( length(vname) == 0 ) return(fname.ref) 
    
    # otherwise return search results for this variable name (exact matching)
    return( rswat_ofind(vname, fuzzy=0, trim=TRUE) )
  }
  
  # if `fname` not found in database, re-scan SWAT+ folder 
  if( !(fname %in% fname.ref$file) ) fname.ref = rswat_oscan()
  
  # catch unrecognized filenames 
  msg.unknown = paste(fname, 'not found in', textio)
  if( !(fname %in% fname.ref$file) ) stop( msg.unknown )
  
  # catch recognized but nonexistent files 
  msg.nofile = 'Check that it is enabled in `print.prt` and/or run the simulation again.'
  fpath = file.path(textio, fname)
  if( !file.exists(fpath) ) stop(paste(msg.unknown, msg.nofile, sep='. '))
  
  # parse 'files_out.out' with a different helper function
  fout.path = file.path(textio, 'files_out.out')
  if( fname == basename(fout.path)) return( rswat_fout(fout.path, checkohg=FALSE) ) 
  
  # parse variable names if they aren't already in the database
  if( !(fname %in% vname.ref$file) ) vname.ref = rswat_oparse(fname=fname)
  idx.vname = fname == vname.ref$file
  
  # potentially messy plaintext parsing routines start here
  dat = tryCatch(
    
    expr = {
      
      # set expected column types of all columns in the file
      head.nm = vname.ref[['name']][idx.vname]
      head.class = vname.ref[['class']][idx.vname]
      head.units = vname.ref[['units']][idx.vname]
      head.index = vname.ref[['index']][idx.vname]
      
      # catch duplicated table headers
      nm.dupe = NULL
      head.isdupe = duplicated(head.nm)
      if( any(head.isdupe) )
      {
        # new names for duplicated headers 
        nm.dupe = paste0('rswat_duplicate_', 1:sum(head.isdupe))
        
        # the first instance of a duplicate can be accessed by name, the rest are renamed
        head.nm[ head.isdupe ] = nm.dupe
      }
      
      # set up required columns when adding dates
      date.nm = c('day', 'mon', 'yr')
      req.nm = NULL
      if(makedates)
      {
        # date-parsing requires 'day', 'mon', 'yr'
        req.nm = date.nm[date.nm %in% head.nm]
      }
      
      # set up indexing columns if requested - these are all non-numeric class
      if(showidx) req.nm = unique( c(head.nm[ head.index ], date.nm) )
      
      # default behaviour is to load all variables
      if( is.null(vname) ) vname = unique( head.nm[ !head.isdupe ] )
      
      # omit unrecognized variable names and append any missing required ones
      vname = vname[vname %in% head.nm]
      vname = unique(c(vname, req.nm))
      vname = vname[ order( match(vname, head.nm) ) ]
      
      # set the number of lines to skip (comments, headers, units) and timestep
      idx.fname = fname == fname.ref$file
      ln.skip = ln.magic[[ fname.ref[['type']][idx.fname] ]] - 1
      tstep = fname.ref[['step']][idx.fname]
      
      # handle requests for output files that aren't ohg or prt type 
      if(length(ln.skip) == 0) stop('unrecognized output file format')
      
      # catch empty tables by scanning in first expected data line
      if( length( readLines(fpath, n = ln.skip + 1) ) < ( ln.skip + 1 ) )
      {
        # prepare an empty dataframe with correctly named columns
        dat = as.data.frame(matrix(NA, 0, length(vname), dimnames=list(NULL, vname)))
        
        # turn off 'makedates' mode (they aren't available in the source file)
        makedates = FALSE
        
      } else {
        
        # load the requested columns using fread 
        fread.cols = split(match(vname, head.nm), head.class[head.nm %in% vname])
        dat = fread(fpath, header=FALSE, skip=ln.skip, select=fread.cols) %>% 
          relocate(order(unlist(fread.cols))) %>%
          as.data.frame %>% 
          setNames(vname)
      }
      
      # assign units to output dataframe
      out.units = head.units[ head.nm %in% vname ]
      has.units = !is.na( out.units )
      if( any(has.units) )
      {
        # mapply over columns of `dat` where units are defined
        dat[has.units] = mapply(function(x, y) set_units(x, y, mode='standard'),
                                x = as.list(dat)[ has.units ], 
                                y = out.units[ has.units ], 
                                SIMPLIFY = FALSE)
      }
      
      # parse dates and append a 'Date' class column, if requested
      if( makedates )
      {
        # date parsing handled by a helper function
        all.dates = rswat_makedates(dat, tstep)
        
        # merge with output data, omitting redundant columns
        nm.omit = c('jday', date.nm)
        idx.redundant = (names(dat) %in% nm.omit)
        dat = cbind(data.frame(date=all.dates), dat[ !idx.redundant ])
      }
      
      # return from trycatch expression
      return(dat)
      
    },
    # redirect function call to data.table::fread on errors
    error = function(err) {
      
      cat('rswat failed to parse the file. Attempting fread...\n')
      fread( file.path(textio, fname) )
    }
  )
  # end of trycatch
  
  # finish
  return(dat)
}

# TODO: combine this with rswat_find
#' search tool for SWAT+ output variable names
rswat_ofind = function(pattern=NULL, fuzzy=-1, trim=TRUE)
{
  # ARGUMENTS:
  #
  # `pattern`: (optional) character vector, regular expressions or literal string to match 
  # `fuzzy`:  numeric, specifying match mode (see details)
  # `trim`: logical, whether to omit detailed metadata about character positions
  #
  # RETURN VALUE:
  #
  # A dataframe of information about SWAT+ variable names matching the search pattern.
  #
  # DETAILS
  # 
  # The function looks for matches among the header names in the rswat database, so only
  # those output files already parsed by this package via functions `rswat_output` or
  # `rswat_oparse` (in the current R session) are searched. To do a search of ALL
  # available SWAT+ output files, run `rswat_odummy` first.
  #
  # If no pattern is supplied, the function returns information on all known variables.
  #
  # `fuzzy < 0` (the default) is for Perl-style regular expressions (see `base::grepl`),
  # `fuzzy = 0` is for exact matches only, and `fuzzy > 0` includes approximate matches
  # up to the specified (Levenshtein) distance (see `base::agrep`). The case (upper or
  # lower) is ignored in approximate matching.
  # 
  
  # make list of output files in current SWAT+ directory if necessary
  fname.ref = .rswat$stor$output$fname
  if( is.null(fname.ref) ) fname.ref = rswat_oscan(fast=FALSE)
  
  # parse header information from output files if necessary
  vname.ref = .rswat$stor$output$vname
  if( is.null(vname.ref) ) vname.ref = rswat_oparse()
  
  # list of all known output variable names
  name.all = vname.ref$name
  
  # perform search with pattern, if supplied (otherwise return all output names)
  name.match = rep(TRUE, length(name.all))
  if( !is.null(pattern) )
  {
    # handle fuzzy matching by `agrep` with `max.distance=fuzzy`
    if(fuzzy > 0) name.match = agrepl(tolower(pattern), tolower(name.all), max.distance=fuzzy)
    
    # handle regexp using `grepl` to find matches
    if(fuzzy < 0) name.match = grepl(pattern, name.all, perl=TRUE)
    
    # handle exact searches
    if(fuzzy == 0) name.match = name.all %in% pattern
  }
  
  # the full output dataframe
  vname.out = vname.ref %>% filter( name.match )
  
  # trim for human readability
  if(trim) vname.out = vname.out %>% arrange(type, file) %>% select(name, units, type, file, step)
  
  # message if we failed to match anything
  msg.info = '. Try running rswat_open(loadall=TRUE) to scan in all available SWAT+ outputs'
  if( nrow(vname.out) == 0 ) cat(paste0('no results for "', pattern, '"', msg.info, '\n'))
  
  return(vname.out)
}


#' 
#' ## output readers (internal)

#' parse a SWAT+ filename/folder for metadata about output files
rswat_oscan = function(fname=NULL, textio=NULL, fast=FALSE)
{
  #
  # ARGUMENTS:
  #
  # `fname`: character, (optional) filename of one particular file to scan  
  # `textio`: character, path to the directory (default NULL uses currently loaded project)
  # `fast`: boolean, whether to skip updating certain time-intensive fields (see DETAILS)
  #
  # RETURN:
  #
  # dataframe with columns 'file', 'path', 'size', 'modified', giving the filename, absolute
  # path, size on disk, and time last modified, respectively; 'type', either 'prt', 'ohg',
  # 'log', or 'unknown' (see below); 'name' and 'group', the SWAT+ object labels defined for
  # 'prt' type files; 'step', the timestep, either 'day', 'month', or 'year'.
  #
  # 'prt' type files appear in 'print.prt' where they can be toggled on/off in SWAT+ simulations;
  # 'ohg' files are object hydrograph files, which are specified in 'object.prt'; 'log' files have
  # extension '.out', and are generated after each simulation.
  #
  # DETAILS:
  #
  # There are a large number of SWAT+ output files each containing many output variables. This
  # function finds them and organizes some useful metadata, similar to `rswat_cio` . Used in
  # combination with `rswat_oparse` to get a comprehensive list of all available SWAT+ outputs.
  #
  # Argument 'fast=TRUE' will return an up to date list of files and paths, but skips updating
  # the 'size', 'modified', 'activated' fields
  # 
  # NOTE: crop output files currently have a table structure that different from the rest and
  # are not currently supported.
  
  # a list of unsupported files (units embedded in headers)
  nm.unsupported = c('basin_crop_yld_aa.txt', 
                     'basin_crop_yld_yr.txt', 
                     'crop_yld_aa.txt', 
                     'basin_crop_yld_aa.txt')
  
  # set default path for 'textio' when `rswat_cio` has been called
  if( is.null(textio) )
  {
    if( exists('.rswat') )
    {
      # copy the path from the package environment
      ciopath = .rswat$ciopath
      if( is.null(ciopath) ) stop('`textio` not found. Try setting `ciopath` with `rswat_cio`')
      
      # copy parent directory
      textio = dirname(ciopath)
      
    } else stop('`textio` not found. Either call `rswat_cio` or provide `textio`')
  }
  
  # initialize storage in package memory as needed and pull filenames dataframe
  df.ini = data.frame(file=character(0))
  if( ! 'output' %in% ls(.rswat$stor) ) .rswat$stor$output = list()
  if( ! 'fname' %in% ls(.rswat$stor$output) ) .rswat$stor$output$fname = df.ini
  fdf.old = .rswat$stor$output$fname
  
  # scan for all filenames in textio directory
  fname.all = list.files(textio, include.dirs=FALSE) 
  
  # exclude: unsupported files, filenames starting with '_' and those already in database
  nm.exclude = unique( c(nm.unsupported, fname.all[startsWith(fname.all, '_')], fdf.old$file) )
  
  # handle case of one single file requested
  if( !is.null(fname) ) 
  {
    # message about unsupported files
    if( fname %in% nm.unsupported ) stop('this file is not yet supported by rswat. Try `fread`')
    
    # check for invalid `fname`
    if( ! fname %in% fname.all ) stop(paste(fname, 'not found in `textio`'))
    
    # add everything but the specified file to exclusion list
    nm.exclude = unique( c(nm.exclude, fname.all[ fname.all != fname ]) )
  }
  
  # scan for non-excluded files having extension 'txt', 'out', or 'ohg', initialize fields
  fdf.new = data.frame( file = fname.all ) %>%
    filter( !( file %in% nm.exclude ) ) %>%
    filter( endsWith(file, '.txt') | endsWith(file, '.out') | endsWith(file, '.ohg')) %>%
    mutate( path = NA ) %>%
    mutate( size = set_units(NA, kilobytes) ) %>%
    mutate( modified = as.POSIXct(NA) ) %>%
    mutate( type = 'unknown' ) %>%
    mutate( step = NA ) %>%
    mutate( name = NA ) %>%
    mutate( group = NA ) %>%
    mutate( oid = NA ) %>%
    mutate( activated = NA )
  
  # filename parsing is skipped when it's been done already
  if( nrow(fdf.new) > 0 )
  {
    # identify prt type files be their suffix and extension
    regexp.suffix = '(_aa\\.txt)|(_yr\\.txt)|(_mon\\.txt)|(_day\\.txt)'
    idx.prt.day = endsWith(fdf.new$file, '_day.txt')
    idx.prt.mon = endsWith(fdf.new$file, '_mon.txt')
    idx.prt.yr = endsWith(fdf.new$file, '_yr.txt') | endsWith(fdf.new$file, '_aa.txt')
    idx.prt = idx.prt.day | idx.prt.mon | idx.prt.yr
    
    # identify OHG type files by their extension and set flags for three kinds of output files
    idx.ohg = endsWith(fdf.new$file, '.ohg')
    fdf.new$type[ idx.ohg ] = 'ohg'
    fdf.new$type[ idx.prt ] = 'prt'
    fdf.new$type[ endsWith(fdf.new$file, '.out') ] = 'log'
    
    # construct a timestep field - all OHG files are daily, and all AA files are yearly
    fdf.new$step[ idx.prt.day | fdf.new$type == 'ohg' ] = 'day'
    fdf.new$step[ idx.prt.mon ] = 'month'
    fdf.new$step[ idx.prt.yr ] = 'year'
    
    # parse the object name from the filename of prt type files
    fdf.new$name[ idx.prt ] = gsub(regexp.suffix, '',  fdf.new$file[ idx.prt ] )
    
    # attempt to parse the object, group, and gis_id names from the OHG filenames (if any)
    ohg.nsplit = 4
    if( any(idx.ohg) )
    {
      # split filenames at expected delimiters
      fn.split = strsplit(fdf.new$file, '_|\\.')
      fn.nsplit = sapply(fn.split, length)
      
      # parse OHG files with expected number of filename components
      idx.ohg.recognized = ( fdf.new$type == 'ohg' ) & ( fn.nsplit == 4 )
      
      # split filenames into 5 pieces using delimiters '_' and '.'
      ohg.mat = matrix(unlist( fn.split[idx.ohg.recognized] ), ohg.nsplit)
      
      # extract 'group' (object type), 'name' (object component), 'oid' (gis ID)
      fdf.new$group[idx.ohg.recognized] = ohg.mat[1,]
      fdf.new$oid[idx.ohg.recognized] = as.integer(ohg.mat[2,])
      fdf.new$name[idx.ohg.recognized] = ohg.mat[3,]
    }
  }
  
  # merge old and new filenames dataframes, resetting path field
  fdf.new = rbind(fdf.new, fdf.old) %>%
    mutate( path = NA )
  
  # set paths for existing files
  idx.existing = fdf.new$file %in% fname.all
  fdf.new$path[ idx.existing ] = file.path(textio, fdf.new$file[idx.existing])
  
  # nonexistent files get NA 'size', 'modified'
  fdf.new$size[ !idx.existing ] = NA
  fdf.new$modified[ !idx.existing ] = NA
  
  # scan 'time.sim' and 'print.prt' for currently enabled output files
  fout = rswat_fout(textio=textio, ohg=TRUE)
  fout.prt = fout %>% filter( type == 'prt' )
  fout.ohg = fout %>% filter( type == 'ohg' )
  
  # set the 'activated' flag for these files
  idx.prt.active = fdf.new$file %in% fout.prt$file
  idx.ohg.active = fdf.new$file %in% fout.ohg$file
  fdf.new$activated[ fdf.new$type %in% c('ohg', 'prt') ] = FALSE
  fdf.new$activated[ idx.prt.active | idx.ohg.active ] = TRUE
  
  # add group names to prt file entries as needed
  if( any( fout.prt$file %in% fdf.new$file  ) )
  {
    # merge 'group' field from 'files_out.out' data into filenames dataframe 
    idx.newval = match(fdf.new$file[ idx.prt.active ], fout.prt$file)
    fdf.new$group[ idx.prt.active ] = fout.prt$group[ idx.newval ]
  }
  
  # these operations can be slow so we include the option to skip
  if( !fast )
  {
    # update 'size' and 'modified' fields
    fdf.existing = file.info(fdf.new$path[idx.existing])
    fdf.new$size[ idx.existing ] = set_units(set_units(fdf.existing$size, bytes), kilobytes)
    fdf.new$modified[ idx.existing ] = fdf.existing$mtime
  }
  
  # update package storage with new data
  .rswat$stor$output$fname = fdf.new
  
  # index filename entries to return
  fname.return = fdf.new$file
  if( !is.null(fname) ) fname.return = fname
  
  # tidy output and finish
  fdf.new %>% 
    filter(file %in% fname.return) %>%
    arrange(group, file) %>% 
    select(file, name, type, step, activated, group, oid, size, modified, path)
}

#' scans a SWAT+ output file to discover variable names and units (if available) 
rswat_oparse = function(fname=NULL, textio=NULL)
{
  #
  # ARGUMENTS:
  #
  # `fname`: character, a file to parse in `textio`
  # `textio`: character, path to the directory containing SWAT+ output files
  #
  # RETURN:
  #
  # dataframe of information about the columns of a SWAT+ output file, with one row per
  # detected field. Boolean attribute 'index' indicates that the column is an indexing
  # variable of time or space, and 'units' are character representations recognizable by 
  # `set_units`. Other columns are documented in `rswat_oscan` 
  #
  # DETAILS:
  #
  
  # set default path for 'textio' when `rswat_cio` has been called
  if( is.null(textio) )
  {
    if( exists('.rswat') )
    {
      # copy the path from the package environment
      ciopath = .rswat$ciopath
      if( is.null(ciopath) ) stop('`textio` not found. Try setting `ciopath` with `rswat_cio`')
      
      # copy parent directory
      textio = dirname(ciopath)
      
    } else stop('`textio` not found. Either call `rswat_cio` or provide `textio`')
  }
  
  # handle requests to parse all files by recursive calls
  if( length(fname) == 0 )
  {
    # grab a current list of scannable files, sorting the OHG files last
    fname = rswat_oscan(textio=textio, fast=TRUE) %>% 
      filter( !is.na(path) ) %>% 
      filter( type %in% c('prt', 'ohg') ) %>%
      arrange( desc(type) ) %>%
      pull(file)
    
    # recursive calls to generate list of dataframes
    cat(paste('parsing', length(fname), 'SWAT+ output files...\n'))
    vdf.list = lapply(fname, function(f) rswat_oparse(f, textio) )
    # DEBUGGING
    #
    # 
    # for(f in fname)
    # {
    #   print(f)
    #   x = rswat_oparse(f, textio) 
    # }
    
    
    # join into single dataframe and finish
    vdf = do.call(rbind, vdf.list)
    
  } else {
    
    # single file requested: load info about this filename from memory, if available
    fname.ref = .rswat$stor$output$fname
    if( !is.null(fname.ref) ) fname.ref = .rswat$stor$output$fname %>% filter(file==fname)
    
    # otherwise parse the filename and add info to database
    if( nrow(fname.ref) == 0 ) fname.ref = rswat_oscan(fname, textio=textio, fast=TRUE)
    
    # read the first few lines into memory and parse them using helper functions
    rswat_rlines(fname.ref$path, omit=NULL, nmax=3)
    rswat_rtext(fname)
    linedf = .rswat$stor$temp[[fname]]$linedf
    
    # tidy up these temporary tables in package environment
    .rswat$stor$temp[[fname]] = list()
    .rswat$stor$txt[[fname]] = list()
    
    # return NULL and add nothing to database if the file is empty
    if( nrow(linedf) == 0 ) return( NULL )
    
    # ... prt files have comments, headers, units; whereas OHG files just have headers
    is.prt = fname.ref$type == 'prt'
    is.ohg = fname.ref$type == 'ohg'
    ln.head = ifelse(is.prt, 2, 1)
    ln.tab = ifelse(is.prt, 4, 2)
    ln.unit = ifelse(is.prt, 3, NA)
    
    # extract the headers and their (midpoint) positions on the line
    head.nm = linedf$string[linedf$line_num == ln.head]
    head.mid = rowMeans(linedf[linedf$line_num == ln.head, c('start_pos', 'end_pos')])
    n.head = length(head.nm)
    
    # reshape as dataframe, adding some file info
    vdf = linedf %>% filter( line_num == ln.head ) %>%
      mutate( name = string ) %>% 
      mutate( units = NA ) %>% 
      select( -c(string, class) ) %>%
      mutate( file = fname ) %>%
      mutate( type = fname.ref$type ) %>%
      mutate( step = fname.ref$step ) 
    
    # identify names of non-numeric indexing columns and add flag for them
    nm.integer = c('jday', 'mon', 'day', 'yr', 'gis_id', 'unit', 'typ_no')
    nm.character = c('name', 'type', 'objtyp', 'hyd_typ')
    vdf$index = vdf$name %in% c(nm.character, nm.integer)
    
    # assign expected class of each output column
    vdf$class = 'numeric'    
    vdf$class[ vdf$name %in% nm.integer ] = 'integer'
    vdf$class[ vdf$name %in% nm.character ] = 'character'
    
    # add units to prt files
    if( is.prt ) 
    {
      # extract unit strings 
      unit.nm = linedf$string[linedf$line_num == ln.unit]
      unit.mid = rowMeans(linedf[linedf$line_num == ln.unit, c('start_pos', 'end_pos')])
      
      # build a dictionary of unusual unit strings and their R-readable equivalents
      # note: I'm assuming 'mton' means 'metric tonne', and not 'milliton' (which is
      # apparently an arcane synonym of 'kilogram')
      unit.lu = do.call(rbind, list(c('ha-m', 'ha m'),
                                    c('kgN', 'kg'),
                                    c('kgP', 'kg'),
                                    c('kg/ha_N', 'kg/ha'),
                                    c('kg/ha_P', 'kg/ha'),
                                    c('mg_pst', 'mg'),
                                    c('tha', 't ha'),
                                    c('kgha', 'kg ha'),
                                    c('mj/m^2', 'mJ/m^2'),
                                    c('mton', 'tonne'),
                                    c('frac', NA),
                                    c('----', NA),
                                    c('---', NA),
                                    c('___', NA)))
      
      # swap in the R-readable unit strings
      idx.translate = unit.nm %in% unit.lu[,1]
      unit.nm[idx.translate] = unit.lu[match(unit.nm[idx.translate], unit.lu[,1]), 2]
      
      # snap units to headers (at midpoints) to handle misalignment issues 
      idx.head = sapply(unit.mid, function(n) which.min(abs(n - head.mid)))
      vdf$units[ idx.head ] = unit.nm
    }
    
    # add units to OHG files based prt files in database (if available)
    if( is.ohg & ( 'vname' %in% ls(.rswat$stor$output) ) )
    {
      # NOTE: OHG outputs only seem to work for these groups/types right now:
      idx.group = fname.ref$group %in% c('sdc', 'hru', 'ru')
      idx.name = fname.ref$name %in% c('tot', 'sur', 'rhg', 'til', 'lat')
      is.supported = idx.group & idx.name
      
      # skip adding units for unsupported types
      if( is.supported )
      {
        # grab a copy of the relevant subset of the database 
        vname.ref = .rswat$stor$output$vname %>% 
          filter( step == 'day' ) %>% 
          filter( type=='prt' ) %>%
          select( name, units, file)
        
        # and split by file
        vname.split = split(vname.ref$name, vname.ref$file)
        
        # build a search pattern - OHG file headers omit a suffix (doublecheck this!)
        idx.hasunits = (!vdf$index) & (head.nm != 'null')
        head.pattern = paste0(head.nm[ idx.hasunits ], '_out')
        
        # scan for prt files with all of these header names and proceed when there is a match
        idx.match = sapply(vname.split, function(v) all(head.pattern %in% v))
        if( any(idx.match) )
        {
          # use the first of the matching prt files as basis for setting units
          vname.lu = vname.ref %>% 
            filter( file %in% names( which(idx.match)[1] ) ) %>%
            filter( name %in% head.pattern ) %>% 
            select( name, units )
          
          # make the replacement
          vdf$units[idx.hasunits] = vname.lu$units[ match(head.pattern, vname.lu$name) ]
          
          # BUGFIX: 'flo' in OHG files has per-day units (not per-second, like 'flo_out')
          vdf$units[vdf$name == 'flo'] = 'm3/day'
        }
      }
    }
  }
  
  # initialize storage in package memory as needed
  if( !( 'vname' %in% ls(.rswat$stor$output) ) ) .rswat$stor$output$vname = vdf
  
  # update with new values
  .rswat$stor$output$vname = .rswat$stor$output$vname %>% 
    filter( !(file %in% fname) ) %>% 
    rbind(vdf)
  
  return(vdf)
} 

#' load the list of 'prt'-type files in 'files_out.out' as an R dataframe
rswat_fout = function(textio=NULL, ohg=TRUE)
{
  #
  # ARGUMENTS:
  #
  # `textio`: character, path to the directory (default NULL uses currently loaded project)
  # `ohg`: boolean, indicating to append info on OHG files from 'object.prt'
  #
  # RETURN:
  #
  # dataframe with columns: 'group' (SWAT+ group for this file), 'type' (either 'prt' or 'ohg'),
  # and 'file' (filename)
  #
  # DETAILS:
  #
  # After completing a simulation, SWAT+ writes information on all of the 'prt'-type files
  # it generated in 'files_out.out'. This is useful for identifying the available 'prt'
  # files, and for checking that a given output file belongs to the latest simulation.
  # Information about OHG type files is stored in a different file, 'object.prt' (scanned
  # only with argument `ohg=TRUE`)
  
  # set default path for 'textio' when `rswat_cio` has been called
  if( is.null(textio) )
  {
    if( exists('.rswat') )
    {
      # copy the path from the package environment
      ciopath = .rswat$ciopath
      if( is.null(ciopath) ) stop('`textio` not found. Try setting `ciopath` with `rswat_cio`')
      
      # copy parent directory
      textio = dirname(ciopath)
      
    } else stop('`textio` not found. Either call `rswat_cio` or provide `textio`')
  }
  
  # 'files_out.out' holds a list of output files created in last simulation
  fread.prt = fread(file.path(textio, 'files_out.out'), skip=1, fill=TRUE) %>% as.data.frame
  
  # BUGFIX: detect and fix spaces in first field causing table parser errors
  if( ncol(fread.prt) > 2 )
  {
    # buggy rows have non-empty third field
    idx.bug = nchar(fread.prt[, 3]) > 0
    
    # repair filenames (discard whatever follows the space in the first buggy field)
    fname.bug = fread.prt[idx.bug, ncol(fread.prt)]
    
    # repair the name, move the filename field, omit the redundant column(s)
    fread.prt[idx.bug, 1] = paste(fread.prt[idx.bug, 1:( ncol(fread.prt) - 1 )], collapse='_')
    fread.prt[idx.bug, 2] = fname.bug
    fread.prt = fread.prt[, 1:2]
  }
  
  # rename the fread columns, clean up names, add 'type' column
  dat = fread.prt %>%  setNames(c('group', 'file') ) %>%
    mutate( type = 'prt' ) %>% 
    select( file, type, group )
  
  # scan for and append OHG data if requested
  if( ohg ) 
  {
    # proceed only if the OHG definitions config file exists
    fname.prt = 'object.prt'
    cio = rswat_cio()
    if( fname.prt %in% cio$file )
    {
      # open the OHG files table, reshape to join with dat
      dat = rswat_open(fname.prt) %>%
        mutate( file = FILENAME ) %>%
        mutate( group = NA ) %>%
        mutate( type = 'ohg' ) %>% 
        select( file, type, group ) %>%
        rbind(dat)
    }
  }
  
  # finished
  return(dat)
  
}

#' build a vector of Date objects to match rows of a SWAT+ output file 
rswat_makedates = function(dat, tstep)
{
  # ARGUMENTS
  #
  # `dat`: dataframe, (subset of) the SWAT+ output file table (see DETAILS)
  # `tstep`: character, one of 'day', 'month', 'year'
  #
  # RETURN
  #
  # A vector of class `Date`, matching the rows of `dat`
  #
  # DETAILS
  #
  # This is a helper function for `rswat_output(..., makedates=TRUE)`
  #
  # 'dat' is expected to have columns 'yr', 'mon', 'day' (integers) and to be arranged in
  # ascending order of date. Dates can be duplicated (eg. the table can have date-indexed
  # data for multiple spatial objects) but the number of duplicates should be the same for
  # each timestep.
  #
  # `tstep` could be derived from the dataframe itself but it's faster to pass it as an
  # argument, since it's one of the fields returned by `rswat_oscan`.
  # 
  
  # parse the start/end dates
  n.obs = nrow(dat)
  nm.ymd = c('yr', 'mon', 'day')
  date.start = as.Date( paste(dat[1, nm.ymd], collapse='-') )
  date.end = as.Date( paste(dat[n.obs, nm.ymd], collapse='-') )
  
  # attempt to detect the number of unique spatial id elements
  
  # 1-row case necessarily only has one spatial element
  if( nrow(dat) == 1 ) { n.id = 1 } else {
    
    # create indicator matrix for change points of time indices
    dmat = diff(as.matrix(dat[nm.ymd])) != 0
    
    # use the most frequently changing one to compute number of time steps
    n.tstep = 1 + max( colSums( dmat ) )
    
    # in most cases, (number of spatial elements) * ( number of timesteps ) = number of rows
    n.id = n.obs / n.tstep
  }
  
  # handle cases where the number of rows is not a multiple of number of unique ids
  if( n.id != round(n.id) )
  {
    # slower but more robust count 
    n.id = dat %>% group_by(yr, mon, day) %>% 
      summarize(n=n(), .groups='drop_last') %>% 
      pull(n) %>% unique
  }
  
  # disable date parsing over year 1900, to avoid Feb 29 bug (it's not a leap year) 
  date.bug = ( date.start < as.Date('1900-03-01') ) & ( date.end > as.Date('1900-03-01') )
  if( date.bug )
  {
    warning('date parsing disabled for time series including year 1900')
    return( dat )
  }
  
  # handle tables with variable numbers of within-timestep rows (IDs), and/or 1900 bug
  if( length(n.id) > 1 )
  {
    # very slow for large datasets - used only as last resort
    all.dates.char = sapply(1:n.obs, function(ii) paste(dat[ii, nm.ymd], collapse='-'))
    all.dates = as.Date(all.dates.char, format='%Y-%m-%d')
    
  } else {
    
    # this should be equivalent to `as.Date(paste(yr, mon, day, sep='-')`, but much faster
    all.dates = rep(seq(date.start, date.end, tstep), each=n.id)
    
    # handle end dates that don't coincide with a timestep interval endpoint
    if( length(all.dates) == n.obs - n.id ) all.dates = c(all.dates, rep(date.end, n.id))
  }
  
  # halt if there is a mismatch in length, otherwise return the vector
  if( length(all.dates) != n.obs ) stop('failed to parse dates')
  return( all.dates )
}

#' 
#' ## model execution

#' run the SWAT+ executable
rswat_exec = function(textio=NULL, exe=NULL, fout=TRUE, quiet=FALSE)
{
  # ARGUMENTS
  # 
  # `textio`: character, path to the text I/O directory for the SWAT+ model 
  # `exe`: character, path to the SWAT+ executable 
  # `fout`: logical, whether to return the list of output files generated in the simulation 
  # `quiet`: logical, suppresses console messages
  #
  # RETURN
  #
  # if `fout==TRUE`, returns a vector of output filenames (otherwise returns nothing).
  #
  # The function makes a system call to run the SWAT+ executable at `exe` in the project
  # directory specified by `textio` (or assigned by a call to `rswat_cio`)
  #
  # DETAILS
  #
  # Existing values in the config files (including time specifications in 'print.prt' and
  # 'time.sim') are used for the simulation. Change them using higher-level functions
  # like `rswat_daily`
  
  
  # if not supplied, try a hardcoded executable path 
  if( is.null(exe) )
  {
    # TODO: detect this in pyqgis module
    speditor.path = 'C:/SWAT/SWATPlus/SWATPlusEditor'
    exe =  file.path(speditor.path, 'resources/app.asar.unpacked/swat_exe/rev60.5.2_64rel.exe')
  }
  
  # set default `textio` directory if `rswat` has been initialized with a project directory
  if( is.null(textio) )
  {
    # check if rswat has been initialized
    err.msg = 'Either supply `textio` or run `rswat_cio` to set default'
    if( !exists('.rswat') ) stop(err.msg)
    if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
    
    # pull the directory from rswat environment
    textio = dirname(.rswat$ciopath)
  }
  
  # shell command prefix to change to the directory (avoids changing R's working directory)
  shell.prefix = paste0('pushd ', normalizePath(textio), ' &&')
  
  # build system call, start timer, and run SWAT
  syscall.string = paste(shell.prefix, tools::file_path_sans_ext(normalizePath(exe)))
  timer.start = Sys.time()
  invisible(shell(syscall.string, intern=quiet))
  
  # stop timer, prepare execution time message
  timer.end = Sys.time()
  seconds.msg = round(difftime(timer.end, timer.start, units='secs')[[1]], 2)
  timer.msg = paste(seconds.msg, 'seconds')
  if( !quiet ) cat(paste0('\n>> finished (', timer.msg, ' runtime) \n'))
  
  # return filenames if requested
  if( fout ) return( rswat_fout(ohg=TRUE)$file ) 
}

#' set up simulation times in SWAT+ config files 'print.prt' and 'time.sim' 
rswat_time = function(dates=NULL, nyskip=0, daily=FALSE, quiet=TRUE)
{
  # ARGUMENTS:
  # 
  # `dates`: integer, vector, or dataframe indicating the dates to simulate (see DETAILS)
  # `nyskip`: integer, the number of years to skip in printed output 
  # `daily`: boolean, indicates to make adjustments for daily output (see DETAILS)
  # 'quiet': boolean, indicating to suppress console messages
  #
  # RETURN:
  #
  # named (Date class) vector of start and end dates currently set in 'time.sim'
  #
  # DETAILS:
  # 
  # Modifies 'print.prt' and 'time.sim' on disk to set up SWAT+ to simulate over the
  # requested dates ('day_start', 'yrc_start', 'day_end', 'yrc_end') and burn-in time
  # (`nyskip`).
  #
  # If `daily == TRUE` then 'step' (in 'time.sim') is set to run daily timesteps,
  # `interval` is set for daily printing, and 'print.prt' is modified to request
  # all of the daily files, and none of the non-daily ones.   
  #
  # Argument 'dates' can be a dataframe containing a 'date' column or a vector of dates
  # (must be coercible with `as.Date`); or an integer indicating the number of timesteps,
  # in which case the existing start date from the file is reused, and the end date is
  # modified accordingly to get the desired number of steps.
  
  # grab copies of 'time.sim' and 'print.prt' 
  time.sim = rswat_open('time.sim', quiet=quiet)
  print.prt = rswat_open('print.prt', quiet=quiet)
  
  # extract currently assigned start/end dates
  date.start = as.Date(paste(time.sim[,c('yrc_start', 'day_start')], collapse='-'), '%Y-%j')
  date.end = as.Date(paste(time.sim[,c('yrc_end', 'day_end')], collapse='-'), '%Y-%j')
  
  # if requested, modify 'step', 'interval', and print table for daily timestep outputs
  if( daily )
  {
    # other possible values for step include 1, 24, 96, 1440
    time.sim$step = 0
    
    # indicates to print 1 output row per timestep
    print.prt[[1]]$interval = 1
    
    # toggle all of the print options off and write this change to disk
    print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
    print.prt[[5]]['daily'] = 'y'
    rswat_write(print.prt[[5]], preview=FALSE, quiet=quiet)
  }
  
  # modify 'nyskip' as requested
  print.prt[[1]]$nyskip = nyskip
  
  # if no dates supplied skip ahead
  if( !is.null(dates) )
  {
    # handle dataframe input
    if( is.data.frame(dates) ) dates = dates$date
    
    # handle numeric input
    if( is.numeric(dates) )
    {
      # coerce to integer and interpret as number of days
      dates = as.integer(dates)
      if( length(dates) > 1 ) stop('numeric-type `dates` input must be a single integer')
      
      # modify end date accordingly
      date.end = date.start + dates
      
    } else {
      
      # for all other input classes, attempt to coerce to Date
      dates = as.Date( dates[!is.na(dates)] )
      if( length(dates) == 0 ) stop('`dates` could not be interpreted as Date object')
      
      # assign new start and end dates
      date.start = min(dates)
      date.end = max(dates)
      
    }
    
    # build dataframe with new dates
    pars.tochange = c(day_start = as.integer(format(date.start, '%j')), 
                      yrc_start = as.integer(format(date.start, '%Y')), 
                      day_end = as.integer(format(date.end, '%j')), 
                      yrc_end = as.integer(format(date.end, '%Y')))
    
    # progress message
    if( !quiet ) cat('writing to time.sim and print.prt...')
    
    # make these changes in 'time.sim' and overwrite on disk
    time.sim[ names(pars.tochange) ] = pars.tochange
    rswat_write(time.sim, preview=F, quiet=TRUE)
    
    # make these changes in 'print.prt' and overwrite on disk
    print.prt[[1]][ names(pars.tochange) ] = pars.tochange
    rswat_write(print.prt[[1]], preview=F, quiet=TRUE)
    if( !quiet ) cat('done\n')
  }
  
  # return the current simulation date range 
  return( c(start=date.start, end=date.end) )
  
}

#' runs simulation to generate/load OHG output, optionally using it as input to an error function
rswat_flo = function(vname='flo', dates=NULL, oid=1, restore=TRUE, errfn=NULL, quiet=FALSE)
{
  #
  # ARGUMENTS:
  #
  # 'vname', character, the output variable name to load
  # 'dates': integer, vector, or dataframe containing the dates to simulate (see rswat_time)
  # 'oid': integer, the "object number", an ID code associated with the desired object
  # 'restore': boolean, indicates to restore a backup of config files modified by the function
  # 'errfn', anonymous function of one or two (equal-length) numeric vector(s) (see DETAILS)
  # 'quiet': boolean, indicating to suppress console messages
  #
  # RETURN:
  #
  # Either a dataframe of output values with dates, or the return value of `errfn`
  #
  # DETAILS:
  #
  # With default settings, this is a helper function for quickly getting discharge values for
  # the current SWAT+ config settings. Argument `errfn` allows these discharge values to be
  # passed directly to an error function (eg Nash Sutcliffe Efficiency) to make it easier to
  # construct anonymous objective functions for your SWAT+ model. `restore=TRUE` prompts the
  # creation of a backup of all modified config files (except the simulation outputs), which
  # is restored after the simulation has run.
  #
  # If `dates` is not supplied, the function uses the return value of `rswat_time()`
  #
  # There are three modes:
  #
  # (1) `errfn` not supplied -> returns the dataframe of OHG data including column 'vname'
  # (2) `errfn(x)` is supplied -> passes output OHG values of 'vname' to errfun and returns result
  # (3) `errfn(x,y)` is supplied -> errfn evaluated as above with `y=dates$obs` or `y=dates$flo`
  #
  # In case (3), if `dates$obs==NULL` the function uses `dates$flo`. ie any column starting
  # with 'flo' ('flow', 'flo_out', etc) will be selected as the second choice.
  #
  # Observed data can thus either be: (1) dealt with elsewhere, (2) baked into the supplied
  # `errfn`, or (3) passed as a column of `dates`. In case (3) `dates` must be a dataframe,
  # and the name of the observations column must start with 'obs' or 'flo' (with 'obs'
  # preferred). This column should be of `numeric` or `units` type. In case (2), any such
  # column is ignored.
  
  # initialize observed data vector
  obs = NULL
  
  # check that rswat has been initialized
  err.msg = 'Run `rswat_cio` first to load a project'
  if( !exists('.rswat') ) stop(err.msg)
  if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
  
  # make a backup of the config stuff modified below
  if(restore)
  {
    # pull the directory from rswat environment
    textio = dirname(.rswat$ciopath)
    
    # create a temporary directory for backups
    tdir = file.path(textio, paste0('_rswat_backup_', basename(tempfile())))
    my_dir(tdir)
    
    # define files to back up
    fname = c('file.cio', 'time.sim', 'print.prt', 'object.prt')
    fname.exists = file.exists( file.path(textio, fname) )
    fname = fname[fname.exists]
    
    # define paths for the original and backup copies
    restore.path = file.path(textio, fname)
    backup.path = file.path(tdir, fname)
    
    # copy the files in a loop
    if( !quiet ) cat('backing up files... ')
    for(ii in seq_along(backup.path)) file.copy(restore.path[ii], backup.path[ii])
  }
  
  # copy observed data (if supplied) from dataframe input
  if( is.data.frame(dates) )
  {
    # look first for anything starting with 'obs'
    obs = dates$obs
    
    # look for 'flo' only if nothing is found 
    if( is.null(obs) ) obs = dates$flo
  }
  
  # write new time period to config files (if necessary), overwrite `dates` in normalized form 
  dates = rswat_time(dates, quiet=TRUE)
  
  # open fifth table of 'print.prt', disable all output files and write the changes
  print.prt = rswat_open('print.prt')[[5]]
  print.prt[, names(print.prt) != 'objects'] = 'n'
  rswat_write(print.prt, preview=F, quiet=TRUE)
  
  # activate OHG output
  fout = rswat_ohg(overwrite=TRUE, oid=oid)
  
  # run the executable
  if( !quiet ) cat('running SWAT+... ')
  rswat_exec(quiet=quiet, fout=FALSE)
  
  # this should restore any config files modified in the above 
  if(restore)
  {
    # delete 'object.prt' and restore backups
    if( !quiet ) cat('restoring backup... ')
    unlink( file.path(textio, 'object.prt') )
    for(ii in seq_along(backup.path)) file.copy(backup.path[ii], restore.path[ii], overwrite=TRUE)
    if( !quiet ) cat('done\n')
    unlink(tdir, recursive=TRUE)
    
    # refresh cached value of the files that were modified above 
    rswat_open('time.sim', quiet=TRUE, reload=TRUE)
    rswat_open('print.prt', quiet=TRUE, reload=TRUE)
  }
  
  # load the output file and finish if no error function supplied
  if( is.null(errfn) ) return( rswat_output(fout$FILENAME, vname=vname, showidx=FALSE) )
  
  # otherwise extract the variable of interest and determine its units (if any)
  sim = rswat_output(fout$FILENAME, vname=vname, showidx=FALSE)[[vname]]
  sim.hasunits = inherits(sim, 'units')
  sim.units = ifelse(sim.hasunits,  as.character( units(sim) ), NA) 
  
  # handle invalid `errfn` argument
  if( !is.function(errfn) ) { stop('errfn must be a function') } else {
    
    # count the number of arguments
    errfn.n = length( formalArgs(errfn) )
    
    # handle error functions of one variable
    if( errfn.n == 1 ) return( errfn(sim) )
    
    # handle error functions of two variables 
    if( errfn.n == 2 )
    {
      # handle missing second argument 
      if( is.null(obs) ) stop('dates$obs not found!')
      
      # check for units in observed data
      obs.hasunits = inherits(sim, 'units')
      
      # if both vectors are missing units, skip ahead
      if( obs.hasunits | sim.hasunits ) 
      {
        # handle units conversion or...
        if( obs.hasunits & sim.hasunits ) sim = set_units(sim, sim.units, mode='standard')
        
        # assign missing units to observed data or...
        if( (!obs.hasunits) & sim.hasunits )
        {
          warning(paste('assuming dates$obs has units of', sim.units))
          obs = set_units(obs, sim.units, mode='standard') 
        }
        
        # assign missing units to simulated data
        if( obs.hasunits & (!sim.hasunits) )
        {
          obs.units = as.character( units(obs) )
          warning(paste('assuming SWAT+ variable', vname, 'has units of', obs.units))
          sim = set_units(sim, obs.units, mode='standard') 
        }
      }
      
      # data and simulation should now be in common units - call the error function
      return( errfn(sim, obs) ) 
    }
  }
}


#' 
#' ## model execution (internal)

#' request a dummy simulation to generate example cases of available output files
rswat_odummy = function(subdir='_rswat_odummy', nday=1, quiet=FALSE)
{
  # ARGUMENTS
  #
  # `subdir`: character, the subdirectory to store the output files
  # `nday`: integer > 0, number of days to simulate
  # `quiet`: logical, suppresses console messages
  #
  # RETURN
  #
  # character, the full path to the dummy simulation outputs folder 
  #
  # DETAILS
  # 
  # SWAT+ simulates many different physical variables, grouping them into output text files
  # according to the the type of watershed feature they belong to and the time interval. Users
  # may opt to have SWAT+ output all of these files in any given simulation, but this results
  # in slow execution times, so it is better to select a subset to print (via 'print.prt').
  #
  # This function creates a dataset to assist in selecting output files. It runs a simulation
  # of length `nday` starting in 1901-01-01, generating all available output files and copying
  # them to `subdir` (where they can't be overwritten by future calls to the SWAT+ executable).
  # All project files are then restored to their original state.
  #
  # The default `nday=1` is enough to generate headers and units for all outputs. It is not 
  # enough, however, to generate example data rows for all timestep options. eg. a 1-day
  # simulation will generate 1 data row in the daily files, but the yearly and monthly files
  # will now have any data rows
  #
  
  # grab project directory from package environment
  if( exists('.rswat') )
  {
    # copy the path from the package environment
    ciopath = .rswat$ciopath
    
    # handle missing ciopath
    if( is.null(ciopath) ) stop('ciopath not found. Try setting it with `rswat_cio`')
    
    textio = dirname(ciopath)
    
  } else stop('"file.cio" not found. Run `rswat_cio` to set its path')
  
  # define files to back up
  fname.b = list.files(textio, include.dirs=FALSE)
  idx.obak = endsWith(fname.b, '.txt') | endsWith(fname.b, '.out')  | endsWith(fname.b, '.ohg')
  idx.cbak = fname.b %in% c('file.cio', 'time.sim', 'print.prt', 'object.prt')
  fname = fname.b[ idx.obak | idx.cbak ]
  
  # copy the files in a loop, saving to subfolder of `textio`
  if( !quiet ) cat('backing up files... ')
  bpath = rswat_copy(fname=fname, overwrite=TRUE, quiet=quiet)
  bdir = dirname(bpath)[1]
  
  # adjust the simulation period
  rswat_time(nday)
  
  # open fifth table of 'print.prt', activate all output files and write the changes
  print.prt = rswat_open('print.prt')[[5]]
  print.prt[, names(print.prt) != 'objects'] = 'y'
  rswat_write(print.prt, preview=F, quiet=TRUE)
  
  # TODO: activate OHG files
  
  # execute the simulation 
  if( !quiet ) cat('running SWAT+... ')
  fout = rswat_exec(quiet=quiet)
  
  # fout is not a complete list of outputs! scan for output files (old and new)
  fname.s = list.files(textio, include.dirs=FALSE)
  idx.new = endsWith(fname.s, '.txt') | endsWith(fname.s, '.out')  | endsWith(fname.s, '.ohg')
  fname.new = fname.s[idx.new]
  
  # copy the new files to subdirectory and delete the originals
  odir = file.path(textio, subdir)
  opath = rswat_copy(to=odir, fname=fname.new, overwrite=TRUE, quiet=quiet)
  unlink( file.path(textio, fname.new) )
  
  # restore backup files and delete backup directory
  if( !quiet ) cat('restoring backup... ')
  rpath = rswat_copy(from=bdir, fname=basename(bpath), overwrite=TRUE, quiet=quiet)
  unlink(bdir, recursive=TRUE)
  if( !quiet ) cat('done\n')
  
  # return the vector of output file paths
  return( opath )
  
}

#' create an object hydrograph output specification file, modifying file.cio appropriately
rswat_ohg = function(overwrite=FALSE, otype='sdc', oid=1, htype='tot', delete=FALSE, quiet=TRUE)
{
  #
  # ARGUMENTS
  #
  # 'overwrite': boolean, whether the overwrite the destination file if it exists already
  # 'otype': character, three character object type code (see DETAILS)
  # 'oid': integer, the "object number", an ID code associated with the desired object
  # 'htype': character, one of 'tot', 'sur', 'lat', 'til', 'rhg', ... (object-dependent)
  # 'delete': boolean, whether to delete "object.prt" from disk and remove it from "file.cio"
  # 'quiet': boolean, indicating to suppress console messages
  #
  # RETURN:
  #
  # The contents of 'object.prt' as a dataframe. As needed this file will be created in the
  # SWAT+ config file folder, and 'file.cio' modified to scan for it. 
  # 
  # DETAILS
  # 
  # When 'delete=TRUE' the function deletes "object.prt" from disk and changes its entry in
  # "file.cio" back to NULL (the default), so that no OHG files are produced.
  #
  # 'object.prt' consists of a list of requested "output hydrograph" files. These are special
  # output files containing only the data for a single spatial object in the watershed model
  # (eg a single HRU or outlet). They can be enabled by adding a row to 'object.prt'
  #
  # ----------------------------------------------------
  # recognized arguments for `otype` (grouped by object type)
  #
  # HRUs:                   'hru', 'hlt', 'ru'
  # reservoirs:             'res'
  # channels:               'cha', 'sdc'
  # export coefficients:    'exc'
  # delivery ratio:         'dr'
  # outlets:                'out'
  #
  # ----------------------------------------------------
  # recognized arguments for `htype`
  #
  # total:                  'tot'
  # recharge:               'rhg'
  # surface:                'sur'
  # lateral:                'lat'
  # tile:                   'til'
  # soil moisture by layer: 'sol'
  #
  # ----------------------------------------------------
  # valid combinations (based on my own limited testing)
  #
  # 'til' AND 'sdc' 
  # 'lat' AND 'sdc'
  # 'sur' AND 'ru' OR 'hru'
  # 'sol' AND 'hru'
  # 'rhg' AND 'ru' OR 'hru' OR 'sdc'
  # 'tot' AND 'ru' OR 'hru' OR 'sdc'
  #
  # other combinations seem to produce only empty or NA tables. The following code
  # sets up SWAT+ to produce all of the above combinations (TODO: catalogue these)
  #
  # nm.ru = c('tot', 'sur', 'rhg')
  # nm.hru = c('tot', 'sur', 'rhg', 'sol')
  # nm.sdc = c('tot', 'rhg', 'til', 'lat' )
  # htypes.test = c(nm.ru, nm.hru, nm.sdc)
  # otypes.test = c(rep('ru', length(nm.ru)), 
  #                 rep('hru', length(nm.hru)),
  #                 rep('sdc', length(nm.sdc)))
  # 
  # rswat_ohg(overwrite=TRUE, otype=otypes.test, htype=htypes.test)
  #
  # -----------------------------------------------------------
  #
  # The defaults will prompt SWAT+ to produce an output file named 'simulation_hru_1_tot.txt',
  # containing simulations of channel flow (and related variables) on the channel with
  # gis_id = 1 (probably the main outlet channel). These model output settings are stored in
  # the file 'object.prt'.
  #
  # The latest documentation for the object hydrograph file (SWAT+ rev60.5) is a bit hazy on
  # the permissible values for `otype` and `htype` ('OBTYP' and 'HYDTYP' in the docs), so this
  # function does no checking for valid input, and will write its input arguments regardless
  # of whether the SWAT+ executable can understand them.
  #
  # Unfortunately the location in 'file.cio' of the parameter specifying to read 'object.prt'
  # is not indexed by a header name (only a row name), so there is chance that future updates
  # to the structure of 'file.cio' could break this function. Currently it is located on the
  # row named 'simulation' in the 4th field position.
  #
  
  # name of the output file (file.cio will also be modified at the end)
  ofile = 'object.prt'
  
  # set default path for 'file.cio' when `rswat_cio` has been called
  if( exists('.rswat') )
  {
    # copy the path from the package environment
    ciopath = .rswat$ciopath
    
    # handle missing ciopath
    if( is.null(ciopath) ) stop('ciopath not found. Try setting it with rswat_cio')
    
  } else stop('"file.cio" not found. Either call `rswat_cio` or provide ciopath')
  
  # construct a comment line to print to the txt file
  comment.text = paste('object.prt: written by rswat on', Sys.time())
  
  # construct field 'NUMB'. Note: duplicated fields here can make the SWAT+ executable crash!
  idtext = as.character( 1:max( sapply(list(otype, oid, htype), length) ) )
  
  # construct the dataframe of file info to write (with character type columns)
  fdf = data.frame(NUMB=idtext, OBTYP=otype, OBTYPNO=as.character(oid), HYDTYP=htype) %>%
    mutate(FILENAME = paste0(paste(OBTYP, OBTYPNO, HYDTYP, sep='_'), '.ohg'))
  
  # format this dataframe into a matrix of strings with correct padding
  fdf = rbind(names(fdf), fdf) 
  nmax = 1 + max( nchar(names(fdf)) )
  fdf.text = format.data.frame(fdf,na.encode=FALSE, justify='left', width=nmax) %>%
    as.matrix %>% apply(1, function(x) paste(x, collapse=''))
  
  # add a comment line
  out.lines = c(comment.text, fdf.text)
  
  # check if the file exists already and print some feedback to user
  msg.mode = ifelse(delete, 'deleting', 'overwriting')
  prtpath = file.path(dirname(ciopath), ofile)
  prt.exists = file.exists(prtpath)
  if( prt.exists )
  {
    msg.warn = paste('file', prtpath, 'already exists. Set overwrite=TRUE to modify it')
    if( !overwrite ) { stop(msg.warn) } else { if( !quiet ) cat(paste(msg.mode, prtpath, '\n')) }
    
  } else {
    
    if(delete) warning('object.prt not found')
    if( !quiet ) cat(paste('writing to', prtpath, '\n'))
  }
  
  # print to output file or delete the file
  if( delete & prt.exists ) unlink(prtpath)
  if( !delete ) writeLines(out.lines, prtpath)
  
  # open file.cio and identify the line number and character position to modify
  rswat_rlines(ciopath) 
  linedf.cio = .rswat$stor$temp[['file.cio']]$linedf
  lnmod = linedf.cio %>% filter(string=='simulation') %>% pull(line_num)
  cposmod = linedf.cio %>% filter(line_num==lnmod, field_num==4) %>% pull(start_pos)
  cendmod = linedf.cio %>% filter(line_num==lnmod, field_num==4) %>% pull(end_pos)
  clen = cendmod - cposmod
  
  # modify the line and write the changes to file.cio
  newval = ifelse(delete, 'null', ofile)
  newval.n = nchar(newval)
  pad.n = clen - nchar(newval.n)
  if( pad.n > 0 ) newval = paste0(newval, paste(rep(' ', pad.n), collapse=''))
  substr(.rswat$stor$txt[['file.cio']][lnmod], cposmod, cposmod + nchar(newval)) = newval
  writeLines(.rswat$stor$txt[['file.cio']], ciopath)
  
  # remove the 'file.cio' data from package environment 
  .rswat$stor$txt = .rswat$stor$txt[names(.rswat$stor$txt) != 'file.cio']
  .rswat$stor$temp = .rswat$stor$temp[names(.rswat$stor$temp) != 'file.cio']
  
  # refresh files list (maintaining ignored files list)
  ignore = rswat_cio(trim=F) %>% filter(ignored) %>% pull(file)
  rswat_cio(ciopath, ignore=ignore, quiet=quiet)
  
  # load new file, or, in delete mode, return nothing 
  if(delete) return(NULL)
  rswat_open('object.prt', quiet=TRUE, reload=TRUE)
  rswat_open('object.prt', quiet=TRUE)
}

# TODO: improve or replace this
#' returns an objective function whose argument is vector of parameter values, output is NSE
my_objective = function(cal, gage, errfn, quiet=TRUE)
{
  # ARGUMENTS:
  # 
  # `cal`: dataframe, the SWAT+ parameters to modify
  # `gage`: dataframe, containing flow and date (see `my_gage_objective`)
  # `errfn`: an (anonymous) error function `errfn(x,y)` for simulations x given observations y
  # `quiet`: logical, suppresses console messages
  #
  # RETURN VALUE:
  #
  # Returns a function `f(x)` that computes the NSE score for a given parameter set (specified
  # in `cal`), supplied in vector `x`, where errors are computed over the time series of flow
  # data in `gage`.
  #
  # DETAILS:
  #
  # `cal` should be the output of one or several (row-binded) `rswat_find(..., trim=T)` calls,
  # having columns for 'name', 'i', 'j', etc. It specifies n parameters of interest, around
  # which an n-to-1 objective function, suitable for numerical optimizers, is constructed and
  # returned. When calling this function, the order of the paramaters in `x` must match the
  # order in `cal` (there is no checking of names etc). 
  #
  # On multirow tables, `i=NA` in `cal` is taken to mean "write all rows of this parameter
  # column". This is the default (trim=TRUE) return value for rswat_find when a match appears
  # in a multirow table. To specify individual rows, either use rswat_find(..., trim=FALSE)
  # or set `i` manually as needed.
  #
  
  # scan for non-numerics
  idx.nn = cal$class != 'numeric'
  if( any(idx.nn) )
  {
    # print a warning
    msg.info = paste(cal$name[idx.nn], collapse=', ')
    warning(paste('removed non-numeric entries from cal:', msg.info))
    
    # remove the non-numeric entries
    cal = cal[!idx.nn,]
  }
  
  # length check
  if( nrow(cal) == 0 ) stop('no numeric parameters found in cal')
  
  # fetch the file data - a copy of this (and `cal`) gets baked in to the function below
  cal.fn = setNames(nm=unique(cal$file))
  n.fn = length(cal.fn)
  cal.values = lapply(cal.fn, rswat_open)
  
  # R makes a copy of the above objects as well as cal and gage upon defining the function
  # below. This is everything we need for optimization baked in a tidy single-argument function
  # TODO: optimize and check for issues related to function closure and lazy evaluation
  
  # begin definition of return function
  function(x=NULL, refresh=FALSE)
  {
    # `x` is the vector of (numeric) SWAT+ parameters. They should be given in the same
    # order as they appeared in `cal`, when this function was created (via a call to
    # `my_objective`). To view this order, call the function without arguments.
    
    # TODO: write up arguments and return sections
    
    # refresh cal.values to get any modifications since the function was defined
    if(refresh) cal.values = lapply(cal.fn, rswat_open)
    
    # if user supplied no parameter values, return the parameter info dataframe
    if( is.null(x) )
    {
      # initialize a new column in cal for values and fill it by looping over filenames
      cal = cal %>% select( -c(string, class, table) ) %>% mutate(value=NA)
      for(fn in cal.fn)
      {
        # loop over cal entries for this file
        fn.idx = which( cal$file == fn )
        for(idx.par in 1:length(fn.idx))
        {
          # index in the cal.values table
          i = cal$i[ fn.idx[idx.par] ]
          j = cal$j[ fn.idx[idx.par] ]
          
          # extract the values, making list of unique ones in multivariate case
          if( !is.na(i) ) par.value = unique(cal.values[[fn]][i,j])
          if( is.na(i) ) par.value = unique(cal.values[[fn]][,j])
          
          # write the value if unique (non-uniqueness indicated by NA)
          if( length(par.value) == 1 ) cal$value[ fn.idx[idx.par] ] = par.value
        }
      }
      
      # tidy and finish 
      return( cal %>% select(value, name, file, dim, everything()) )
    }
    
    # user supplied parameter values: loop over filenames to write them
    for(fn in cal.fn)
    {
      # grab the subset of `cal` and replacement values in x
      fn.idx = which( cal$file == fn )
      fn.x = x[fn.idx]
      
      # loop over cal entries for this file
      for(idx.par in 1:length(fn.idx))
      {
        # index in the cal.values table
        i = cal$i[ fn.idx[idx.par] ]
        j = cal$j[ fn.idx[idx.par] ]
        
        # make the replacement
        if( !is.na(i) ) cal.values[[fn]][i,j] = fn.x[idx.par]
        if( is.na(i) ) cal.values[[fn]][,j] = fn.x[idx.par]
      }
      
      # finished with the table, write the changes
      rswat_write(cal.values[[fn]], fname=fn, preview=FALSE, quiet=quiet)
    }
    
    # TODO: memoize so we can skip this?
    # run the simulation and return the objective function value
    return( rswat_flo(dates=gage, errfn=errfn, quiet=quiet) )
  }
}

#' 
#' ## multicore model execution 

#' run a batch of simulations in parallel
rswat_pexec = function(x, amod, wipe=TRUE, ...)
{
  # ARGUMENTS:
  #
  # `x`: n by p numeric matrix of parameters, the columns of which are valid input to `amod`
  # `amod`: a parameter modification function created via `rswat_amod(...)` (see DETAILS)
  # `wipe`: whether to stop the cluster workers and erase their data on disk when finished 
  # `...`: arguments to pass to `rswat_flo`
  #
  # RETURN:
  # 
  # a dataframe with columns `date`, `out_1`, `out_2`, ... `out_n`, where n = `ncol(x)`,
  # and `out_j` is the output of `rswat_flo(...)` after running `amod(x[,j])`
  # 
  # DETAILS:
  #
  # Typical model fitting routines make a series of parameter modifications, running the model
  # after each change to generate output and score the model performance associated with a
  # given parameter set. When some/all of these modifications are unrelated (ie not depending
  # on one another) they can be run in parallel. This function allows a batch of parameter sets
  # to be evaluated in this way with SWAT+ OHG file outputs.
  #
  # A set of n numeric parameter vectors should be supplied as the (p-dimensional) columns
  # of matrix `x`. The function applies `amod` to these columns to generate the corresponding
  # SWAT+ config files, which are then copied to a temporary SWAT+ project folder, where the
  # simulation is run by a worker node created by `rswat_cluster()`. 
  #

  # set up OHG output before setting up the cluster
  # TODO: write a function that just sets up / restores these settings
  # TODO: write a function that loads and joins OHG data to a gage df
  # TODO: simplify rswat_flo with these two functions
  
  
  # copy OHG files to temporary location as they are generated
  
  
  # copy the OHG files one at a time to the project folder and open them
  
  # shut down cluster and restore original state of project folder
  
}

#' parallel implementation of adaptive differential evolution algorithm from the DEoptimR package 
rswat_pardevol = function(cal, gage, errfn=NULL, quiet=TRUE, NP=NULL, maxiter=100, ...)
{
  # `cal`: dataframe, the SWAT+ parameters to modify
  # `gage`: dataframe, containing flow and date (see `my_gage_objective`)
  # `errfn`: an (anonymous) error function `errfn(x,y)` for simulations x given observations y
  # `quiet`: logical, suppresses console messages
  # `NP`: total initial population size
  # `maxiter`: the maximum number of iterations to run the algorithm
  # `...`: other arguments to JDEoptim (besides `NP`, `lower`, `upper`)

  # check that a SWAT+ project has been loaded
  err.msg = 'Run `rswat_cio` first to load a project'
  if( !exists('.rswat') ) stop(err.msg)
  if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
  textio = dirname(.rswat$ciopath)
  
  # set the default objective function
  if( is.null(errfn) ) errfn = function(x, y) { my_nse(x, y, L=2, normalized=TRUE) }
  
  # TODO: accept multiple initial vectors like cal$ini to define a population
  # copy the bounds
  lower = cal$min
  upper = cal$max
  ini = cal$ini
  
  # set default population size then decrement to adjust for appended members later
  if( is.null(NP) ) NP = ( 10 * length(lower) )
  NP = NP - 1
  
  # define a dummy optimization problem (1 iteration) to get initial population from JDEoptim
  ini_fn = function(x) { return(0) }
  opt.ini = JDEoptim(lower = lower, 
                     upper = upper, 
                     fn = ini_fn,
                     maxiter = 2,  
                     add_to_init_pop = ini, 
                     details = TRUE, 
                     NP = NP) #, ...)
  
  # initialize the cluster
  cluster.out = rswat_cluster(quiet=quiet)
  dir.node = cluster.out$path
  cpath = dirname(dir.node[1]) 
  
  # construct a parameter modification function and copy the current values
  cal.amod = rswat_amod(cal)
  cal.restore = cal.amod(quiet=quiet, reload=TRUE)
  
  # build list of temporary files that will carry parameters to each simulation, and their sources
  fmod = unique(cal$file)
  path.src = setNames(file.path(textio, fmod), fmod)
  fmod.pop = lapply(1:NP, function(x) setNames(file.path(cpath, paste0(x, '_', fmod)), fmod))
  
  # while loop starts here
  # igen = 1
  # while(igen < maxiter)
  
  # create temporary config files for each simulation in the population, in a loop
  if( !quiet ) pb = txtProgressBar(max=NP, style=3)
  cat(paste('writing', length(fmod)*NP, 'population parameter files to disk...\n'))
  for(idx.pop in 1:NP)
  {
    # write the files in the currently loaded SWAT+ project folder
    cal.amod(opt.ini$poppar[,idx.pop], quiet=TRUE, reload=FALSE)
    
    # copy them to the staging area and update progress message if necessary
    file.copy(path.src, fmod.pop[[idx.pop]])
    if( !quiet ) setTxtProgressBar(pb, idx.pop)
  }
  if( !quiet ) close(pb)
  
  # restore the original parameters of the currently loaded project
  cal.amod(cal.restore$value, reload=FALSE)
  
  
  
  
  # 
  

  
  # for each population member, modify parameter files and copy to cluster directory
  # for(ipop in 1:NP)
  #
  
  ## run simulations in parallel to get scores for all population members:
  # first copy the files, then run the executable. Parallelize using clusterApplyLB
  # 
  # 
  # 
  
  # define another dummy optimization (1 iteration) to get mutated population
  
  
  
  
  
  
  # garbage collection
  rswat_cluster(wipe=TRUE)
}

#' 
#' ## multicore model execution (internal)

#' create a cluster of nodes for running SWAT+ simulations (possibly in parallel)
rswat_cluster = function(nc=NULL, cpath=NULL, wipe=FALSE, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `nn`: positive integer, the number of nodes to create
  # `cpath`: character, path to cluster directory, where SWAT+ files for each node are stored
  # `wipe`: logical, indicating to stop the cluster and delete `cpath`
  #
  # RETURN:
  # 
  # With default `wipe=FALSE`, returns a named list with two entries:
  #
  #   'path', character vector of paths to the SWAT+ project folders for each node. 
  #   'cl', a `SOCKcluster` object for sending simulation requests to nodes 
  #
  # Both objects have length `nn` but are otherwise unconnected. ie you don't necessarily
  # have to use the project in `path[m]` for a job sent to node `cl[m]`
  #
  # DETAILS:
  #
  # The function creates and populates a list of `nn` SWAT+ project folders, each containing
  # copies of all essential config files for running the currently loaded model. The folders
  # are named "node_1", "node_2", etc, and located in the subdirectory ".rswat_cluster" of
  # the current SWAT+ project folder, unless otherwise specified with `npath`.
  #
  # `rswat_cluster(wipe=TRUE)` should be called when you are finished with the cluster. This
  # erases "cpath" and shuts down all workers. Until this happens, a copy of of the cluster
  # reference object and project paths are stored in `.rswat$cluster`, and subsequent calls to
  # `rswat_cluster(..., wipe=FALSE)` will simply return this list, ignoring `nc` and `cpath`
  
  # check that a SWAT+ project has been loaded
  err.msg = 'Run `rswat_cio` first to load a project'
  if( !exists('.rswat') ) stop(err.msg)
  if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
  textio = dirname(.rswat$ciopath)
  
  # initialize internal storage as needed
  if( !exists('cluster', envir=.rswat) ) .rswat$cluster = list(cl=list(), path=character(0))
  
  # handle `wipe` calls
  if( wipe )
  {
    if( !quiet ) cat('shutting down workers and cleaning up...')
    
    # remove cluster from memory and disk
    if( 'cluster' %in% class(.rswat$cluster$cl) ) stopCluster(.rswat$cluster$cl)
    if( length(.rswat$cluster$path) > 0 ) unlink(dirname(.rswat$cluster$path)[1], recursive=TRUE)
    
    # clean up internal storage
    .rswat$cluster = list(cl=list(), path=character(0))
    
    if( !quiet ) cat('done\n')
    
    # continue only if non-default arguments supplied
    if( is.null(nc) & is.null(cpath) ) return(invisible()) 
  }
  
  # create the cluster if it doesn't exist already
  if( length(.rswat$cluster$cl) == 0 )
  {
    if( !quiet ) cat('\ninitializing new cluster...')
    
    # delete any orphaned cluster files
    if( length(.rswat$cluster$path) > 0 )
    {
      unlink(.rswat$cluster$path, recursive=TRUE)
      .rswat$cluster$path = character(0)
    }
    
    # set default number of cores as needed then create the cluster
    if( is.null(nc) ) nc = detectCores()
    .rswat$cluster$cl = makeCluster(nc) 
  }
  
  # create SWAT+ project folders for each node if they don't exist already
  if( length(.rswat$cluster$path) == 0 )
  {
    if( !quiet ) cat(paste('\ncopying SWAT+ files to', nc, 'nodes...\n'))
    
    # set the default folder locations for the nodes, erase any existing ones
    if( is.null(cpath) ) cpath = file.path(textio, '.rswat_cluster')
    .rswat$cluster$path  = file.path(cpath, paste0('node_', 1:nc))
    unlink(.rswat$cluster$path, recursive=TRUE)
    
    # make a list of files needed for running simulations on nodes (exclude output files)
    fname.all = list.files(textio, include.dirs=FALSE)
    fname.sim = fname.all[ !endsWith(fname.all, '.txt') ]
    
    # copy these files to each node folder and store the folder paths in memory
    fname.node = sapply(.rswat$cluster$path , function(nodepath) rswat_copy(to=nodepath,
                                                                            fname=fname.sim,
                                                                            quiet=quiet))
  }
  
  # finished
  return(.rswat$cluster)
}

#' 
#' ## miscellaneous

#'  compute Nash–Sutcliffe model efficiency coefficient (NSE) 
my_nse = function(qobs, qsim, L=2, normalized=FALSE)
{
  # compute the standard NSE coefficient
  nse = 1 - drop_units( sum( abs(qsim - qobs)^L ) / sum( abs(qobs - mean(qobs))^L ) )
  
  # normalize, if requested
  if(normalized)
  {
    nse = 1 / (2 - nse)
  }
  
  return(nse)
}



#+ include=FALSE
#my_markdown('rswat_output')
