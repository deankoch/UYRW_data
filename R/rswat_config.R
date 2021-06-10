#' ---
#' title: "rswat_config"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **rswat_config**: R functions for managing the SWAT+ configuration text file directory
#' 
#' SWAT+ has an enormous number of model parameters. These are stored as several dozen
#' plaintext configuration files - usually in a directory called "TxtInOut" - which the model
#' executable reads as input when running simulations. 
#' 
#' Generally speaking, these text files contain a comment line, then one or more
#' whitespace delimited tables with 1-2 header lines. This is great for human readability -
#' open a file in any text editor and you can immediately understand what's going on. However
#' they make programming somewhat painful, as it's difficult to determine automatically how
#' these tables are laid out and therefore difficult to read/write their entries properly.
#' 
#' The obvious solution is to dig through the SWAT+ (FORTRAN) code and figure out the exact
#' character positions to look for each field. But this would be very tedious and prone to
#' failure - SWAT+ is in active development so future changes in SWAT+ parameter names and
#' file structures are likely, and these could break a table parser that uses file-specific
#' rules.
#' 
#' Our implementation instead scans the files listed in 'file.cio', tabulates their whitespace
#' delineated fields, then merges the results into tables wherever there is a consistent pattern
#' of row-length and class (as detected by R's built-in `is.numeric` and some sensible rules for
#' snapping and spacing). ie. it learns the file structures, names, and spacing patterns on the
#' fly. This will hopefully make it robust to future changes in SWAT+.
#' 
#' See [demo_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_txtinout.md)
#' for a demonstration of `rswat_config` on a real project. 

#' 
#' ## libraries

#' [`here`](https://cran.r-project.org/web/packages/here/index.html) simplifies paths
#' involving a project working directory
library(here)

#' [`dplyr`](https://dplyr.tidyverse.org/R) syntax simplification for complex table operations 
library(dplyr)

#' [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) faster table
#' loading
library(data.table)

#' [`units`](https://cran.r-project.org/web/packages/units/index.html) units for numerics
library(units)


#' 
#' ## dependencies

#' `rswat_find` (defined below) requires `my_adist` which is defined in the helper script
#' [rswat_docs](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md)
source(here('R/rswat_docs.R'))

#' 
#' ## .rswat environment
#' 
#' The environment `.rswat` is initialized here to store various dataframes and characters
#' strings that are used as global variables in the `rswat` codebase. When this code is turned
#' into a package, the environment will be properly hidden 

# define (internal) environment to store the SWAT+ project file data
if( !exists('.rswat', mode='environment') ) .rswat = new.env( parent=emptyenv() )


#' 
#' ## core functions for managing SWAT+ configuration files

#' open 'file.cio' and list its contents, initialize rswat storage
rswat_cio = function(ciopath=NULL, trim=TRUE, wipe=FALSE, reload=FALSE, ignore=NULL, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `ciopath`: (character string) path to the SWAT+ master watershed 'file.cio'
  # `trim`: (logical) indicates to trim output for readability and omit files in `ignore`
  # `wipe`: (logical) indicates to flush all data for the current project from R
  # `reload`: (logical) indicates to import all files into memory
  # `ignore`: (character vector) 'file' or 'group' names to exclude 
  # `quiet`: (logical) suppresses console messages
  #
  # RETURN VALUE:
  #
  # A dataframe with information on all files listed in `ciopath`
  #
  # DETAILS:
  #
  # `ciopath` should point to the SWAT+ master watershed file 'file.cio'. This is parsed to
  # discover the config files associated with a SWAT+ project, and a dataframe of info on these
  # files is returned. Subsequent calls may `ciopath` to get this dataframe without reloading
  # anything. Including `ciopath` in a subsequent call prompts the function to scan again for
  # files (eg to find newly added ones), and drops any previously assigned argument to 'ignore'.
  #
  # Any filename or group name matching an element of input argument `ignore` will tagged
  # as 'ignored' in the return dataframe (and in memory), in order to filter out files that you
  # don't want to have load by default. Ignored files can still be loaded, they just won't be
  # included by default with `rswat_open()` calls.
  #
  # `wipe` is used for clearing memory, or for starting over without ending your R session.
  # It is invoked automatically whenever `rswat_cio` is called with `ciopath` location different
  # from the currently assigned one (if any). 
  #
  # `reload==TRUE` simply chains the `rswat_cio` call with `rswat_open(reload=TRUE)`, for a
  # quick one-liner to load all of the files in a project. Subsequent calls to `rswat_open`
  # (with default `reload==FALSE`) will be very fast
  #
  
  # handle calls with no `ciopath` argument
  if( is.null(ciopath) )
  {
    # check if the function has been run already
    if( 'ciopath' %in% ls(.rswat) )
    {
      # handle wipe calls
      if(wipe)
      {
        rm(list = c('ciopath', 'cio', 'stor'), envir=.rswat, inherits=FALSE)
        if( !quiet ) cat('all project data has been removed from R\n')
        return()
      }
      
      # grab the objects from memory and update exclusion list if necessary
      ciopath = .rswat$ciopath
      cio = .rswat$cio
      if(!is.null(ignore))
      {
        cio = cio %>% mutate(ignored = (group %in% ignore) | (file %in% ignore) )
        .rswat$cio = cio
      }
      
    } else {

      # message in case the function hasn't been run, or something else goes wrong
      stop('file.cio not found. Initialize it with `rswat_cio(ciopath)`')
      
    }
    
  } else {
  
    # detect directory input instead of 'file.cio' path
    if( basename(ciopath) != 'file.cio' )
    {
      # formulate two likely paths assuming that ciopath is a directory
      ciopath.a = file.path(ciopath, 'file.cio')
      ciopath.b = file.path(ciopath, 'Scenarios/Default/TxtInOut/file.cio')
      
      # assign the path or else halt if we can't find 'file.cio'
      ab.exists = file.exists( c(ciopath.a, ciopath.b) )
      if( any(ab.exists) )
      {
        ciopath = c(ciopath.a, ciopath.b)[ which(ab.exists)[1] ]
        if( !quiet ) cat(paste('setting `ciopath` to', ciopath, '\n'))
        
      } else {  stop('failed to set `ciopath`') }
      
    } else {
      
      if( !file.exists(ciopath) ) stop('"file.cio" not found')
    } 
    
    # if this isn't the first call, check current value of `ciopath` in memory
    if( 'ciopath' %in% ls(.rswat) )
    {
      # wipe project data `ciopath` argument is different from current value
      wipe = ifelse(normalizePath(.rswat$ciopath) != normalizePath(ciopath), TRUE, wipe)
    } 
    
    # initialize master storage list as needed
    is.initial = ( ! 'stor' %in% ls(.rswat) )
    if( is.initial | wipe )
    {
      # initialize empty package storage list objects in memory
      assign('stor', list(data=list()), envir=.rswat, inherits=FALSE)
      .rswat$stor$linedf = data.frame()
      .rswat$stor$txt = list()
      .rswat$stor$data = list()
      .rswat$stor$temp = list()
      
    }
    
    # read the text (writes to `.rswat$stor$txt` and `.rswat$stor$temp`) and print comment
    rswat_rlines(ciopath)
    if( !quiet ) cat(paste(.rswat$stor$txt[[basename(ciopath)]][[1]], '\n'))
    
    # read in group names from first column 
    out.nm = .rswat$stor$temp[[basename(ciopath)]]$linedf %>% 
      group_by(line_num) %>% 
      filter(field_num==1) %>% 
      pull(string)
    
    # create output table, filtering nulls
    cio = .rswat$stor$temp[[basename(ciopath)]]$linedf %>% 
      group_by(line_num) %>% 
      mutate(group = out.nm[line_num-1]) %>% 
      mutate(field_num = field_num - 1) %>%
      filter(field_num > 0, string != 'null') %>% 
      mutate(file = string) %>%
      mutate(path = file.path(dirname(ciopath), file)) %>%
      mutate(exists = file.exists(path)) %>%
      mutate(size = set_units(set_units(file.info(path)$size, bytes), kilobytes)) %>%
      mutate(modified = file.info(path)$mtime) %>%
      mutate(ignored = FALSE )  %>%
      mutate(msg=NA, ntab=NA, nvar=NA, nskip=NA, nline=NA) %>% 
      as.data.frame() 
    
    # update exclusion list if necessary
    if( !is.null(ignore) )
    {
      cio = cio %>% mutate(ignored = (group %in% ignore) | (file %in% ignore) )
    }
    
    # tidy up and initialize project metadata objects in the package environment
    .rswat$stor$temp = .rswat$stor$temp[-which(names(.rswat$stor$temp) == basename(ciopath))]
    assign('ciopath', ciopath, envir=.rswat, inherits=FALSE)
    assign('cio', cio, envir=.rswat, inherits=FALSE)
    
    # differentiate between initial and subsequent calls
    if( ! (is.initial | wipe) )
    {
      # prune storage for entries no longer found in 'file.cio'
      .rswat$stor$txt = .rswat$stor$txt[names(.rswat$stor$txt) %in% cio$file]
      .rswat$stor$data = .rswat$stor$data[names(.rswat$stor$data) %in% cio$file]
      if( nrow(.rswat$stor$linedf) > 0 )
      {
        .rswat$stor$linedf = .rswat$stor$linedf %>% filter(file %in% cio$file)
      }
    }
  }
  
  # append metadata from any loaded files
  if( nrow(.rswat$stor$linedf) > 0 )
  {
    # compute stats by file
    linedf.stats = .rswat$stor$linedf %>% 
      group_by(file) %>% 
      summarize(.groups='drop_last',
        ntab=length(unique(table[!is.na(table)])), 
        nvar=n(),
        nline=length(unique(line_num))-1,
        nskip=list(length(unique(line_num[skipped])))) %>%
      as.data.frame
    
    # add them to the return dataframe, and overwrite in storage
    nm.stats = c('nline', 'nskip', 'ntab', 'nvar')
    cio[match(linedf.stats$file, cio$file), nm.stats] = linedf.stats[nm.stats]
    .rswat$cio = cio
  }
  
  # trim, if requested, and finish 
  if(trim) cio = cio %>% 
    filter(exists) %>% 
    select(file, group, size, modified, nline, nskip, ntab, nvar) %>%
    select_if(~!all(is.na(.)))
  
  # reload files if requested
  if(reload)
  {
    # load files into memory and update the cio with stats
    rswat_open(reload=TRUE, quiet=quiet) 
    cio = rswat_cio(ciopath, reload=FALSE, trim=trim, ignore=ignore, quiet=quiet)
  }
  
  # return up-to-date files list
  return(cio)

}

#' wrapper for methods to load SWAT+ config files into memory and/or return them as dataframes
rswat_open = function(fname=character(0), reload=FALSE, simplify=TRUE, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `fname`: (character vector) 'file' or 'group' names listed in `rswat_cio()`
  # `reload`: (logical) indicating to load the data into memory, but not return it
  # `quiet`: (logical) suppresses console messages
  #
  # RETURN VALUE:
  #
  # Returns a named list containing all the tabular data associated with `fname`. Each list
  # entry is named after a source file, and consists of either a dataframe (if the file has
  # a single table) or a list of dataframes (if the file has multiple tables). Toggling
  # `simplify==FALSE` will wrap the single-table case(s) in a list, for consistency with the
  # other cases.
  # 
  # If `fname` is empty (the default) the function returns the result of `rswat_cio()`
  # (a list of files in the TxtInOut directory).
  #
  # If `reload==TRUE`, the function call loads and parses all requested files into memory,
  # overwriting anything already there. This is useful if you need to reload project data after
  # it's been modified on disk by external programs.
  
  # parse the `fname` argument, assigning all existing files by default
  cio = rswat_cio(trim=FALSE) %>% filter(exists) 
  listmode = length(fname) == 0
  if(listmode) fname = cio %>% filter(!ignored) %>% pull(file) 
  cio.match = cio %>% filter( (file %in% fname) | (group %in% fname))
  
  # message if no filenames match input string
  if(nrow(cio.match) == 0) warning(paste('no matches for', paste(fname, collapse=', '))) 
  
  # exit if not loading anything - `rswat_open()` calls end here
  if( (!reload) & listmode )
  {
    # trim the output from `rswat_cio(trim=FALSE)`
    cio.out = cio %>% 
      filter( !ignored ) %>% 
      filter( exists ) %>% 
      select( file, group, size, modified, nline)
    
    # finish
    return(cio.out)
  }
  
  # check what's been loaded into memory already, enter loading loop if required 
  idx.loaded = cio.match$file %in% names(.rswat$stor$data)
  if( reload ) idx.loaded = rep(FALSE, nrow(cio.match))
  if( any(!idx.loaded) )
  {
    # initial console message
    if(!quiet) print(paste('loading', sum(!idx.loaded), 'file(s)'))
    fname.toload = cio.match$file[!idx.loaded]
    
    # wipe any existing entries for the file(s) from the `linedf` dataframe in memory
    idx.existing = .rswat$stor$linedf$file %in% fname.toload
    if(any(idx.existing)) .rswat$stor$linedf = .rswat$stor$linedf[!idx.existing,]
    
    # build console progress messages with fixed width, and initialize a progress bar object
    if(!quiet) 
    {
      msg.width = max(nchar(fname.toload))
      msg.pad = sapply(fname.toload, function(x) paste0(rep(' ', msg.width - nchar(x)), collapse=''))
      msg.console = paste(' > ', paste(fname.toload, msg.pad))
      pb.width = getOption('width') - max(nchar(msg.console)) - 5
      pb = txtProgressBar(max=length(fname.toload) + 1, style=3, width=pb.width)
    }
    
    # loop over files
    for(fname.load in fname.toload)
    {
      # print progress message prior to loading the file
      if(!quiet)
      {
        setTxtProgressBar(pb, which(fname.toload == fname.load))
        cat(msg.console[which(fname.toload == fname.load)])
        flush.console()
      }
      rswat_rfile(fname.load)
    }
    
    if(!quiet) 
    {
      setTxtProgressBar(pb, length(fname.toload) + 1 )
      close(pb)
    }
  }

  # prepare the requested data in a named list, then simplify if requested
  values.out = .rswat$stor$data[cio.match$file]
  if( simplify )
  {
    # collapse redundant within-file lists, then deal with single-file lists
    idx.singles = sapply(values.out, length) == 1
    values.out[idx.singles] = lapply(values.out[idx.singles], function(x) x[[1]])
    
    # collapse redundant 
    if(length(values.out) == 1) values.out = values.out[[1]]
  }
  
  # pull a copy of the requested dataframes
  values.out = .rswat$stor$data[cio.match$file]
  
  # simplify as needed before returning the list
  if( simplify )
  {
    # collapse redundant within-file lists, then collapse any single-file lists
    idx.singles = sapply(values.out, length) == 1
    values.out[idx.singles] = lapply(values.out[idx.singles], function(x) x[[1]])
    if(length(values.out) == 1) values.out = values.out[[1]]
  }
  return(values.out)
}

# TODO: combine this with rswat_ofind
#' search tool for SWAT+ config parameter text, names, and filenames
rswat_find = function(pattern='*', fuzzy=-1, trim=TRUE, include=NULL, ignore=NULL)
{
  # ARGUMENTS:
  #
  # `pattern`: character vector, the string(s) to search for
  # `fuzzy`:  numeric, specifying tolerance for approximate matches (see DETAILS)
  # `trim`: logical, whether to omit detailed metadata about character positions
  # `include`: (optional) character vector, filenames to include in searches
  # `ignore`: (optional) character vector, filenames to omit from searches
  #
  # RETURN VALUE:
  #
  # A dataframe of information about SWAT+ names matching the search pattern(s).
  #
  # DETAILS
  # 
  # Returns metadata on matches of `pattern` with SWAT+ parameter names
  # If no pattern is supplied, the function returns information on all parameters in the 
  # subset of files determined by `include` and `ignore`. If `include` is supplied, only
  # results from those files are returned (default is all files). Argument `ignore` has
  # the opposite effect, and supercedes anything in `include`.
  #
  # `fuzzy = -1` is for exact matches only, `fuzzy = 0` includes substring matches, and
  # `fuzzy > 0` includes approximate substring matches. See `?my_adist`.
  
  # default string for `ignore` should match nothing
  if( is.null(ignore) ) ignore =  ' '
  
  # default is to include all files but `ignore` always takes precedence
  if( is.null(include) ) include = rswat_cio()$file
  include = include[ ! include %in% ignore ]
  
  # identify ignored and included files and build index of subset of database to search
  is.included = .rswat$stor$linedf$file %in% include
  is.ignored = .rswat$stor$linedf$file %in% ignore
  which.include = which(is.included & (!is.ignored) )
  
  # handle various invalid calls
  err.msg = 'file metadata not found. Have you run rswat_cio and loaded a file?'
  if( length(is.ignored) == 0 ) stop(err.msg)
  if( length(is.included) == 0 ) stop('`include` is empty. Try omitting `ignore`')
  if( all(is.ignored) ) stop('all known files are listed in `ignore`')
  
  # copy the names list to search and pass it to string distance function
  pattern.all = paste(unique(pattern), collapse='|')
  name.all = .rswat$stor$linedf$name[which.include]
  dist.all = my_adist(pattern.all, name.all)
  
  # initialize results vector to exact matches only
  idx.out = dist.all == 0
  int.out = which(idx.out)

  # add substring matches as needed
  if( !(fuzzy < 0) )
  {
    # excluding exact matches from this index, sort according to weighted score
    idx.smatch = !( idx.out ) & ( dist.all < 1 )
    int.smatch = which(idx.smatch)[ order( dist.all[idx.smatch] ) ]
    
    # add sorted indices to the stack of results
    int.out = c(int.out, int.smatch)
  }
  
  # approximate search mode
  if( fuzzy > 0 )
  {
    # sort remaining elements into bins of equal distance to `pattern` 
    adist.bin = unique( sort( round(dist.all[ !idx.smatch ], 2) ) )
    
    # find matching elements from the closest bins, add sorted indices to stack
    idx.amatch = round(dist.all, 2) %in% adist.bin[ seq_along(adist.bin) < ceiling(fuzzy) + 1 ]
    int.amatch = which(idx.amatch)[ order( dist.all[idx.amatch] ) ]
    int.out = c(int.out, int.amatch)
  }
  
  # handle no-match cases
  if( length(int.out) == 0 )
  {
    # console message about failed search
    pattern.msg = paste0('\"', pattern, '\"')
    nomatch.msg = paste0('No exact matches for ', pattern.msg, '. ')
    
    # don't repeat search when fuzzy = -Inf
    if( fuzzy == -Inf )
    {
      # print a message and return empty dataframe
      cat( paste(nomatch.msg, '\n') )
      return( vartable %>% slice(int.match) %>% select( name, file, pstart, description ) )
    }
    
    # increment fuzzy until we get a match (fuzzy==1 will always produce a match)
    if( fuzzy == 0 ) newfuzzy = 1
    
    # if no exact matches try switching to substring matching
    if( fuzzy < 0 ) newfuzzy = 0
    
    # message about increasing fuzzy
    nomatch.info = paste('Repeating search at fuzzy level', newfuzzy, '\n')
    cat( paste0(nomatch.msg, nomatch.info) )
    
    # run the search again with higher fuzzy level
    return( rswat_find(pattern, newfuzzy, trim=trim, include=include, ignore=ignore) )
  }
  
  # extract the subset of database containing matches
  linedf.out = .rswat$stor$linedf[ which.include[int.out], ]
  
  # finish if not tidying up output
  if( !trim ) return(linedf.out)

  # omit skipped lines, header lines, etc
  linedf.out = linedf.out %>% filter( !is.na(i) )

  # omit all but fist data row, filter 'name' columns
  linedf.out = linedf.out %>% filter(i == 1) %>% filter(name != 'name')
  
  # multirow columns are represented with NAs in place of row index and string
  linedf.out$i[linedf.out$dim > 1] = NA
  linedf.out$string[linedf.out$dim > 1] = NA
  
  # tidy output and finish
  linedf.out = linedf.out %>% select(name, string, class, dim, file, table, i, j)
  rownames(linedf.out) = c()
  return(linedf.out)
}

#' write a dataframe or list of them to the corresponding SWAT+ config file(s)
rswat_write = function(value, fname=NULL, tablenum=NULL, preview=TRUE, reload=TRUE, quiet=FALSE)
{
  #
  # ARGUMENTS:
  #
  # `value`: dataframe or list containing the SWAT+ table(s) with modified parameters
  # `fname`: (optional) character, name of file containing the table
  # `tablenum`: (optional) integer, list index for the table within the file
  # `preview`: logical, whether to return dataframe listing the changes, but not write them
  # `reload`: logical, whether to reload the file after writing, to update file data in memory
  # `quiet`: logical, passed to rswat_load if `reload==TRUE` (otherwise ignored)
  #
  # RETURN:
  #
  # Returns nothing but writes `value` to the file `fname` on disk when `preview=FALSE`.
  # Otherwise returns a dataframe with information on the line-by-line changes that would be
  # made to the config file(s) in non-preview mode.
  #
  # DETAILS:
  #
  # `value` is usually the return value from an `rswat_open` call after some modification of
  # the contents - that is, either dataframe of SWAT+ parameters, or a list of them. These
  # objects are compared against the known SWAT+ files to determine which files to modify,
  # ie you can't write a brand new file using this function, only modify existing ones.
  #
  # If `value` is a list of tables from the same file (eg. from 'print.prt' or 'weather-wgn.cli'),
  # then ALL tables from that file should be included, in the same order as they are returned
  # in an `rswat_open` call, and the list entries should be unnamed. If `values` is a list of
  # tables from multiple files, its entries should be named after the filename.
  #
  # `fname` and `tablenum` are usually not required as `rswat_index` will be called to
  # identify them automatically based on column names. These arguments are ignored when
  # `value` is a list. 
  #
  
  # grab a list of all filenames and the input object names
  cio = rswat_cio(trim=FALSE)
  nm.head = names(value)
  
  # coerce vector input to dataframe
  if( is.vector(value) & !is.list(value) ) value = data.frame(as.list(value))
  
  # handle dataframe (and vector) input
  if( is.data.frame(value) )
  {
    # resolve destination for this data
    value.index = rswat_index(nm.head, fname=fname, tablenum=tablenum)
    
    # `rswat_index` should throw an error if either of these is not unique
    fname = value.index$fname
    tablenum = value.index$tablenum
    
  } else {
  
    # check whether names of list input match filenames
    idx.named = nm.head %in% cio$file
    
    # case of one file with multiple tables: unnamed list entries, all dataframes
    if( length(idx.named) == 0 & all(sapply(value, is.data.frame)) )
    {
      # search for each of the tables individually using their table number
      fname.res = mapply(function(nm, idx) rswat_index(nm, tablenum=idx)$fname, 
                         nm = lapply(value, names), 
                         idx = 1:length(value)) 

      # check that all matches are to the same filename
      fname = unique(fname.res)
      if(length(fname) > 1) stop('list entry contained tables matching multiple files')
 
      # recursive call in lapply loop over table numbers
      write.out = lapply(1:length(value), function(idx) {
        rswat_write(value[[idx]], fname=fname, tablenum=idx, preview, reload, quiet)
      })
      
      # unlist and rbind the result before returning in preview mode
      if( preview ) write.out = do.call(rbind, write.out)
      return( write.out )
    }

    # list of filenames case
    if( all(idx.named) )
    {
      # recursive call in lapply loop over filenames
      write.out = lapply(nm.head, function(nm) {
        rswat_write(value[[nm]], fname=nm, tablenum, preview, reload, quiet)
      })
      
      # unlist and rbind the result before returning in preview mode
      if( preview ) write.out = do.call(rbind, write.out)
      return( write.out )
    }
    
    # halt if it's neither of the above two cases
    stop('rswat_write did not recognize the tables in `values`')
  }

  # grab a copy of the relevant lines descriptions for this file and modify for output 
  linedf = rswat_find(include=fname, trim=FALSE) %>% 
    filter(name %in% nm.head) %>% 
    filter(table == tablenum) %>% 
    arrange(j,i) %>%
    mutate(current_value = string) %>%
    mutate(replacement = NA) 
  
  # consistency check for column lengths
  if( !all(nrow(value) == linedf$dim) )
  {
    vnames.msg = paste0('[', paste(nm.head, collapse=', '), ']')
    info.msg = paste('Expected', linedf$dim[1],  'row(s) from input columns', vnames.msg)
    stop(paste0(info.msg, ' (got ', nrow(value), ')'))
  }
  
  # copy the table from memory
  value.old = .rswat$stor$data[[fname]][[tablenum]][, nm.head] 
  
  # find index of columns that have been modified, handling single entry case separately
  if( is.data.frame(value.old)  )
  {
    # build matrix of changed value indicators
    cn.new = sapply(1:ncol(value.old), function(x) !identical(value.old[,x], value[,x]) )
    m.new = matrix(FALSE, nrow(value), ncol(value))
    m.new[,cn.new] = sapply(which(cn.new), function(cn) ! value.old[,cn] == value[,cn])
    
  } else { 
    
    # mx1 case messes with sapply so we handle it separately
    m.new = value.old != value[[1]]
    cn.new = any(m.new)
  }
  
  # NOTE: cn.new picks up integer-as-numeric mismatches, which may be ignored later
  # because we will coerce them to the right type.
  
  # handle no-change case
  if( !any(unlist(m.new)) )
  {
    # tidy up columns of 0-row table
    linedf.new = linedf[integer(0),] %>% 
      select(file, table, i, j, dim, 
             line_num, start_col, end_col, 
             name, current_value, replacement)
    
    # return it in preview mode
    if(preview) return(linedf.new)
    return(invisible())
  }
  
  # indicate whether an NA is being replaced or not
  isna.old = is.na(value.old)
  isna.new = is.na(value)
  m.new[ isna.old & isna.new ] = FALSE
  m.new[ isna.old & !isna.new ] = TRUE
  m.new[ !isna.old & isna.new ] = TRUE
  
  # copy the relevant subset of the character info dataframe
  linedf.new = linedf[which(m.new),]
  if( length(value.old) > 1 )
  {
    # vectorization magic here relies on the `arrange(j,i)` call in linedf definition! 
     value.new = unlist(lapply(which(cn.new), function(cn) {
       as.list(value[m.new[,cn],cn])
       }), recursive=F)
     
  } else {
    
    # 1x1 case messes with sapply so we handle it separately
    value.new = value[[1]]
  }

  # convert to character representation with appropriate padding/rounding
  char.new = rswat_2char(value.new, linedf.new)

  # finished preview mode - append replacement values and tidy up columns
  if(preview) return(linedf.new %>% 
                       mutate(replacement = char.new$string) %>%
                       select(file, table, i, j, dim, 
                              line_num, start_col, end_col, 
                              name, current_value, replacement))
  
  # load the raw text and make the changes in a loop
  txt.out = .rswat$stor$txt[[fname]]
  for(idx in 1:nrow(linedf.new))
  {
    idx.ln = linedf.new$line_num[idx]
    idx.start = linedf.new$start_col[idx]
    idx.end = linedf.new$end_col[idx]
    substr(txt.out[idx.ln], idx.start, idx.end) = char.new$string_padded[idx]
  }
  
  # write the changes to disk
  writeLines(txt.out, cio %>% filter(file==fname) %>% pull(path))
  
  # refresh the data in memory if requested
  if(reload) invisible( rswat_open(fname, reload=TRUE, quiet=quiet) )
}

#' tool for copying SWAT+ config files and making backups
rswat_copy = function(from=NULL, to=NULL, fname=NULL, overwrite=FALSE, quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `from`: character, path to the source directory
  # `to`: character, path to the destination directory
  # `fname`: character vector, name of files in `from` to copy
  # `overwrite`: logical, whether to overwrite any existing files in path `to`
  # `quiet`: logical, suppresses console messages
  #
  # RETURN VALUE:
  #
  # a vector of paths to the files copied in their new location
  #
  # DETAILS:
  #
  # When not supplied, `from` is set to the currently loaded SWAT+ project directory (usually
  # ".../TxtInOut") and when `fname` is not supplied it is assigned all config files listed by
  # `rswat_cio()`. The special argument `fname='.'` also copies any non-config files found in
  # `from` (such as weather input files), but excludes directories. 
  #
  # When neither `to` nor `from `is supplied, `from` is set as above, and `to` is set to
  # `from`/rswat_backup_<foo>, where <foo> is a random string that is very unlikely to collide
  # with any previous backups. This makes it easy to create a backup; just call `rswat_copy()`
  # without arguments.
  #
  # When `from` but not `to` is supplied, `to` is set to the currently loaded SWAT+ project
  # directory. This allows a backup to be restored by calling `rswat_copy(from=...)`. Files
  # modified by `rswat_copy` are reloaded only if they have been loaded already in the R
  # session.
  #

  # grab project directory from package environment
  if( exists('.rswat') )
  {
    # copy the path from the package environment
    ciopath = .rswat$ciopath
    
    # handle missing ciopath then assign (SWAT+ project) config files directory
    if( is.null(ciopath) ) stop('ciopath not found. Try setting it with `rswat_cio`')
    textio = dirname(ciopath)
    
  } else stop('"file.cio" not found. Run `rswat_cio` to set its path')
  
  # grab current config files list
  cio = rswat_cio(trim=F)
  
  # set default source and destination paths as needed
  to.default = file.path(textio, paste0('_rswat_backup_', basename(tempfile()) ) )
  if( is.null(from) ) from = textio 
  if( is.null(to) )
  {
    # default destination is either textio or a subdirectory, depending on `from`
    if( from == textio ) to = to.default 
    if( from != textio ) to = textio
  }
  
  # default files list includes everything listed in 'file.cio' (and the file itself)
  if( is.null(fname) ) fname = c('file.cio', cio$file)
  
  # special argument '.' copies all files
  if( all(fname=='.') ) fname = list.files(from, include.dirs=FALSE)
  
  # define source and destination file paths and check for existing ones
  dest.path = file.path(to, fname)
  dest.exists = file.exists(dest.path)
  src.path = file.path(from, fname)
  src.exists = file.exists(src.path)
  
  # check for and fix missing files listed in `fname`
  if( !all(src.exists) )
  {
    # update the source and destination file lists to remove missing items
    fname = fname[src.exists]
    src.path = src.path[src.exists]
    src.exists =  src.exists[src.exists]
    dest.path = dest.path[src.exists]
    dest.exists = dest.exists[src.exists]
  }
  
  # make list of existing destination files (possibly empty)
  fname.overwrite = fname[dest.exists]
  
  # handle existing files when overwrite not enabled 
  if( ( !overwrite ) & ( length(fname.overwrite) > 0 ) )
  {
    # warn of overwrites requested
    msg1 = 'the following files already exist in directory'
    msg2 = '(change `to` or set `overwrite=TRUE` to overwrite):\n'
    warning(paste(msg1, to, msg2, paste(fname.overwrite, collapse=', ')))
    
    # update the source and destination file lists to remove conflicts
    idx.overwrite = fname %in% fname.overwrite
    fname = fname[!idx.overwrite]
    src.path = src.path[!idx.overwrite]
    src.exists =  src.exists[!idx.overwrite]
    dest.path = dest.path[!idx.overwrite]
    dest.exists = dest.exists[!idx.overwrite]
    fname.overwrite = character(0)
  }
  
  # count number of files to be written
  n.tocopy = length(fname)
  
  # error if we are left with nothing to write, then create directory as needed
  if( n.tocopy == 0 ) stop('Nothing to write')
  my_dir(to)
  
  # copy the files in a loop
  if( !quiet ) cat( paste('copying', n.tocopy, 'file(s) to', basename(to), '...\n') )
  if( !quiet ) pb = txtProgressBar(max=n.tocopy, style=3)
  for(idx.file in 1:n.tocopy)
  {
    file.copy(src.path[idx.file], dest.path[idx.file], overwrite=overwrite)
    if( !quiet ) setTxtProgressBar(pb, idx.file)
  }
  if( !quiet ) close(pb)
  
  # reload any config files that were replaced in current SWAT+ project directory
  if( overwrite & ( length(fname.overwrite) > 0 ) & (to == textio) )
  {
    # handle 'file.cio' replacements by refreshing files list
    if('file.cio' %in% fname.overwrite)
    {
      # maintain ignored files list
      ignore = cio %>% filter(ignored) %>% pull(file)
      cio = rswat_cio(ciopath, trim=FALSE, ignore=ignore, quiet=TRUE)
    }

    # check for overwrites that coincide with currently loaded files and reload them (if any)
    fname.loaded = cio %>% filter( !is.na(ntab) ) %>% filter( !ignored )
    idx.coinc = fname.overwrite %in% fname.loaded
    if( any(idx.coinc) ) 
    {
      if( !quiet ) cat( paste('reloading', sum(idx.coinc), 'file(s)...') )
      rswat_open(fname.overwrite[idx.coinc], reload=TRUE, quiet=TRUE)
    }
  }
  
  # end the console messages and return the paths to the files written
  if( !quiet ) cat('done\n')
  return(dest.path)
}

#' summarize differences between two SWAT+ models
rswat_compare = function(compare, reference=NULL, ignore=NULL, quiet=FALSE)
{
  # ARGUMENTS:
  # 
  # 'compare': character, path to directory of the SWAT+ model to compare against
  # 'reference': character, path to directory of the reference SWAT+ model (will be loaded)
  # `ignore`: character vector, files or groups to ignore (passed to `rswat_cio`)
  # `quiet`: logical, suppresses console messages
  #
  # RETURN:
  #
  # dataframe with rows 'reference', 'compare', fields tabulating all
  # parameter values that aren't identical. 
  #
  # DETAILS:
  #
  # default NULL `reference` indicates to use the currently loaded SWAT+ model
  #
  
  # default reference project is the currently loaded one
  if( is.null(reference) )
  {
    # check that rswat has been initialized and pull the directory from rswat environment
    err.msg = 'Run `rswat_cio` first to load a project, or specify `reference`'
    if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
    reference = dirname(.rswat$ciopath)
  }
  
  # copy the environment list to back up current rswat session
  b.rswat = .rswat
  
  # make a list of files in common between the two directories
  fname.tocheck = intersect(list.files(reference), list.files(compare))
  
  # load all text data to prune identical files 
  if( !quiet ) cat('\npruning identical files...')
  dat.comp  = lapply(file.path(compare, fname.tocheck), readLines)
  dat.ref = lapply(file.path(reference, fname.tocheck), readLines)
  idx.identical = mapply(function(x,y) identical(x[-1], y[-1]), dat.comp, dat.ref)
  fname.tocheck = fname.tocheck[!idx.identical]
  # checksums would be faster but we want to ignore comment text in comparison

  # load the comparison project and copy relevant parameter data
  if( !quiet ) cat('loading comparison project...\n\n')
  cio.comp = rswat_cio(compare, reload=FALSE, ignore=ignore, quiet=quiet, trim=FALSE)
  fname.comp = rswat_open() %>% filter(file %in% fname.tocheck) %>% pull(file)
  fdata.comp = rswat_open(fname.comp, quiet=quiet)
  
  # load the reference project copy relevant parameter data
  if( !quiet ) cat('loading reference project...\n\n')
  cio.ref = rswat_cio(reference, reload=FALSE, ignore=ignore, quiet=quiet, trim=FALSE)
  fname.ref = rswat_open() %>% filter(file %in% names(fdata.comp)) %>% pull(file)
  fdata.ref = rswat_open(fname.ref, quiet=quiet)
  
  # prune to elements in common and make sure order matches
  fname.both = intersect(names(fdata.comp), names(fdata.ref))
  fdata.comp = fdata.comp[ match(names(fdata.comp), fname.both) ]
  fdata.ref = fdata.ref[ match(names(fdata.ref), fname.both) ]
  
  # collapse list entries for files containing multiple tables
  tnum.both = rep(1, length(fname.both))
  can.collapse = !sapply(fdata.ref, is.data.frame)
  if( any( can.collapse ) )
  {
    # collect filenames and numbers of tables 
    nc.ref = sapply(fdata.ref[can.collapse], length)
    nc.equal = nc.ref == sapply(fdata.comp[can.collapse], length)
    
    # omit matched files with different numbers of tables
    if( !all( nc.equal ) )
    {
      # console message
      fn.omit = fname.both[can.collapse][!nc.equal]
      msg.omit = paste(fn.omit, collapse=', ')
      msg.nc = '\nThe following have unmatched table numbers and are excluded from results:\n\n'
      cat( paste(msg.nc, msg.omit, '\n\n') )
      
      # recursive call excluding these files
      ignore.upd = c(ignore, fn.omit) 
      return(rswat_compare(compare, reference, ignore=ignore.upd, quiet=quiet, trim=trim))
    }
    
    # strip top-level list (result should be a list of dataframes)
    collapsed.ref = unlist( fdata.ref[can.collapse], recursive=FALSE)
    collapsed.comp = unlist( fdata.comp[can.collapse], recursive=FALSE)
    
    # make corresponding table number and filename vectors
    fnew = unlist(lapply( seq_along(nc.ref), function(x) rep(names(nc.ref)[x], each=nc.ref[x]) ))
    tnew = unlist(lapply( seq_along(nc.ref), function(x) 1:nc.ref[x] ))
    
    # update master lists
    fname.both = c( fname.both[ !can.collapse ], fnew )
    tnum.both = c( tnum.both[ !can.collapse ], tnew )
    fdata.ref = c(fdata.ref[ !can.collapse ], collapsed.ref)
    fdata.comp = c(fdata.comp[ !can.collapse ], collapsed.comp)
  }
  
  # sanity check for dimensions
  dim.ref = lapply(fdata.ref, dim)
  dim.comp = lapply(fdata.comp, dim)
  dim.equal = mapply(function(x,y) identical(x,y), dim.ref, dim.comp)
  ncol.equal = unlist(mapply(function(ref, comp) ref[2] == comp[2], dim.ref, dim.comp))
  nrow.equal = unlist(mapply(function(ref, comp) ref[1] == comp[1], dim.ref, dim.comp))
  
  # ignore (but warn of) pairs of tables having different numbers of columns
  if( any( !ncol.equal ) )
  {
    # prepare a message about the mismatched columns
    msg.fn = paste(fname.both[!ncol.equal], collapse=', ')
    msg.dim = '\nThe following have unmatched columns and are excluded from results:\n\n'
    cat( paste(msg.dim, msg.fn, '\n\n') )
  }
  
  # tables with different nrow likely come from models with different numbers of HRUs
  if( any( !nrow.equal ) )
  {
    # prepare a message about the mismatched rows
    msg.fn = paste(fname.both[!nrow.equal], collapse=', ')
    msg.dim = '\nThe following have unmatched rows'
    msg.info = '(likely due to different numbers of HRUs):\n\n'
    cat( paste(msg.dim, msg.info, msg.fn, '\n\n') )
    
    # coerce to correct nrow by adding dummy rows or subsetting
    fdata.comp[ !nrow.equal ] = lapply( which(!nrow.equal), function(idx) {
      
      # copy dataframes for the two files
      comp = fdata.comp[[idx]]
      ref = fdata.ref[[idx]]
      nr = nrow(ref)
      nc = nrow(comp)
      
      # modify, return new dataframe for fdata.comp
      if(nr < nc) return( comp[1:nr,] )
      if(nr > nc) return( rbind(comp, ref[, nr + ( 1:(nr-nc) ) ]) )
    })
  }
  
  # call rswat_write in preview mode to get dataframe of changes (without writing anything)
  write.list = lapply(which(ncol.equal), function(x) rswat_write(fdata.comp[[x]],
                                                                 fname=fname.both[x],
                                                                 tablenum=tnum.both[x], 
                                                                 preview=TRUE) )
  
  # merge into one dataframe and tidy up 
  write.out = do.call(rbind, write.list) %>%
    mutate(reference = current_value) %>%
    mutate(compare = replacement) %>%
    select(-c(current_value, replacement))

  # restore rswat session and finish
  .rswat = b.rswat
  return(write.out)
}

#' construct an anonymous function that reads/modifies a selection of SWAT+ parameters
rswat_amod = function(parm)
{
  # 
  # ARGUMENTS:
  #
  # `parm`: dataframe, the SWAT+ parameters to modify (see DETAILS)
  #
  # RETURN VALUE:
  #
  # Returns an anonymous function whose first argument `x`, a numeric vector, specifies
  # new numeric value(s) to write to the corresponding SWAT+ file(s) (as specified in `parm`).
  # This function also has arguments `quiet` (for suppressing console messages,
  # default TRUE), and `reload` (for forcing the function to reload the file instead of
  # using the current values in memory, default TRUE)
  #
  # DETAILS:
  #
  # This is a helper function to assist in building anonymous functions that perform
  # simple read/write operations, where we aren't interested in the entire config file.
  #
  # `parm` must contain fields 'name' and 'file', and these must correspond to numeric
  # SWAT+ parameters. Optionally, the field 'i' (row number) can be supplied to specify
  # particular HRUs/LSUs etc. `i=NA` is taken to mean "all rows of this parameter column",
  # and negative integers specify all EXCEPT a particular row. The outputs from `rswat_find`
  # and/or `rswat_compare` calls are valid input to `parm`.  
  # 
  # The returned function, when called with default arguments, will return a dataframe
  # of the form returned by `rswat_find` with the additional column `value` appended,
  # displaying the parameter value(s) as currently specified in the SWAT+ config file(s).
  # If the first argument `x` is supplied, its entries overwrite the value(s) on disk, and
  # the function returns the new value(s) in the dataframe
  #
  
  # rswat_find each requested parameter to get its class and dimension
  parm.fname = sapply(split(parm, parm$file), function(x) x$name, simplify=FALSE)
  parm.list = lapply(names(parm.fname), function(nm) rswat_find(parm.fname[[nm]], include=nm) )
  parm.new = do.call(rbind, parm.list) %>% select(-string)
  
  # append the supplied row numbers
  if( 'i' %in% names(parm) ) parm.new = parm.new %>% select(-i) %>% 
    right_join(parm[,c('name', 'file', 'i')], by=c('name', 'file')) 
  
  # reorder to match input parm and overwrite it
  parm = parm.new[match(parm$name, parm.new$name),]
  
  # scan for non-numerics
  idx.nn = parm$class != 'numeric'
  if( any(idx.nn) )
  {
    # print a warning
    msg.info = paste(cal$name[idx.nn], collapse=', ')
    warning(paste('removed non-numeric entries from parm:', msg.info))
  }
  
  # copy `parm`, omitting any non-numeric entries / string column
  parm.bake = parm[!idx.nn,] 
  if( 'string' %in% names(parm.bake) ) parm.bake = parm.bake %>% select(-string)
  
  # length check
  if( nrow(parm.bake) == 0 ) stop('no numeric parameters found in parm')
  
  # begin definition of return function (`parm.bake` gets baked-in)
  function(x=NULL, quiet=TRUE, reload=TRUE)
  {
    # `x` is the vector of (numeric) SWAT+ parameters. They should be given in the same
    # order as they appeared in `parm`, when this function was created (via a call to
    # `rswat_amod`). To view this order, call the function without arguments.
    #
    # `quiet` suppresses console messages, and `reload=FALSE` allows the function to
    # pull parameter values from memory (useful for speeding up objective function
    # related write calls)
    
    # reload the files associated with parameters in `parm`
    n.parm = nrow(parm.bake)
    parm.fn = setNames(nm=unique(parm.bake$file))
    parm.values = lapply(parm.fn, function(fn) rswat_open(fn, reload=reload, quiet=TRUE) )
    
    # initialize output dataframe
    parm.out = parm.bake %>% mutate(value=NA)
    
    # if user supplied no parameter values, return the parameter info dataframe
    if( is.null(x) )
    {
      #  fill appended field by looping over filenames
      for(fn in parm.fn)
      {
        # loop over parm.bake entries for this file
        fn.idx = which( parm.bake$file == fn )
        for(idx.par in 1:length(fn.idx))
        {
          # row index in the `parm.values` table for this parameter
          i = parm.bake$i[ fn.idx[idx.par] ]
          
          # find column index the hard way, in case 'parm.bake$j' wasn't supplied
          j = which( names( parm.values[[fn]] ) == parm.bake$name[ fn.idx[idx.par] ] )
          
          # extract the values, making list of unique ones in multivariate case
          if( !is.na(i) ) parm.value = unique(parm.values[[fn]][i,j])
          if( is.na(i) ) parm.value = unique(parm.values[[fn]][,j])
          
          # copy the value if unique (non-uniqueness indicated by NA)
          if( length(parm.value) == 1 ) parm.out$value[ fn.idx[idx.par] ] = parm.value
        }
      }
    }
    
    # handle parameter overwrite calls
    if( !is.null(x) )
    {
      # type and length checks before we modify any files
      msg.fail = paste('first argument must be a length', n.parm, 'numeric vector!')
      if( !is.numeric(x) | ( length(x) != nrow(parm.bake) ) ) stop(msg.fail)
      
      # loop over filenames to write them
      for(fn in parm.fn)
      {
        # grab the subset of `parm.bake` and replacement values in `x`
        fn.idx = which( parm.bake$file == fn )
        fn.x = x[fn.idx]
        
        # loop over `parm.bake` entries for this file
        for(idx.par in 1:length(fn.idx))
        {
          # row index in the `parm.bake` table for this parameter
          i = parm.bake$i[ fn.idx[idx.par] ]
          
          # find column index the hard way, in case 'parm.bake$j' wasn't supplied
          j = which( names( parm.values[[fn]] ) == parm.bake$name[ fn.idx[idx.par] ] )
          
          # make the replacement of a single entry
          if( !is.na(i) ) parm.values[[fn]][i,j] = fn.x[idx.par]
          
          # make the replacement of an entire column
          if( is.na(i) ) parm.values[[fn]][,j] = fn.x[idx.par]
        }
        
        # finished with the table, write the changes
        rswat_write(parm.values[[fn]], fname=fn, preview=FALSE, reload=reload, quiet=quiet)
        
      } 
      
      # set new values in the output dataframe
      parm.out$value = x
    }
    
    # tidy up return dataframe
    return( parm.out %>% select(value, everything()) )
  }
  
}



#' 
#' ## internal functions for managing SWAT+ configuration files

#' line reader for SWAT+ text files
rswat_rlines = function(txtpath, omit=1, nmax=-1L)
{
  # ARGUMENTS:
  #
  # `txtpath`: character, the full path to the SWAT+ text file to be read
  # `omit`: integer vector, line numbers to omit from the output dataframe
  # `nmax`: integer, only the first `nmax` lines are loaded and parsed 
  # 
  # RETURN:
  #
  # returns nothing, but writes two objects in the package environment:
  
  #   `.rswat$stor$txt[[fname]]`, the output of `readLines` for the file,
  #
  #   `.rswat$stor$temp[[fname]]$linedf`, dataframe describing the text, with columns:
  #
  #       'string': character, the text field stripped of whitespace
  #       'line': integer, the line number of the text field
  #       'field_num' integer, the ordering of text fields on a line
  #       'start_pos', integer, line position of the first character in the text field 
  #       'end_pos', integer, line position of the last character in the text field
  # 
  # DETAILS:
  #
  # All input is assumed to be whitespace delimited, and the function does no type detection.
  # It expects the package storage list object `.rswat$stor` to exist already (probably via a
  # `rswat_cio` call), but the text and temp storage sublists ('txt' and 'temp') are created
  # if they don't exist already. Any existing data in `.rswat$stor$txt[[fname]]` and/or
  # `.rswat$stor$temp[[fname]]` is wiped. 
  #
  # The first line a SWAT+ text file is always a comment string, so it is omitted by default 
  # (however it can be accessed in `.rswat$stor$txt[[fname]][[1]]`). `nmax > 0` sets a maximum
  # for the number of lines to read - with default -1 everything is loaded.
  #

  # pull filename from path
  fname = basename(txtpath)
  
  # initialize package storage if necessary
  if(is.null(.rswat$stor$temp)) .rswat$stor$temp = list()
  if(is.null(.rswat$stor$txt)) .rswat$stor$txt = list()
  
  # read line-by-line text into package environment
  .rswat$stor$txt[[fname]] = readLines(txtpath, n=nmax)
  txt.n.full = length(.rswat$stor$txt[[fname]])
  
  # initialize temporary storage list for this file if necessary
  if(is.null(.rswat$stor$temp)) .rswat$stor$temp = list()
  
  # handle empty files
  if( txt.n.full == 0  )
  {
    # initialize the table in memory with no data
    .rswat$stor$temp[[fname]] = list(linedf=data.frame(string = character(0),
                                                       line_num = integer(0), 
                                                       field_num = integer(0),
                                                       start_pos = integer(0),
                                                       end_pos = integer(0)))
    
    # return nothing
    return(invisible())
    
  }
  
  # build index of lines to parse then measure and strip any leading whitespace
  idx = ! 1:txt.n.full %in% omit
  txt.n = length(.rswat$stor$txt[[fname]][idx])
  txt.trim = trimws(.rswat$stor$txt[[fname]][idx], 'l')
  txt.wslead = nchar(.rswat$stor$txt[[fname]][idx]) - nchar(txt.trim)
  
  # split fields at remaining whitespace 
  txt.wsmatch = gregexpr('\\s+', txt.trim, perl=T)
  txt.wsr = regmatches(txt.trim, txt.wsmatch, invert=TRUE)
  
  # clean up any empty strings matched at the end of a row
  idx.empty = sapply(txt.wsr, function(x) any(sapply(x, nchar)==0)) 
  txt.wsr[idx.empty] = lapply(txt.wsr[idx.empty], function(x) x[-length(x)])
  
  # count characters in each field
  txt.flen = lapply(txt.wsr, nchar)
  
  # enumerate the fields (columns) on each line, make matching list of row (line) numbers
  txt.fn = lapply(txt.wsr, length)
  txt.cn = lapply(txt.wsr, seq_along)
  txt.rn = mapply(function(x, y) rep(x, y), (1:txt.n.full)[idx], txt.fn, SIMPLIFY=FALSE)
  
  # count the whitespace trailing each field
  txt.wstrail = lapply(regmatches(txt.trim, txt.wsmatch), nchar)
  
  # make sure the whitespace counter matches the field length counter in length
  len.diff = sapply(txt.flen, length) - sapply(txt.wstrail, length)
  txt.wstrail = mapply(function(x,y) c(x, rep(0, y)), txt.wstrail, len.diff, SIMPLIFY=FALSE)
  
  # make a vector of character start positions for each string
  txt.start = mapply(function(x, y, z) 1 + x + c(0, cumsum(y) + cumsum(z)), 
                     txt.wslead,
                     txt.flen,
                     txt.wstrail,
                     SIMPLIFY=FALSE)
  
  # trim the redundant start position at end of each line
  txt.start = lapply(txt.start, function(x) x[-length(x)])
  
  # make a vector of character end positions for each string
  txt.end = mapply(function(x, y) x + y - 1, txt.start, txt.flen, SIMPLIFY=FALSE)
  
  # write the line description dataframe to package storage
  .rswat$stor$temp[[fname]] = list(linedf=data.frame(string = unlist(txt.wsr),
                                                     line_num = unlist(txt.rn), 
                                                     field_num = unlist(txt.cn),
                                                     start_pos = unlist(txt.start),
                                                     end_pos = unlist(txt.end)))
}

#' class detection to interpret SWAT text as R objects 
rswat_rtext = function(fname, yn=FALSE)
{
  # ARGUMENTS:
  #  
  # `fname`: character, the file to parse, the name of a list entry in `.rswat$stor$temp`
  # `yn`: logical, indicates to convert 'y'/'n' to TRUE/FALSE
  #
  # RETURN VALUE:
  #
  # the function returns nothing but has the side effect of writing an unnamed list to
  # `.rswat$stor$temp[[fname]]$values` containing fields in `.rswat$stor$temp[[fname]]$linedf`
  # converted to the appropriate R type (either integer, numeric, logical, or character). A 
  # 'class' field is also appended to `.rswat$stor$temp[[fname]]$linedf`
  #
  # DETAILS:
  #
  # The function expects field strings (stripped of whitespace) in the column 'string' of the
  # dataframe `.rswat$stor$temp[[fname]]$linedf`, which can be generated by a call to
  # `rswat_rlines`.
  #
  
  # initialize package storage for the data, if necessary
  if(is.null(.rswat$stor$temp[[fname]]$values)) .rswat$stor$temp[[fname]]$values = list()
  
  # copy the literal strings to translate
  s = .rswat$stor$temp[[fname]]$linedf$string
  s.n = length(s)
  
  # quit if the file is empty
  if( s.n == 0 ) return( invisible() )
  
  # handle booleans
  if(!yn)
  {
    # turn off boolean detection
    is.bool = rep(FALSE, s.n)
    
  } else {
    
    # translate 'y'/'n' as TRUE/FALSE
    is.bool = s %in% c('n', 'y') 
    s[s=='y'] = 'TRUE'
    s[s=='n'] = 'FALSE'
  }
  
  # replace 'null' with empty character
  s[grepl('null', s, fixed=TRUE)] = ''
  
  # detect numeric (using R's built-in interpreter) and integer, the rest is treated as character
  is.num = !is.na(suppressWarnings(as.numeric(s))) & !is.bool
  is.int = is.num & !grepl('.', s, fixed=TRUE)
  is.num = is.num & !is.int
  is.char = !is.int & !is.num & !is.bool 
  
  # vector of classes for the input
  classes = c('logical', 'character', 'numeric', 'integer')
  s.class = classes[apply(cbind(is.bool, is.char, is.num, is.int), 1, which)]
  
  # look in `linedf` for classes for NA fields (else, R interprets NA as character)
  if(!is.null(.rswat$stor$temp[[fname]]$linedf$class))
  {
    na.class = .rswat$stor$temp[[fname]]$linedf$class[is.na(s)]
    s.class[is.na(s)][!is.na(na.class)] = na.class[!is.na(na.class)]
  }

  # coerce the appropriate type, writing directly to storage
  .rswat$stor$temp[[fname]]$values = lapply(1:s.n, function(x) as(s[x], s.class[x]) )
  
  # append class column to linedf
  .rswat$stor$temp[[fname]]$linedf$class = s.class
  
}

#' read and interpret a SWAT+ configuration file
rswat_rfile = function(fname, reload=TRUE, yn=FALSE)
{
  # ARGUMENTS:
  #
  # `fname`: (character) the name of the SWAT+ config file
  # `reload`: (logical), indicating to reload and parse the text of the file 
  # `yn`: logical, indicates to convert 'y'/'n' to TRUE/FALSE
  #
  # RETURN VALUE:
  #
  # Returns nothing, but loads into memory and parses the file `fname`, writing tabular data
  # to `.rswat$stor$data[[fname]]` and adding the corresponding metadata to `.rswat$stor$linedf`
  # 
  # DETAILS:
  #
  # This function parses data tables by identifying lines with all character type fields as
  # headers, then scanning the lines below for a consistent class/length structure. It does
  # this iteratively until the end of the file, writing what it finds to package storage as a
  # list of dataframes (`.rswat$stor$data[[fname]]`).
  #
  # Entries of these dataframes are mapped to segments of the source text file in the output
  # dataframe `linedf`, with one row per data field. The columns 'file', 'line_num', 'class',
  # 'field_num', 'start_col', 'end_col', and 'nprec' indicate how to read/write fields, and
  # the columns 'table', 'i', 'j' are indices for finding them in `values`.
  #
  # Note that the `yn` flag should be used with care, as it causes problems with files containing
  # the string 'n' as a description or name field (eg cal_parms.cal).
  # 
    
  # full path the file
  txtpath = rswat_cio(trim=FALSE) %>% filter(file==fname) %>% pull(path)
  
  # reload if requested and/or the data aren't loaded 
  txt.loaded = !is.null(.rswat$stor$txt[[fname]])
  linedf.loaded = !is.null(.rswat$stor$temp[[fname]]$linedf)
  values.loaded = !is.null(.rswat$stor$temp[[fname]]$values)
  if( reload | !txt.loaded | !linedf.loaded | !values.loaded )
  {
    # load and parse the text, add comment to package storage
    rswat_rlines(.rswat$cio$path[.rswat$cio$file == fname] )
    .rswat$cio$msg[.rswat$cio$file == fname] = .rswat$stor$txt[[fname]][[1]]
    
    # detect class and coerce all fields, then make storage in package environment for the output
    rswat_rtext(fname, yn=yn)
    .rswat$stor$data[[fname]] = list()
    
    # wipe any existing entries for the file(s) from the `linedf` dataframe in memory
    idx.existing = .rswat$stor$linedf$file %in% fname
    if(any(idx.existing)) .rswat$stor$linedf = .rswat$stor$linedf[!idx.existing,]
  }

  # table number for recursive calls
  table_num = length(.rswat$stor$data[[fname]]) + 1
  
  # initial scan to detect table headers and rows among the lines
  rswat_tfind(fname)

  # this modifies `.rswat$stor$temp[[fname]]$linedf`. pull results
  skipped = .rswat$stor$temp[[fname]]$linedf$skipped
  header = .rswat$stor$temp[[fname]]$linedf$header
  tabular = .rswat$stor$temp[[fname]]$linedf$tabular
  
  # most of the simple 1-table files are dealt with in this branch
  if( all(skipped | tabular | header) )
  {
    # construct the table values and metadata and append them to storage in memory
    tmake.result = rswat_tmake(fname)
    linedf.out = tmake.result$linedf %>% mutate(table = table_num, file = fname)
    .rswat$stor$linedf = rbind(.rswat$stor$linedf, linedf.out)
    .rswat$stor$data[[fname]] = c(.rswat$stor$data[[fname]], list(tmake.result$values))
    
    # tidy and quit
    .rswat$stor$temp[[fname]] = list()
    return(invisible())
  }
  
  # if everything that remains is skipped (can't be interpreted), we are done
  if( all(skipped) )
  {
    # merge the skipped lines into storage, tidy and quit
    .rswat$stor$linedf = rbind(.rswat$stor$linedf, .rswat$stor$temp[[fname]]$linedf)
    .rswat$stor$temp[[fname]] = list()
    return(invisible())
  }
  
  # There is at least one non-conforming row - find the line numbers of problematic rows
  ln.all = .rswat$stor$temp[[fname]]$linedf$line_num
  ln.unique = unique(ln.all)
  ln.bad = sort(unique(ln.all[which(!tabular & !header & !skipped)]))
  
  # find the next header line (or, if there aren't any more, find nrow)
  class.byln = split(.rswat$stor$temp[[fname]]$linedf$class, ln.all)
  head.byln = sapply(class.byln, function(x) all(x=='character'))
  ln.head = ln.unique[head.byln]
  ln.nexthead = ifelse(length(ln.head) < 2, max(ln.all)+1, ln.head[2])
  
  # increment next-header line in case of contiguous all-character lines
  if( ln.nexthead == ln.head[1] + 1 )
  {
    # at least one row should lie between headers
    ln.nexthead = ifelse(length(ln.head) < 3, max(ln.all)+1, ln.head[3])
  }
  
  # attempt to repair any bad lines before the next header
  if(any(ln.bad < ln.nexthead)) 
  {
    # define lines to repair ('long' -> missing header, so check all rows in that case)
    ln.tofix = ln.unique[ (ln.unique > ln.head[1]) & (ln.unique < ln.nexthead) ]
    long.all = .rswat$stor$temp[[fname]]$linedf$long
    if( !any(long.all[ ln.all %in% ln.tofix ]) ) ln.tofix = ln.bad[ln.bad < ln.nexthead]
    
    # skip if no lines to repair
    if(length(ln.tofix) > 0)
    {
      # fill missing values by creating new headers and/or fields in temp `linedf` and `values`
      rswat_tfix(fname, ln=ln.tofix, ref=ln.head[1], yn=yn)
      
      # scan a second time, but include only rows before the next header
      rswat_tfind(fname, idx = .rswat$stor$temp[[fname]]$linedf$line_num < ln.nexthead)
      
    }
  }
  
  # extract current table before sending what remains to recursive call
  tmake.result = rswat_tmake(fname)
  linedf.out = tmake.result$linedf %>%
    filter(line_num < ln.nexthead) %>%
    mutate(skipped = (line_num < ln.nexthead) & !header & !tabular) %>%
    mutate(table = table_num, file = fname)
  
  # store in memory
  .rswat$stor$linedf = rbind(.rswat$stor$linedf, linedf.out)
  .rswat$stor$data[[fname]] =  c(.rswat$stor$data[[fname]], list(tmake.result$values))
  
  # unprocessed lines get passed to the next function call
  ln.remains = unique(tmake.result$linedf$line_num[! tmake.result$linedf$line_num < ln.nexthead ])

  # skip if there aren't any unprocessed lines left
  if(length(ln.remains) > 0)
  {
    idx.temp.remains = .rswat$stor$temp[[fname]]$linedf$line_num %in% ln.remains
    .rswat$stor$temp[[fname]]$linedf = .rswat$stor$temp[[fname]]$linedf[idx.temp.remains,]
    .rswat$stor$temp[[fname]]$values = .rswat$stor$temp[[fname]]$values[idx.temp.remains]
    
    # recursive call
    rswat_rfile(fname, reload=FALSE, yn=yn)
  }
  
  # tidy and quit
  .rswat$stor$temp[[fname]] = list()
  return(invisible())
}

#' make a table from a parsed SWAT+ file text 
rswat_tmake = function(fname, idx=NULL)
{
  # ARGUMENTS:
  #
  # `fname`: (character) the name of the SWAT+ config file
  # `idx`: integer vector, indexing a subset of the lists in `.rswat$stor$temp`
  #
  # RETURN VALUE:
  #
  # A list containing:
  #
  #   'values', the dataframe of SWAT+ parameters
  #   'linedf', dataframe describing the fields in `values`
  #
  # DETAILS:
  #
  # The function expects `.rswat$stor$temp[[fname]]` to be a list containing the
  # data in `values` (list) and a matching metadata table in `linedf` (dataframe),
  # eg as generated by doing `rswat_rlines` > `rswat_rtext` > `rswat_tfind`
  
  # initialize output
  outlist = list(linedf=data.frame(), values=list())
  
  # handle default index (all rows)
  n.lines = nrow(.rswat$stor$temp[[fname]]$linedf)
  if(is.null(idx)) idx = 1:n.lines
  
  # pull some important tags
  skipped = .rswat$stor$temp[[fname]]$linedf$skipped[idx]
  tabular = .rswat$stor$temp[[fname]]$linedf$tabular[idx]
  header = .rswat$stor$temp[[fname]]$linedf$header[idx]
  
  # modify tabular to include only the first contiguous block of matches
  idx.tail = which(!( tabular | header | skipped ))
  idx.tail = ifelse(length(idx.tail)==0, length(tabular) + 1, idx.tail[1])
  tabular = tabular & ( (1:length(tabular)) < idx.tail )
  
  # copy `linedf` subsets for header and all unparsed lines (tagging as skipped, non-tabular)
  linedf.header = .rswat$stor$temp[[fname]]$linedf[idx,][header, ]
  linedf.remains = .rswat$stor$temp[[fname]]$linedf[idx,][!header & !tabular,] %>%
    mutate(skipped = TRUE, tabular = FALSE)

  # add properties of tabular elements of `linedf`, then bind the rest and copy to output list
  outlist$linedf = .rswat$stor$temp[[fname]]$linedf[idx,][tabular,] %>% 
    cbind(rswat_align(.)) %>% 
    mutate(name = linedf.header$string[field_num]) %>%
    mutate(dim = length(unique(line_num))) %>% 
    mutate(i = match(line_num, unique(line_num))) %>%
    mutate(j = field_num) %>%
    bind_rows(linedf.remains) %>%
    bind_rows(linedf.header)
  
  # reshape `values` as dataframe and write to output list
  nm.tab = setNames(nm = linedf.header$string)
  outlist$values = data.frame(lapply(nm.tab, function(nm) {
    do.call(c, .rswat$stor$temp[[fname]]$values[idx][tabular][outlist$linedf$name==nm]) }))
  
  # tidy rownames, handling 0-row case, then finish
  if(nrow(outlist$values) != 0) rownames(outlist$values) = 1:nrow(outlist$values)
  rownames(outlist$linedf) = 1:nrow(outlist$linedf)
  return(outlist)
}

#' identify the first table with column names in a block of SWAT+ config text
rswat_tfind = function(fname, idx=NULL)
{
  # ARGUMENTS:
  #
  # `fname`: character, the name of the file entry in `.rswat$stor$temp`
  # `idx`: integer vector, indexing a subset of the lists in `.rswat$stor$temp`
  #
  # RETURN:
  #
  # Returns nothing, but appends to `.rswat$stor$temp[[fname]]$linedf` the columns:
  #
  #   'skipped', TRUE iff the field is on a row before the header
  #   'header', TRUE iff field is part of the header for the table
  #   'tabular', TRUE iff field is part of a data row in this chunk
  #   'short', TRUE iff the field is part of a row with fewer fields than the header
  #   'long', TRUE iff the field is part of a row with more fields than the header
  #   'ljust', TRUE iff the field is left-aligned with the header
  #   'rjust', TRUE iff the field is right-aligned with the header  
  #
  # Rows not indexed by `idx` are assigned FALSE for all of these attributes
  #
  # DETAILS:
  #
  # This identifies the first table in the input block of text by assuming the first
  # all-character type line is a header. 
  #
  # Subsequent rows are then labelled `tabular=TRUE` if they satisfy some criteria:
  #
  # This function ignores unlabeled columns in the headers line
  #
  ##
  
  # handle default index (all rows)
  n.lines = nrow(.rswat$stor$temp[[fname]]$linedf)
  if(is.null(idx)) idx = 1:n.lines
  
  # initialize output columns in `linedf` as needed
  nm = c('skipped', 'header', 'tabular', 'short', 'long', 'ljust', 'rjust')
  nm.exists = nm %in% names(.rswat$stor$temp[[fname]]$linedf)
  if( any(!nm.exists) )
  {
    # initialize missing columns to empty booleans
    add.df = setNames(data.frame(matrix(logical(0), n.lines, sum(!nm.exists))), nm[!nm.exists])
    
    # add to storage
    .rswat$stor$temp[[fname]]$linedf = cbind(.rswat$stor$temp[[fname]]$linedf, add.df)
  }
  
  # grab a copy of the subset to be populated and initialize
  out.df = .rswat$stor$temp[[fname]]$linedf[idx, nm]
  out.df[, c('short', 'long', 'ljust', 'rjust')] = NA
  out.df[, c('skipped', 'header', 'tabular')] = FALSE
  
  # pull relevant subsets from linedf as vectors the length of `idx`
  line_num = .rswat$stor$temp[[fname]]$linedf$line_num[idx]
  field_num = .rswat$stor$temp[[fname]]$linedf$field_num[idx]
  start_pos = .rswat$stor$temp[[fname]]$linedf$start_pos[idx]
  end_pos = .rswat$stor$temp[[fname]]$linedf$end_pos[idx]
  classes = .rswat$stor$temp[[fname]]$linedf$class[idx]
  
  # split classes and start/end position by line number, find number of fields per line
  ln.unique = unique(line_num)
  class.byln = split(classes, line_num)
  n.byln = lapply(class.byln, length)
  
  # identify header candidates as all character-type lines
  header.byln = sapply(class.byln, function(x) all(x=='character'))
  
  # finished with no-header case
  if(!any(header.byln))
  {
    # write the new columns to storage and finish
    .rswat$stor$temp[[fname]]$linedf[idx, nm] = out.df
    return(invisible())
  }
  
  # find line number of first header, count its fields, find their index in output dataframe
  ln.head = ln.unique[header.byln][1]
  n.head = sum(line_num %in% ln.head)
  out.df$header = line_num %in% ln.head

  # tag rows before header as skipped, find max number of rows
  out.df$skipped = line_num < ln.head
  row.n = sum(ln.unique > ln.head)

  # finished with 0-row case
  if(row.n == 0)
  {
    # write the new columns to storage and finish
    .rswat$stor$temp[[fname]]$linedf[idx, nm] = out.df
    return(invisible()) 
  }

  # compare row lengths with header line to determine tabular candidates
  out.df$short = unlist(lapply(n.byln, function(n) rep(n < n.head, n)))
  out.df$long = unlist(lapply(n.byln, function(n) rep(n > n.head, n)))
  out.df$tabular = line_num > ln.head & !out.df$short & !out.df$long & !out.df$header
  
  # check for matching start/end positions, this time comparing with header
  tab.start.mat = matrix(start_pos[out.df$tabular|out.df$header], n.head)
  tab.end.mat = matrix(end_pos[out.df$tabular|out.df$header], n.head)
  tab.ljust = as.vector(tab.start.mat[,-1] - tab.start.mat[,1] == 0)
  tab.rjust = as.vector(tab.end.mat[,-1] - tab.end.mat[,1] == 0)
  
  # update justification indicators in output for tabular fields
  out.df$ljust[out.df$tabular] = tab.ljust
  out.df$rjust[out.df$tabular] = tab.rjust

  # finished with 1-row case and <2 tabular row case
  if(row.n == 1 | length(unique(line_num[out.df$tabular])) < 2)
  {
    # write the new columns to storage and finish
    .rswat$stor$temp[[fname]]$linedf[idx, nm] = out.df
    return(invisible()) 
  }

  # among the matching-length rows, check for class matches with first row
  tab.class.mat = matrix(classes[out.df$tabular], n.head)
  tab.class.match.ln = apply(tab.class.mat, 2, function(x) identical(x, tab.class.mat[,1]))
  tab.class.match = rep(tab.class.match.ln, each=n.head)
  
  # update the tabular index, write new columns to storage, finish
  out.df$tabular[out.df$tabular] = out.df$tabular[out.df$tabular] & tab.class.match
  .rswat$stor$temp[[fname]]$linedf[idx, nm] = out.df
  return(invisible())

}

#' generate missing rows in linedf for incomplete lines in a SWAT+ table
rswat_tfix = function(fname, ln=integer(0), ref=integer(0), yn=FALSE)
{
  # ARGUMENTS:
  #
  # `fname`: character, the name of the file entry in `.rswat$stor$temp` to modify
  # `ln`: integer vector, a subset of line numbers in `.rswat$stor$temp[[fname]]$linedf`
  # `ref`: integer, the line number of the reference line, probably the header
  # `yn`: logical, indicating to interpret 'y'/'n' as boolean
  #
  # RETURN:
  #
  # Returns nothing but (possibly) modifies the temporary `.rswat$stor$temp[[fname]]$linedf`
  # dataframe, by adding new rows for any missing header entries (with line_num=`ref`), and
  # adding dummy data rows for any missing fields (among the line numbers `ln`). For every 
  # row added, a corresponding NA value is added to `.rswat$stor$temp[[fname]]$values` 
  #
  # DETAILS:
  #
  # The function attempts to repair the line metadata to account for missing fields which
  # were interpreted as whitespace by the file reader. 'rswat_tfind` must be run first to
  # classify row types.
  #


  # grab a copy of the line numbers of all entries and an index of the subset requested
  n.old = nrow(.rswat$stor$temp[[fname]]$linedf)
  ln.all = .rswat$stor$temp[[fname]]$linedf$line_num
  idx.ln = ln.all %in% ln
  
  # set defaults: reference is first header, fix anything afterwards that is 'short'
  if(length(ref) == 0) ref = unique(ln.all[.rswat$stor$temp[[fname]]$linedf$header])[1]
  if(length(ln) == 0) ln = unique(ln.all[.rswat$stor$temp[[fname]]$linedf$short & (ln.all>ref)])

  # construct missing headers in memory, if necessary
  idx.long = .rswat$stor$temp[[fname]]$linedf$long[idx.ln]
  if(any(idx.long[!is.na(idx.long)]))
  {
    # pick the longest 'long' example to copy spacing data from
    ln.long = unique(ln.all[idx.ln][ idx.long[!is.na(idx.long)] ])
    ln.example = ln.long[which.max(sapply(ln.long, function(n) sum(ln.all == n)))]
    
    # construct new header entries for the missing column names
    n.missing = sum(ln.all==ln.example) - sum(ln.all==ref)
    name.newheader = paste0('unknown', 1:n.missing)
    linedf.newheader = .rswat$stor$temp[[fname]]$linedf[ln.all==ln.example,] %>% 
      filter(! field_num %in% 1:sum(ln.all==ref) ) %>%
      mutate(string = name.newheader) %>%
      mutate(class = 'character') %>%
      mutate(line_num = ref) %>%
      mutate(header = TRUE, tabular = FALSE, skipped=FALSE, long = FALSE) %>%
      mutate(ljust = NA, rjust = NA)
    
    # write to memory and update line numbers and index vectors
    .rswat$stor$temp[[fname]]$linedf = rbind(.rswat$stor$temp[[fname]]$linedf, linedf.newheader)
    ln.all = .rswat$stor$temp[[fname]]$linedf$line_num
    idx.ln = ln.all %in% ln
  }
  
  # make a copy of the reference line information to use as a template
  linedf.ref = .rswat$stor$temp[[fname]]$linedf[ln.all==ref, ] %>% 
    mutate(string=NA, class=NA, line_num=NA) %>%
    arrange(field_num)
  
  # for all requested lines, find matching start/end positions, set justification flags
  start.match = match(.rswat$stor$temp[[fname]]$linedf$start_pos[idx.ln], linedf.ref$start_pos)
  end.match = match(.rswat$stor$temp[[fname]]$linedf$end_pos[idx.ln], linedf.ref$end_pos)
  ljust = !is.na(start.match)
  rjust = !is.na(end.match)
  
  # make the vector of new field numbers, and an index of `ln` to modify
  field.match = end.match
  field.match[is.na(field.match)] = start.match[is.na(field.match)]
  idx.mod = !is.na(field.match)
  ln.mod = unique(ln.all[idx.ln][idx.mod])
  
  # tag elements not modified because of a mismatch in start and end positions
  ln.skip = unique(ln.all[idx.ln][!idx.mod])
  .rswat$stor$temp[[fname]]$linedf$skipped[ln.all %in% ln.skip] = TRUE
  
  # build list of elements to add to memory (if any)
  if( length(ln.mod) > 0 )
  {
    # overwrite `field_num', 'ljust', 'rjust', 'tabular' in memory
    .rswat$stor$temp[[fname]]$linedf$field_num[idx.ln][idx.mod] = field.match[idx.mod]
    .rswat$stor$temp[[fname]]$linedf$ljust[idx.ln][idx.mod] = ljust[idx.mod]
    .rswat$stor$temp[[fname]]$linedf$rjust[idx.ln][idx.mod] = rjust[idx.mod]
    
    # merge a list of dataframes, each a modified copy of `linedf.ref` containing missing fields
    field_num = .rswat$stor$temp[[fname]]$linedf$field_num
    linedf.mod = do.call(rbind, lapply(ln.mod, function(n) 
        {
          # this function called once per modified line:
          field.omit = field_num[ln.all == n]
          linedf.out = linedf.ref[-field.omit,]
          if( nrow(linedf.out) > 0 )
          {
            linedf.out$line_num = n
            linedf.out$header = FALSE
            linedf.out$class = 'character'
            linedf.out$ljust = n==1
            linedf.out$rjust = !linedf.out$ljust
          }
          return(linedf.out)
        }
      )
    )
    
    # skip if there were no changes
    if( nrow(linedf.mod) > 0 )
    {
      # merge with the main list in memory and update vector of line numbers, index of headers  
      .rswat$stor$temp[[fname]]$linedf = rbind(.rswat$stor$temp[[fname]]$linedf, linedf.mod)
      idx.header = .rswat$stor$temp[[fname]]$linedf$header
      
      # loop over columns, imputing class, justification, start/end position for the new fields
      example.nm = names(linedf.mod)[ ! names(linedf.mod) %in% c('string', 'line_num') ]
      for(field.mod in unique(linedf.mod$field_num))
      {
        # find all non-NA, non header entries for the field (if any)
        idx.field = !idx.header & ( .rswat$stor$temp[[fname]]$linedf$field_num == field.mod )
        idx.na = is.na(.rswat$stor$temp[[fname]]$linedf$string[idx.field])
        idx.example = which(idx.field)[!idx.na]
        
        # skip if there are no such examples
        if(length(idx.example) > 0)
        {
          # copy metadata from the first example of this field elsewhere in the table
          example.linedf = .rswat$stor$temp[[fname]]$linedf[idx.example[1], example.nm]
          repl.linedf = as.data.frame(lapply(example.linedf, rep, sum(idx.na)))
          .rswat$stor$temp[[fname]]$linedf[which(idx.field)[idx.na], example.nm] = repl.linedf
        }
      }
    }
    
  }
  
  # skip when no new elements (header or data field) were added
  n.new = nrow(.rswat$stor$temp[[fname]]$linedf)
  if(n.new > n.old)
  {
    # build index of new values in temp `linedf` (these should all lie at the end!) 
    idx.add = ! (1:n.new) %in% (1:n.old)
    
    # build NAs in correct class
    NA.add = lapply(.rswat$stor$temp[[fname]]$linedf$class[idx.add], function(x) as(NA,x))
    
    # build index to restore ordering by line then field number
    line_num = .rswat$stor$temp[[fname]]$linedf$line_num
    field_num = .rswat$stor$temp[[fname]]$linedf$field_num
    idx.ord = order(line_num, field_num)
    .rswat$stor$temp[[fname]]$linedf = .rswat$stor$temp[[fname]]$linedf[idx.ord,]
    
    # add new (NA) entries to values list (same as calling `rswat_rtext(fname)`, but faster)
    .rswat$stor$temp[[fname]]$values = c(.rswat$stor$temp[[fname]]$values, NA.add)[idx.ord]
  }

}

#' clean up positional data for a table in `linedf`
rswat_align = function(linedf)
{
  # ARGUMENTS:
  #
  # `linedf`: dataframe, the subset of .rswat$stor$linedf to process
  #
  # RETURN:
  #
  # dataframe with rows matching `linedf`, and columns
  #   'rjust_col', logicalm indicating if the column is right-justified
  #   'start_col', integer, the minimum character position for the field
  #   'end_col', integer, the maximum character position for the field
  #   'nprec', integer or NA, the number of significant digits (for numeric)
  #
  # DETAILS:
  #
  # Assigns field breakpoints and justification, setting sensible defaults when
  # they can't be determined from the existing file. This is an internal function
  # meant to be called from `rswat_tmake`
  #

  # catch calls for function/table values that don't exist
  linedf.n = nrow(linedf)
  if(linedf.n == 0)
  {
    # empty dataframe serves only to set column names and types
    return( data.frame(rjust_col=logical(0), 
                       start_col=integer(0), 
                       end_col=integer(0), 
                       nprec=integer(0)) ) 
    
  }

  # pull field numbers and their names 
  j = linedf$field_num
  j.unique = unique(j)
  j.name = unique(linedf$name)
  
  # count significant digits in numeric class columns
  nprec = rep(NA, linedf.n)
  idx.num = linedf$class=='numeric'
  if( any(idx.num) )
  {
    # count digits after the dot, write to nprec
    sig.matches = gregexpr('\\.[0-9]+', linedf$string[idx.num], perl=T)
    nprec[idx.num] = sapply(sig.matches, function(x) attr(x, 'match.length')) - 1
  }
  
  # aggregate properties by field:
  
  # assign default justifications (left for 'name' columns, first column, otherwise right)
  rjust.def = rep(TRUE, length(j.unique))
  rjust.def[ j.unique == 1 | grepl('[Nn]ame', j.name) ] = FALSE
  
  # pull existing name, start_pos, end_pos, rjust, ljust
  start_pos.byj = split(linedf$start_pos, j)
  end_pos.byj = split(linedf$end_pos, j)
  ljust.byj = split(linedf$ljust, j)
  rjust.byj = split(linedf$rjust, j)
  
  # check for unanimous left and/or right justification tags
  ljust.all = sapply(ljust.byj, all)
  rjust.all = sapply(rjust.byj, all)
  
  # categorize fields by how certain we are of their justification
  ljust = ljust.all & !rjust.all
  rjust = rjust.all & !ljust.all
  other = (!ljust.all & !rjust.all) | (ljust.all & rjust.all) 
  other[is.na(other)] = TRUE
  
  # compute extrema of start/end positions by field
  start_min = sapply(start_pos.byj, min)
  end_max = sapply(end_pos.byj, max)
  
  # deal with `other` cases (undecidable, or at least one row with inconsistent justification)
  if(any(other))
  {
    # loop over problematic columns
    for(j.other in which(other))
    {
      # each left justification adds +1, each right justification adds -1
      just.score = sum(ljust.byj[[j.other]], na.rm=TRUE) - sum(rjust.byj[[j.other]], na.rm=TRUE)
      
      # score determines column justification (reverting to default in case of tie)
      ljust[j.other] = ifelse(just.score == 0, !rjust.def[j.other], just.score > 1)
      rjust[j.other] = ifelse(just.score == 0, rjust.def[j.other], just.score < 1)
      
      # right justified case
      if( rjust[j.other] & any(rjust.byj[[j.other]]) )
      {
        # recompute maximum using other rows (if possible)
        end_max[j.other] = max( end_pos.byj[[j.other]][ rjust.byj[[j.other]] ] )
      }
      
      # left justified case
      if( ljust[j.other] )
      {
        # case: there are other good rows for this field
        if( any(ljust.byj[[j.other]]) )
        {
          # recompute minimum start pos using other rows
          start_min[j.other] = min( start_pos.byj[[j.other]][ ljust.byj[[j.other]] ] )
          
        } else if(j.unique[j.other] == 1) {
          
          # left-justified first column with no additional examples: assume start pos 1
          start_min[j.other] = 1
        }
        
      }
    }
  }
  
  # with justifications decided, initialize start/end positions by field
  start_pos = rep(NA, length(j.unique))
  end_pos = rep(NA, length(j.unique))
  
  # Some of these are fixed already
  start_pos[ljust] = start_min[ljust]
  end_pos[rjust] = end_max[rjust]
  
  # if row 1 is right-justified, assume start_pos of 1
  if( is.na(start_pos[1]) ) start_pos[1] = 1
  
  # 'L... ...R' patterns require guessing a breakpoint in between the two extrema
  break_pos = end_max + c(round( ( start_min[-1] - end_max[-length(end_max)] ) / 2 ), 0)
  end_pos[is.na(end_pos)] = break_pos[is.na(end_pos)]
  
  # the rest can be found via previous field's endpoint (+1 charwidth of whitespace)
  start_pos[ is.na(start_pos) ] = ( end_pos[ which(is.na(start_pos)) - 1 ] + 1 ) + 1

  # vectorize field-wise results to ordering of linedf and finish
  return(  data.frame(rjust_col = unsplit(rjust, j),
                      start_col = unsplit(start_pos, j),
                      end_col = unsplit(end_pos, j),
                      nprec = nprec) )
  
}

#' identify the filename and table number associated with a dataframe
rswat_index = function(value, fname=NULL, tablenum=NULL)
{
  # ARGUMENTS:
  #
  # `value`: dataframe (or character vector), the SWAT+ table (or its column names)
  # `fname`: (optional) character, name of file containing the table
  # `tablenum`: (optional) integer, list index for the table within the file
  #
  # RETURN VALUE:
  #
  # A list containing entries `fname` and `tablenum`
  #
  # DETAILS:
  #
  # This identifies a single table among the SWAT+ files containing the supplied
  # headers (or dies trying). The `value` argument should be one of dataframes returned
  # by `rswat_open` with column names unchanged (rows can be different).
  #
  # Situations of multiple matches are resolved, if possible, using the input arguments
  # `fname` and/or `tablenum`. Any unresolved or invalid input should halt the function
  # but produce an informative error message.
  
  # convert dataframe input
  if(is.data.frame(value)) { nm.head = names(value) } else { nm.head = value }
  
  # stop on invalid `nm.head` input 
  if(is.null(nm.head) | !is.character(nm.head) ) stop('unrecognized input value')
  
  # default behaviour is to match any fname and tablenum
  cio = .rswat$cio %>% filter(!is.na(ntab))
  if( is.null(fname) ) fname = cio$file
  if( is.null(tablenum) ) tablenum = 1:max(cio$ntab)
  idx.data.exclude = !( names(.rswat$stor$data) %in% fname )
  
  # initial filter for matching number of headers
  data.dim = lapply(.rswat$stor$data[!idx.data.exclude], function(fn) sapply(fn, ncol))
  fname.trim = names(which(sapply(data.dim, function(fn) length(nm.head) %in% fn)))
  
  # pull the lines metadata and look for matches in header names
  idx.trim = (.rswat$stor$linedf$file %in% fname.trim) & (.rswat$stor$linedf$table %in% tablenum)
  linedf.trim = .rswat$stor$linedf[idx.trim,] %>% 
    filter(i==1) %>% 
    group_by(file, table) %>%
    mutate(match = all(nm.head %in% name), .groups='drop_last') %>%
    filter(match) %>%
    select(file, table, string, name) %>% as.data.frame
  fname.match = unique(linedf.trim$file)

  # some strings for error messages
  vnames.msg = paste0('[', paste(nm.head, collapse=', '), ']')
  fname.msg = paste0('(', paste(fname, collapse=', '), ').')
  match.msg = paste0('', paste(fname.match, collapse=', '), '')
  n.match = length(fname.match)

  # stop if `fname` is supplied as an argument, but isn't among the matches
  if( !identical(fname, cio$file) & !any(fname %in% fname.match) )
  {
    err.msg = paste('input names', vnames.msg, 'were not found in', fname.msg)
    info.msg = paste0('(try ', match.msg, ')')
    stop( paste(err.msg, ifelse(n.match > 0, info.msg, '')) )
  } 
  
  # catch no-match case
  if( n.match == 0 )
  {
    err.msg = paste('input table', vnames.msg, 'not recognized in any of the loaded files.')
    stop(paste(err.msg, 'Are you sure the file has been loaded in this session?'))
  }
    
  # catch multiple matches of same length in different files
  if( n.match > 1 )
  {
    err.msg = paste('input table', vnames.msg, 'matched multiple files:', match.msg) 
    stop(paste(err.msg, 'Try specifying `fname`'))
  }

  # find the table number that matches the input variables
  fname.tablenum = unique(linedf.trim$table)
  table.msg = paste(fname.tablenum, collapse=', ')
  
  # stop if the supplied table number doesn't match the supplied variable names
  if( !identical(tablenum, 1:max(cio$ntab)) & !any(tablenum %in% fname.tablenum) )
  {
    err.msg = paste('Input names', vnames.msg, 'are only found in table', table.msg)
    stop(paste0(err.msg, ' (user specified table ', tablenum, ').'))
  }
  
  # catch multiple table matches in a file
  if( length(fname.tablenum) > 1 )
  {
    err.msg = paste0('Input names', vnames.msg, ' matched multiple tables (', table.msg)
    stop(paste0(err.msg, '). Try specifying tablenum'))
  }
  
  # the filename, table number, and headers are consistent, so we're done 
  return(list(fname=fname.match, tablenum=fname.tablenum))
  
}

#' convert a dataframe of SWAT+ parameters to their character representation
rswat_2char = function(value, linedf, quiet=FALSE)
{

  # ARGUMENTS:
  #
  # `value`: list containing the new SWAT+ parameter values to write
  # `linedf`: dataframe, the subset of .rswat$stor$linedf corresponding to `value`
  #
  # RETURN:
  #
  # list of two character vectors:
  #   'string': character representation for the input `values`
  #   'string_padded': padded version of 'string', respecting width and justification
  #
  # DETAILS:
  #
  # The function assumes that `value` and `linedf` are provided in matching order. 
  #
  
  # TODO: catch errors of too-big integers
  # TODO: warn of type conversions 
  
  # pull classes
  is.bool = linedf$class == 'logical' 
  is.int = linedf$class == 'integer'
  is.num = linedf$class == 'numeric'
  is.char = !(is.bool | is.int | is.num)
  
  # initialize output strings
  vstr = rep(NA, length(value))
  
  # write character, integer, fixed precision numeric, convert boolean to 'y'/'n'
  if(any(is.char)) vstr[is.char] = sapply(value[is.char], as.character)
  if(any(is.bool)) vstr[is.bool] = c('n', 'y')[ sapply(value[is.bool], as.integer) + 1 ]
  if(any(is.int)) vstr[is.int] = as.character(sapply(value[is.int], as.integer))
  if(any(is.num)) vstr[is.num] = mapply(function(x,y) formatC(x, y, format='f'), 
                                        x = value[is.num], y = linedf$nprec[is.num])
  
  # truncate as needed to respect fixed width
  len.max = linedf$end_col - linedf$start_col + 1
  vlen = nchar(vstr)
  idx.trunc = vlen > len.max
  if(any(idx.trunc))
  {
    if(!quiet) warning(paste(sum(idx.trunc), 'fields(s) had to be truncated'))
    newstart = vlen[idx.trunc] - len.max[idx.trunc] 
    vstr[idx.trunc] = substr(vstr[idx.trunc], newstart, vlen[idx.trunc]) 
  }
  
  # make a copy of output string with whitespace padding
  pad.n = sapply(len.max - vlen, function(n) max(n,0) )
  pad = sapply(pad.n, function(n) paste0(rep(' ', n), collapse='') )
  vstr.pad = vstr
  vstr.pad[linedf$rjust_col] = mapply(paste0, pad, vstr)[linedf$rjust_col]
  vstr.pad[!linedf$rjust_col] = mapply(paste0, vstr, pad)[!linedf$rjust_col]
  return(list(string=vstr, string_padded=vstr.pad))
  
}


#+ include=FALSE
#my_markdown('rswat_config')
