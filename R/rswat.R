#' ---
#' title: "rswat"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **rswat**: R functions for building, reading, and editing SWAT+ configuration files
#' 
#' I may turn all of this into a package eventually, especially the file I/O interface, which took
#' some work but has proven very useful.
#' 
#' see also: [SWAT+](https://swat.tamu.edu/software/plus/), 
#' [SWAT+ Automatic Workflow (AW)](https://celray.github.io/docs/swatplus_aw/introduction.html), 
#' [SWATplusR](https://github.com/chrisschuerz/SWATplusR)
#' 

#' ## dependencies
#' 

# `dplyr` syntax simplification for complex table operations 
library(dplyr)

# `data.table` faster table loading
library(data.table)


#' ## initialization:
#' We may later add a start-up script to scan for likely paths to the SWAT+ executable,
#' as well as the OSGEO4W root which is needed when setting up the PyQGIS environment
#' in a call to the QSWAT+ workflow wrapper (this requires installation of QGIS-LTR,
#' and the path will be platform dependent!)
#' 

# TODO: make this a package

#' ## config file I/O interface
#' 
#' NOTE: an environment `.rswat` is initialized here (overwriting anything with that
#' name). It stores file data and lookup tables, and only the functions in this chunk
#' should be touching it. Eventually I will make this a package so it's properly hidden
#'
#' The I/O interface functions work by scanning the files listed in 'file.cio', tabulating
#' their white-space delineated fields, then merging the results into tables wherever
#' there is a consistent pattern of row-length and class (as detected by R's built-in
#' `is.numeric` and some sensible rules for snapping and spacing. This will hopefully make
#' it robust to version-specific changes in SWAT+ parameter names and file structures
#' (and whatever changes come with future releases). 

#' (internal) environment to store the SWAT+ project file data
.rswat = new.env(parent=emptyenv())

#' open 'file.cio' and list its contents
rswat_cio = function(ciopath=NULL, trim=TRUE, wipe=FALSE, reload=FALSE, ignore=NULL)
{
  # ARGUMENTS:
  #
  # `ciopath`: (character string) path to the SWAT+ master watershed 'file.cio'
  # `trim`: (logical) indicates to trim output for readability and omit files in `ignore`
  # `wipe`: (logical) indicates to flush all data for the current project from R
  # `reload`: (logical) indicates to import all files into memory
  # `ignore`: (character vector) 'file' or 'group' names to exclude 
  #
  # RETURN VALUE:
  #
  # A dataframe with information on all files listed in `ciopath`
  #
  # DETAILS:
  #
  # When called with default arguments, returns a list of all files that can be imported
  # with calls to `rswat_open`.
  #
  # The function assumes `ciopath` points to a SWAT+ master watershed file (probably
  # 'file.cio'), which is parsed as a dataframe and written to memory. Subsequent calls may
  # omit the `ciopath` argument to get a copy of this dataframe without reloading the file.
  #
  # Any filename or group name matching an element of input argument `ignore` will tagged
  # as such, in order to filter out files that you don't want to have load by default. 
  # Ignored files can be loaded, they just won't be included by default with `rswat_open()`
  #
  # `wipe` is used for clearing memory, or for starting over without ending your R session.
  # It is invoked automatically whenever `rswat_cio` is called with a new `ciopath` location. 
  # If `ciopath` is not supplied and `wipe==TRUE`, the function flushes everything and 
  # returns nothing
  #
  # `reload==TRUE` simply chains the `rswat_cio` call with `rswat_open(reload=TRUE)`, for a
  # quick one-liner to load all of the files in a project. Subsequent calls to `rswat_open`
  # (with default `reload==FALSE`) will be very fast
  #
  
  # handle calls with no `ciopath` argument
  if(is.null(ciopath))
  {
    # check if the function has been run already
    if( 'ciopath' %in% ls(.rswat) )
    {
      # handle wipe calls
      if(wipe)
      {
        rm(list = c('ciopath', 'cio', 'stor'), envir=.rswat, inherits=FALSE)
        print('all project data has been removed from R')
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
    if(basename(ciopath) != 'file.cio')
    {
      # formulate two likely paths assuming that ciopath is a directory
      ciopath.a = file.path(ciopath, 'file.cio')
      ciopath.b = file.path(ciopath, 'Scenarios/Default/TxtInOut/file.cio')
      
      # assign the path or else halt if we can't find 'file.cio'
      ab.exists = file.exists( c(ciopath.a, ciopath.b) )
      if( any(ab.exists) )
      {
        ciopath = c(ciopath.a, ciopath.b)[ which(ab.exists)[1] ]
        cat(paste('setting `ciopath` to', ciopath, '\n'))
        
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
    if(is.initial | wipe)
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
    print(.rswat$stor$txt[[basename(ciopath)]][[1]])
    
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
    if(!is.null(ignore))
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
  if(nrow(.rswat$stor$linedf) > 0)
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
    
    # find number of skipped lines
    .rswat$stor$linedf %>% 
      filter(skipped) %>%
      group_by(file) %>% 
      summarize(.groups='drop_last', nskip=length(unique(line_num))) %>%
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
    rswat_open(reload=TRUE) 
    cio = rswat_cio(trim=trim, wipe=FALSE, ignore=ignore)
  }
  
  # return up-to-date files list
  return(cio)

}

#' wrapper for various methods to load SWAT+ files into memory and/or return them as dataframes
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
  # With default `reload==FALSE`, the function returns a named list containing all the tabular
  # data associated with `fname`. Each list entry is named after a source file, and consists
  # of either a dataframe (if the file has a single table) or a list of dataframes (if the file
  # has multiple tables). Toggling `simplify==FALSE` will wrap the single-table case(s) in a
  # list, for consistency with the other cases.
  # 
  # If `fname` is empty (the default), all non-ignored files are loaded. See `rswat_cio`, and
  # its `ignore` argument.
  #
  # If `reload==TRUE`, the function call returns nothing but loads and parses all requested
  # files into memory, overwriting anything already there. This is useful if you need to reload
  # project data after it's been modified on disk by external programs.
  
  # parse the `fname` argument, assigning all non-ignored files by default
  cio = rswat_cio(trim=FALSE) %>% filter(exists) 
  if(length(fname)==0) fname = cio %>% filter(!ignored) %>% pull(file) 
  cio.match = cio %>% filter( (file %in% fname) | (group %in% fname))
  
  # message if no filenames match input string
  if(nrow(cio.match) == 0) warning(paste('no matches for', paste(fname, collapse=', '))) 
  
  # check what's been loaded into memory already, enter loading loop if required 
  idx.loaded = cio.match$file %in% names(.rswat$stor$data)
  if(reload) idx.loaded = rep(FALSE, nrow(cio.match))
  if(any(!idx.loaded))
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
  if(simplify)
  {
    # collapse redundant within-file lists, then deal with single-file lists
    idx.singles = sapply(values.out, length) == 1
    values.out[idx.singles] = lapply(values.out[idx.singles], function(x) x[[1]])
    
    # collapse redundant 
    if(length(values.out) == 1) values.out = values.out[[1]]
  }
  
  # prepare the requested data in a named list
  if(!reload)
  {
    # pull a copy of the requested dataframes
    values.out = .rswat$stor$data[cio.match$file]
    
    # simplify as needed before returning the list
    if(simplify)
    {
      # collapse redundant within-file lists, then collapse any single-file lists
      idx.singles = sapply(values.out, length) == 1
      values.out[idx.singles] = lapply(values.out[idx.singles], function(x) x[[1]])
      if(length(values.out) == 1) values.out = values.out[[1]]
    }
    return(values.out)
    
  } else {
    
    # reload mode returns nothing
    return(invisible())
  }
}

#' search tool for SWAT+ parameter text, names, and filenames
rswat_find = function(pattern=NULL, fuzzy=-1, intext=FALSE, trim=TRUE, include=NULL, ignore=NULL)
{
  # ARGUMENTS:
  #
  # `pattern`: (optional) character vector, regular expressions or literal string to match 
  # `fuzzy`:  numeric, specifying match mode (see details)
  # `intext`: logical, indicating to search parameter values in addition to names
  # `trim`: logical, whether to omit detailed metadata about character positions
  # `include`: (optional) character vector, filenames to include in searches
  # `ignore`: (optional) character vector, filenames to omit from searches
  #
  # RETURN VALUE:
  #
  # A dataframe of information about SWAT+ names matching the search pattern.
  #
  # DETAILS
  # 
  # Returns metadata on matches of pattern with SWAT+ parameter names, and optionally
  # (via `intext`) the text of the parameter values themselves.
  #
  # If no pattern is supplied, the function returns information on all parameters in the 
  # subset of files determined by `include` and `ignore`. If `include` is supplied, only
  # results from those files are returned (default is all files). Argument `ignore` has
  # the opposite effect, and supercedes anything in `include`.
  #
  # `fuzzy < 0` (the default) is for Perl-style regular expressions (see `base::grepl`),
  # `fuzzy = 0` is for exact matches only, and `fuzzy > 0` includes approximate matches
  # up to the specified (Levenshtein) distance (see `base::agrep`). The case (upper or
  # lower) is ignored in approximate matching.
  # 
  # TODO: write some examples: eg.`pattern` matches an element
  # of the 'name' column of a table, all parameters in that row are returned.
  #
  
  # default empty string for pattern matches everything in perl mode
  if( is.null(pattern) ) pattern = ''
  
  # default exclusion string (single space) matches nothing
  if( is.null(ignore) ) ignore =  ' '
  
  # default is to include all files but `ignore` always takes precedence
  if( is.null(include) ) include = rswat_cio()$file
  include = include[ ! include %in% ignore ]
  
  # find indices of ignored and included files
  idx.include = .rswat$stor$linedf$file %in% include
  idx.ignore = .rswat$stor$linedf$file %in% ignore
  
  # handle premature calls and other invalid uses
  err.msg = 'file metadata not found. Have you run rswat_cio and loaded a file?'
  if( length(idx.ignore) == 0 ) stop(err.msg)
  if( length(idx.include) == 0 ) stop('`include` is empty. Try omitting `ignore`')
  if( all(idx.ignore) ) stop('all known files are listed in `ignore`')

  # extract the column subsets needed for searching
  name.all = .rswat$stor$linedf$name[idx.include]
  n.name = length(name.all)
  #file.all = .rswat$stor$linedf$file[idx.include]
  
  # initialize (optional) in-text match vectors of the right length
  string.match = rep(FALSE, sum(idx.include))
  if(intext) string.all = .rswat$stor$linedf$string[idx.include]
  
  # handle fuzzy matching
  if(fuzzy > 0)
  {
    # `agrep` with default max.distance to fuzzy match
    name.match = agrepl(tolower(pattern), tolower(name.all), max.distance=fuzzy)
    if(intext) string.match = agrepl(tolower(pattern), tolower(string.all), max.distance=fuzzy)
  }

  # handle regexp
  if(fuzzy < 0)
  {
    # `grepl` to find matches
    name.match = grepl(pattern, name.all, perl=TRUE)
    if(intext) string.match = grepl(pattern, string.all, perl=TRUE)
  } 
  
  # handle exact searches
  if(fuzzy == 0)
  {
    # simple matching
    name.match = name.all %in% pattern
    if(intext) string.match = string.all %in% pattern
  }
  
  # extract results
  linedf.out = .rswat$stor$linedf[ which(idx.include)[name.match | string.match], ]
  
  # for human readability
  if(trim)
  {
    # omit skipped lines, header lines, etc
    linedf.out = linedf.out %>% filter( !is.na(i) )
    
    # extra trimming when not searching parameter values
    if(!intext)
    {
      # omit all but fist data row, filter 'name' columns
      linedf.out = linedf.out %>% filter(i == 1) %>% filter(name != 'name')
      
      # multirow columns are represented with NAs in place of row index and string
      linedf.out$i[linedf.out$dim > 1] = NA
      linedf.out$string[linedf.out$dim > 1] = NA
    }
    
    # tidy output and finish
    return( linedf.out %>% arrange(tabular, file, table) %>%
             select(name, string, class, dim, file, table, i, j) )
    
  } else {
    
    # or just return the full table subset
    return(linedf.out)
  }
}

#' write a parameter value to its file
rswat_write = function(value, fname=NULL, tablenum=NULL, preview=TRUE, reload=TRUE, quiet=FALSE)
{
  # In development. Currently only supports writing a single dataframe to (single file).
  #
  # TODO: vectorization of list input (named according to file, or unnamed) 
  #
  # ARGUMENTS:
  #
  # `value`: dataframe (or list?), the SWAT+ table with desired parameter values
  # `fname`: (optional) character, name of file containing the table
  # `tablenum`: (optional) integer, list index for the table within the file
  # `preview`: logical, whether to return dataframe listing the changes, but not write them
  # `reload`: logical, whether to reload the file after writing, to update file data in memory
  # `quiet`: logical, passed to rswat_load if `reload==TRUE` (otherwise ignored)
  #
  # RETURN VALUE:
  #
  # Returns nothing but writes `value` to the file `fname` on disk when `preview=FALSE`.
  # Otherwise returns a dataframe with information on the line-by-line changes that would be
  # made to the config file(s) in non-preview mode.
  #
  
  # grab a list of all filenames and the input object names
  cio = rswat_cio(trim=FALSE)
  nm.head = names(value)
  
  # coerce vector input to dataframe
  if(is.vector(value) & !is.list(value)) value = data.frame(as.list(value))
  
  # handle dataframe (and vector) input
  if(is.data.frame(value))
  {
    # resolve destination for this data
    value.index = rswat_index(nm.head, fname=fname, tablenum=tablenum)
    
    # `rswat_index` should throw an error if either of these is not unique
    fname = value.index$fname
    tablenum = value.index$tablenum
    
  } else {
    
    # check whether names of list input match filenames
    idx.named = nm.head %in% cio$file
    
    # case: input is the list of tables for a file
    if( length(idx.named) == 0 & all(sapply(value, is.data.frame)) )
    {
      # search for each of the tables individually
      fname.res = mapply(function(nm, n) rswat_index(nm, tablenum=n)$fname, 
                         nm = lapply(value, names), 
                         n = 1:length(value)) 
      
      # this should be all the same name
      fname = unique(fname.res)
      if(length(fname) > 1) stop('tables in this list matched multiple files')
      
    }
    
    # case: `value` names are not all filenames  
    if( !all(  ) | is.null(nm.head) )
    {
      
      
      
      
    }
    
    # multiple files for this input `value`
    if(length(nm.head) > 1)
    {
      print('multiple files case, in development')
      # recursive call for each file
    }
    
    # we have a valid filename and table number for the data, so recursive call to finish
    #return(rswat_write(tablenum, fname=, tablenum=value.tablenum, preview=preview))

    
    # beyond this point, should have valid assignments to these variables:
    # fname = 
    # tablenum = 
    # ... for all list entries. These can be merged by file and then sent of to a 
    # recursive call 
    return()
    
  }
  
  # grab a copy of the relevant lines descriptions for this file 
  linedf = rswat_find(include=fname, intext=TRUE, trim=FALSE) %>% 
    filter(name %in% nm.head) %>% 
    filter(table == tablenum) %>% 
    arrange(j,i)
  
  # consistency check for column lengths
  if( !all(nrow(value) == linedf$dim) )
  {
    vnames.msg = paste0('[', paste(nm.head, collapse=', '), ']')
    info.msg = paste('Expected', linedf$dim[1],  'row(s) from input columns', vnames.msg)
    stop(paste0(info.msg, ' (got ', nrow(value), ')'))
  }
  
  # copy the table from memory and find index of columns that have been modified
  value.old = .rswat$stor$data[[fname]][[tablenum]][, nm.head] 
  cn.new = sapply(1:ncol(value.old), function(x) !identical(value.old[,x], value[,x]) )
  
  # NOTE: cn.new picks up integer-as-numeric mismatches, which may be ignored later
  # because we will coerce them to the right type.
  
  # build matrix of changed value indicators
  m.new = matrix(FALSE, nrow(value), ncol(value))
  m.new[,cn.new] = sapply(which(cn.new), function(cn) ! value.old[,cn] == value[,cn])
  
  # handle no-change case
  if(!any(unlist(m.new)))
  {
    if(preview) return(linedf[integer(0),])
    return(invisible())
  }
  
  # indicate whether an NA is being replaced or not
  isna.old = is.na(value.old)
  isna.new = is.na(value)
  m.new[ isna.old & isna.new ] = FALSE
  m.new[ isna.old & !isna.new ] = TRUE
  m.new[ !isna.old & isna.new ] = TRUE
  
  # vectorization magic here relies on the `arrange(j,i)` call above! 
  linedf.new = linedf[which(m.new),]
  value.new = unlist(lapply(which(cn.new), function(cn) as.list(value[m.new[,cn],cn])), recursive=F)
  char.new = rswat_2char(value.new, linedf.new)

  # finished preview mode
  if(preview) return(linedf.new %>% 
                      mutate(current_value = string) %>%
                      mutate(replacement = char.new$string) %>% 
                      select(file, table, i, j, name, current_value, replacement) )
  
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
  if(reload)
  {
    rswat_open(fname, reload=TRUE, quiet=quiet) 
  }
}

#' load SWAT+ output files as dataframe
rswat_output = function(fname=NULL, vname=NULL, add_units=TRUE, add_dates=TRUE, textio=NULL)
{
  # In development. Mostly tested on daily outputs
  # TODO: tidy up and split dates handler into its own function
  # TODO: handle vectorization for multiple files, with results returned as list
  # TODO: set default fname by reading "files_out.out"
  #
  # ARGUMENTS:
  #
  # `fname`: (optional) character vector, SWAT+ output filename(s), with or without extension
  # `vname`: (optional) character or integer vector, indexing the columns to read
  # `add_units`: logical, whether to add units to the columns (where available) 
  # `add_dates`: logical, whether to replace default time-index columns with a column of Dates
  # `textio`: character, path to the directory containing `fname`
  #
  # RETURN VALUE:
  #
  # Returns a dataframe containing the requested variables, 'gis_id', and either 'jday', 'yr'
  # (if `add_dates=FALSE`) or a column of Date objects ('date') generated from 'jday', 'yr'
  #
  
  # add .txt extension (if it has none)
  fname = ifelse(grepl('.txt', fname), fname, paste0(fname, '.txt'))
  
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
  
  # anything in SWAT+ project dir with .txt extension is checked
  txt.fname = grep('.txt', list.files(textio), value=TRUE)
  txt.name = gsub('.txt', '', txt.fname)
  
  # return file list when `fname` not supplied
  if( length(fname)==0 ) return(txt.name)
  
  # catch invalid input
  if( !(fname %in% txt.fname) ) stop(paste(fname, 'not found in', textio))
  fpath = file.path(textio, fname)
  
  # catch `_warnings.txt` and similar
  if( grepl('warnings', fname) ) return(data.frame(msg=readLines(fpath)))
  
  # define the magic line numbers
  ln.msg = 1  # comment
  ln.head = 2 # headers (variable names)
  ln.unit = 3 # units (for a subset of headers)
  ln.tab = 4 # first row of the tabular data
  
  # read the first few lines into memory and parse them using helper functions
  rswat_rlines(fpath, nmax=ln.tab)
  rswat_rtext(fname)
  linedf = .rswat$stor$temp[[fname]]$linedf
  msg = .rswat$stor$txt[[fname]][ln.msg]
  
  # tidy up package environment
  .rswat$stor$temp[[fname]] = list()
  .rswat$stor$txt[[fname]] = list()
  
  # counts to check for structural problems with the tables
  n.unit = sum(linedf$line_num == ln.unit)
  n.head = sum(linedf$line_num == ln.head)
  n.tab = sum(linedf$line_num == ln.tab)
  
  # catch location/time headers appearing on units line instead of headers line
  if( n.head < n.unit )
  {
    # move the headers to correct line in memory
    n.tomove = n.unit - n.head
    idx.tomove = (linedf$line_num == ln.unit) & ( linedf$field_num %in% (1:n.tomove) )
    linedf$line_num[idx.tomove] = ln.head
    
    # modify field numbers to maintain correct order
    linedf$field_num[idx.tomove] = linedf$field_num[idx.tomove] - n.tomove
    linedf = linedf %>% arrange(line_num, field_num)
    
    # update dependencies
    n.unit = sum(linedf$line_num == ln.unit)
    n.head = sum(linedf$line_num == ln.head)
  }
  
  # extract column headers and their classes
  head.nm = linedf$string[linedf$line_num == ln.head]
  head.start = linedf$start_pos[linedf$line_num == ln.head]
  head.class = linedf$class[linedf$line_num == ln.tab]
  
  # for adding dates we require these three columns (if available)
  req.nm = c('jday', 'yr', 'gis_id')
  req.nm = req.nm[req.nm %in% head.nm]
  
  # default behaviour is to load all variables
  if(is.null(vname)) vname = head.nm
  if(add_dates) vname = unique(c(vname, req.nm))
  idx.head = head.nm %in% vname
  
  # catch empty tables
  if(n.tab == 0)
  {
    # make an empty dataframe with named columns
    dat.out = as.data.frame(matrix(NA, 0, n.head, dimnames=list(NULL, head.nm[idx.head])))
    
    # turn off 'dates' mode (as they aren't available in the source file)
    add_dates = FALSE
  } 

  # handle tables with data
  if(n.tab > 0)
  {
    # BUGFIX: 'type' header with empty column (found in "basin_psc_day.txt")
    if(n.head > n.tab)
    {
      # index of the offending linedf row(s)
      string.erase = c('type')
      idx.erase = (linedf$string %in% string.erase) & (linedf$line_num == ln.head)
      if( any(idx.erase) )
      {
        # remove and update dependencies
        linedf = linedf[-which(idx.erase),]
        n.head = sum(linedf$line_num == ln.head)
        head.nm = linedf$string[linedf$line_num == ln.head]
        head.start = linedf$start_pos[linedf$line_num == ln.head]
        vname = vname[! vname %in% string.erase]
        idx.head = head.nm %in% vname
        
      } else {
        
        stop(paste('detected', n.head, 'headers, but only', n.tab, 'data columns'))
      }
      
    }
    
    # BUGFIX: unlabeled output columns! (found in "channel_sdmorph_day.txt")
    if(n.head < n.tab)
    {
      # print a warning when this happens
      n.unlab = n.tab - n.head
      msg.warn = paste('omitting', n.unlab, 'unlabeled column(s)')
      warning(msg.warn)
      
      # omit the columns - this assumes they lie in the final column(s) of the table
      idx.head = c(idx.head, rep(FALSE, n.unlab))
      head.nm = c(head.nm, paste0('unlabelled_', 1:n.unlab))
      n.head = n.head + n.unlab
    }
    
    # catch invalid variable names
    if(any( !(vname %in% head.nm) ))
    {
      err.msg = paste('The following variable name(s) were not found in', fname, ':')
      info.msg = paste(vname[!(vname %in% head.nm)], collapse=', ')
      stop(paste(err.msg, info.msg, collapse='\n'))
    }

    # load the requested columns using fread
    dat.out = fread(fpath, 
                    header=FALSE, 
                    skip=ln.tab-1, 
                    col.names=head.nm[idx.head], 
                    colClasses=head.class, 
                    drop=which(!idx.head)) %>% as.data.frame
  }
  
  # TODO: make this a function
  if(add_dates)
  {
    # attempt to detect period of simulation results
    n.obs = nrow(dat.out)
    n.gis = ifelse(is.null(dat.out$gis_id), 1, length(unique(dat.out$gis_id)))
    n.name = ifelse(is.null(dat.out$name), 1, length(unique(dat.out$name)))
    n.typeno = ifelse(is.null(dat.out$typ_no), 1, length(unique(dat.out$typ_no)))
    n.id = max(n.gis, n.name, n.typeno)
    
    # define the start/end dates
    date.start = as.Date(paste(dat.out$yr[1], dat.out$jday[1], sep='-'), format='%Y-%j')
    date.end = as.Date(paste(dat.out$yr[n.obs], dat.out$jday[n.obs], sep='-'), format='%Y-%j')
    
    # handle cases where the number of rows is not a multiple of number of unique ids
    if( n.obs/n.id != round(n.obs/n.id) )
    {
      # slower but more robust count 
      n.id = dat.out %>% group_by(yr, mon, day) %>% 
        summarize(n=n(), .groups='drop_last') %>% 
        pull(n) %>% unique
    }
    
    # express period in various time units to detect time step
    n.da = as.integer(difftime(date.end, date.start, units='days'))
    n.wk = as.integer(difftime(date.end, date.start, units='weeks'))
    n.time = c(day=n.da, week=n.wk, month=round(n.wk/4), year=round(n.wk/52))
    tstep = names(which.min( abs( n.time - (n.obs/n.id) ) ))
    
    # catch tables with variable numbers of ids per timestep
    if(length(n.id) > 1)
    {
      # really slow for large datasets - use only as last resort
      all.dates = mapply(function(x, y, z) as.Date(paste(x, y, z, sep='-')),
                         x = dat.out$yr, y = dat.out$mon, z = dat.out$day, SIMPLIFY = FALSE)
      
    } else {
      
      # this should be equivalent to `as.Date(paste(yr, mon, day, sep='-')`, but much faster
      all.dates = rep(seq(date.start, date.end, tstep), each=n.id)
    }
    
    # merge with output data, omitting redundant columns
    nm.omit = c('jday', 'mon', 'day', 'yr')
    idx.redundant = (names(dat.out) %in% nm.omit)
    dat.out = cbind(data.frame(date=all.dates), dat.out[ !idx.redundant ])
    
    # update the indexing variables used below
    idx.head = idx.head & !( head.nm %in% nm.omit )
    
    # # update the indexing variables used below
    # n.head = ncol(dat.out)
    # head.nm = names(dat.out)
    # 
    # # -Inf ensures that no units field will match the newly added 'date' field
    # head.start = c(-Inf, head.start[!idx.redundant])
  }
  
  # TODO: make this a function
  if(add_units)
  {
    # extract unit strings 
    unit.nm = linedf$string[linedf$line_num == ln.unit]
    unit.start = linedf$start_pos[linedf$line_num == ln.unit]
    
    # build a dictionary of unusual unit strings and their R-readable equivalents
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
    
    # note: I'm assuming 'mton' means 'metric tonne', and not 'milliton' (which is
    # apparently an arcane synonym of 'kilogram')
    
    # swap in the R-readable unit strings
    idx.translate = unit.nm %in% unit.lu[,1]
    unit.nm[idx.translate] = unit.lu[match(unit.nm[idx.translate], unit.lu[,1]), 2]
    
    # snap units to columns (they don't always align)
    head.unit = rep(NA, n.head)
    idx.unit = sapply(unit.start, function(n) which.min(abs(n - head.start)))
    head.unit[idx.unit] = unit.nm 
    
    # index of output dataframe columns which have a unit mapped to them
    cn.unit = which( !is.na( head.unit[idx.head] ) ) + as.integer(add_dates)
    # ^the as.integer(add_dates) increments index when we've added the new date column 
    
    # assign units to output dataframe
    if(length(cn.unit) > 0)
    {
      # the -as.integer(add_dates) decrements index as needed (dates column has no header!)
      dat.out[cn.unit] = mapply(function(x, y) set_units(x, y, mode='standard'), 
                                x = as.list(dat.out)[ cn.unit ], 
                                y = head.unit[idx.head][ cn.unit - as.integer(add_dates) ], 
                                SIMPLIFY=FALSE)
      
      #########
      # DEBUGGING
      # for(idx in 1:length(x))
      # {
      #   print(idx)
      #   set_units(x[[idx]], y[[idx]], mode='standard')
      # }
      ########
      
    }
  }

  return(dat.out)
}

#' load or write a backup of all SWAT+ text config files (or a subset of them)
rswat_backup = function(bpath=NULL, fname=NULL, bmode='backup', overwrite=FALSE)
{
  # In development. 
  #
  # ARGUMENTS:
  #
  # `bpath`: (optional) character, the path to the directory to write the backup
  # `fname`: (optional) character vector, name of config files to copy
  # `bmode`: character, one of 'backup', 'reload', or 'restore' (see DETAILS)
  # `overwrite`: logical, whether to overwrite any existing files in `bpath`
  #
  # RETURN VALUE:
  #
  # Returns nothing, with behaviour depending on argument `bmode`:
  #
  # if 'backup', copies the requested files to `bpath` folder
  # if 'reload', loads the requested files from `bpath` into memory
  # if 'restore', overwrites currently loaded project with requested backup in `bpath`
  # 
  # DETAILS:
  #
  # This function serves both for creating backups (the default), and for opening
  # SWAT+ files from text I/O directories external to the currently loaded one (ie.
  # `ciopath`, as defined in the last call to `rswat_cio`). Note however that a backup
  # can only be loaded when the current text I/O directory contains a like-named file
  # (when this is not the case you can simply copy the file manually).
  #
  # In 'reload' mode, backup parameters can be accessed via `rswat_open(fname)`. To reset
  # the parameters for a file to their current on-disk values (ie in the files in the
  # directory set by `rswat_cio`), simply reload them using `rswat_open(fname, reload=TRUE)`.
  # In 'restore' mode, backup parameters overwrite (on disk) the current ones.
  #
  # By default (`fname=NULL`) the function loads or copies all available config files.
  # The special argument `fname='.'` in backup mode also copies any non-config files found
  # in the text I/O directory (such as weather input files), but not subdirectories.
  #
  # If `bpath` is not supplied, the function assigns the default `ciopath/rswat_backup`.
  # If the `bpath` folder does not exist, it is created, unless we are in read-mode
  # (ie `reload=TRUE`). `bpath` can be either an absolute path, or a string (containing
  # no '/' or '\' characters, naming a subdirectory of the current text I/O directory)
  #

  # pull the current text I/O directory
  cio = rswat_cio(trim=F)
  textio = cio %>% pull(path) %>% dirname %>% unique
  
  # set default backup path as needed
  if( is.null(bpath) ) bpath = file.path(textio, 'rswat_backup')
  
  # if `bpath` has slashes, treat as a path, otherwise treat as a subdirectory name
  if( !grepl('\\\\|\\/', bpath) ) bpath = file.path(textio, bpath)
 
  # default files list includes everything listed in 'file.cio'
  if( is.null(fname) ) fname = c('file.cio', cio$file)
  
  # special argument '.' copies all files (eg. including weather data)
  if( all(fname=='.') ) fname = list.files(textio, include.dirs=FALSE)
  
  # define proposed backup file paths
  bfile.path = file.path(bpath, fname)
  bfile.exists = file.exists(bfile.path)
  
  # define file paths of currently loaded environment
  main.path = file.path(textio, fname)
  main.exists = file.exists(main.path)
  
  # write-mode
  if( bmode == 'backup' )
  {
    # check for and fix bad fname input
    if( !all(main.exists) )
    {
      # warn of missing source files
      msg1 = 'the following files were not found in current text I/O directory'
      msg2 = '\n(change this directory via `rswat_cio` with argument `ciopath`):'
      warning(paste(msg1, textio, ':\n', paste(fname[!main.exists], collapse=', '), msg2))
      
      # update the source and destination file lists
      fname = fname[main.exists]
      main.path = main.path[main.exists]
      main.exists = main.exists[main.exists]
      bfile.path = bfile.path[main.exists]
      bfile.exists = bfile.exists[main.exists]
    }
    
    # handle overwrites 
    if( !overwrite & any(bfile.exists) )
    {
      # warn of existing backups
      fname.existing = fname[bfile.exists]
      msg1 = 'the following files already exist in backup directory'
      msg2 = '(change `bpath` or set `overwrite=FALSE` to overwrite the old copies):\n'
      warning(paste(msg1, bpath, msg2, paste(fname.existing, collapse=', ')))
      
      # update the source and destination file lists
      fname = fname[!bfile.exists]
      main.path = main.path[!bfile.exists]
      main.exists = main.exists[!bfile.exists]
      bfile.path = bfile.path[!bfile.exists]
      bfile.exists = bfile.exists[!bfile.exists]
    }
    
    # count number of files to be written
    n.tocopy = length(fname)
    
    # error if we are left with nothing to write
    if( n.tocopy == 0 ) stop('Nothing to write')
    
    # create the directory as needed
    my_dir(bpath)
    
    # copy the files in a loop
    pb = txtProgressBar(max=n.tocopy, style=3)
    for(idx.file in 1:n.tocopy)
    {
      file.copy(main.path[idx.file], bfile.path[idx.file])
      setTxtProgressBar(pb, idx.file)
    }
    close(pb)
    return(invisible())
  }
  
  # read-mode or loading files to prep for restore
  if( bmode %in% c('reload', 'restore') )
  {
    # check for and fix bad fname input
    if( !all(bfile.exists) )
    {
      # warn of missing backup files
      msg1 = 'the following files were not found in backup directory'
      warning(paste(msg1, bpath, ':\n', paste(fname[!main.exists], collapse=', ')))
      
      # update the source file lists
      fname = fname[bfile.exists]
      bfile.path = bfile.path[bfile.exists]
      bfile.exists = bfile.exists[bfile.exists]
    }
    n.toload = length(bfile.path)
    
    # check the requested backups all match a currently loaded file
    main.loaded = fname %in% cio$file
    if( any( !main.loaded ) )
    {
      # warn of backups for files not found in current text I/O directory 
      fname.notloaded = fname[!main.loaded]
      msg1 = 'the following backup(s) matched no file in the current text I/O directory'
      msg2 = '\n(change this directory via `rswat_cio` with argument `ciopath`):'
      warning(paste(msg1, textio, ':\n', paste(fname.notloaded, collapse=', '), msg2))
      
      # update the source and destination file lists
      fname = fname[main.loaded]
      main.path = main.path[main.loaded]
      main.exists = main.exists[main.loaded]
      bfile.path = bfile.path[main.loaded]
      bfile.exists = bfile.exists[main.loaded]
    }
    
    # error if we are left with nothing to load
    if( n.toload == 0 ) stop('Nothing to load')
    
    # temporarily modify master text I/O files list to point to the backup(s)
    fpath.original = .rswat$cio$path
    .rswat$cio$path[ match(fname, cio$file) ] = bfile.path
    
    # TODO: dimensional sanity check 
    # load the files
    rswat_open(fname, reload=TRUE)
    
    # restore the current text I/O paths in master list
    .rswat$cio$path = fpath.original
  }
  
  # overwrite existing files with backup data
  if( bmode == 'restore' )
  {
    # loop over requested files
    for(idx in 1:length(fname))
    {
      # load the backup data into temporary variable
      fdat = rswat_open(fname[idx])
      
      # reload the (old) file, overwrite with data from memory
      rswat_open(fname[idx], reload=TRUE, quiet=TRUE)
      rswat_write(fdat, preview=FALSE)
    }
  }
  
  return(invisible())
  
}


#' ## internal functions for file I/O interface
#' 

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

#' ## functions for reading and cataloguing SWAT+ output files and variables

#' scans a SWAT+ folder for output files and compiles metadata about them as dataframe
rswat_oscan = function(textio=NULL, ignore=NULL)
{
  #
  # ARGUMENTS:
  #
  # 'ciopath': character, path to the directory (default NULL uses currently loaded project)
  # 'ignore': character vector, a list of filenames to exclude from the scan
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
  # function finds them and organizes some useful metadata, similar to `rswat_cio` . Use in
  # combination with `rswat_oinit` to get a comprehensive list of all available SWAT+ outputs.
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
  
  # scan al files in textio directory
  textio.fname = list.files(textio, include.dirs=FALSE)
  
  # exclude filenames appearing in 'ignore' and filenames prefixed with '_'
  ignore = unique( c(ignore, textio.fname[startsWith(textio.fname, '_')]) )
  
  # scan for files having extension 'txt' or 'out', omit ignored, initialize output fields
  fdf = data.frame( file = textio.fname ) %>%
    filter( endsWith(file, '.txt') | endsWith(file, '.out')) %>%
    filter( !( file %in% ignore ) ) %>%
    mutate( path = file.path(textio, file) ) %>%
    mutate( size = set_units(set_units(file.info(path)$size, bytes), kilobytes) ) %>%
    mutate( modified = file.info(path)$mtime ) %>%
    mutate( type = 'unknown' ) %>%
    mutate( step = NA ) %>%
    mutate( name = NA )
  
  # flag for log files
  fdf$type[ endsWith(fdf$file, '.out') ] = 'log'
  
  # 'files_out.out' holds a list of output files created in last simulation
  fread.prt = fread(file.path(textio, 'files_out.out'), skip=1, fill=T) %>% as.data.frame
  
  # BUGFIX: detect and fix spaces in first field causing table parser errors
  if( ncol(fread.prt) > 2 )
  {
    # buggy rows have non-empty third field
    idx.bug = nchar(fread.prt[, 3]) > 0
    
    # repair filenames (discard whatever follows the space in the first buggy field)
    fread.prt[idx.bug, 2] = fread.prt[idx.bug, ncol(fread.prt)]
    
    # omit the redundant fread column 
    fread.prt = fread.prt[, 1:2]
  }
  
  # name the fread columns for convenience and clean up names
  fread.prt = fread.prt %>% setNames(c('group', 'file') ) %>%
    mutate( group = gsub('_AA', '', group) ) %>%
    mutate( group = tolower(group) ) %>%
    mutate( group = gsub('-', '_', group) )
  
  # merge 'files_out.out' data with output dataframe and add prt flag
  fdf = fdf %>% left_join(fread.prt, by='file')
  fdf$type[ !is.na(fdf$group) ] = 'prt'
  
  # add a flag for object hydrograph files
  if( 'object.prt' %in% rswat_cio()$file )
  {
    # these files aren't listed in 'files_out.out' and they have a different structure
    nm.print = rswat_open('object.prt')$filename
    fdf$type[ fdf$file %in% nm.print ] = 'ohg'
  }
  
  # construct a timestep field - all OHG files are daily, and all AA files are yearly
  fdf$step[ endsWith(fdf$file, '_day.txt') | fdf$type == 'ohg' ] = 'day'
  fdf$step[  endsWith(fdf$file, '_mon.txt') ] = 'month'
  fdf$step[ endsWith(fdf$file, '_yr.txt') | endsWith(fdf$file, '_aa.txt') ] = 'year'
  
  # parse the object name from the filename for prt type files
  regexp.suffix = '(_aa\\.txt)|(_yr\\.txt)|(_mon\\.txt)|(_day\\.txt)'
  name.prt = gsub(regexp.suffix, '',  fdf$file[ fdf$type == 'prt' ] )
  fdf$name[ fdf$type == 'prt' ] = name.prt
  
  # tidy and return
  fdf %>% arrange(group, file) %>%  select(file, name, type, step, size, modified, path, group)
}

#' scans a SWAT+ output file to discover variable names and units (if available) 
rswat_oparse = function(fname=NULL, textio=NULL)
{
  #
  # ARGUMENTS:
  #
  # 'fname': character vector, a file to parse in `textio`
  # 'textio': character, path to the directory containing SWAT+ output files
  #
  # RETURN:
  #
  # dataframe of information about the columns of a SWAT+ output file, with one row per
  # detected field. Boolean attribute 'index' indicates that the column is an indexing
  # variable (of time or space, or an identifier), and 'required' indicates that the field
  # is required for rswat_output to determine dates and to match specific watershed objects.
  #
  # DETAILS:
  
  # add .txt extension to `fname` (if it has none)
  fname = ifelse(grepl('.txt', fname), fname, paste0(fname, '.txt'))
  
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
  
  # handle scan-all requests
  if( length(fname) == 0 )
  {
    # grab a list of all scannable files
    oscan = rswat_oscan(textio) %>% filter(type %in% c('prt', 'ohg'))
    
    # recursive calls to generate list of dataframes with filename field appended
    dat.out = lapply(oscan$file, function(f) rswat_oparse(f, textio) %>% mutate( file = f ) )
    
    # join into single dataframe and finish
    return( do.call(rbind, dat.out) )
  }
  
  
  # make sure requested file can be found
  fpath = file.path(textio, fname)
  if( !file.exists(fpath) ) stop(paste(fname, 'not found in', textio))
  
  # read the first few lines into memory and parse them using helper functions
  rswat_rlines(fpath, omit=NULL, nmax=3)
  rswat_rtext(fname)
  linedf = .rswat$stor$temp[[fname]]$linedf
  
  # tidy up these temporary tables in package environment
  .rswat$stor$temp[[fname]] = list()
  .rswat$stor$txt[[fname]] = list()
  
  # detect header, unit lines (if present) as all character type ...
  ln.ischar = sapply( split(linedf$class, linedf$line_num), function(x) all(x == 'character') )
  
  # ... prt files have comments, headers, units; whereas OHG files just have headers
  is.prt = all( ln.ischar[2:3] )
  ln.head = ifelse(is.prt, 2, 1)
  ln.tab = ifelse(is.prt, 4, 2)
  ln.unit = ifelse(is.prt, 3, NA)
  
  # extract the headers and their character start positions on the line
  head.nm = linedf$string[linedf$line_num == ln.head]
  head.start = linedf$start_pos[linedf$line_num == ln.head]
  n.head = length(head.nm)
  
  # reshape as dataframe
  dat.out = linedf %>% filter( line_num == ln.head ) %>%
    mutate( name = string ) %>% 
    mutate( units = NA ) %>% 
    select( -c(string, class) ) 
  
  # add a flag for important indexing variables
  nm.required = c('jday', 'yr', 'gis_id')
  nm.index = c(nm.required, c('mon', 'day', 'yr', 'unit', 'gis_id', 'name'))
  dat.out$required = dat.out$name %in% nm.required
  dat.out$index = dat.out$name %in% nm.index
  
  ## assign expected class of each output column
  dat.out$class = 'numeric'    
  dat.out$class[dat.out$index] = 'integer'
  dat.out$class[dat.out$name == 'name'] = 'character'
  
  # ohg files don't have units, so we're done
  if( !is.prt ) return(dat.out)
  
  # extract unit strings 
  unit.nm = linedf$string[linedf$line_num == ln.unit]
  unit.start = linedf$start_pos[linedf$line_num == ln.unit]
  
  # build a dictionary of unusual unit strings and their R-readable equivalents
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
  
  # note: I'm assuming 'mton' means 'metric tonne', and not 'milliton' (which is
  # apparently an arcane synonym of 'kilogram')
  
  # swap in the R-readable unit strings
  idx.translate = unit.nm %in% unit.lu[,1]
  unit.nm[idx.translate] = unit.lu[match(unit.nm[idx.translate], unit.lu[,1]), 2]
  
  # snap units to columns (they don't always align exactly)
  idx.head = sapply(unit.start, function(n) which.min(abs(n - head.start)))
  dat.out$units[ idx.head ] = unit.nm
  
  return(dat.out)
} 

#' request a dummy simulation of 1 day to generate example cases of available output files
rswat_odummy = function(subdir, quiet=FALSE)
{
  # ARGUMENTS
  #
  # 'subdir': character, the subdirectory to store the output files
  # `quiet`: logical, suppresses console messages
  #
  # RETURN
  #
  # dataframe containing the absolute paths to the example files along with their names
  # as listed in 'files_out.out'
  #
  # DETAILS
  # 
  # SWAT+ simulates many different physical variables, grouping them into output text files
  # according to the the type of watershed feature they belong to and the time interval. Users
  # may opt to have SWAT+ output all of these files in any given simulation, but this results
  # in slow execution times, so it is better to select a subset to print (via 'print.prt').
  #
  # This function creates a dataset to assist in selecting output files. It runs a simulation
  # over a single day in 1900, generating all available output files and copying them to `subdir`
  # (where they can't be overwritten by future calls to the SWAT+ executable). All project files
  # are then restored to their original state.
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
  
  # create a temporary directory for backups
  tdir = file.path(textio, paste0('_rswat_', basename(tempfile())))
  my_dir(tdir)
  
  # define paths for the backup copies - only .txt and .out files are affected here
  fname = list.files(textio, include.dirs=FALSE)
  fname = fname[ endsWith(fname, '.txt') | endsWith(fname, '.out') ]
  restore.path = file.path(textio, fname)
  backup.path = file.path(tdir, fname)
  
  # copy the files in a loop
  cat('backing up files...\n')
  for(ii in seq_along(backup.path)) file.copy(restore.path[ii], backup.path[ii])
  
  # load 'time.sim' and 'print.prt' into memory and create a backup
  time.sim = time.sim.bak = rswat_open('time.sim', quiet=quiet)
  print.prt = print.prt.bak = rswat_open('print.prt', quiet=quiet)
  
  # define the dummy date range and make the changes in time.sim and print.prt
  test.dates = c(day_start=1, yrc_start=1900, day_end=2, yrc_end=1900)
  time.sim[names(test.dates)] = test.dates
  print.prt[[1]][names(test.dates)] = test.dates
  
  # ensure that we don't skip years and set the timestep to daily
  time.sim$step = 0
  print.prt[[1]]$nyskip = 0
  print.prt[[1]]$interval = 1
  
  # write the changes to the files on disk
  rswat_write(time.sim, preview=F, quiet=quiet)
  rswat_write(print.prt[[1]], preview=F, quiet=quiet)
  
  # activate all outputs, write changes to disk
  print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'y'
  rswat_write(print.prt[[5]], preview=F, quiet=quiet)
  
  # execute the simulation and scan for new output files
  rswat_exec(quiet=quiet)
  fname.new = list.files(textio, include.dirs=FALSE)
  fname.new = fname.new[ endsWith(fname.new, '.txt') | endsWith(fname.new, '.out') ]
  
  # define absolute paths for the output files and their destinations
  dest.dir = file.path(textio, subdir)
  fpath.new = file.path(textio, fname.new)
  fpath.dest = file.path(dest.dir, fname.new)
  my_dir(dest.dir)
  
  # copy the new files to subdirectory and delete the originals
  for(ii in seq_along(fpath.dest)) file.copy(fpath.new[ii], fpath.dest[ii])
  unlink(fpath.new)
  
  # restore the backup parameter values
  rswat_write(time.sim.bak, preview=F, quiet=quiet)
  rswat_write(print.prt.bak[[1]], preview=F, quiet=quiet)
  rswat_write(print.prt.bak[[5]], preview=F, quiet=quiet)
  
  # delete the new output files and restore backups
  cat('restoring backup...\n')
  for(ii in seq_along(backup.path)) file.copy(backup.path[ii], restore.path[ii])
  unlink(tdir, recursive=TRUE)
  
  # return a dataframe describing the new files
  return( rswat_oscan(dest.dir) )
  
}

#' build/access a database of available SWAT+ output files and variable names
rswat_oinit = function(quiet=FALSE, rescan=TRUE, reload=FALSE)
{
  # ARGUMENTS
  #
  # `quiet`: logical, suppresses console messages
  # `rescan`: logical, indicating to scan the textio directory for new output files
  # `reload`: logical, indicating to run a dummy simulation to build outputs database
  #
  # RETURN
  #
  # in development
  #
  # DETAILS
  #
  # Initializes the `stor$output` list in `.rswat` environment, containing dataframes
  # `fname` and `vname`, which list the output files and variables stored in them (with
  # related information).
  #
  # These tables are populated by scanning the `textio` directory (by default, the parent
  # directory of 'file.cio' as set by `rswat_cio`) for available .txt and .out files.
  # Optionally the function runs a simulation with all outputs toggled on (for 1 day),
  # parsing the results to get a comprehensive list of output variables matched with file
  # names.
  #
  # Note: `rswat_cio()` must be run first to specify the path to 'file.cio' (this is
  # also where SWAT+ sends its output files)
  
  # set up extensions to read
  ext = c('prt', 'ohg')
  
  # check that `rswat_cio` has been called
  if( !exists('.rswat') ) stop('`rswat_cio` must be run first')
  
  # set 'textio' path
  textio = dirname(.rswat$ciopath)

  # initialize storage in package memory as needed
  if( ! 'output' %in% ls(.rswat$stor) ) .rswat$stor$output = list()
  if( ! 'fname' %in% ls(.rswat$stor$output) ) .rswat$stor$output$fname = data.frame()
  if( ! 'vname' %in% ls(.rswat$stor$output) ) .rswat$stor$output$vname = data.frame()

  # this part skipped after initial run (unless reload requested)
  if(reload) 
  {
    # generate dummy output files to serve as references
    if( !quiet ) cat('generating all output files... \n')
    tsubdir = paste0('_rswat_', basename(tempfile()))
    textio.dummy = file.path(textio, tsubdir)
    fdf.dummy = rswat_odummy(tsubdir, quiet) %>% filter(type %in% ext)
    n.dummy = nrow(fdf.dummy)
    
    # read the first few lines of each file to determine output variables and units
    if( !quiet ) cat(paste('parsing', n.dummy, 'output files... \n'))
    headers.list = lapply(fdf.dummy$file, function(f) {
      rswat_oparse(f, textio.dummy) %>% mutate(file=f)
      })

    # reshape as long dataframe and merge with file info, copying result to package storage
    .rswat$stor$output$vname = fdf.dummy %>% 
      mutate( fname = name ) %>%
      select( file, fname, type, step ) %>% 
      left_join(do.call(rbind, headers.list), by='file') %>% 
      select( name, units, class, step, everything() )
    
    # update files list and toggle to rescan textio directory later
    .rswat$stor$output$fname = fdf.dummy %>% select( -c(path, group) )
    rescan = TRUE
    
    # remove the dummy simulation files
    unlink(textio.dummy, recursive=TRUE)
  }
  
  # scan for current output files as needed
  oscan.current = .rswat$stor$output$fname
  rescan = ifelse( nrow(.rswat$stor$output$fname) == 0, TRUE, rescan )
  if( rescan )
  {
    # list files currently found in textio directory
    oscan.current = rswat_oscan(textio) %>% 
      filter(type %in% ext) %>%
      select( -c(path, group) )
  }
  
  # handles case of scan before load
  if( nrow(.rswat$stor$output$fname) == 0 )
  {
    # only the files currently in textio are known
    .rswat$stor$output$fname = oscan.current 
    
  } else {
    
    # add files discovered in dummy simulation
    .rswat$stor$output$fname = .rswat$stor$output$fname %>% 
      select( -c(size, modified) ) %>%
      left_join(oscan.current, by=c('file', 'name', 'type', 'step'))
  }

  # return the files list
  return(.rswat$stor$output$fname %>% arrange(modified) )
}







#' ## functions for running QSWAT+ and opening its output files
#' 

#' assembles required inputs for a QSWAT+ project
qswat_setup = function(cid, catchments, projdir=NULL, wipe=FALSE, config=NULL, quiet=FALSE)
{
  # `cid`: integer, the 'catchment_id' of the subwatershed in `catchments$boundary`
  # `catchments` list of sf objects, the output of `my_find_catchments(...)`
  # `projdir`: character, the path to the desired QSWAT+ project directory 
  # `wipe`: logical, indicating to erase all existing data in `projdir` 
  # `config`: named list, name-value pairs to add to the configuration JSON
  # `quiet`: logical, suppresses console messages
  #
  # this requires the UYRW_data workflow to be completed to generate the input data files
  # listed in the metadata CSVs loaded by `my_metadata()`.
  #
  # Note that since we are using the stock SSURGO/STATSGO mukeys, there should be no need to
  # import a custom usersoil database
  #
  # TODO: add lakes
  # Some data prep will be required for QSWAT+ to delineate channels correctly around lakes,
  # possibly using the [SWAT2Lake](https://projects.au.dk/wet/software/#c55181) QGIS plugin
  # described [here](https://www.sciencedirect.com/science/article/abs/pii/S1364815218302500).
  # This remains a work in progress.
  #
  # 
  
  # NAs are represented in the GeoTiff by an integer -- usually something large and negative.
  # The default value for this integer when using `raster::writeRaster` is so large that
  # it can cause an integer overflow error in one of the python modules used by QSWAT+ (at 
  # least on my 64bit machine). We instead use the value recommended in the QSWAT+ manual:
  tif.na.val = -32767
  
  # unpack input
  boundary = catchments$boundary[catchments$boundary$catchment_id == cid, ]
  io = catchments$io[catchments$io$catchment_id == cid, ]
  pts = catchments$pts[catchments$pts$catchment_id == cid, ]
  demnet = catchments$demnet[catchments$demnet$catchment_id == cid, ] %>% na.exclude
  
  # TODO: move this to a separate function (to be run in combination with SWAT+ Editor)
  # for now we are testing with the PNWNAMet weather data
  wname = 'pnwnamet_uyrw'
  
  # look up input data file locations
  dem.path = here(my_metadata('get_dem')['swat_dem', 'file'])
  soils.path = here(my_metadata('get_soils')['swat_soils_tif', 'file'])
  landuse.path = here(my_metadata('get_landuse')['swat_landuse_tif', 'file'])
  landuselu.path = here(my_metadata('get_landuse')['swat_landuse_lookup', 'file'])
  wdat.path = here(my_metadata('get_meteo')[wname, 'file'])
  subwatersheds.meta = my_metadata('get_subwatersheds')
  taudem.meta = my_metadata('taudem', data.dir=subwatersheds.meta['taudem', 'file'])
  
  # set default project directory
  if( is.null(projdir) ) 
  {
    # default name from USGS gage site name, default location from parent of dem raster file
    projnm = boundary$catchment_name
    projdir = here(file.path(dirname(dem.path), projnm))
  } 
  
  # project name is always the project directory name
  projnm = basename(projdir)
  
  # handle overwrite calls and create the directory if necessary
  if(wipe & file.exists(projdir)) unlink(projdir, recursive=TRUE)
  my_dir(projdir)
  
  # data inputs go into a subdirectory
  datadir = file.path(projdir, 'inputs')
  my_dir(datadir)
  
  # define the files to write
  {
    files.towrite = list(
      
      # path to QSWAT project folder
      c(name='proj',
        file=projdir, 
        type='directory',
        description='QSWAT+ project directory'),
      
      # watershed boundary polygon (the AOI, a subwatershed from `my_find_catchments`)
      c(name='boundary',
        file=file.path(datadir, 'boundary.geojson'), 
        type='GeoJSON',
        description='polygon delineating subwatershed for the SWAT+ model'),
      
      # DEM raster ('swat_dem' from 'get_dem.R', cropped to AOI)  
      c(name='dem',
        file=file.path(datadir, 'dem_in.tif'), 
        type='GeoTIFF',
        description='QSWAT+ DEM'),
      
      # land use raster for the UYRW ('swat_landuse_tif' from 'get_landuse.R', cropped to AOI)
      c(name='landuse',
        file=file.path(datadir, 'landuse_in.tif'), 
        type='GeoTIFF',
        description='SWAT+ land use classification'),
      
      # soils raster for the UYRW ('swat_tif' from 'get_soils.R', cropped to AOI)
      c(name='soils',
        file=file.path(datadir, 'soil_in.tif'), 
        type='GeoTIFF',
        description='SWAT soils classification, maps to soil table in SWAT+ database'),
      
      # lookup table for 'landuse' ('swat_landuse_lookup' from 'get_landuse.R')
      c(name='landuse_lookup',
        file=file.path(datadir, 'landuse_lookup_in.csv'), 
        type='CSV',
        description='integer code for landuse, maps to `plants_plt` table in SWAT+ database'), 
      
      # outlets shapefile (for now based on 'USGS_sites' from 'get_streamgages.R')
      c(name='outlets',
        file=file.path(datadir, 'outlets_in.shp'), 
        type='ESRI Shapefile',
        description='outlet point locations, used by QSWAT+ to delineate subbasins'),
      
      # streams shapefile (simplified 'flowlines' from 'get_basins.R')
      c(name='streams',
        file=file.path(datadir, 'streams_in.shp'), 
        type='ESRI Shapefile',
        description='stream geometries to "burn" into DEM prior to running TauDEM'),
      
      # directory to write SWAT weather data (input text files)
      c(name='wdat',
        file=file.path(datadir, wname),
        type='directory',
        description='directory for writing SWAT weather input text files'),
      
      # JSON containing metadata and parameters for QSWAT+ workflow in PyQGIS
      c(name='config',
        file=file.path(datadir, paste0(projnm, '.json')),
        type='JSON',
        description='configuration file for run_qswatplus.py module')
      
    )
  }
  
  # write metadata to csv in QSWAT+ project directory
  qswat.meta = my_metadata(projnm, files.towrite, overwrite=TRUE, data.dir=projdir, v=!quiet)
  
  # extract  boundary polygon, write GeoJSON, coerce to `sp` for compatibility with `raster`
  boundary.path = here(qswat.meta['boundary', 'file'])
  if( file.exists(boundary.path) ) unlink(boundary.path)
  st_write(boundary, boundary.path, quiet=quiet)
  boundary.sp = as(boundary, 'Spatial')
  
  # crop/mask the DEM, soils, and land use rasters
  dem = mask(crop(raster(dem.path) , boundary.sp), boundary.sp)
  landuse = mask(crop(raster(landuse.path) , boundary.sp), boundary.sp)
  soils = mask(crop(raster(soils.path) , boundary.sp), boundary.sp)
  
  # write to new location
  writeRaster(dem, here(qswat.meta['dem', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(landuse, here(qswat.meta['landuse', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  writeRaster(soils, here(qswat.meta['soils', 'file']), NAflag=tif.na.val, overwrite=TRUE)
  
  # drop attributes from stream network before writing as shapefile 
  st_write(st_geometry(demnet), here(qswat.meta['streams', 'file']), append=FALSE, quiet=quiet)
  
  # copy outlet point geometries to shapefile, adding required QSWAT+ attributes 
  id.empty = as.integer(0)
  io.df = data.frame(ID=as.integer(1:nrow(io)), INLET=io$inlet, RES=id.empty, PTSOURCE=id.empty)
  st_write(st_sf(io.df, geom=st_geometry(io)), 
           here(qswat.meta['outlets', 'file']), 
           append=FALSE, 
           quiet=quiet)
  
  # open the table, drop redundant rows, copy to QSWAT+ project folder
  landuse.lu = read.csv(landuselu.path) %>% filter( Value %in% unique(landuse) )
  write.csv(landuse.lu, qswat.meta['landuse_lookup', 'file'], row.names=FALSE)
  
  # TODO: this next chunk goes into a different weather builder function
  # delete/create weather directory as needed
  wdat.dir = qswat.meta['wdat', 'file']
  if(dir.exists(wdat.dir)) unlink(wdat.dir, recursive=TRUE)
  my_dir(wdat.dir)
  
  # for now we are testing weather inputs with PNWNAMet data
  wdat = readRDS(wdat.path)
  
  # load weather data and set NA values for humidity (bugfix for SWAT+ Editor)
  wdat$tables$hmd = wdat$tables$tmax
  wdat$tables$hmd[] = NA
  
  # add 5km buffer for grabbing weather grid points
  boundary.buff = st_buffer(boundary, dist=set_units(5, km))
  include = as.vector(st_intersects(wdat$coords_sf, boundary.buff, sparse=FALSE))
  
  # load DEM and call the weather station data export function
  if(!quiet) print('writing weather station data files...')
  invisible(my_swat_wmeteo(wdat, exdir=wdat.dir, form='qswat', include=include, quiet=quiet))
  
  # TODO: change my_metadata() (and all dependencies) to use JSON
  # convert the metadata csv to JSON (adding line breaks for readability)
  qswat.meta.out = cbind(name=rownames(qswat.meta), data.frame(qswat.meta, row.names=NULL))
  
  # TODO: put these defaults in a JSON file in /data
  
  # derive initial (dummy) start/end dates from first 2 days of available weather
  wdat.start = min(wdat$dates)
  
  # set default channel drop threshold from earlier taudem analysis (may be too low)
  drop.channel = as.integer(gsub('stream threshold:', '', taudem.meta['nstream', 'description']))
  
  # set default stream drop threshold to 3X the channel drop
  drop.stream = 3 * drop.channel
  
  # SWAT+ Editor writes contents of TxtInOut, but it can be skipped if you only need geometries
  skip.editor = FALSE

  # define default QSWAT+ parameters
  {
    config.def.list = list(
      
      # the QSWAT+ project name
      c(name='name',
        file=projnm, 
        type='parameter',
        description='QSWAT+ project name'),
      
      # the source of the input weather data 
      c(name='wname',
        file=wname, 
        type='parameter',
        description='initial input weather data for SWAT+ simulation'),
      
      # initial value for start_yr
      c(name='start_yr',
        file=format(wdat.start, '%Y'), 
        type='parameter',
        description='initial value of start year for SWAT+ simulations'),
      
      # initial value for start_day 
      c(name='start_day',
        file=as.integer(format(wdat.start, '%d')), 
        type='parameter',
        description='initial value of start day for SWAT+ simulations'),
      
      # initial value for end_yr
      c(name='end_yr',
        file=format(wdat.start + 1, '%Y'), 
        type='parameter',
        description='initial value of end year for SWAT+ simulations'),
      
      # initial value for end_day 
      c(name='end_day',
        file=as.integer(format(wdat.start + 1, '%d')), 
        type='parameter',
        description='initial value of end day for SWAT+ simulations'),
      
      # channel drop threshold
      c(name='drop_channel',
        file=drop.channel, 
        type='parameter',
        description='threshold for channel delineation (in number of cells)'),
      
      # stream drop threshold
      c(name='drop_stream',
        file=drop.stream, 
        type='parameter',
        description='threshold for stream delineation (in number of cells)'),
      
      # note: this functionality (triggered by landuse 'WATR') seems to be gone in latest SWAT+ rev
      # reservoir threshold percentage
      c(name='res_thresh',
        file=101,
        type='parameter',
        description='threshold for resrevoir delineation (percent)'),
      
      # indicator to skip SWAT+ Editor routine (in initial QSWAT+ run)
      c(name='skip_editor',
        file=skip.editor, 
        type='parameter',
        description='whether to skip SWAT+ Editor call to write config text files')
    )
  }
  

  # overwrite with user supplied settings 
  if( !is.null(config) )
  {
    # config should be a named vector or list containing the new parameters
    for(nm in names(config))
    {
      idx.new = which(sapply(config.def.list, function(x) x['name'] == nm ))
      config.def.list[[idx.new]]['file'] = as.character(config[[nm]])
    }
  } 
  
  # reshape as dataframe
  config.def.out = data.frame(do.call(rbind, config.def.list))
  
  # update the metadata csv
  qswat.meta = my_metadata(projnm, config.def.list, overwrite=TRUE, data.dir=projdir, v=!quiet)
  
  # copy `config` to JSON
  config.path = qswat.meta['config', 'file']
  config.out = rbind(qswat.meta.out, config.def.out)
  writeLines(toJSON(config.out, pretty=TRUE), config.path)
  
  # finish
  return(qswat.meta)
}

#' run the QSWAT+ workflow for a project created by `my_prepare_qswatplus`
qswat_run = function(qswat, quiet=FALSE)
{
  # `quiet`: logical, suppresses console messages
  # 'qswat': dataframe, the return value of `qswat_setup`
  #
  # DETAILS: alternative input for `qswat`: (character), the path to the JSON config
  # file created by `my_prepare_qswatplus`
  
  # handle dataframe input and extract json config file path
  jsonpath = qswat
  if(is.data.frame(jsonpath)) jsonpath = jsonpath['config', 'file']
  
  # path to the python launcher
  exepath = 'H:/UYRW_data/python/run_qswatplus.cmd'
  
  # call the launcher with this JSON file (runs a python script)
  system2(exepath, normalizePath(jsonpath), stdout=ifelse(quiet, FALSE, ''))
  if(!quiet) cat('\n>> finished')
}

#' read the shapefiles from a QSWAT+ project and generate some summary info
qswat_read = function(qswat)
{
  # In development
  #
  # ARGUMENTS:
  #
  # `qswat`: dataframe, the return value of `qswat_setup`
  #
  # RETURN VALUE:
  #
  # A list containing:
  # 'sta': list of model stats and info
  # 'dem': the input DEM (passed to TauDEM)
  # 'out': the input outlets (passed to TauDEM)
  # 'cha', 'hru', 'lsu', 'sub': watershed geometries generated by QSWAT+ 
  #
  # DETAILS:
  #
  # The input dataframe can also be specified by passing a character string in `qswat`
  # specifying the absolute path to the metadata CSV
  #
  
  # handle character string input
  #if( is.character(qswat) ) 
  
  # identify the shapefiles directory where we can find the HRU and LSU geometries 
  shpdir = file.path(qswat['proj', 'file'], 'Watershed/Shapes')
  tifdir = file.path(qswat['proj', 'file'], 'Watershed/Rasters')
  
  # load dem, outlets, channels and subbasins shapefiles
  dem = raster(qswat['dem', 'file'])
  out = read_sf(file.path(shpdir, 'outlets_in.shp'))
  riv = read_sf(file.path(shpdir, 'rivs1.shp'))
  subb = read_sf(file.path(shpdir, 'subs1.shp'))
  
  # 'hrus1.shp' appears to be an early iteration, before merging by dominant HRU
  hru2 = read_sf(file.path(shpdir, 'hrus2.shp'))
  
  # 'lsu1.shp' appears to be the same as 'lsus2.shp', but with fewer attributes
  lsu2 = read_sf(file.path(shpdir, 'lsus2.shp'))
  
  # merge LSU and HRU attribute data (they match because we picked "Dominant HRU" method)
  hru.df = inner_join(as.data.frame(st_drop_geometry(lsu2)), 
                      as.data.frame(st_drop_geometry(hru2)),
                      by = c('Subbasin', 'Channel', 'Landscape', 'X.Subbasin', 'Area'))
  
  # build an sf object from the merged dataset
  hru = st_sf(hru.df, geometry=lsu2$geometry)
  
  # these HRU-specific centroids are to replace the subbasin-level lat/long coordinates 
  hru.centroids = st_centroid(st_geometry(hru))
  hru.coords = st_coordinates(st_transform(hru.centroids, crs=4326))
  
  # add them to the dataframe along with DEM values and areas, remove some detritus
  hru.out = hru %>%
    mutate( long = set_units(hru.coords[, 1], degrees) ) %>%
    mutate( lat = set_units(hru.coords[, 2], degrees) ) %>%
    mutate( elev = set_units(extract(dem, st_sf(hru.centroids)), m) ) %>%
    mutate( area = st_area(st_geometry(hru)) ) %>%
    mutate( id = HRUS, frac = X.Subbasin ) %>%
    select( -c(Area, Lat, Lon, Elev, X.Landscape, X.Subbasin, HRUS) ) %>%
    select( id, Channel, LINKNO, everything() )
  
  # TODO: option to take a random sample of points from each HRU to get elevation medians
  # st_sample(st_geometry(hru), rep(10, nrow(hru)))
  
  # create watershed boundary polygon
  bou = st_union(subb)

  # extract the drop thresholds and compute area of one cell
  tcha = as.integer(qswat['drop_channel', 'file'])
  tsub = as.integer(qswat['drop_stream', 'file'])
  acell = set_units(prod(res(dem)), m^2)
  
  # bundle metadata output into a list
  stats = list(
    
    # project name and important files
    name = qswat['name', 'file'],
    
    # counts of subbasins, channels, hrus
    counts = c(nsub = nrow(subb),
               ncha = nrow(riv),
               nhru = nrow(hru)),

    # watershed total area
    area = set_units(st_area(st_union(subb)), km^2),
    elevation = c(min = set_units(cellStats(dem, min), m), 
                  median = set_units(cellStats(dem, median), m), 
                  max = set_units(cellStats(dem, max), m)),
    

    
    # copy the channels and subbasins (ie streams) thresholds, and express as areas
    threshold_area = c(cha=set_units(tcha * acell, km^2), sub=set_units(tsub * acell, km^2)),
    threshold_ncell = c(cha=as.integer(tcha), sub=as.integer(tsub)),
    
    # some useful file/directory paths
    paths = c(sql = file.path(qswat['proj', 'file'], 'swatplus_datasets.sqlite'),
              txtio = file.path(qswat['proj', 'file'], 'Scenarios/Default/TxtInOut'))
  )

  # return everything in a list
  return(list(sta=stats, dem=dem, out=out, cha=riv, hru=hru.out, lsu=lsu2, sub=subb, bou=bou))
  
}

# plot various GIS components of a QSWAT+ project
qswat_plot = function(dat, r=NULL, titles=NULL, pal=NULL, style='cont', breaks=NULL, spos=NULL, 
                      legwd=0.2, addto=NULL)
{
  # 'dat': list, the qswat+ project data (output of qswat_read)
  # 'r': Raster, to plot in background (default is DEM)
  # 'titles': list of character vectors, with names 'main', 'sub', and 'legend'
  # 'pal': character vector, a colour palette to use for the background raster
  # 'style': character, raster plot style (see tmap::tm_raster)
  # 'breaks': list (or dataframe) supplying custom breakpoints and labels (see details)
  # 'spos': character vector, the scalebar position (passed to tmap::tm_layout)
  # 'legwd': numeric, fraction of plot width to use for legend 
  # 'addto': tmap object, an existing map to append to
  #
  # DETAILS:
  #
  # The defaults for 'titles$main' and `titles$sub` are, respectively, 'dat$stats$name'
  # (the project name) and three lines of count info (area, HRUs, subbasins) about the model,
  # extracted from `dat$stats` and `dat$bou`. The default for `titles$legend` is "elevation (m)",
  # unless `r` is supplied, in which case it is set to `names(r)`. Any user-supplied arguments
  # to `titles` will override these defaults.
  #
  # The 'titles' text is always written outside the plot frame on the right margin, with only
  # the main title bolded. Subtitles are separated from the legend title (below) with a
  # horizontal rule. If 'main' and 'sub' contain multiple entries, they are collapsed with the
  # separator '\n', so that each entry is printed on a separate line. Text size is scaled as
  # needed to fit the legend width (adjusted by `legwd`).
  #
  # A (km level) scalebar is automatically added with breaks spanning 1/4 of the plot frame
  # (east-west). If `spos` is missing, it is assigned automatically to the least crowded corner
  # apart from top-right (which already cluttered with legend items).
  #
  # See `?hcl.colors` for a wide range of good options for setting 'pal' 
  #
  # typical styles are 'cont' for continuous (the default) and 'pretty' for categorical.
  # Breakpoints are determined by ... when `breaks` is a dataframe, its non-character column
  # is interpreted as a lookup key for the (numerical) raster data, and the function automatically
  # sets the appropriate breakpoints
  #
  #
  
  # unpack `dat`
  stats = dat[['sta']]
  dem = dat[['dem']]
  out = dat[['out']]
  riv = dat[['cha']]
  subb = dat[['sub']]
  hru = dat[['hru']]
  lsu = dat[['lsu']]
  bou = dat[['bou']]
  
  # unpack `titles`
  msg.main = titles[['main']]
  msg.sub = titles[['sub']]
  msg.legend = titles[['legend']]
  
  # set default main title
  if( is.null(msg.main) )
  {
    # format in decimal degrees
    coords = round(abs(colMeans(st_drop_geometry(subb)[, c('Lat', 'Lon')])), 3)
    msg.coords = paste( paste0( paste0(coords, '\u00B0'), c('N', 'W') ), collapse=' ')
    
    # DMS alternative
    # coords = colMeans(st_drop_geometry(subb)[, c('Lat', 'Lon')])
    # NS.dms = gsub('d', '°', as.character(dd2dms(coords['Lat'], NS=TRUE)))
    # EW.dms = gsub('d', '°', as.character(dd2dms(coords['Lon'], NS=FALSE)))
    # msg.coords = paste0('(', NS.dms, ' ', EW.dms, ')')
    
    msg.main = paste0(gsub('_', ' ', stats$name), ' (', msg.coords, ')')
  }
  
  # collapse vectors as needed
  if( length(msg.main) > 1 ) msg.main = paste(msg.main, collapse='\n')
  
  # generate default subtitle as needed
  if( is.null(msg.sub) )
  {
    # computed from 'bou' (boundary geometry) and 'stats' in `dat`
    msg.sub = c(paste(round(set_units(stats$area, km^2), 1), 'square km'),
                paste(stats$counts['ncha'], 'channels'),
                paste(stats$counts['nsub'], 'subbasins')) 
  }

  # set default legend title
  if( is.null(msg.legend) )
  {
    # based on Raster name attribute
    msg.legend = paste(names(r), 'codes')
    
    # when using default `r` (DEM), add a better label
    if( is.null(r) ) msg.legend = 'elevation (m)'
  }

  # add horizontal rule above legend title and collapse subtitles as needed
  hrule = paste(rep('_', nchar(msg.legend)), collapse='')
  msg.legend = paste(c(hrule, msg.legend), collapse='\n')
  if( length(msg.sub) > 1 ) msg.sub = paste(msg.sub, collapse='\n')

  # default raster is DEM (override with `r=NA` to omit rasters)
  if( is.null(r) ) r = dem
  
  # determine scale bar positioning
  if( is.null(spos) )
  {
    # use quadrant intersection with boundary polygon to order the corners by crowdedness
    spacious.idx = which.min(st_area(st_intersection(st_make_grid(bou, n=c(2,2)), bou))[-4])
    
    # list of tm_layout arguments in matching order
    spacious = list(c('LEFT', 'BOTTOM'), 
                    c('RIGHT', 'BOTTOM'), 
                    c('LEFT', 'TOP'))

    # make the replacements
    if(is.null(spos)) spos = spacious[[ spacious.idx ]]
  }
  
  # default palette is like an improved rainbow()
  if( is.null(pal) ) pal = hcl.colors(1e3, palette='Dark 3')
  
  # set up legend breaks and labels as needed
  is.lookup = FALSE
  leg.labels = leg.breaks = NULL
  if( !is.null(breaks) )
  {
    # interpret dataframe input as lookup table
    if( is.data.frame(breaks) )
    {
      # force fixed style breakpoints
      is.lookup = TRUE
      
      # split into list
      breaks.df = breaks
      breaks = as.list(breaks.df)
    }
    
    # identify label/values entries
    breaks.cl = sapply(breaks, class)
    idx.label = breaks.cl == 'character'
    
    # breakpoints are converted to character when no labels supplied
    leg.breaks.in = breaks[[ which(breaks.cl != 'character') ]]
    if( sum(idx.label) > 0 ) leg.labels = breaks[[ which(idx.label) ]]
    #leg.labels = as.character(round(leg.breaks.in, 3))
    
    # for categorical plots convert midpoints to interval endpoints
    style.cont = style %in% c('cont', 'order', 'log10')
    if( ( length(leg.labels) == length(leg.breaks.in) ) & is.lookup )
    {
      # I think 
      style = 'fixed'
      bmin = min(leg.breaks.in)
      leg.breaks = c(bmin, leg.breaks.in + c(diff(leg.breaks.in)/2, 1))
      
    } else {
      
      # otherwise we leave them unchanged
      leg.breaks = leg.breaks.in
    }
  }
  
  # define breaks for the scale bar
  km.breaks = pretty(1e-3 * diff(st_bbox(bou)[c('xmin', 'xmax')]) / 4, 2, min.n=2)
  if( km.breaks[1] != 0 ) km.breaks = c(0, km.breaks)
  
  # initialize tmap object: for debugging, try design mode with tmap_design_mode(T)
  tmap.out = addto + tm_shape(bou) + tm_polygons(alpha=0) + tm_add_legend('title', title=msg.sub)
  
  # add raster layer (skipping on non-rasterLayer input)
  if( class(r) == 'RasterLayer' ) 
  {
    # pipe to the one already initialized
    tmap.out = tmap.out + 
      tm_shape(r) +
      tm_raster(legend.reverse=TRUE, 
                palette=pal, 
                style=style, 
                title=msg.legend,
                breaks=leg.breaks,
                labels=leg.labels)
  }
  
  # add geometries and aesthetics
  tmap.out = tmap.out +
    tm_shape(hru) + tm_polygons(alpha=0, border.col=adjustcolor('white', alpha=0.25), lwd=1) +
    tm_shape(subb) + tm_polygons(alpha=0, border.col='black') +
    tm_shape(riv) + tm_lines(alpha=0.1, lwd='Order', scale=5, legend.lwd.show=FALSE) +
    tm_shape(out) + tm_dots(size=0.5) + tm_shape(out) + tm_dots(size=0.2, col='grey50') +
    tm_scale_bar(breaks=km.breaks, position=spos, text.size=0.8) + 
    tm_grid(n.x=2, n.y=2, projection=4326, alpha=0.2) +
    tm_layout(main.title = msg.main,
              main.title.fontface ='bold',
              main.title.size = 1,
              main.title.position = 'right',
              legend.outside = TRUE,
              legend.outside.position = c('right', 'top'),
              legend.outside.size = legwd,
              legend.text.size = 0.8,
              inner.margins = rep(1e-2, 4),
              frame = FALSE)

  # finish
  return(tmap.out)
}



#' ## helper functions for calibrating SWAT+ and running simulations
#' 



#' run the SWAT+ executable (NOTE: requires `rswat` helper function)
rswat_exec = function(quiet=FALSE, textio=NULL)
{
  # 'textio': character, path to the text I/O directory for the SWAT+ model 
  # `quiet`: logical, suppresses console messages
  #
  # `object`, if supplied, specifies the output files to write, where the default is to not print
  # with exceptions listed in `object` as varname-timestep pairs. eg. `object=c(basin_wb='yearly')`
  # specifies to print only the yearly data for the 'basin_wb' file (and none of the others).
  # Unnamed strings in `object` specify to print all files for that timestep (regardless of any
  # more specific exceptions in `object`), eg. both `object=c(basin_wb='yearly', 'yearly')` and
  # `object='yearly'` have the effect of printing all yearly files. 
  #
  # If `dates` is supplied, the 'print.prt' and 'time.sim' files are updated to match the
  # its range; Otherwise the existing values in the file are used.
  #
  # Note: this does not alter the time step of the simulation (in parameter `step` of
  # "time.sim"), or attempt to detect it from `dates`
  
  # TODO: detect this in pyqgis module
  # set the default executable path
  exe = 'C:/SWAT/SWATPlus/SWATPlusEditor/resources/app.asar.unpacked/swat_exe/rev60.5.2_64rel.exe'
  
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
  
  # build system call and run SWAT
  syscall.string = paste(shell.prefix, tools::file_path_sans_ext(normalizePath(exe)))
  invisible(shell(syscall.string, intern=quiet))
  if(!quiet) cat('\n>> finished\n')
}

#' run a daily SWAT+ simulation of a physical variable, for a given location and time period 
rswat_daily = function(dates=NULL, loc=1, ofile=NULL, vname=NULL, exec=TRUE, quiet=FALSE)
{
  # ARGUMENTS:
  # 
  # 'dates': vector or dataframe containing the dates to simulate (see DETAILS)
  # 'loc': integer, character, or sf geometry; specifying the location (see DETAILS) 
  # 'ofile': character vector or list, the desired output file(s) (see DETAILS)
  # 'vname': character vector, the SWAT+ name of the physical variable(s) to return
  # 'exec': boolean, if FALSE the function loads existing results without running a new simulation
  # 'quiet': boolean, indicating to suppress console messages
  #
  # `object`: character vector, with entries from 'daily', 'monthly', 'yearly', 'avann'
  #
  # RETURN:
  # 
  # A dataframe containing the simulated data for the requested time period.
  #
  # DETAILS:
  # 
  # Runs a simulation by calling the SWAT+ executable (this can be slow!), then loads
  # the output into R. `rswat_cio` must be run first to set 'ciopath'. 
  # 
  # Argument 'dates' can be a dataframe containing a 'date' column (eg. pass a streamgage
  # observation dataset to get back matching simulated data), or a vector of dates. The 
  # function will run a simulation that covers the entire period (filling any gaps), so
  # the desired time series can also be specified by `dates=c(start_date, end_date)`.
  # When 'dates' is supplied, the function writes the corresponding values to the files
  # 'print.prt', and 'time.sim'. Otherwise the simulation runs over whatever period is
  # currently set in those files.
  #
  # TODO: handle different types of physical variables through loc and vname. For now the
  # function just requests the channel flow data for the main outlet

  # check if `rswat` has been initialized with a project directory
  err.msg = 'Either supply `textio` or run `rswat_cio` to set default'
  if( !exists('.rswat') ) stop(err.msg)
  if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
  
  # pull the directory from rswat environment
  textio = dirname(.rswat$ciopath)
  
  # find the object id value associated with the supplied location if necessary
  if( !is.numeric(loc) )
  {
    # # attempt to find the watershed shapefile
    # shp.dir = gsub('Scenarios.+', 'Watershed/Shapes', textio)
    # shp.fn = list.files(shp.dir)
    # shp.path = file.path(shp.dir, shp.fn[ grepl('riv.+\\.shp', shp.fn) ])
    # 
    # # warn of multiple matches
    # if( length(shp.path) > 1 ) warning(paste('more than one channels shapefile found in', shp.dir))
    # 
    # # handle no-match case
    # if( length(shp.path) > 0 ) 
    # {
    #   # set default `oid` when the shapefile can't be found 
    #   oid = 1
    #   
    # } else {
    #   
    #   # load the first of the results and snap gage record site 
    #   riv = read_sf(shp.path)
    #   idx.riv = which.min(st_distance(oid, riv))
    #   oid = riv$Channel[idx.riv]
    # }
    
    stop('non-integer `loc` not implemented yet!')
    # TODO: write methods for finding gis_id based on location
  }
  
  # determine the requested output file
  #rswat_output()
  
  
  # message before writing any required changes to print.prt and time.sim
  if( !quiet ) cat('> setting up time.sim and print.prt...\n')
  
  # write time period info to print.prt and time.sim
  if( !is.null(dates) )
  {
    # handle dataframe input
    if( is.data.frame(dates) ) dates = dates$date

    # check for valid input 
    dates = as.Date( dates[!is.na(dates)] )
    if( length(dates) == 0 ) stop('no non-NA entries in `dates`')
    
    # set the start and end dates
    pars.tochange = c('day_start', 'yrc_start', 'day_end', 'yrc_end')
    dstart = as.integer(format(min(dates), '%j'))
    ystart = as.integer(format(min(dates), '%Y'))
    dend = as.integer(format(max(dates), '%j'))
    yend = as.integer(format(max(dates), '%Y'))
    
    # load 'time.sim' and set 'step' = 0 for daily timestep operation
    time.sim = rswat_open('time.sim', quiet=TRUE)
    time.sim$step = 0
    
    # make any start/end date changes to 'time.sim' and overwrite on disk
    time.sim[pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(time.sim, preview=F, quiet=TRUE)
    
    # load 'print.prt' and set 'nyskip' = 0 to ensure we can see all of the output
    print.prt = rswat_open('print.prt', quiet=TRUE)
    print.prt[[1]]$nyskip = 0
    
    # make any changes to start/end dates and overwrite 'print.prt' on disk
    print.prt[[1]][pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(print.prt[[1]], preview=F, quiet=TRUE)
  }
  
  # handle specification of output filenames ("objects") in 'print.prt'
  object = NULL
  if( !is.null(object) )
  {
    # only load 'print.prt' as needed
    if( is.null(dates) ) print.prt = rswat_open('print.prt', quiet=TRUE)
    
    # grab the full list of valid object names
    object.all = print.prt[[5]]$objects
    
    # reset all values to no-print, then toggle requested files
    print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
    print.prt[[5]]$daily[ object.all %in% object ] = 'y'

    # write the changes to print.prt
    rswat_write(print.prt[[5]], preview=F, quiet=TRUE)
  }

  # run the simulation and return output data as R dataframe
  rswat_exec(quiet=quiet)
  return( rswat_output(object.nm, vname) %>% filter( gis_id == loc ) )
}


#' ## miscellaneous and functions in development

#' returns an objective function whose argument is vector of parameter values, output is NSE
my_objective = function(cal, gage, textio, quiet=TRUE)
{
  # ARGUMENTS:
  # 
  # `cal`: dataframe, the SWAT+ parameters to modify
  # `gage`: dataframe, containing flow and date (see `my_gage_objective`)
  # `textio`: character, path to the text I/O directory for the SWAT+ model 
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
  function(x=NULL, refresh=FALSE, draw=FALSE)
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
    return(my_gage_objective(gage, textio, quiet=quiet, draw=draw))
  }
}

# TODO: work on this
#' set default bounds for a calibration parameter using data in cal_parms.cal
my_bounds = function(param, fuzzy=TRUE)
{
  # initialize defaults and open cal_parms.cal
  bds.out = c(-Inf, Inf)
  bds.all = rswat_open('cal_parms.cal')
  
  # find exact matches
  idx.exact = bds.all$name == param
  if(any(idx.exact))
  {
    # finished
    return(c(bds.all$abs_min[idx.exact], bds.all$abs_max[idx.exact]))
    
  } else {
    
    # TODO: vectorize (or at least put in a loop)
    warn.msg = paste(param, 'had no exact matches in cal_parms.cal.')
    
    # handle fuzzy matching
    if(fuzzy)
    {
      # try fuzzy matching at different levels (Levenshtein edits distances)
      idx.f1 = agrep(param, bds.all$name, max.distance=1)[1]
      idx.f2 = agrep(param, bds.all$name, max.distance=2)[1]
      idx.f3 = agrep(param, bds.all$name, max.distance=3)[1]
      
      # fuzzy level 1
      if( !is.na(idx.f1 > 1) )
      {
        warning(paste(warn.msg, 'Using fuzzy match', bds.all$name[idx.f1], '(distance=1)'))
        bds.out = c(bds.all$abs_min[idx.f1], bds.all$abs_max[idx.f1])
        return(bds.out)
      }
      
      # fuzzy level 2
      if( !is.na(idx.f2 > 1) )
      {
        warning(paste(warn.msg, 'Using fuzzy match', bds.all$name[idx.f2], '(distance=2)'))
        bds.out = c(bds.all$abs_min[idx.f2], bds.all$abs_max[idx.f2])
        return(bds.out)
      }
      
      # fuzzy level 3
      if( !is.na(idx.f3 > 1) )
      {
        warning(paste(warn.msg, 'Using fuzzy match', bds.all$name[idx.f3], '(distance=3)'))
        bds.out = c(bds.all$abs_min[idx.f3], bds.all$abs_max[idx.f3])
        return(bds.out)
        
      }
    } else { warning(paste(warn.msg, 'Reverting to -Inf, Inf')) }
    
  }
  
  return(bds.out)
}

#' write weather input text files for QSWAT and SWAT2012
my_swat_wmeteo = function(wdat, exdir, form='qswat', include=logical(0), suffix='', quiet=FALSE)
{
  # ARGUMENTS:
  #
  # `wdat`: list of time series data: 'coords_sf', 'dates', 'tables', 'elevation' (see DETAILS) 
  # `exdir`: character string, path to directory to write output files
  # `form` : character string specifying output structure, either 'qswat' (the default) or 'swat'
  # `include`: (optional) boolean vector of same length as `wdat$coords_sf`, for writing subsets
  # `suffix`: (optional) suffix to append to filenames
  # `quiet`: logical, suppresses console messages
  #
  # RETURN VALUE:
  #
  # A list of all filenames written and their directory (`exdir`)
  #
  #
  # DETAILS: 
  #
  # Writes one of two different file structures, depending on `form`:
  #
  # For `swat`, we write one file per SWAT variable found in `wdat`. This contains (lat/long)
  # coordinates and the full time series of all point data sources (eg. 'pcp1.pcp' contains
  # precipitation data from all locations, as a numeric matrix). Elevation is also written to
  # these files (as integer) whenever it is supplied in `wdat`. Note, however, that the elevations
  # stored in these files are ignored by SWAT, which bases any elevation-related calculations on
  # the subbasin elevations listed in the *.sub files. Similarly, SWAT maps point weather data
  # sources to subbasins based on the integer IDs listed in the *.sub files, and not the lat/long
  # values in the weather input files (eg. integer ID "23" refers to the 23rd column of 'pcp1.pcp')
  #
  # With 'qswat', the point data location info DOES matter (it is used by QSWAT to construct the
  # *.sub files), and is stored separately from the actual time series data: Each weather variable
  # has a single text file (eg. 'pcp.txt') that lists the point coordinates and their elevations;
  # whereas the time series data go into location-specific files (eg. 'tmp_grid23.txt'). It's not
  # clear to me whether the elevation data are important in the 'qswat' case, but you should
  # probably include them to be on the safe side.
  #
  # The required entries of `wdat` are:
  #
  #   coords_sf: sfc POINT object with `name` attribute (unique names for each point data source)
  #   dates: vector of Date objects, in same order as the rows of the weather tables
  #   tables: named list of dataframes, one per variable, where column names match coords_sf$name
  #   elevation: (optional) named list of numeric elevations (in metres)
  # 
  # Supported SWAT variable names are: 
  # 
  # 'pcp' (precip, in mm/day)
  # 'tmin' and 'tmax' (temperature in degrees Celsius, where both min and max must be specified)
  # 'slr' (daily solar radiation, in MJ/m2)
  # 'wnd' (average speed, in m/s)
  # 'hmd' (relative humidity, expressed as fraction)
  # 
  # Entries of `wdat$tables` that do not match these names are ignored (nothing written). Note
  # that since 'tmin' and 'tmax' appear in the same SWAT input file ('.tmp'), they must both be
  # supplied or the function will write neither.
  #
  # Output filenames in 'swat' mode have the form '<varname><suffix>.<varname>' (eg. with
  # `suffix=1`, the precip data is written to the file 'pcp1.pcp'). In 'qswat' mode, they have the
  # form '<varname>.txt' for location data, and '<varname><suffix>_<pointname>.txt' for time series
  # data (eg. 'pcp1.txt' contains the location info for time series files 'pcp1_grid1.txt',
  # 'pcp1_grid2.txt', etc). The only exception is for temperature, where there is a single '.tmp'
  # file instead of separate tmin and tmax files.
  #
  # The optional `include` vector specifies (as boolean index) a subset of `wdat$coords_sf` to
  # include in the output. Note that for 'swat' mode, this controls the number of columns in the
  # output, and therefore the mapping (of columns to subbasins) in *.sub files should be adjusted
  # accordingly. By default all the data are written.
  #
  
  # missing data field (NA) is coded as "-99.0"
  na.value = -99
  
  # set EPSG code for latitude/longitude
  epsg.geo = 4326
  
  # define SWAT variable names, and expected input variable names
  vn.list = list(pcp='pcp', slr='slr', wnd='wnd', hmd='hmd', tmp=c('tmax', 'tmin'))
  
  # first date in the time series
  origin.date = wdat$dates[1]
  
  # check for invalid `form` argument
  if(!(form %in% c('qswat', 'swat')))
  {
    stop('`form` must either be "qswat" or "swat"') 
  }
  
  # if elevations not supplied, use missing data value
  if(is.null(wdat$elevation))
  {
    wdat$elevation =  setNames(rep(na.value, nrow(wdat$coords_sf)), wdat$coords_sf$name)
  }
  
  # handle unassigned `include` argument
  if(length(include)==0)
  {
    # default behaviour is to write all points
    include = setNames(rep(TRUE, nrow(wdat$coords_sf)), wdat$coords_sf$name)
  }
  
  # build index of `wdat` to write, extract geographic coordinates
  coords = wdat$coords_sf[include,]
  coords.geo = st_coordinates(st_transform(coords, epsg.geo))
  n.coords = nrow(coords)
  
  # contruct table of geographic (lat/long) coordinates, append elevations
  coords.tab = setNames(data.frame(coords.geo, row.names=coords$name), c('long', 'lat'))
  coords.tab$elevation = wdat$elevation[include]
  
  # it's unclear if there is a limit on precision - 3 decimal places should be plenty
  coords.tab = round(coords.tab, 3)
  
  # check for input matching SWAT variable names
  vn.idx = sapply(vn.list, function(nm) all(nm %in% names(wdat$tables)))
  vn.in = names(vn.idx)[vn.idx]
  
  # warn of unpaired temperature input (one but not both of 'tmin' or 'tmax' supplied)
  if(!vn.idx['tmp'] & any(c('tmin', 'tmax') %in% names(wdat$tables)))
  {
    warning('one of tmin/tmax not supplied. Temperature data will not be written')
  }
  
  # construct a list of strings to use as station-variable names (filenames, in 'qswat' mode)
  svn = setNames(lapply(vn.in, function(vn) paste(paste0(vn, suffix), coords$name, sep='_')), vn.in)
  
  # set up output directory (creates it if it doesn't exist)
  my_dir(exdir)
  
  # replace NA fields with numeric flag for missing data
  wdat$tables = lapply(wdat$tables, function(xx) xx %>% replace(is.na(.), na.value))
  
  # generate SWAT-readable text input files
  if(tolower(form)=='swat')
  {
    # Note that the first 4 lines of these files are treated as comments by SWAT
    l2.string = paste(c('Lati  ', substring(as.character(coords.tab[['lat']]), 1, 4)), collapse=' ')
    l3.string = paste(c('Long  ', substring(as.character(coords.tab[['long']]), 1, 4)), collapse=' ')
    l4.string = paste(c('Elev  ', substring(as.character(coords.tab[['elev']]), 1, 4)), collapse=' ')
    
    # define paths to the output files (one per variable)
    wstn.path = sapply(vn.in, function(vn) file.path(exdir, paste0(vn, suffix, '.txt')))
    
    # prepare Julian date-year strings that start each line
    dates.string = format(wdat$dates[include], '%Y%j')
    
    # write the variables one at a time in a loop
    if(!quiet) pb = txtProgressBar(max=length(vn.in), style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      l1.string = paste('Station ', paste(svn[[vn]], collapse=','))
      if(!quiet) print(paste('writing', basename(wstn.path[vn]), 'to directory', exdir))
      
      # handle temperature data, which requires concatentation of two variables
      if(vn=='tmp')
      {
        # we get 10 characters total to write the minmax vals (with no delimiters), or 5 per extremum
        tsmin.matrix = sapply(wdat$tables[['tmin']][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        tsmax.matrix = sapply(wdat$tables[['tmax']][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        
        # concatenate the max and min (entrywise), then concatenate lines
        ts.matrix = t(sapply(1:nrow(tsmin.matrix), function(idx) paste0(tsmax.matrix[idx,], tsmin.matrix[idx,])))
        ts.out = t(apply(ts.matrix, 1, paste0))
        
      } else {
        
        # handle standard variables, creating matrix of values, then concatenating by line
        ts.matrix = sapply(wdat$tables[[vn]][, include], function(xx) sprintf(xx, fmt='%05.1f'))
        ts.out = t(apply(ts.matrix, 1, paste0))
        
      }
      
      # append dates to beginning of each line of numeric data 
      ts.lines = sapply(1:nrow(ts.out), function(idx) paste(c(dates.string[idx], ts.out[idx,]), collapse=''))
      
      # append comment lines and write to disk
      writeLines(c(l1.string, l2.string, l3.string, l4.string, ts.lines), con=wstn.path[vn])
      if(!quiet) setTxtProgressBar(pb, idx.vn)
      
    }
    if(!quiet) close(pb) 
    
    # finish and return filenames in list
    return(list(exdir=exdir,
                stations=setNames(basename(wstn.path), vn.in)))
    
  }
  
  # generate QSWAT-readable text input files
  if(tolower(form)=='qswat')
  {
    # build wdat station file tables for QSWAT as matrices of text
    wdat.wstn = lapply(setNames(nm=vn.in), function(vn) { 
      cbind(ID=1:n.coords, 
            NAME=svn[[vn]], 
            LAT=unname(coords.tab['lat']),
            LONG=unname(coords.tab['long']),
            ELEVATION=unname(coords.tab['elevation']))
    })
    
    # define paths to the output files (with time series stored separately from station location info)
    wstn.path = sapply(names(svn), function(fn) file.path(exdir, paste0(fn, suffix, '.txt')))
    wstn.ts.path = lapply(svn, function(fn) setNames(file.path(exdir, paste0(fn, '.txt')), nm=coords$name))
    
    # write the station location data
    sapply(names(svn), function(vn) write.csv(wdat.wstn[[vn]], file=wstn.path[vn], quote=F, row.names=F))
    
    # the first line of each time series data file is the origin date (without spaces) 
    origin.string = paste0(gsub('-', '', origin.date))
    
    # write the station time series data in a loop
    if(!quiet) pb = txtProgressBar(max=length(vn.in)*n.coords, style=3)
    for(idx.vn in 1:length(vn.in))
    {
      # print name of the variable to write
      vn = vn.in[idx.vn]
      if(!quiet) print(paste('writing', n.coords, vn, 'files to directory', exdir))
      
      # loop over grid point locations
      for(idx.coords in 1:n.coords)
      {
        # identify point name and output path for the text file
        stn.name = coords$name[idx.coords]
        out.path = wstn.ts.path[[vn]][stn.name]
        if(!quiet) setTxtProgressBar(pb, idx.coords + (idx.vn-1)*n.coords)
        
        # handle temperature data, which requires concatentation of two variables
        if(vn=='tmp')
        {
          # write txt containing comma-separated concatenation of variables by row
          ts.out = sapply(wdat$tables[vn.list[['tmp']]], function(dat) dat[[stn.name]])
          writeLines(c(origin.string, apply(ts.out, 1, paste, collapse=',')), con=out.path)
          
        } else {
          
          # write txt for usual case of a single variable in each file (eg. prec, wind)
          ts.out = wdat$tables[[vn.list[[vn]]]][[stn.name]]
          writeLines(c(origin.string, ts.out), con=out.path)
          
        }
        
      }
      
    }
    if(!quiet) close(pb) 
    
    # finish and return filenames in list
    return(list(exdir=exdir,
                stations=setNames(basename(wstn.path), vn.in), 
                data=lapply(wstn.ts.path, basename)))
  }
  
}

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


#' The following two functions are meant for future code optimization, where it makes sense
#' to disable the default SWAT+ printouts and enable a specific object hydrograph (OHG).
#' These OHG files are specific to a particular 'gis_id', so by omitting a lot of redundant
#' information they speed up execution quite a bit.
#' 
#' The plan is to initially execute a simulation once with ALL default printouts enabled, in
#' order to get the available output names and their units (there are no units rows in the OHG
#' files). eg this could be done as part of `qswat_run` or `rswat_cio`, and the mappings could
#' be stored in the package environment (similar to .rswat$linedf for config files)
#' 
#' From there it would be straightforward to hide the intermediate output file operations from
#' the user - the goal is have a function that can request simulations of a specific variable
#' in a specific time/place, without having to figure out which file(s) are required to make
#' that happen  


#' create an object hydrograph output specification file, modifying file.cio appropriately
rswat_ohg = function(overwrite=FALSE, otype='sdc', oid=1, htype='tot', fname=NULL, ciopath=NULL)
{
  #
  # ARGUMENTS
  #
  # 'overwrite': boolean, whether the overwrite the destination file if it exists already
  # 'otype': character, one of 'hru', 'sdc', ... (possibly 'res', and others? see Details) 
  # 'oid': integer, the "object number", an ID code associated with the desired object
  # 'htype': character, one of 'tot', 'sur', 'lat', 'til', 'rhg', ... (object-dependent)
  #
  # RETURN:
  #
  # Returns a dataframe containing the table written to 'object.prt'
  # 
  # DETAILS
  #
  # Writes the file 'object.prt' to `ciopath`, containing the table of output hydrograph
  # parameters, and modifies 'file.cio' to refer to the newly created file `fname`
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
  
  # the output file to write (in addition to file.cio)
  ofile = 'object.prt'
  ofile.len = nchar(ofile)
  
  # column widths: entry 2 is for last column (the filename), entry 1 for everything else
  cwidth = c(7, 50)
  
  # set default path for 'file.cio' when `rswat_cio` has been called
  if( is.null(ciopath) )
  {
    if( exists('.rswat') )
    {
      # copy the path from the package environment
      ciopath = .rswat$ciopath
      
      # handle missing ciopath
      if( is.null(ciopath) ) stop('ciopath not found. Try setting it with rswat_cio')
      
    } else stop('"file.cio" not found. Either call `rswat_cio` or provide ciopath')
  }
  
  # set a default filename when it is not specified manually
  if( is.null(fname) ) 
  {
    fname = paste0(paste('simulation', otype, oid, htype, sep='_'), '.txt')
  }
  
  # construct a comment line to print to the txt file
  comment.text = paste('object.prt: written by rswat on', Sys.time())
  
  # bundle the file data into a list
  headers = c('numb', 'obtyp', 'obtypno', 'hydtyp', 'filename')
  value = c(as.list(headers), list(1, otype, oid, htype, fname))
  nhead = length(headers)
  
  # determine column start/end positions - repeated for header then data line
  cstart = rep( ( cwidth[1] + 1 ) * ( ( 1:nhead ) - 1 ), 2)
  cend = cstart + c(rep(cwidth[1], nhead - 1), cwidth[2])
  
  # assign classes and other metadata about the table and its headers
  oclass = c('integer', 'character', 'integer', 'character', 'character')
  linedf.out = data.frame(class = c(rep('character', nhead), oclass),
                          start_col = cstart,
                          end_col = cend,
                          rjust_col = rep(FALSE, 2*nhead))
  
  # define padded strings to concatenate to form each line as list
  out.split = split( rswat_2char(value, linedf.out)$string_padded,  
                     f = !( 1:(2*nhead) %in% 1:nhead ) )
  
  # concatenate the list as character vector
  out.lines = c(comment.text, 
                do.call(c, lapply(out.split, function(x) paste(x, collapse=' '))))
  
  # check if the file exists already
  prtpath = file.path(dirname(ciopath), ofile)
  if( file.exists(prtpath) )
  {
    msg.warn = paste('file', prtpath, 'already exists. Set overwrite=TRUE to modify it')
    if( !overwrite )
    {
      warning(msg.warn)
      return()
      
    } else cat(paste('overwriting', prtpath, '\n'))
    
  } else cat(paste('writing to', prtpath, '\n'))
  
  # print to output file
  writeLines(out.lines, prtpath)
  
  # open file.cio and identify the line number and character position to modify
  rswat_rlines(ciopath) 
  linedf.cio = .rswat$stor$temp[['file.cio']]$linedf
  lnmod = linedf.cio %>% filter(string=='simulation') %>% pull(line_num)
  cposmod = linedf.cio %>% filter(line_num==lnmod, field_num==4) %>% pull(start_pos)
  
  # modify the line and write the changes to file.cio
  substr(.rswat$stor$txt[['file.cio']][lnmod], cposmod, cposmod + ofile.len) = ofile
  writeLines(.rswat$stor$txt[['file.cio']], ciopath)
  
  # remove the 'file.cio' data from package environment 
  .rswat$stor$txt = .rswat$stor$txt[names(.rswat$stor$txt) != 'file.cio']
  .rswat$stor$temp = .rswat$stor$temp[names(.rswat$stor$temp) != 'file.cio']
  
  # refresh files list and load the new file
  rswat_cio(ciopath)
  rswat_open('object.prt', quiet=TRUE)
}





#' ## Phasing out

#' TODO: phase out this function (replaced by rswat_daily)
#' evaluate errors in prediction for a SWAT+ model
my_gage_objective = function(gage, textio=NULL, oid=NULL, quiet=FALSE, draw=FALSE, exec=TRUE)
{
  # 'gage': dataframe of flow and date
  # 'textio': character, path to the text I/O directory for the SWAT+ model 
  # 'oid': (optional) sf point or integer id code
  # 'draw': logical, indicates to plot the results (or add to plot, if one exists already)
  # 'exec': logical, indicates to run the simulation (else uses existing output files)
  #
  # for now we only support daily timesteps
  
  # set default `textio` directory if `rswat` has been initialized with a project directory
  if( is.null(textio) )
  {
    # check if rswat has been initialized
    err.msg = 'Either supply `textio` or run `rswat_cio` to set it memory'
    if( !exists('.rswat') ) stop(err.msg)
    if( !exists('ciopath', envir=.rswat) ) stop(err.msg)
    
    # pull the directory from rswat environment
    textio = dirname(.rswat$ciopath)
  }
  
  # find the channel id value associated with the supplied point if necessary
  if( !is.integer(oid) )
  {
    # attempt to find the watershed shapefile
    shp.dir = gsub('Scenarios.+', 'Watershed/Shapes', textio)
    shp.fn = list.files(shp.dir)
    shp.path = file.path(shp.dir, shp.fn[ grepl('riv.+\\.shp', shp.fn) ])
    
    # warn of multiple matches
    if( length(shp.path) > 1 ) warning(paste('more than one channels shapefile found in', shp.dir))
    
    # handle no-match case
    if( length(shp.path) > 0 ) 
    {
      # set default `oid` when the shapefile can't be found 
      oid = 1
      
    } else {
      
      # load the first of the results and snap gage record site 
      riv = read_sf(shp.path)
      idx.riv = which.min(st_distance(oid, riv))
      oid = riv$Channel[idx.riv]
    }
  }
  
  # TODO: detect this automatically from `gage`
  # assign objects to write and dates to simulate
  object = c(channel_sd='daily')
  vname = 'flo_out'
  dates = gage$date
  fname = paste0(names(object), '_day')
  
  # run the simulation and extract response data
  if(exec) rswat_run(textio, dates=dates, object=object, quiet=quiet)
  sim = rswat_output(fname, vname) %>% filter(gis_id==oid)
  
  # TODO: optimize this inner join
  # handle skipped years, missing days etc
  idx.gage = match(sim$date, gage$date)
  qsim = gage$flow[idx.gage]
  qobs = sim[[vname]]
  
  # TODO: add alternatives
  # compute objective function value
  obj.val = my_nse(qsim, qobs, L=2)
  
  # plot the data, if requested 
  if(ifelse(is.logical(draw), draw, TRUE))
  {
    # find a color for the normalized NSE
    qalpha = 0.5
    tcol = rainbow(60)[as.integer(format(Sys.time(), '%M'))]
    qcol = ifelse(is.logical(draw), tcol, draw)
    if(is.numeric(qcol))
    {
      qalpha = qcol
      qcol = 'blue'
    }
    
    # initialize the plot if required
    idx.plot = gage$date %in% sim$date
    if( dev.cur()==1 ) plot(flow~date, data=gage[idx.plot,], pch='')
    
    # add new simulation data
    lines(flow~date, data=gage[idx.plot,], lwd=2)
    lines(flo_out~date, data=sim, col=adjustcolor(qcol, alpha=qalpha))
    
    # wipe title and overwrite with objective value
    obj.msg = paste('score =', round(obj.val, 3))
    ovr.msg = paste(rep('\U2588', 25), collapse='')
    title(ovr.msg, col.main='white', adj=0)
    title(obj.msg, col.main=qcol, adj=0)
    
  }
  
  return(obj.val)
  # TODO: allow different return value (residuals, fitted, etc)
}

#' TODO: phase out this function (replaced by rswat_exe)
#' run the SWAT+ executable (NOTE: requires `rswat` helper function)
rswat_run = function(textio, dates=NULL, info=FALSE, object=NULL, quiet=FALSE)
{
  # 'textio': character, path to the text I/O directory for the SWAT+ model 
  # `dates`: (optional) vector of Dates from which to derive simulation period
  # `info`: logical, whether to return current time.sim and print.prt instead of running SWAT+
  # `object`: character vector, with entries from 'daily', 'monthly', 'yearly', 'avann'
  # `quiet`: logical, suppresses console messages
  #
  # `object`, if supplied, specifies the output files to write, where the default is to not print
  # with exceptions listed in `object` as varname-timestep pairs. eg. `object=c(basin_wb='yearly')`
  # specifies to print only the yearly data for the 'basin_wb' file (and none of the others).
  # Unnamed strings in `object` specify to print all files for that timestep (regardless of any
  # more specific exceptions in `object`), eg. both `object=c(basin_wb='yearly', 'yearly')` and
  # `object='yearly'` have the effect of printing all yearly files. 
  #
  # If `dates` is supplied, the 'print.prt' and 'time.sim' files are updated to match the
  # its range, so that the simulation runs the full time period in `dates`.
  #
  # Note: this does not alter the time step of the simulation (in parameter `step` of
  # "time.sim"), or attempt to detect it from `dates`
  
  # TODO: detect this in pyqgis module
  # set the default executable path
  exe = 'C:/SWAT/SWATPlus/SWATPlusEditor/resources/app.asar.unpacked/swat_exe/rev60.5.2_64rel.exe'
  
  # handle dates
  if( !is.null(dates) )
  {
    # check for valid input 
    dates = dates[!is.na(dates)]
    if( length(dates) == 0 ) stop('no non-NA entries in `dates`')
    if( !is(dates, 'Date') ) stop('supplied `dates` not of "Date" class')
    
    # set the start and end dates
    pars.tochange = c('day_start', 'yrc_start', 'day_end', 'yrc_end')
    dstart = as.integer(format(min(dates), '%j'))
    ystart = as.integer(format(min(dates), '%Y'))
    dend = as.integer(format(max(dates), '%j'))
    yend = as.integer(format(max(dates), '%Y'))
    
    # write any date changes to time.sim
    if( !quiet ) print('> writing time.sim')
    time.sim = rswat_open('time.sim', quiet=quiet)
    time.sim[pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(time.sim, preview=F, quiet=quiet)
    
    # write any date changes for print.prt
    if( !quiet ) print('> writing print.prt')
    print.prt = rswat_open('print.prt', quiet=quiet)
    print.prt[[1]][pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(print.prt[[1]], preview=F, quiet=quiet)
  }
  
  # deal with the effects of `object` on print.prt
  if( !is.null(object) )
  {
    # load print.prt if we don't have it already
    if( is.null(dates) ) print.prt = rswat_open('print.prt', quiet=quiet)
    
    # grab the full list of valid object names
    object.all = print.prt[[5]]$objectects
    object.nm = names(object)
    
    # expand the unnamed entries to include all files for that timestep
    object.exp = lapply(object, function(x) setNames(rep(x, length(object.all)), object.all))
    object.exp[ object.nm != '' ] = object[object.nm != '']
    object = do.call(c, object.exp)
    
    # reset all values to no-print
    print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
    
    # a simple loop suffices as this array shouldn't ever get very large
    for(idx in 1:length(object))
    {
      nm = names(object[idx])
      print.prt[[5]][print.prt[[5]]$object == nm, object[idx]] = 'y'
    }
    
    # write the changes to print.prt
    rswat_write(print.prt[[5]], preview=F, quiet=quiet)
  }
  
  # shell command prefix to change to the directory (avoids changing R's working directory)
  shell.prefix = paste0('pushd ', normalizePath(textio), ' &&')
  
  # build system call and run SWAT
  syscall.string = paste(shell.prefix, tools::file_path_sans_ext(normalizePath(exe)))
  invisible(shell(syscall.string, intern=quiet))
  if(!quiet) cat('\n>> finished')
}

## DEPRACATED: remove once rswat_daily() is tested and working
#' run a daily SWAT+ simulation of a physical variable, for a given location and time period 
rswat_daily2 = function(dates=NULL, loc=NULL, vname='flo_out', quiet=FALSE, textio=NULL)
{
  # ARGUMENTS:
  # 
  # 'dates': vector or dataframe containing the dates to simulate (see DETAILS)
  # 'loc': integer, character, or sf geometry; specifying the location (see DETAILS) 
  # 'vname': character vector, the SWAT+ name of the physical variable(s) to return
  # 'fname': character vector, the SWAT+ filename for the desired output file(s)
  # 'quiet': boolean, indicating to supress console messages
  # 'textio': character, path to the text I/O directory for the SWAT+ model 
  #
  # `object`: character vector, with entries from 'daily', 'monthly', 'yearly', 'avann'
  #
  # RETURN:
  # 
  # A dataframe containing the simulated data for the requested time period.
  #
  # DETAILS:
  # 
  # Runs a simulation by calling the SWAT+ executable (this can be slow!), then loads
  # the output into R.
  # 
  # Argument 'dates' can be a dataframe containing a 'date' column (eg. pass a streamgage
  # observation dataset to get back matching simulated data), or a vector of dates. The 
  # function will run a simulation that covers the entire period (filling any gaps), so
  # the desired time series can also be specified by `dates=c(start_date, end_date)`.
  # When 'dates' is supplied, the function writes the corresponding values to the files
  # 'print.prt', and 'time.sim'. Otherwise the simulation runs over whatever period is
  # currently set in those files.
  #
  # TODO: handle different types of physical variables through loc and vname. For now the
  # function just requests the channel flow data for the main outlet
  #
  # The default path for 'textio' is the directory set by `rswat_cio` (if it has been
  # called already).
  
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
  
  # default `loc` id value (1) specifies main outlet
  if( is.null(loc) ) loc = as.integer(1)
  
  # find the object id value associated with the supplied location if necessary
  if( !is.integer(loc) )
  {
    # # attempt to find the watershed shapefile
    # shp.dir = gsub('Scenarios.+', 'Watershed/Shapes', textio)
    # shp.fn = list.files(shp.dir)
    # shp.path = file.path(shp.dir, shp.fn[ grepl('riv.+\\.shp', shp.fn) ])
    # 
    # # warn of multiple matches
    # if( length(shp.path) > 1 ) warning(paste('more than one channels shapefile found in', shp.dir))
    # 
    # # handle no-match case
    # if( length(shp.path) > 0 ) 
    # {
    #   # set default `oid` when the shapefile can't be found 
    #   oid = 1
    #   
    # } else {
    #   
    #   # load the first of the results and snap gage record site 
    #   riv = read_sf(shp.path)
    #   idx.riv = which.min(st_distance(oid, riv))
    #   oid = riv$Channel[idx.riv]
    # }
    
    stop('non-integer `loc` not implemented yet!')
    # TODO: write methods for finding gis_id based on location
  }
  
  # define "objects" in 'print.prt' (output files) based on the requested 'fname', 'vname'
  if( vname == 'flo_out' ) 
  {
    # TODO: generalize this with a lookup table
    
    # the SWAT+ object name as listed in 'print.prt'
    object = 'channel_sd'
    
    # the actual filename on disk (without extension)
    object.nm = paste0(object, '_day')
    
  } else {
    
    stop('this output variable is not supported yet!')
    
  }
  
  # message before writing any required changes to print.prt and time.sim
  if( !quiet ) cat('> setting up time.sim and print.prt...\n')
  
  # write time period info to print.prt and time.sim
  if( !is.null(dates) )
  {
    # handle dataframe input
    if( is.data.frame(dates) ) dates = dates$date
    
    # check for valid input 
    dates = as.Date( dates[!is.na(dates)] )
    if( length(dates) == 0 ) stop('no non-NA entries in `dates`')
    
    # set the start and end dates
    pars.tochange = c('day_start', 'yrc_start', 'day_end', 'yrc_end')
    dstart = as.integer(format(min(dates), '%j'))
    ystart = as.integer(format(min(dates), '%Y'))
    dend = as.integer(format(max(dates), '%j'))
    yend = as.integer(format(max(dates), '%Y'))
    
    # load 'time.sim' and set 'step' = 0 for daily timestep operation
    time.sim = rswat_open('time.sim', quiet=TRUE)
    time.sim$step = 0
    
    # make any start/end date changes to 'time.sim' and overwrite on disk
    time.sim[pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(time.sim, preview=F, quiet=TRUE)
    
    # load 'print.prt' and set 'nyskip' = 0 to ensure we can see all of the output
    print.prt = rswat_open('print.prt', quiet=TRUE)
    print.prt[[1]]$nyskip = 0
    
    # make any changes to start/end dates and overwrite 'print.prt' on disk
    print.prt[[1]][pars.tochange] = c(dstart, ystart, dend, yend)
    rswat_write(print.prt[[1]], preview=F, quiet=TRUE)
  }
  
  # handle specification of output filenames ("objects") in 'print.prt'
  if( !is.null(object) )
  {
    # only load 'print.prt' as needed
    if( is.null(dates) ) print.prt = rswat_open('print.prt', quiet=TRUE)
    
    # grab the full list of valid object names
    object.all = print.prt[[5]]$objects
    
    # reset all values to no-print, then toggle requested files
    print.prt[[5]][, names(print.prt[[5]]) != 'objects'] = 'n'
    print.prt[[5]]$daily[ object.all %in% object ] = 'y'
    
    # write the changes to print.prt
    rswat_write(print.prt[[5]], preview=F, quiet=TRUE)
  }
  
  # run the simulation and return output data as R dataframe
  rswat_exec(quiet=quiet)
  return( rswat_output(object.nm, vname) %>% filter( gis_id == loc ) )
}



#+ include=FALSE
#my_markdown('rswat')