rswat
================
Dean Koch
2021-03-05

**Mitacs UYRW project**

**rswat**: R functions for building, reading, and editing SWAT+
configuration files

This is an all-in-one set of tools for running scripted R workflows on a
SWAT+ project: A PyQGIS script wrapper (similar to SWAT+ AW) builds the
model in QSWAT+; and helper functions (rswat\_\*) handle the reading and
writing of TxtInOut files and execution of simulations (similar to
SWATPlusR), along many other miscellaneous tasks, like loading and
merging model geometry data.

I may turn some of this into a package eventually, especially the file
I/O interface, which took some work but has proven very useful.

references: [SWAT+](https://swat.tamu.edu/software/plus/) [SWAT+
Automatic Workflow
(AW)](https://celray.github.io/docs/swatplus_aw/introduction.html)
[SWATplusR](https://github.com/chrisschuerz/SWATplusR) \#\# dependencies

``` r
# `dplyr` syntax simplification for complex table operations 
library(dplyr)

# `data.table` faster table loading
library(data.table)
```

## initialization:

We may later add a start-up script to scan for likely paths to the SWAT+
executable, as well as the OSGEO4W root which is needed when setting up
the PyQGIS environment in a call to the QSWAT+ workflow wrapper (this
requires installation of QGIS-LTR, and the path will be platform
dependent\!)

config file I/O interface

NOTE: an environment `.rswat` is initialized here (overwriting anything
with that name). It stores file data and lookup tables, and only the
functions in this chunk should be touching it. Eventually I will make
this a package so it’s properly hidden

The I/O interface functions work by scanning the files listed in
‘file.cio’, tabulating their white-space delineated fields, then
merging the results into tables wherever there is a consistent pattern
of row-length and class (as detected by R’s built-in `is.numeric` and
some sensible rules for snapping and spacing. This will hopefully make
it robust version-specific differences in SWAT+ parameter names and file
structures (and future changes).

``` r
# TODO: make this a package

# (internal) environment to store the SWAT+ project file data
.rswat = new.env(parent=emptyenv())

# open 'file.cio' and list its contents
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
      }
      
    } else {

      # message in case the function hasn't been run, or something else goes wrong
      stop('file.cio not found. Initialize it with `rswat_cio(ciopath)`')
      
    }
    
  } else {
  
    # check that the filename is 'file.cio'
    if(basename(ciopath) != 'file.cio') warning('path does not point to "file.cio"!')
    
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
      mutate(size = units::set_units(file.info(path)$size, bytes)) %>%
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
    select(file, group, size, nline, nskip, ntab, nvar) %>%
    select_if(~!all(is.na(.)))
  
  # load files if requested then return up to date files list
  if(reload)
  {
    # load files into memory and update the cio with stats
    rswat_open(reload=TRUE) 
    cio = rswat_cio(trim=trim, wipe=FALSE, ignore=ignore)
  }
  return(cio)

}

# wrapper for various methods to load SWAT+ files
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
  # data associated with `swatname`. Each list entry is named after a source file, and consists
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
  
  # parse the `swatname` argument, assigning all non-ignored files by default
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

# search tool for SWAT+ parameter text, names, and filenames
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
  # up to the specified (Levenshtein) distance (see `base::agrep`).
  # 
  # check these examples: eg.`pattern` matches an element
  # of the 'name' column of a table, all parameters in that row are returned.
  #
  # eg. `pattern = "time.sim start"` returns matches for the pattern 'start' from
  # the file "time.sim" only
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
    name.match = 1:n.name %in% agrep(pattern, name.all, max.distance=fuzzy)
    if(intext) string.match = 1:n.name %in% agrep(pattern, string.all, max.distance=fuzzy)
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
  linedf.out = .rswat$stor$linedf[which(idx.include)[name.match | string.match], ]
  
  
  # for human readability
  if(trim)
  {
    # omit skipped lines, header lines, etc
    linedf.out = linedf.out %>% filter(!is.na(i))
    
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

# write a parameter value to its file
rswat_write = function(value, fname=NULL, tablenum=NULL, preview=TRUE, reload=TRUE, quiet=FALSE)
{
  # 
  # newvalue: list? dataframe? vector?
  #
  # `preview`: logical, returns a list of edits, without writing them to disk
  
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
  
  # handle no-change case
  if(!any(cn.new))
  {
    if(preview) return(linedf[integer(0),])
    return(invisible())
  }
  
  # build matrix of changed value indicators
  m.new = matrix(FALSE, nrow(value), ncol(value))
  m.new[,cn.new] = sapply(which(cn.new), function(cn) ! value.old[,cn] == value[,cn])
  
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

# load SWAT+ output files as dataframe
rswat_output = function(fname=NULL, vname=NULL, add_units=TRUE, add_dates=TRUE)
{
  # add .txt extension (if it has none)
  fname = ifelse(grepl('.txt', fname), fname, paste0(fname, '.txt'))
  
  # pull the project directory
  ciodir = dirname(.rswat$ciopath)
  if(is.null(ciopath)) stop('ciopath not found. Have you run rswat_cio() yet?')
  
  # anything in SWAT+ project dir with .txt extension is checked
  txt.fname = grep('.txt', list.files(ciodir), value=TRUE)
  txt.name = gsub('.txt', '', txt.fname)
  
  # return file list when `fname` not supplied
  if( length(fname)==0 ) return(txt.name)
  
  # catch invalid input
  if( !(fname %in% txt.fname) ) stop(paste(fname, 'not found in', ciodir))
  fpath = file.path(ciodir, fname)
  
  # catch `_warnings.txt` and similar
  if( grepl('warnings', fname) ) return(data.frame(msg=readLines(fpath)))
  
  # define the magic line numbers
  ln.msg = 1
  ln.head = 2
  ln.unit = 3
  ln.tab = 4
  
  # run the first few lines into memory and parse them using the package worflow
  rswat_rlines(fpath, nmax=ln.tab)
  rswat_rtext(fname)
  linedf = .rswat$stor$temp[[fname]]$linedf
  msg = .rswat$stor$txt[[fname]][ln.msg]
  
  # tidy up package environment
  .rswat$stor$temp[[fname]] = list()
  .rswat$stor$txt[[fname]] = list()
  
  # check for structural problems with the tables
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
  
  # extract column headers and their start positions, column classes
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
    
  } else {
    
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

    # TODO: make this a function
    if(add_units)
    {
      # extract unit strings 
      unit.nm = linedf$string[linedf$line_num == ln.unit]
      unit.start = linedf$start_pos[linedf$line_num == ln.unit]
      
      # build a dictionary of unusual unit strings and their R-readable equivalents
      # note: I'm assuming 'mton' means 'metric tonne', and not 'milliton' (ie kilogram)
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
      
      # snap units to columns
      head.unit = rep(NA, n.head)
      head.unit[sapply(unit.start, function(n) which.min(abs(n - head.start)))] = unit.nm 
      
      # assign non-NA units to output
      cn.unit = which( !is.na( head.unit[idx.head] ) )
      if(length(cn.unit) > 0)
      {
        dat.out[cn.unit] = mapply(function(x, y) units::set_units(x, y, mode='standard'), 
                                  x = as.list(dat.out)[cn.unit], 
                                  y = head.unit[idx.head][cn.unit], 
                                  SIMPLIFY=FALSE)
      }
    }
    
    # TODO: make this a function
    if(add_dates & n.tab > 0)
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
      dat.out = cbind(data.frame(date=all.dates), dat.out[ !(names(dat.out) %in% nm.omit) ])
    }
    
  }
  
  return(dat.out)
}

## internal functions for file I/O interface

# line reader for SWAT+ text files
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

# class detection to interpret SWAT text as R objects 
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

# read and interpret a SWAT+ configuration file
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

# make a table from a parsed SWAT+ file text 
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

# identify the first table with column names in a block of SWAT+ config text
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

# generate missing rows in linedf for incomplete lines in a SWAT+ table
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

# clean up positional data for a table in `linedf`
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

# identify the filename and table number associated with a dataframe
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

# convert a dataframe of SWAT+ parameters to their character representation
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
```
