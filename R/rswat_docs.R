#' ---
#' title: "rswat_docs"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **rswat_docs**: R functions for parsing and searching the SWAT+ inputs documentation PDF
#' 
#' See the vignette
#' [demo_rswat_docs](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_rswat_docs.md)
#' for a demonstration of how to use `rswat_docs`
#' 
#' Note that the functions in this Rscript are independent of the others loaded by "rswat.R"
#' ("rswat_config.R", "rswat_output.R", etc). ie you can download/source "rswat_docs.R" on its
#' own and it should work without issues.
#' 
#' TODO: look into parameters.bsn ('igen' missing, possibly others)
#' 
#' ## .rswat environment
#' 
#' We need a place to store the text data from the PDF. The line below defines an environment
#' that can be accessed from inside the functions below. It is unnecessary (but harmless) if you
#' have already sourced
#' [rswat_config.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md),
#' and is a temporary fix until I bundle things into a package.

# define (internal) environment to store the SWAT+ project file data
if( !exists('.rswat', mode='environment') ) .rswat = new.env( parent=emptyenv() )

#' 
#' ## libraries
#' 

#' [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) simplifies
#' and speeds up table operations.
library(data.table) 

#' [`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html) provides a pleasant
#' syntax for complicated table operations 
library(dplyr)

#' [`pdftools`](https://cran.r-project.org/web/packages/pdftools/index.html) renders
#' PDF textboxes as character vectors in R
library(pdftools)

#' 
#' ## core functions

#' case-insensitive search of SWAT+ IO documentation PDF from within R
rswat_docs = function(pattern=NULL, fname=NULL, fuzzy=0, descw=0.5, full=FALSE, quiet=FALSE, doc='b')
{
  # ARGUMENTS:
  # 
  # `pattern`, character, the search string to match against names and descriptions
  # `fname`, character, a SWAT+ filename to limit search results 
  # `fuzzy`, numeric, the fuzziness level to use in matching (see DETAILS)
  # `descw`, numeric in [0,1], the weight given to description (versus name) matches 
  # `full`, logical, whether to include full descriptions instead of abbreviated ones
  # `quiet`, logical, suppresses progress bar (passed to `rswat_pdf_open`)
  # `doc`, character, either 'inputs', 'outputs', or 'both' (or leading substrings)
  #
  # RETURN:
  #
  # If `pattern=NULL` (the default), the function to returns a dataframe listing the filenames
  # known to `rswat_docs`.
  #
  # For non-NULL `pattern`, the function returns a dataframe containing names and descriptions
  # of matching entries in the SWAT+ inputs/outputs documentation. One or both documents are
  # searched, depending on the value of `doc` (default both)
  #
  # DETAILS:
  #
  # All matching is case-insensitive. If `descw` is 0, only the variable names are searched;
  # If `descw` is 1 only the descriptions are searched. Intermediate values match both lists
  # and order the results by the weighted sum of the two string distances.
  #
  # Negative `fuzzy` will attempt to return exact matches only; fuzzy=0 also includes exact
  # substring matches. Higher `fuzzy` levels indicate to include approximate matches of
  # substrings up to the specified fuzziness level (non-integer `fuzzy` gets rounded up):
  #
  # fuzziness is defined by calling `my_adist` to compute a string distance against `pattern`
  # (based on Leveinstein distance and string length heuristics), and the resulting distance scores
  # are binned into groups of like values. `fuzzy` indicates how many of these groups to include in
  # search results. eg `fuzzy=1` includes (all elements) of the closest group, `fuzzy=2` includes
  # the two closest groups, and so on.
  
  # regex for filenames (assume lowercase)
  regex.fname = '^[a-z]+[-_]*[a-z]*[-_]*[a-z]+\\.[a-z]{2,3}$'
  
  # formatting for console printout headers
  lrule = c('\n~~~', '~~~\n') 
  
  # sort out search target(s) and initialize dataframe variables 
  include.in = grepl('^i|^b', doc)
  include.out = grepl('^o|^b', doc)
  filetable = vartable = NULL

  # make sure weighting factor lies in the correct range and the PDF(s) are loaded
  descw = max(min(descw, 1), 0)
  in.loaded = !is.null(.rswat$docs_in$filetable)
  out.loaded = !is.null(.rswat$docs_out$filetable)
  if( include.in & !in.loaded ) rswat_inputs_pdf_open(quiet=quiet, reload=TRUE)
  if( include.out & !out.loaded ) rswat_outputs_pdf_open(reload=TRUE)
  
  # copy the inputs database list from storage if needed
  if(include.in)
  {
    vartable = .rswat$docs_in$vartable
    filetable = .rswat$docs_in$filetable
  }
  
  # copy the outputs database list from storage if needed 
  if(include.out)
  {
    vartable = vartable %>% rbind(.rswat$docs_out$vartable)
    filetable = filetable %>% rbind(.rswat$docs_out$filetable)
  }
  
  # default return value is the files summary table
  if( is.null(pattern) & is.null(fname) ) return( filetable )
  
  # TODO: handle dataframe input similar to `rswat_write`
  if( is.data.frame(pattern) )
  {
    is.fname = TRUE
    stop('not yet implemented')
  }
  
  # handle integer input to `pattern`, treated as list of page numbers
  if( is.numeric(pattern) )
  {
    # can't request a page number in 'b' mode
    if(include.in & include.out) stop('doc must be set to "i" or "o" to specify the document')
  
    # load the raw text
    if( include.in ) rawtext = .rswat$docs_in$rawtext
    if( include.out  ) rawtext = .rswat$docs_out$rawtext
    
    # coerce to integer and correct range if necessary
    page.nmax = length(rawtext)
    page.warn = paste('valid page numbers are: 1, 2, ...,', page.nmax)
    if( ( pattern > page.nmax ) | ( pattern > page.nmax ) ) warning(page.warn)
    pattern = max(min(as.integer(pattern), page.nmax), 1)
    
    # extract printable text and append page number indicators
    page.msg = paste(lrule[1], 'page', pattern, lrule[2])
    cat.out = paste0(page.msg, rawtext[pattern])
    
    # print the data and finish
    cat(cat.out)
    return(invisible())
  }
  
  # pattern is treated as filename in special case 
  pattern.check = is.null(fname) & !is.null(pattern) & ( descw != 1 )
  pattern.isfname = ifelse(pattern.check, grepl(regex.fname, tolower(pattern)), FALSE)
  if( pattern.isfname )
  {
    # swap inputs when filename supplied as pattern
    fname = pattern 
    pattern = NULL
  }
  
  # default value for `fname` matches everything
  pattern.msg = paste0('\"', pattern, '\"')
  if( is.null(fname) ) fname = '*'
  
  # same for `pattern`, but we use an alternative console message in this case
  is.file = FALSE
  if( is.null(pattern) )
  {
    # set flag for alternate message and build the new string to cat
    is.file = TRUE
    pattern.msg = paste('file(s)', paste(fname, collapse=', '))
    
    # set special search pattern and fuzzy level
    pattern = '*' 
    fuzzy = 0
  }
  
  # prune to the relevant rows of the PDF text databases
  vartable = vartable %>% filter( grepl(fname, file) )
  filetable = filetable %>% filter( grepl(fname, file) )
  
  # catch unknown filename requests
  if( (fname != '*') & !( fname %in% filetable$file ) )
  {
    cat( paste('fname', paste0('\"', fname, '\"'), 'not recognized\n') ) 
    return( vartable %>% dplyr::select( name, file, pstart, description ) )
  }
  
  # initialize score vectors
  adist.nm = adist.desc = rep(1, nrow(vartable))
  
  # search names and descriptions as needed
  if( descw > 0 ) adist.desc = my_adist(pattern, vartable$description_full, lu.split=fuzzy<0)
  if( descw < 1 ) adist.nm = my_adist(pattern, vartable$name)
  
  # the first results are always the exact matches
  idx.ematch = ( adist.nm * adist.desc ) == 0
  
  # initialize ordered vector of match indices
  int.match = which(idx.ematch)
  
  # compute weighted sum of the two string distance scores
  adist.sum = ( (1 - descw) * adist.nm ) + ( descw * adist.desc)
  
  # as needed, find substring matches (string distance < 1), sort
  if( !(fuzzy < 0) )
  {
    # excluding exact matches from this index, sort according to weighted score
    idx.smatch = !(idx.ematch) & ( ( adist.nm < 1 ) | ( adist.desc < 1 ) )
    int.smatch = which(idx.smatch)[ order( adist.sum[idx.smatch] ) ]
    
    # add sorted indices to the stack of results
    int.match = c(int.match, int.smatch)
  }
  
  # approximate search mode
  if( fuzzy > 0 )
  {
    # sort remaining elements into bins of equal distance to `pattern` 
    idx.rem = !( idx.smatch | idx.ematch )
    adist.bin = unique( sort( round(adist.sum[idx.rem], 2) ) )
    
    # find matching elements from the closest bins, add sorted indices to stack
    idx.amatch = round(adist.sum, 2) %in% adist.bin[ seq_along(adist.bin) < ceiling(fuzzy) + 1 ]
    int.amatch = which(idx.amatch)[ order( adist.sum[idx.amatch] ) ]
    int.match = c(int.match, int.amatch)
  }
  
  # handle no-match cases
  if( length(int.match) == 0 )
  {
    # console message about failed search
    nomatch.msg = paste0('No exact matches for ', pattern.msg, '. ')
    
    # don't repeat search when fuzzy = -Inf
    if( fuzzy == -Inf )
    {
      # print a message and return empty dataframe
      cat( paste(nomatch.msg, '\n') )
      return( vartable %>% slice(int.match) %>% dplyr::select( name, file, pstart, description ) )
    }
    
    # increment fuzzy until we get a match (fuzzy==1 will always produce a match)
    if( fuzzy == 0 ) newfuzzy = 1
    
    # if no exact matches try switching to substring matching
    if( fuzzy < 0 ) newfuzzy = 0
    
    # message about increasing fuzzy
    nomatch.info = paste('Repeating search at fuzzy level', newfuzzy, '\n')
    cat( paste0(nomatch.msg, nomatch.info) )
    
    # run the search again with higher fuzzy level
    return( rswat_docs(pattern, fname, fuzzy=newfuzzy, descw=descw, full=full) )
  }
  
  # copy subset of database matching pattern and count files and individual matches
  vartable.match = vartable %>% slice(int.match)
  n.files = length(unique(vartable.match$file))
  n.results = nrow(vartable.match)
  
  # message about search results
  msg.approx = ifelse(fuzzy < 1, 'exact', 'approximate')
  msg.matchtype = paste(msg.approx, 'result(s) for', pattern.msg)
  msg.search = paste(n.results, msg.matchtype, 'in', n.files, 'file(s)\n')
  
  # alternate message and row order for file requests
  if(is.file)
  {
    msg.search = paste(n.results, 'result(s) in', pattern.msg, '\n') 
    vartable.match = vartable.match %>% arrange(as.integer(rownames(vartable.match)))
  }
  
  # print the message
  cat(msg.search)
  
  # print the full description in single match case, or where requested by `printall`
  if( n.results == 1 )
  {
    # add a header to each section of output and print to console
    match.msg = paste(lrule[1], vartable.match$file, ':', vartable.match$name, lrule[2])
    cat.out = paste0(match.msg, vartable.match$description_full, '\n\n')
    cat(cat.out)
  }
  
  # dplyr::select the requested description (full or trimmed)
  if( full ) vartable.match = vartable.match %>% mutate( description = description_full )
  
  # remove unneeded columns and omit rownames (to not confuse them with page numbers)
  vartable.out = vartable.match %>% dplyr::select( name, file, pstart, description )
  rownames(vartable.out) = c()
  
  # return the dataframe of matches
  return( vartable.out )
}


#' load and parse the SWAT+ inputs documentation PDF
rswat_inputs_pdf_open = function(pdfpath=NULL, reload=FALSE, quiet=FALSE, desc.maxlen=60)
{
  # ARGUMENTS:
  # 
  # `pdfpath`, character, the absolute path to the IO PDF document
  # `reload`, logical, whether to proceed if the file has been loaded already
  # `quiet`, logical, whether to suppress console output
  # `desc.maxlen`, integer > 0, the number of characters to include in shortened descriptions
  #
  # RETURN:
  # 
  # returns nothing but writes the following entries to the list `.rswat$docs_in` (in package
  # environment), initializing/overwriting as needed:
  #
  #  `path`, the path to the PDF
  #  `rawtext`, large list of character vectors (one per page), the newline-delimited strings
  #  `vartable`, dataframe of parsed text from the doc
  #  `filetable`, dataframe summarizing filenames found in the doc
  #
  # DETAILS:
  #
  # Loads and renders the PDF using `pdftools`, parses each page using `rswat_pdf_parse`,
  # handling table joins, garbage collection, and tidying of output.
  #
  # Users may want to adjust `desc.maxlen` to fit the width of their console.
  #
  # For now this has only been tested on the latest IO doc (as of May 2021) 
  
  # a default path to look for the PDF
  pdfpath.def = 'D:/UYRW_data/development/inputs_swatplus_rev60_5.pdf'
  
  # suffix used to indicate a clipped description
  desc.suffix = '...'
  
  # initial data to keep track (in a loop) of the tables spanning multiple pages 
  ragged = FALSE
  section = last.section = 'FILE.CIO'
  
  # initialize internal storage if necessary
  if( !( 'docs_in' %in% ls(.rswat) ) )
  {
    # `rawtext` stores output of `pdftools::pdf_text`, `vartable` and `filetable` are derived
    .rswat$docs_in = list( path=character(), 
                        rawtext=character(), 
                        vartable=data.frame(), 
                        filetable=data.frame() )
  }
  
  # handle unspecified path 
  if( is.null(pdfpath) )
  {
    # carry forward the existing path, or assign a default one
    pdfpath = ifelse( length(.rswat$docs_in$path) == 1,  .rswat$docs_in$path, pdfpath.def)
  }
  
  # update path in package storage then load the PDF using `pdftools`
  .rswat$docs_in$path = pdfpath
  .rswat$docs_in$rawtext = pdftools::pdf_text(pdfpath)
  
  # handle calls that don't load anything
  if( nrow(.rswat$docs_in$filetable) > 0 & !reload)
  {
    # print the first page of the doc to console and return loaded path
    if( !quiet ) cat(.rswat$docs_in$rawtext[1])
    return(pdfpath)
  } 
  
  # split page data at newlines
  pdf.delim =  strsplit(.rswat$docs_in$rawtext, '\n')
  pdf.npage = length(pdf.delim)
  
  # loop over pages to get a list of dataframes containing page metadata and parsed text 
  cat('reading inputs PDF...')
  pageout.list = vector(mode='list', length=pdf.npage)
  for(pnum in seq_along(pdf.delim))
  {
    # print a progress message
    msg.maxlen = desc.maxlen
    ws.trail = paste(rep(' ', msg.maxlen), collapse='')
    msg.section = strtrim( paste0('reading section ', section, '...', ws.trail), msg.maxlen)
    msg.percent = paste(round(100 * pnum / pdf.npage, 3), '%      ') 
    if( !quiet ) cat( paste0('\r', msg.section, msg.percent) )
    if( !quiet ) flush.console()
    
    # line-by-line text from the pdf page
    pdftext = pdf.delim[[pnum]]
    
    # run the parser, append a page number, and add result to storage list
    pageout.list[[pnum]] = .rswat_inputs_pdf_parse(pdftext, ragged=ragged, section=section)
    
    # pull the last row of the output, assign section name to carry forward
    pageout.last = data.table::last(pageout.list[[pnum]])
    last.section = section
    section = ifelse(is.null(pageout.list[[pnum]]), last.section, pageout.last$section) 
    
    # NULL output gets skipped (the page had to variable name definition tables)
    if( !is.null(pageout.list[[pnum]]) )
    {
      # append the page number
      pageout.list[[pnum]] = pageout.list[[pnum]] %>% mutate( page = pnum ) 
      
      # set ragged mode if the last table may run over to next page 
      ragged = pageout.last$table
      if( last.section != section ) ragged = TRUE
    }
  }
  
  # row-bind, filter redundant entries, switch to lowercase names, autofill 'name' field
  .rswat$docs_in$vartable = do.call(rbind, pageout.list)  %>% filter(table) %>% filter(!t_head) %>% 
    mutate( file = tolower(section) ) %>%
    mutate( nm = tolower(nm) ) %>%
    mutate( nm = gsub(',cont.', '', nm) ) %>%
    mutate( name = nm[nafill(replace(seq_along(nm), is.na(nm), NA_integer_), 'locf')] ) %>%
    mutate( description = desc ) %>%
    dplyr::select( page, line, file, name, description ) %>%
    
    # concatenate description lines for each variable name and summarize page numbers
    group_by( file, name ) %>%
    summarize(line = first(line), 
              pstart = min(page), 
              pend = max(page), 
              description_full = paste0(description, collapse = '\n'), .groups='keep') %>%
    
    # trim whitespace and clip descriptions for dataframe printing
    mutate( description = gsub('\n+', ' ', description_full) ) %>%
    mutate( description = gsub('\\s+', ' ', description) ) %>%
    mutate( suffix = c('', '...')[ 1 + as.integer( nchar(description) > desc.maxlen + 1 ) ] ) %>%
    mutate( description = paste0(strtrim(description, desc.maxlen), suffix) ) %>%
    dplyr::select( -suffix ) %>%
    
    # omit entries with empty descriptions, header listings, reorder to match pdf
    filter( nchar(description) > 1 ) %>%
    filter( name != 'header' ) %>%
    arrange(pstart, line) %>%
    mutate( step = NA ) %>%
    data.frame
  
  # build summary table of all filenames and variables parsed by the function
  .rswat$docs_in$filetable = .rswat$docs_in$vartable %>% group_by(file) %>%
    summarize( startpage = first(pstart), 
               npage = 1 + last(pend) - first(pstart), 
               ndef = n(), 
               .groups='keep') %>% 
    
    # arrange in same order as pdf
    arrange(startpage) %>% 
    dplyr::select(file, ndef, npage, startpage) %>% 
    mutate( type='input' ) %>%
    mutate( step = NA ) %>%
    data.frame
  
  # finish
  cat('done\n')
}

#' load and parse the SWAT+ outputs documentation PDF
rswat_outputs_pdf_open = function(pdfpath=NULL, reload=FALSE, desc.maxlen=60)
{
  # ARGUMENTS:
  # 
  # `pdfpath`, character, the absolute path to the IO PDF document
  # `reload`, logical, whether to proceed if the file has been loaded already
  # `desc.maxlen`, integer > 0, the number of characters to include in shortened descriptions
  #
  # RETURN:
  # 
  # returns nothing but writes the following entries to the list `.rswat$docs_out` (in package
  # environment), initializing/overwriting as needed:
  #
  #  `path`, the path to the PDF
  #  `rawtext`, large list of character vectors (one per page), the newline-delimited strings
  #  `vartable`, dataframe of parsed text from the doc
  #  `filetable`, dataframe summarizing filenames found in the doc
  #
  # DETAILS:
  #
  # Loads and renders the PDF using `pdftools`, parses each page using `rswat_output_pdf_parse`,
  # handling table joins, garbage collection, and tidying of output.
  #
  # Users may want to adjust `desc.maxlen` to fit the width of their console.
  #
  # For now this has only been tested on the latest outputs doc (as of July 2021) 
  
  # a default path to look for the PDF
  pdfpath.def = 'D:/UYRW_data/development/outputs_swatplus_rev60_5.pdf'
  
  # suffix used to indicate a clipped description
  desc.suffix = '...'
  
  # initial data to keep track (in a loop) of the tables spanning multiple pages 
  ragged = FALSE
  section = last.section = 'Water Balance Landscape Output Files'
  
  # initialize internal storage if necessary
  if( !( 'docs_out' %in% ls(.rswat) ) )
  {
    # `rawtext` stores output of `pdftools::pdf_text`, `vartable` and `filetable` are derived
    .rswat$docs_out = list( path=character(), 
                            rawtext=character(), 
                            vartable=data.frame(), 
                            filetable=data.frame() )
  }
  
  # handle unspecified path 
  if( is.null(pdfpath) )
  {
    # carry forward the existing path, or assign a default one
    pdfpath = ifelse( length(.rswat$docs_out$path) == 1,  .rswat$docs_out$path, pdfpath.def)
  }
  
  # update path in package storage then load the PDF using `pdftools`
  .rswat$docs_out$path = pdfpath
  .rswat$docs_out$rawtext = pdftools::pdf_text(pdfpath)
  
  # handle calls that don't load anything
  if( nrow(.rswat$docs_out$filetable) > 0 & !reload)
  {
    # print the first page of the doc to console and return loaded path
    if( !quiet ) cat(.rswat$docs_out$rawtext[1])
    return(pdfpath)
  } 
  
  cat('reading outputs PDF...')
  
  # split page data at newlines
  pdf.delim = strsplit(.rswat$docs_out$rawtext, '\n')
  pdf.npage = length(pdf.delim)
  
  # loop over pages to get a list of dataframes - omit last two pages (incomplete/unreliable)
  pageout.list = vector(mode='list', length=pdf.npage-2)
  for(pnum in seq(length(pdf.delim)-2))
  {
    # line-by-line text from the pdf page
    pdftext = pdf.delim[[pnum]]
    
    # run the parser and append a page number
    pageout.list[[pnum]] = .rswat_outputs_pdf_parse(pdftext, section=section) %>%
      mutate(page = pnum)
    
    # pull the last row of the output, assign section name to carry forward
    pageout.last = last(pageout.list[[pnum]])
    last.section = section
    section = ifelse(is.null(pageout.list[[pnum]]), last.section, pageout.last$section)
    
  }
  
  # concatenate the pages into a single dataframe, keeping only info on relevant lines
  pageout = do.call(rbind, pageout.list) %>% filter( table | !is.na(filename) )
  
  # expand the short-form filenames by replacing wildcard with time period string
  tstrings = c(day='day', mon='month', yr='year', aa='year')
  fn.short = pageout %>% filter(!table) %>% select(section, filename)
  fn.shortlist = split(fn.short$filename, fn.short$section)
  fn.longlist = lapply(fn.shortlist, function(fnvec) {
    fn = unlist( lapply( names(tstrings), function(tstr) gsub('\\*', tstr, fnvec)) )
    setNames( rep(tstrings, each=length(fnvec)), fn )
    })
  
  # expand the variables info dataframe by appending long-form filenames
  vars.list = pageout %>% filter(table) %>% group_split(section) 
  vars.expanded = lapply(seq_along(vars.list), function(idx) {
    
    group.list = mapply(function(fn, step) vars.list[[idx]] %>% mutate(filename=fn, step=step),
                         fn=names(fn.longlist[[idx]]),
                         step=fn.longlist[[idx]],
                         SIMPLIFY=FALSE)
    
    do.call(rbind, group.list)
    
    })
  
  # collapse into single dataframe and tidy up
  .rswat$docs_out$vartable = do.call(rbind, vars.expanded) %>% 
    rename( file = filename ) %>%
    rename( description = desc ) %>%
    dplyr::select(page, line, section, file, name, step, description) %>%
    
    # concatenate description lines for each variable name and summarize page numbers
    group_by( step, file, name ) %>%
    summarize(line = first(line),
              pstart = min(page),
              pend = max(page),
              description_full = paste0(description, collapse = '\n'), .groups='keep') %>%

    # trim whitespace and clip descriptions for dataframe printing
    mutate( description = gsub('\n+', ' ', description_full) ) %>%
    mutate( description = gsub('\\s+', ' ', description) ) %>%
    mutate( suffix = c('', '...')[ 1 + as.integer( nchar(description) > desc.maxlen + 1 ) ] ) %>%
    mutate( description = paste0(strtrim(description, desc.maxlen), suffix) ) %>%
    dplyr::select( -suffix ) %>%

    # omit entries with empty descriptions, header listings, reorder to match pdf
    filter( nchar(description) > 1 ) %>%
    filter( name != 'header' ) %>%
    arrange(pstart, line) %>%
    data.frame

  # build summary table of all filenames and variables parsed by the function
  .rswat$docs_out$filetable = .rswat$docs_out$vartable %>% group_by(step, file) %>%
    summarize( startpage = first(pstart),
               npage = 1 + last(pend) - first(pstart),
               ndef = n(),
               .groups='keep') %>%

    # arrange in same order as pdf
    arrange(startpage) %>%
    dplyr::select(file, ndef, npage, startpage, step) %>%
    mutate(type='output') %>%
    data.frame
    
  # finish
  cat('done\n')
}


#' 
#' ## internal functions
#' These are used by core functions and not meant to be called by the user
#' 


#' parse a page from the inputs pdf
.rswat_inputs_pdf_parse = function(pdftext, ragged=FALSE, header=NULL, section=NA)
{
  # Line-by-line parsing of pages from SWAT+ IO PDF document using `pdftools` output
  #
  # ARGUMENTS
  #
  # `pdftext`: character, one page of (newline delimited) output from `pdftools::pdf_text` 
  # `ragged`: logical, whether to check for a continued table appearing at the top
  # `header`: character vector of length 2, a pair of keywords for identifying tables
  # `section`: character, default string to use for section titles (filenames)
  #
  # RETURN:
  #
  # A dataframe with nrow == length(pdftext) summarizing the table(s) found in
  # the supplied page of text lines. 
  #
  #
  # DETAILS:
  #
  # This is an internal function meant to be called by `rswat_pdf_open`
  #
  # Tables are identified as starting immediately after line(s) containing the two
  # (whitespace delimited) fields supplied in `header`. Column  spacing is determined from
  # the spacing in the header line(s). 
  #
  # In `ragged` mode, the first line(s) of `pdftext` are assumed to belong to an earlier
  # table, for which there is (possibly) no header line. The `section` argument allows
  # an earlier section name to be carried forward to these trailing table rows

  # handle empty pages
  if( length(pdftext) == 0 ) return(NULL)
  
  # this default should work for parsing SWAT+ inputs docs
  if( is.null(header) ) header = c('Variable name', 'Definition')

  # regex for table header lines 
  regex.thead = paste0('^(\\s*)(', header[1], ')(\\s+)(', header[2],')$')

  # regex for page header lines (two forms)
  regex.l = '^(\\s*[0-9]+\\s*SWAT\\+)'
  regex.r = '^(\\s*SWAT\\+ INPUTS\\s*[0-9]+)'
  regex.phead = paste0(regex.l, '|', regex.r)
  
  # regex for section headers (all-caps with period and extension)
  regex.section = '^[A-Z]+[-_]*[A-Z]*[-_]*[A-Z]+\\.[A-Z]{2,3}$'
  
  # BUGFIX: exceptions nonstandard header formats
  regex.exceptions = c('^recall\\_day\\.rec$')
  regex.section = paste0('(', paste(c(regex.section, regex.exceptions), collapse=')|('), ')')
  
  # regex for variable names (all-caps, assume first character is alphabetical)
  #regex.fname = '([A-Z][A-Z0-9]*_*[A-Z0-9]+)'
  regex.fname = '([A-Z][A-Z0-9]*_*[A-Z0-9]*)'
  
  # allow the following suffixes to appear after a variable declaration (eg. for soils.sol)
  nm.suffix = c(', cont\\.', '\\(layer \\#\\)', '\\(top\\slayer\\)')
  regex.suffix =  paste0('((', paste(nm.suffix, collapse=')|('), '))')
  regex.fname = paste0(regex.fname, regex.suffix, '?')
  
  # regex for footer lines
  regex.foot = '^\\s+[0-9]+$'
  
  # detect and remove page header line (missing on some pages)
  is.header = grepl(regex.phead, pdftext[1])
  if(is.header) pdftext = pdftext[-1]
  if( length(pdftext) == 0 ) return(NULL)
  
  # detect and remove page footer line (missing on most pages)
  if( grepl(regex.foot, last(pdftext)) ) pdftext = pdftext[-length(pdftext)]
  if( length(pdftext) == 0 ) return(NULL)
  
  # make dataframe to store text metadata for the trimmed page
  page.n = length(pdftext)
  page.df = data.frame(line = 1:length(pdftext),
                       section = NA,
                       table = FALSE,
                       t_head = grepl(regex.thead, pdftext),
                       nm_start = NA,
                       nm = NA,
                       desc_start = NA,
                       desc = NA)
  
  # parse first table (if there is one)
  if( any(page.df$t_head) )
  {
    # integer row number of first data line and a guess for the last one 
    ln.min = which(page.df$t_head)[1] + 1
    ln.max = which(page.df$t_head)[2] - 1
    if( is.na(ln.max) ) ln.max = page.n
    
    # expected column positions based on table header line
    text.header = pdftext[ ln.min - 1 ]
    nm.pos = regexpr(header[1], text.header)[1]
    desc.pos = regexpr(header[2], text.header)[1]
    ln.desc = integer(0)
    
    # data row parsing (skipped when table header is on final line)
    if( !( ln.max < ln.min ) )
    {
      # lines declaring a variable name should start with whitespace
      text.nm = substr(pdftext[ln.min:ln.max], 1, desc.pos-1)
      regex.nm = paste0('(^\\s{', nm.pos - 1, '})', regex.fname, '(\\s*)$')
      is.nm = grepl(regex.nm, text.nm)

      # extract names and copy to output dataframe
      ln.nm = c(ln.min:ln.max)[ is.nm ]
      page.df$nm[ page.df$line %in% ln.nm ] = gsub(' ', '', text.nm[is.nm])
      page.df$nm_start[ page.df$line %in% ln.nm ] = nm.pos
      
      # regex for description lines without a variable name
      regex.desc = paste0('(^\\s{', desc.pos - 1, '})')
      is.desc = is.nm | grepl(regex.desc, pdftext[ln.min:ln.max]) 
      
      # include empty lines that are sometimes interleaved with table entries
      is.desc = is.desc | ( nchar(pdftext[ln.min:ln.max]) == 0 )
      
      # extract descriptions and copy to output dataframe
      ln.desc = c(ln.min:ln.max)[ is.desc ]
      text.desc = substr(pdftext[ln.desc], desc.pos, nchar(pdftext[ln.desc]))
      page.df$desc[ page.df$line %in% ln.desc ] = text.desc
      page.df$desc_start[ page.df$line %in% ln.desc ] = desc.pos
    }

    # flag table-related lines
    page.df$table[ page.df$line %in% c(ln.desc, ln.min-1) ] = TRUE
    
    # parse any remaining tables in recursive call
    if( sum(page.df$t_head) > 1 )
    {
      # identify the unprocessed lines, parse them
      ln.remaining = page.df$line[ !( page.df$line < which(page.df$t_head)[2] ) ] 
      df.remaining = .rswat_inputs_pdf_parse(pdftext[ln.remaining], header=header)
      df.remaining$line = ln.remaining
      
      # join with the rest
      idx.join = page.df$line %in% ln.remaining
      page.df[idx.join,] = df.remaining
    }
  }
  
  # parse section headers, ignoring known tables
  ln.unknown = page.df %>% filter( !table ) %>% pull(line)
  is.section = grepl(regex.section, pdftext[ln.unknown])
  
  # if we find anything, add section attribute
  if( any(is.section) )
  {
    # copy line numbers of section headers and pull the strings
    ln.section = ln.unknown[is.section]
    text.section = pdftext[ln.section]
    
    # loop over section line numbers
    for( idx.ln in seq_along(ln.section) ) 
    {
      # set all subsequent lines to have this section label
      ln.start = ln.section[idx.ln]
      page.df$section[ !( page.df$line < ln.start ) ] = text.section[idx.ln]
    }
  }
  
  # as needed, parse tail of previous table
  if( ragged & !page.df$table[1] & is.na(page.df$section[1]) )
  {
    # set up line numbers to parse
    ln.min = min(page.df$line)
    ln.max = which( page.df$t_head | !is.na(page.df$section) )[1] - 1
    if( is.na(ln.max) ) ln.max = max(page.df$line)
    
    # regex for lines that appear to be a name declaration
    regex.ragnm1 = paste0('(^\\s*', regex.fname, '\\s{2,})')
    regex.ragnm2 = paste0('(^\\s+', regex.fname, ')[^a-z]')
    is.nm = grepl(paste0(regex.ragnm1, '|', regex.ragnm2), pdftext[ln.min:ln.max])
    
    # bail if we can't find any by calling the function again with `ragged=FALSE`
    if( !any(is.nm) ) return( .rswat_inputs_pdf_parse(pdftext, header=header, section=section) )

    # get name column start position from first match
    ln.nm = c(ln.min:ln.max)[is.nm][1]
    nm.pos = regexpr('[A-Z]', pdftext[ln.nm])[[1]]
    
    # find start position of descriptions from above line (in well-behaved case)
    text.desc = substr(pdftext[ln.nm], nm.pos, nchar(pdftext[ln.nm]))
    offset.desc = attr(gregexpr(regex.ragnm1, text.desc)[[1]],'match.length')
    desc.pos = nm.pos + offset.desc

    # BUGFIX: handle misaligned descriptions appearing before first name declaration
    if( ln.nm > ln.min ) desc.pos = 1 + attr(regexpr('^\\s+', pdftext[ln.min]), 'match.length')

    # proceed only if we can match a header description
    is.unmatched = desc.pos < nm.pos + 1
    if( is.unmatched ) return( .rswat_inputs_pdf_parse(pdftext, header=header, section=section) )
    
    # build a dummy header line so we can parse table by a recursive call
    header.dummy = c('XxXx', 'YyYy')
    ws.pre = ''
    if( nm.pos > 1 ) ws.pre = paste(rep(' ', nm.pos-1), collapse='')
    ws.delim = paste(rep(' ', desc.pos - nm.pos - nchar(header.dummy[1]) ), collapse='')
    text.header = paste0(ws.pre, header.dummy[1], ws.delim, header.dummy[2])
    
    # recursive call to get table data (removing the dummy header and matching line numbers)
    pdftext.sub = c(text.header, pdftext[ln.min:ln.max])
    ragged.out = .rswat_inputs_pdf_parse(pdftext.sub, header=header.dummy, section=section )[-1,]
    ragged.out$line = ln.min:ln.max
    
    # join with existing output table
    idx.join = page.df$line %in% c(ln.min:ln.max)
    page.df[idx.join,] = ragged.out

  }
  
  # identify name declarations that have a misaligned description 
  is.shifted = !is.na(page.df$nm) & ( page.df$desc == '' ) & c(FALSE, !is.na(page.df$nm)[-1])
  
  # shift the description lines to the correct place (down one line)
  if( any(is.shifted) )
  {
    # copy description from line above, then wipe description at old location
    page.df$desc[which(is.shifted)] = page.df$desc[which(is.shifted) - 1]
    page.df$desc[which(is.shifted) - 1] = ''
  }

  # omit the suffixes that appear in certain variable name declarations
  idx.nm = !is.na(page.df$nm)
  regex.suffix = '(,cont\\.)|(\\(layer\\#\\))|(\\(toplayer\\))'
  if( any( idx.nm ) ) page.df$nm[idx.nm] = gsub(regex.suffix, '', page.df$nm[idx.nm])
  
  # increment line numbers if we removed a header at the beginning
  if( is.header ) page.df$line = page.df$line + 1
  
  # set default section header, then finish
  page.df$section[ is.na(page.df$section) ] = section
  return(page.df)
}

#' parse a page from the outputs pdf
.rswat_outputs_pdf_parse = function(pdftext, section=NA)
{
  # Line-by-line parsing of pages from SWAT+ outputs PDF document using `pdftools` output
  #
  # ARGUMENTS
  #
  # `pdftext`: character, one page of (newline delimited) output from `pdftools::pdf_text` 
  # `section`: character, default string to use for section titles
  #
  # RETURN:
  #
  # A dataframe with nrow == length(pdftext) summarizing the table(s) found in
  # the supplied page of text lines. 
  #
  #
  # DETAILS:
  #
  # This is an internal function meant to be called by `rswat_pdf_open`
  #
  # Table entries are identified as starting with the string "real ::" 
  # Filename lines (preceeding a table) have a single word with no spaces and end in
  # '*.txt'.
  #
  # There are no headers in this document, so sections are labelled according to the
  # document section titles. This are identified as the first line of text following a
  # table.
  #
  # In `ragged` mode, the first line(s) of `pdftext` should belong to an earlier table

  # regex for table entry lines and their values
  regex.tline = '^\\s+real\\s+::' 
  regex.name = '::.*\\!'
  regex.units = '\\!.*\\s+\\|'
  regex.desc = '\\|.*'

  # regex for leading filename definitions
  regex.filename = '[a-z_-]\\*.txt$'
  
  # identify and repair hanging descriptions with the too-long lines describing organic pool
  regex.hanging = 'org\\(20\\%\\)'
  is.hanging = grepl(regex.hanging, pdftext)
  idx.hanging = which( is.hanging )
  if( length(idx.hanging) > 0 )
  {
    # clean up whitespace, cut/paste hanging text to the correct line
    hanging.clean = gsub('^\\s+', '', pdftext[idx.hanging])
    pdftext[idx.hanging-1] = paste(pdftext[idx.hanging-1], hanging.clean)
    
    # remove the offending line
    pdftext = pdftext[-idx.hanging]
  }
  
  # remove header line
  if( grepl('\\s+C:\\\\', pdftext[1]) ) pdftext = pdftext[-1] 
  
  # remove empty lines
  is.empty = nchar(pdftext) == 0
  if( any(nchar(pdftext) == 0) ) pdftext = pdftext[ !is.empty ]
  
  # handle empty pages
  if( length(pdftext) == 0 ) return(NULL)
  
  # identify filename lines, variable definition lines, and empty lines
  is.filename = grepl(regex.filename, pdftext)
  is.tab = grepl(regex.name, pdftext)
  
  # capture the relevant text (and some junk)
  name.capt = regmatches(pdftext[is.tab], gregexec(regex.name, pdftext[is.tab]))
  units.capt = regmatches(pdftext[is.tab], gregexec(regex.units, pdftext[is.tab]))
  desc.capt = regmatches(pdftext[is.tab], gregexec(regex.desc, pdftext[is.tab]))

  # make dataframe to store text metadata for the trimmed page
  page.n = length(pdftext)
  page.df = data.frame(line = 1:length(pdftext),
                       section = section,
                       table = is.tab,
                       filename = NA,
                       name = NA,
                       units = NA,
                       desc = NA)
  
  # add variable info fields after trimming delimiters and whitespace
  page.df$name[ is.tab ] = gsub(':|= 0|\\s|\\!|\\.', '', unlist(name.capt))
  page.df$units[ is.tab ] = gsub('\\!|\\s+\\|', '', unlist(units.capt))
  page.df$desc[ is.tab ] = gsub('\\|', '', unlist(desc.capt))
  page.df$filename[ is.filename ] = pdftext[ is.filename ]
  
  # tidy up leading and doubled whitespace
  page.df$units = gsub('^\\s+', '', page.df$units)
  page.df$units = gsub('\\s{2,}', ' ', page.df$units)
  page.df$desc = gsub('\\s{2,}', ' ', page.df$desc)
  
  # section title is first line of text unless there is a hanging table from prev page
  idx.tbreak = NULL
  if( !is.tab[1] ) idx.tbreak = 0
  
  # identify line numbers where a table has ended mid-page
  is.midbreak = ( diff(is.tab) < 0 ) & head(is.tab, -1)
  idx.tbreak = c( idx.tbreak, which( is.midbreak ) )
  
  # if a table ends, look ahead to next (nonempty) line for next section title
  if( length(idx.tbreak) > 0 )
  {
    for(idx.table in seq_along(idx.tbreak) )
    {
      # scan for nonempty lines following the end of the table
      ln.expected = 1 + idx.tbreak[idx.table]
      if( !( ln.expected > length(pdftext) ) )
      {
        # set all subsequent table rows to have this section label
        page.df$section[ !( page.df$line < ln.expected ) ] = pdftext[ln.expected]
      }
    }
  }
  
  # finished
  return(page.df)
}


#' 
#' ## cross-listed functions
#' These may be used in other applications besides `rswat_docs`. I include `my_adist`
#' here because it allows `rswat_docs` to not depend on `rswat_config`

#' `base::adist`-based string distance ranking
my_adist = function(pattern, lu, lu.split=FALSE, costs=NULL)
{
  # Computes a case-insensitive string distance between `pattern` and `lu`
  #
  # ARGUMENTS
  #
  # `pattern`: character, a search keyword to compare with entries of `lu`
  # `lu`: character vector, a set of strings to compare with `pattern`
  # `lu.split`: logical, whether to split `lu` at punctuation and whitespace (see DETAILS)
  # `costs`: named list, costs for the three types of string edits, passed to `adist`
  #
  # RETURN
  #
  # Numeric vector of same length as `lu`, providing a ranking of string distance between
  # `pattern` and `lu`. 
  # 
  # DETAILS
  #
  # All matching is case insensitive. When `lu.split=TRUE`, each element of `lu` gets split
  # at punctuation and whitespace, and the function compares against each component separately,
  # returning the smallest of these string distances.
  #
  # String distances returned by the function satisfy the following:
  #
  #   (1) exact matches have distance 0 
  #   (2) exact substring matches have distance in (0,1)
  #   (3) approximate substring matches have distance in [1, Inf)
  # 
  # Results in group (2) are ordered according the the relative difference in string length
  # between `pattern` and the respective elements of `lu`. Results in group (3) are ordered
  # according to the ratio of the number of string edits (ie. Levenstein distance) to the
  # length of `pattern`.
  # 
  # The special search pattern '*' matches everything exactly (ie the function returns all 0s)
  #
  # pipe characters in `pattern` are treated as logical OR delimiters - ie. `pattern` is split
  # at the pipes, string distance is computed separately for each component, and the parallel
  # minimum is returned. `pattern` is also split at any whitespace characters, and the component
  # distances are averaged in the result (to get a kind of approximate OR). Note that pipes
  # are parsed first, then whitespace.
  
  # set default costs vector
  if( is.null(costs) ) costs = list(ins=2, del=1, sub=1)
  
  # handle empty search pattern case
  if( pattern == '*' ) return( rep(0, length(lu)) )
  
  # count number of characters in pattern
  pattern.len = nchar(pattern)
  
  # detect logical OR in pattern
  pattern.pipe = strsplit(pattern, '\\|')[[1]]
  pattern.pipe = pattern.pipe[nchar(pattern.pipe) > 0]
  if( length(pattern.pipe) > 1 )
  {
    # match the split patterns separately and return parallel minimum
    dist.mat = sapply(pattern.pipe, function(p) my_adist(p, lu, lu.split=lu.split, costs=costs))
    return( apply(dist.mat, 1, min) )
  }
  
  # detect logical AND in pattern
  pattern.split = strsplit(pattern, '\\s+')[[1]]
  pattern.split = pattern.split[nchar(pattern.split) > 0]
  if( length(pattern.split) > 1 )
  {
    # match the split patterns separately and return means
    dist.mat = sapply(pattern.split, function(p) my_adist(p, lu, lu.split=lu.split, costs=costs))
    return( rowMeans(dist.mat) )
  }
  
  # count reference string lengths and assign NAs max length + 1
  lu.len = nchar(lu)
  lu.len[is.na(lu.len)] = max(lu.len, na.rm=TRUE) + 1
  
  # and compute relative length difference 
  match.minlen = pmin(lu.len, pattern.len)
  match.maxlen = pmax(lu.len, pattern.len)
  lu.rlen = 1 - ( match.minlen/match.maxlen )
  
  # convert to lowercase
  pattern = tolower(pattern)
  lu = tolower(lu)
  
  # identify exact matches and initialize distances 
  idx.exact = lu %in% pattern 
  dist.out = as.integer(!idx.exact)
  
  # find Levenstein distances to substrings as proportion of pattern length
  dist.suba = adist(pattern, lu, costs=costs, partial=TRUE) / pattern.len
  dist.suba[ is.na(dist.suba) ] = max(dist.suba, na.rm=TRUE) + 1
  
  # use relative string length as base ranking for exact substring matches
  idx.sub = ( dist.suba == 0 ) & !idx.exact
  if( any(idx.sub) ) dist.out[idx.sub] = lu.rlen[idx.sub]
  
  # rank approximate substring matches by Levenstein distance 
  idx.suba = dist.suba > 0
  dist.out[idx.suba] = 1 + dist.suba[idx.suba] 
  
  # match over split lookup strings if requested
  if( lu.split )
  {
    # split at punctuation characters, ignoring beginning/end of string
    regex.p = '[[:punct:]]|\\s+'
    
    # split lookup strings at delimiters
    lu.list = strsplit(lu, regex.p)
    lu.len = sapply(lu.list, length)
    is.p = lu.len > 0
    
    # recursive call to compute individual distances
    if( any(is.p) )
    {
      # pass vectorized list in recursive call
      lu.vec = unlist(lu.list[is.p])
      adist.split = my_adist(pattern, lu.vec, lu.split=FALSE, costs=costs)
      
      # inverse vectorization to rebuild list
      idx.end = cumsum(lu.len[is.p])
      idx.start = c(1, idx.end[-length(idx.end)] + 1)
      
      # find minimum distance by list element and overwrite existing value
      dist.split = mapply(function(x,y) min(adist.split[x:y]), x=idx.start, y=idx.end)
      dist.out[is.p] = pmin(dist.split, dist.out[is.p])
    }
  }
  
  return(dist.out)
}

#+ include=FALSE
#my_markdown('rswat_docs')
