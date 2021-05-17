#' ---
#' title: "demo_rswat_docs.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_rswat_docs.R**: Access variable definition tables in SWAT+ documentation PDF from R
#' 
#' This script demonstrates the `rswat_docs` function, which reads and parses the SWAT+
#' inputs PDF ("CHAPTER FILE.CIO: SWAT+ INPUT DATA") returning its name/definition
#' table entries as dataframes, or printing them to the console.
#' 
#' ## introduction
#' 
#' Users of the SWAT+ model may find the number of built-in process submodels and associated parameters
#' a bit overwhelming. There are a LOT of variables to keep track of. The latest configuration
#' file documentation PDF ("inputs_swatplus_rev60_5.pdf",
#' [available here](https://swatplus.gitbook.io/docs/user/io)) runs over 250 pages! And that's just
#' variable name definitions.
#' 
#' As I learn the model, I find I'm spending a lot of time scrolling this PDF to look up definitions,
#' units, and coding schemes. This is fine at the first, as you don't want to just skim through and miss
#' something important when you're learning. But later on, when using the doc as a reference,
#' it gets tedious. Variable names are often mentioned multiple times in the document before we get
#' to their full definitions, and it can be hard to track down the SWAT+ version of a SWAT name
#' (or names you can't remember exactly) using just ctrl-F.
#' 
#' To streamline things I've written some helper functions that load the PDF and build a big
#' list of all its "Variable Name" - "Definition" entries. The list can be searched using
#' approximate matching (allowing character substitutions, etc). R users may find this a quicker
#' and more direct route to accessing the variable definitions they need, and for exploring the model.
#' 

#'
#' ## libraries
#' [pdftools](https://cran.r-project.org/web/packages/pdftools/index.html) is required to render
#' the PDF textboxes as character vectors in R,
#' [helper_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md) and
#' [rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
#' load some required libraries, global variables, and some helper functions.

library(here)
library(pdftools)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))

#' Note that "rswat.R" is sourced in the line above to initialize the `.rswat` environment in
#' your session. The relevant functions definitions in that file are `rswat_docs`, `rswat_pdf_open`,
#' `rswat_pdf_parse`, and `rswat_match`. Other functions are not needed at this point.


#' 
#' ## getting started
#' 
#' Users will need to download "inputs_swatplus_rev60_5.pdf" from the SWAT+
#' [I/O docs](https://swatplus.gitbook.io/docs/user/io) page and change the PDF path variable below to
#' point to their local copy. `rswat_pdf_open` will then load and parse the PDF for you:

# set the PDF path
pdfpath = 'D:/UYRW_data/development/inputs_swatplus_rev60_5.pdf'

# load the file into memory
rswat_pdf_open(pdfpath, quiet=TRUE)

#' This function call does a lot of stuff in the background - it uses the `pdftools` package to
#' render text boxes into character strings, then loops over pages to parse table entries into 
#' a large dataframe. Definitions spanning multiple lines get concatenated, and tables spanning
#' multiple pages get merged automatically.
#' 
#' Print a list of files in this database using `rswat_docs()`

# list available files (print only the first 25)
rswat_docs() %>% head(25)
rswat_docs() %>% nrow

#' The parser finds 105 different name-definition tables in the document, each associated with
#' a different SWAT+ configuration file. The `ndef` field counts the number of definitions for
#' each table, `startpage` indicates the page (in the PDF) on which this table starts, and
#' `npage` counts the number of pages it runs.
#' 
#' Let's look at the example of the "snow.sno" file, which controls snowfall/snowmelt processes:
rswat_docs() %>% filter(file=='snow.sno')

#' This starts on page 176 and runs for three pages. To view the full text of one of these pages
#' in R, simply pass a page number or a vector of them to `rswat_docs`:

# print a specific page of the PDF
rswat_docs(176)

#' We can see that this page contains the start of the snow parameters table, and also the tail
#' end of a previous table ("septic.sep"). The "snow.sno" table runs for two more pages. View the
#' entire table as a dataframe by passing the associated filename to `rswat_docs`:

# load table for an example filename
rswat_docs('snow.sno')

#' Longer definitions are clipped (they end with '...') so that they can fit cleanly into an
#' R console printout of a dataframe (similar to `tibble::print.tbl`). To access the full definition,
#' you can use the `full=TRUE` argument
#' 

# if you like tibbles, use `full=TRUE` and pipe to `tibble` to copy the full descriptions
rswat_docs('snow.sno', full=TRUE) %>% tibble %>% print


#' Notice the *slash-"n"'s*, which indicate where a newline happened in the source file. Since R is
#' printing the literal strings here, the output looks ugly unless we pipe it to a `cat` call.
#' `rswat_docs` will do this by default (instead of returning the dataframe) when it finds one
#' single match for a search query.
 
# eg. search for the pattern "title" in filename "snow.sno" like this:
rswat_docs('title', fname='snow.sno')

#' The result is a printout of the full description as it would appear in the PDF (including newlines).

#' 
#' ## searching names
#' 
#' The first argument of `rswat_docs` can be filename (as in the first two calls above), or a
#' search string for some keyword (as in the last function call). When the argument `fname` is
#' missing, all filenames are included in the search:

# eg. repeat the last call without specifying "snow.sno" and get 99 matches (print the first few)
rswat_docs('title') %>% head

#' By default, when exact matches are found, `rswat_docs` returns (only) them. If no exact matches
#' are found, it then looks for substring matches and returns those. If still nothing is found, it then
#' tries approximate matching with increasing fuzziness until it gets at least one match. 
#' 
#' This can be pretty effective for tracking down variable names when you have a rough idea of the
#' name but you're not sure about the exact abbreviation in use. eg. a search for the Hargreaves PET
#' coefficient finds 'harg_pet' right away:

rswat_docs('Hargreaves')

#' It's not a very sopthisticated search tool, however, so you will get false positives with less-unique
#' letter combinations. eg. a search for (snow) "melt" parameters turns up some things that have nothing
#' to do with melting

rswat_docs('melt')

#' and a search for "snowmelt" omits some relevant entries from file "snow.sno" because their names
#' are less similar to the search pattern than a match found in "temperature.cha":

rswat_docs('snowmelt')

#' 
#' ## searching descriptions
#' 
#' To avoid the problem above with vague keywords, it's best to search the variable *descriptions* text.
#' Specify this search mode using the `indesc=TRUE` argument:

# a better snow melt parameter search
rswat_docs('melt', indesc=TRUE)

#' this turns up two places where a code must be set to enable the Hargreaves model
rswat_docs('Hargreaves', indesc=TRUE)


#+ include=FALSE
# Development code
#my_markdown('demo_rswat_docs', 'R/demo')  
  