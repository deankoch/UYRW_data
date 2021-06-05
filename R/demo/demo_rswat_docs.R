#' ---
#' title: "demo_rswat_docs.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **demo_rswat_docs.R**: Access variable definition tables in the SWAT+ documentation PDF from R
#' 
#' This script demonstrates
#' [rswat_docs.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md), which
#' reads and parses the SWAT+ inputs PDF ("inputs_swatplus_rev60_5.pdf") into a large name/definition
#' dataframe, whose entries can then be searched or printed to the console.
#' 
#' 
#' ## introduction
#' 
#' Users of the SWAT+ model may find the number of built-in process submodels and associated parameters
#' a bit overwhelming. There are a LOT of variables to keep track of. The latest configuration
#' file documentation PDF ("inputs_swatplus_rev60_5.pdf",
#' [available here](https://swatplus.gitbook.io/docs/user/io)) runs over 250 pages! And that's just
#' input variable name definitions.
#' 
#' It's too much to scroll through all the time, so I've written an indexing and search tool to help R-based
#' SWAT+ users like me navigate this document. This code will render the PDF into text strings, building
#' a giant searchable list of all "Variable Name"/"Definition" entries.
#' 
#' 
#' ## Why not just ctrl-f?
#' 
#' You could search for keywords using your favorite PDF viewer, except that: 
#' 
#' * the 'find' tool in viewers like Acrobat or Sumatra searches for literal strings, so to get straight to
#' a definition you have to be able to remember (or guess) the particular abbreviation in use, or a substring of it
#' * many abbreviations aren't really guessable for the unitiated ("SED_DET" is a... rainfall distribution code?)
#' * a generic variable name like 'LONG' or 'FLO' can appear dozens of times in the document before you get to the
#' definition you're interested in
#' * variable names are slightly different in SWAT+ versus SWAT2012 and the theory documentation
#' was written for the latter
#' 
#' By my count there are 1000+ variable names defined in this document - I don't think anyone is going to
#' memorize them all. So when you know what you're looking for but can't quite remember the exact name
#' it can be a real chore to track it down. `rswat_docs` helps with this by allowing fuzzy
#' matching of multiple keywords to both names and definitions.
#'
#' ## libraries and installation
#' 
#' The easiest way to try this code out yourself is to simply download the Rscript sourced below
#' ([rswat_docs.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md))
#' and install any missing packages on your machine.

# library `here` is not strictly necessary; I just use it to simplify paths
library(here)
source(here('R/rswat_docs.R'))

#' `rswat_docs.R` will initialize an environment called `.rswat` in your session, and define
#' four functions; `rswat_docs`, `rswat_pdf_open`, `rswat_pdf_parse`, and `my_adist`. 
#' 
#' ## getting started
#' 
#' Users will need to download "inputs_swatplus_rev60_5.pdf" from the SWAT+
#' [I/O docs](https://swatplus.gitbook.io/docs/user/io) page and change the PDF path variable below to
#' point to their local copy. `rswat_pdf_open` will then load and parse the PDF for you:

# set the PDF path
pdfpath = here('development/inputs_swatplus_rev60_5.pdf')

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

#' The parser finds 108 different name-definition tables in the document, each associated with
#' a different SWAT+ configuration file. The `ndef` field counts the number of definitions for
#' each table, `startpage` indicates the page (in the PDF) on which this table starts, and
#' `npage` counts the number of pages a table runs for.
#' 
#' ## accessing documentation from R
#' 
#' Let's look at the example of the "snow.sno" file, which controls snowfall/snowmelt processes:
rswat_docs() %>% filter(file=='snow.sno')

#' This starts on page 176 and runs for three pages. To view the full text of one of these pages
#' in R, simply pass a page number or a vector of them:

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

# if you like tibbles, use `full=TRUE` and pipe to `tibble`
rswat_docs('snow.sno', full=TRUE) %>% tibble %>% print

#' Since R is printing the literal description strings, which often contain newline characters, the
#' output is full of "\\n"s, which causes the description text to look messy. If we pipe these strings
#' to `cat`, the newlines will render correctly and show us the original text (more or less) as it
#' appeared in the PDF.
#' 
#' By default, `rswat_docs` does this automatically whenever a search query yields a unique match.
#' For example the following call selects the "falltmp" variable from "snow.sno". Since this the only
#' match for "falltmp", the function prints its full description in addition to returning the dataframe:

# demonstrate filename argument and single match behaviour
rswat_docs('falltmp', fname='snow.sno')

#' In this case the first argument (`pattern`) is our search keyword, and we have passed the filename
#' in the second argument (`fname`). `rswat_docs` will automatically detect when `pattern` is a filename
#' and return the full table of variables for the file as it did above, with the exception of
#' description-only queries (where `descw=1`); an example of this can be found in the next section.
#' 
#' ## searching for keywords 
#' 
#' Some SWAT+ variable names are unique enough to turn up a single match in searches. For example we
#' can repeat the previous call without `fname` and get the same unique result:

# demonstrate unique single match among all files
rswat_docs('falltmp')

#' By default, when exact (substring) matches are found, `rswat_docs` returns only them. If no
#' exact matches are found, it returns approximate matches. This can be pretty effective for tracking
#' down variable names when you have a rough idea of the name but you're not sure about the exact
#' abbreviation in use. 
#' 
#' For example we can find entries related to the Hargreaves-Samani PET model like this:

# search for "Hargreaves" in descriptions and names
rswat_docs('Hargreaves')

#' Three parameters are matched (exactly) because the literal string 'Hargreaves' appears in their
#' descriptions. By default `rswat_docs` searches both names and descriptions, but this can be 
#' changed via parameter `descw`. This is a number from 0 to 1 (inclusive) giving the weight
#' to assign to matches in descriptions (versus matches in names) when ordering results.
#' 
#' For example with `descw=0` descriptions are ignored and only names are searched:

# search names only
rswat_docs('Hargreaves', descw=0)

#' In this case no exact matches are found so the function reverts to approximate matching
#' and finds a unique best result ("harg_pet", which turned up first in the last search).
#' This match is based on character order and length. The other results from before, "pet"
#' and "ipet" have little in common with "Hargreaves" in that sense, so they are not matched.
#' 
#' Description-only searching (`descw=1`) is useful for finding links between tables:

# demonstrate description-only search 
rswat_docs('snow.sno', descw=1)

#' In this case `pattern` is a filename, but since `descw=1` it is interpreted as a search query.
#' So instead of returning the 10 variables in the file "snow.sno" (as before), the function searches
#' for the keyword "snow.sno" in the description text, and it finds one exact match. This match is to
#' the description of variable"snow" from another file, "hru-data.hru", which provides a key for joining
#' to the rows in "snow.sno"
#' 
#' The default `descw=0.5` provides equal weight to descriptions and names. This works well for
#' matching broad keywords to variable names that may or may not be an abbreviation of those keywords.
#' For example a search for "tile" turns up a number of variables related to tile drainage on the
#' basis of their descriptions
 
# demonstrate mixture of name and description matches
rswat_docs('tile')

#'
#' ## multiple keywords and fuzzy matching

#' Two of the results in the last call (from file "septic.sep") were matched only because of the
#' word "tex**tile**" in their descriptions. To avoid these kinds of false positives you can turn
#' off substring matching with `fuzzy=-1`

# example of exact (no substring) matching
rswat_docs('tile', fuzzy=-1)

#' The redundant results are gone but we are now missing the "cn" result (in file "codes.bsn"), because
#' it uses the term "tiled-drained". If you need to match more than one keyword in one search, you can
#' separate multiple words in `pattern` by a pipe: 
 
# example of multiple keywords with OR operation
rswat_docs('tile|tiled', fuzzy=-1)

#' Pipes in `pattern` are interpreted as a logical OR operator. Similarly, whitespace works like an
#' approximate OR operator; whitespace-delimited elements of `pattern` are searched separately,
#' and their string distances averaged to rank results:
 
# example of whitespace delimited keywords
rswat_docs('tile runoff ratio')

#' This works pretty well for identifying a small number of "best" matches to a set of keywords.
#' In this case we dropped the `fuzzy` argument, so the function uses the default 0 (exact
#' substring matching). If you want more results, set `fuzzy` to have a value >= 1:

# example of higher fuzziness level
rswat_docs('tile runoff ratio', fuzzy=1)

#' One additional result is added to the list. With positive `fuzzy`, the function ranks all
#' approximate matches by their "distance" to the search pattern, bins the results into groups of
#' (nearly) equal rank, and returns the first `ceiling(fuzzy)` groups.
#' 
#' More simply, this just means `fuzzy` controls the number of results indirectly. Note that you may
#' get more than one additional result by setting `fuzzy=1`, and more than two with `fuzzy=2`, etc. To
#' request a fixed number of results, say `n`, simply set `fuzzy=Inf` to return everything, and pipe the
#' results to `head(n)`:

# example of setting a fixed number of results (10)
rswat_docs('tile runoff ratio', fuzzy=Inf) %>% head(10)

#' Here, `rswat_docs` returns a dataframe with all 1080 possible matches, and `head(10)` extracts
#' the top 10. The dataframe rows are ordered from best to worst, so eg. the first three matches
#' in this case are the same as what we got with `fuzzy=1`

#'
#' ## thoughts and development plans
#' 
#' This is my first attempt at writing a text search tool, so it's pretty simple and ad-hoc in many
#' ways. But I think it does the job well enough. Feel free to use it in your own project. If there's
#' any interest in an R package based on this code, please let me know and I'll start tidying it up for 
#' CRAN. 
#' 
#' Note: I don't know if this tool finds and parses every table in the PDF correctly. It relies on
#' what I *assumed* are consistent patterns in the formatting of the document; eg. all-caps variable
#' names, left-justified table entries, filename headers preceeding the table on a line of their own.
#' Exceptions can cause the code to only partially read a table, or fail entirely to detect it.
#' This appears to work for all of the tables that I have so far been interested in, but I don't have
#' time to go through and manually count table entries to verify this myself.
#' 
#' Note also that the PDF itself has some typos and errors. For example the 'channel.cha' table has
#' names and descriptions offset by one row, and most of the names for 'nutrients.cha' have not been
#' updated to their SWAT+ versions. `rswat_docs` can't detect or repair these kinds of problems, it
#' simply renders whatever is in the PDF. 
#' 
#' In future it would be good to improve support for boolean queries and maybe do some indexing on
#' startup to make it snappier in `fuzzy=-1` mode. I also plan to work on extensions that support the
#' SWAT2012 documentation files.
#' 

#+ include=FALSE
# Development code
# library(here)
# source(here('R/helper_main.R'))
# my_markdown('demo_rswat_docs', 'R/demo')  
  