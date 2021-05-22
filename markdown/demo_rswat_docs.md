demo\_rswat\_docs.R
================
Dean Koch
2021-05-22

**Mitacs UYRW project**

**demo\_rswat\_docs.R**: Access variable definition tables in the SWAT+
documentation PDF from R

This script demonstrates
[rswat\_docs.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md),
which reads and parses the SWAT+ inputs PDF
(“inputs\_swatplus\_rev60\_5.pdf”) into a large name/definition
dataframe, whose entries can then be searched or printed to the console.

## introduction

Users of the SWAT+ model may find the number of built-in process
submodels and associated parameters a bit overwhelming. There are a LOT
of variables to keep track of. The latest configuration file
documentation PDF (“inputs\_swatplus\_rev60\_5.pdf”, [available
here](https://swatplus.gitbook.io/docs/user/io)) runs over 250 pages\!
And that’s just input variable name definitions.

It’s too much to scroll through all the time, so I’ve written an
indexing and search tool to help R-based SWAT+ users like me navigate
this document. This code will render the PDF into text strings, building
a giant searchable list of all “Variable Name”/“Definition” entries.

## Why not just ctrl-f?

You could search for keywords using your favorite PDF viewer, except
that:

  - the ‘find’ tool in viewers like Acrobat or Sumatra searches for
    literal strings, so to get straight to a definition you have to be
    able to remember (or guess) the particular abbreviation in use, or a
    substring of it
  - many abbreviations aren’t really guessable for the unitiated
    (“SED\_DET” is a… rainfall distribution code?)
  - a generic variable name like ‘LONG’ or ‘FLO’ can appear dozens of
    times in the document before you get to the definition you’re
    interested in
  - variable names are slightly different in SWAT+ versus SWAT2012 and
    the theory documentation was written for the latter

By my count there are 1000+ variable names defined in this document - I
don’t think anyone is going to memorize them all. So when you know what
you’re looking for but can’t quite remember the exact name it can be a
real chore to track it down. `rswat_docs` helps with this by allowing
fuzzy matching of multiple keywords to both names and definitions.

## libraries and installation

The easiest way to try this code out yourself is to simply download the
Rscript sourced below
([rswat\_docs.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_docs.md))
and install any missing packages on your machine.

``` r
# library `here` is not strictly necessary; I just use it to simplify paths
library(here)
source(here('R/rswat_docs.R'))
```

    ## Warning: package 'pdftools' was built under R version 4.0.5

    ## Using poppler version 21.04.0

`rswat_docs.R` will initialize an environment called `.rswat` in your
session, and define four functions; `rswat_docs`, `rswat_pdf_open`,
`rswat_pdf_parse`, and `my_adist`.

## getting started

Users will need to download “inputs\_swatplus\_rev60\_5.pdf” from the
SWAT+ [I/O docs](https://swatplus.gitbook.io/docs/user/io) page and
change the PDF path variable below to point to their local copy.
`rswat_pdf_open` will then load and parse the PDF for you:

``` r
# set the PDF path
pdfpath = here('development/inputs_swatplus_rev60_5.pdf')

# load the file into memory
rswat_pdf_open(pdfpath, quiet=TRUE)
```

    ## done

This function call does a lot of stuff in the background - it uses the
`pdftools` package to render text boxes into character strings, then
loops over pages to parse table entries into a large dataframe.
Definitions spanning multiple lines get concatenated, and tables
spanning multiple pages get merged automatically.

Print a list of files in this database using `rswat_docs()`

``` r
# list available files (print only the first 25)
rswat_docs() %>% head(25)
```

    ##               file ndef npage startpage
    ## 1         file.cio    1     1         2
    ## 2         time.sim    5     1         3
    ## 3        print.prt   46     3         5
    ## 4       object.prt    6     1         7
    ## 5       object.cnt   20     1         8
    ## 6  constituents.cs    6     1        17
    ## 7        codes.bsn   24     4        18
    ## 8   parameters.bsn   36    12        21
    ## 9  weather-sta.cli   10     2        33
    ## 10 weather-wgn.cli    6     2        34
    ## 11    wind-dir.cli    5     1        41
    ## 12         pcp.cli   10     2        42
    ## 13         tmp.cli   10     2        43
    ## 14         slr.cli    9     1        45
    ## 15         hmd.cli    5     1        46
    ## 16         wnd.cli    9     1        47
    ## 17        atmo.cli   11     1        48
    ## 18         hru.con   17     2        51
    ## 19     initial.cha    5     1        53
    ## 20     channel.cha   10     1        54
    ## 21   hydrology.cha    4     1        55
    ## 22    sediment.cha   13     1        56
    ## 23   nutrients.cha   40    10        57
    ## 24 channel-lte.cha   11     2        67
    ## 25      metals.cha    2     1        67

``` r
rswat_docs() %>% nrow
```

    ## [1] 108

The parser finds 108 different name-definition tables in the document,
each associated with a different SWAT+ configuration file. The `ndef`
field counts the number of definitions for each table, `startpage`
indicates the page (in the PDF) on which this table starts, and `npage`
counts the number of pages a table runs for.

## accessing documentation from R

Let’s look at the example of the “snow.sno” file, which controls
snowfall/snowmelt processes:

``` r
rswat_docs() %>% filter(file=='snow.sno')
```

    ##       file ndef npage startpage
    ## 1 snow.sno   10     3       176

This starts on page 176 and runs for three pages. To view the full text
of one of these pages in R, simply pass a page number or a vector of
them:

``` r
# print a specific page of the PDF
rswat_docs(176)
```

    ## 
    ## ~~~ page 176 ~~~
    ##        160     SWAT+ INPUT/OUTPUT FILE DOCUMENTATION, VERSION 2016
    ##         Variable name          Definition
    ##         MINPS                  Concentration of mineral phosphorus in the septic tank
    ##                                effluent (mg/L). Required.
    ##         ORGPS                  Organic phosphorus in STE (mg-P/L). ORGP for a
    ##                                conventional system is typically 1 mg-p/L.
    ##                                Required.
    ##         FCOLIS                 Total number of fecal coliform in STE (cfu/100mL).
    ##                                FCOLI for a conventional system is typically 1E7
    ##                                cfu/100mL. The value varies greatly for different types of
    ##                                septic systems (See Table A-1 of Siegrist et al., 2005).
    ##                                Required.
    ## 
    ## SNOW.SNO
    ## The SNOW.SNO file contains the input variables for snow. Below is a partial listing of the snow.sno file.
    ## 
    ## snow.sno
    ## NAME     FALLTMP MELTTMP MELTMX MELTMN TIMP     COVMX COV50    INIT_MM
    ## snow001       1.0     2.0     6.0    3.0    1.0     0.0    0.0       0.0
    ## 
    ## 
    ##       Variable name       Definition
    ##       TITLE               The first line is reserved for user comments. This line is not
    ##                           processed by the model and may be left blank.
    ##                           Optional.
    ##       HEADER              Headers for the snow.sno file.
    ##       NAME                Name of the snow parameters
    ##       FALLTMP             Snowfall temperature (ºC).
    ##                           Mean air temperature at which precipitation is equally likely
    ##                           to be rain as snow/freezing rain. The snowfall temperature
    ##                           should be between –5 ºC and 5 ºC.
    ##                           A default recommended for this variable is SFTMP = 1.0.
    ##                           Required in watersheds where snowfall is significant.
    ##       MELTTMP             Snow melt base temperature (ºC).
    ##                           The snow pack will not melt until the snow pack temperature
    ##                           exceeds a threshold value, Tmlt. The snow melt base
    ##                           temperature should be between –5 ºC and 5 ºC.
    ##                           A default recommended for this variable is SMTMP = 0.50.
    ##                           Required in watersheds where snowfall is significant.

We can see that this page contains the start of the snow parameters
table, and also the tail end of a previous table (“septic.sep”). The
“snow.sno” table runs for two more pages. View the entire table as a
dataframe by passing the associated filename to `rswat_docs`:

``` r
# load table for an example filename
rswat_docs('snow.sno')
```

    ## 10 result(s) in file(s) snow.sno

    ##       name     file pstart                                                     description
    ## 1    title snow.sno    176 The first line is reserved for user comments. This line is n...
    ## 2     name snow.sno    176                                     Name of the snow parameters
    ## 3  falltmp snow.sno    176 Snowfall temperature (ºC). Mean air temperature at which pre...
    ## 4  melttmp snow.sno    176 Snow melt base temperature (ºC). The snow pack will not melt...
    ## 5   meltmx snow.sno    177 Melt factor for snow on June 21 (mm H2O/ºC-day). If the wate...
    ## 6   meltmn snow.sno    178 Melt factor for snow on December 21 (mm H2O/ºC-day). If the ...
    ## 7     timp snow.sno    178 Snow pack temperature lag factor. The influence of the previ...
    ## 8    covmx snow.sno    178                             Minimum snow water content (mm H20)
    ## 9    cov50 snow.sno    178                                               Fraction of COVMX
    ## 10 init_mm snow.sno    178               Initial snow water content at start of simulation

Longer definitions are clipped (they end with ‘…’) so that they can fit
cleanly into an R console printout of a dataframe (similar to
`tibble::print.tbl`). To access the full definition, you can use the
`full=TRUE` argument

``` r
# if you like tibbles, use `full=TRUE` and pipe to `tibble`
rswat_docs('snow.sno', full=TRUE) %>% tibble %>% print
```

    ## 10 result(s) in file(s) snow.sno 
    ## # A tibble: 10 x 4
    ##    name    file    pstart description                                                                                   
    ##    <chr>   <chr>    <int> <chr>                                                                                         
    ##  1 title   snow.s~    176 "The first line is reserved for user comments. This line is not\nprocessed by the model and m~
    ##  2 name    snow.s~    176 "Name of the snow parameters"                                                                 
    ##  3 falltmp snow.s~    176 "Snowfall temperature (ºC).\nMean air temperature at which precipitation is equally likely\nt~
    ##  4 melttmp snow.s~    176 "Snow melt base temperature (ºC).\nThe snow pack will not melt until the snow pack temperatur~
    ##  5 meltmx  snow.s~    177 "Melt factor for snow on June 21 (mm H2O/ºC-day).\nIf the watershed is in the Northern Hemisp~
    ##  6 meltmn  snow.s~    178 "Melt factor for snow on December 21 (mm H2O/ºC-day).\nIf the watershed is in the Northern He~
    ##  7 timp    snow.s~    178 "Snow pack temperature lag factor.\nThe influence of the previous day’s snow pack temperature~
    ##  8 covmx   snow.s~    178 "Minimum snow water content (mm H20)"                                                         
    ##  9 cov50   snow.s~    178 "Fraction of COVMX"                                                                           
    ## 10 init_mm snow.s~    178 "Initial snow water content at start of simulation"

Since R is printing the literal description strings, which often contain
newline characters, the output is full of “\\n”s, which causes the
description text to look messy. If we pipe these strings to `cat`, the
newlines will render correctly and show us the original text (more or
less) as it appeared in the PDF.

By default, `rswat_docs` does this automatically whenever a search query
yields a unique match. For example the following call selects the
“falltmp” variable from “snow.sno”. Since this the only match for
“falltmp”, the function prints its full description in addition to
returning the dataframe:

``` r
# demonstrate filename argument and single match behaviour
rswat_docs('falltmp', fname='snow.sno')
```

    ## 1 exact result(s) for "falltmp" in 1 file(s)
    ## 
    ## ~~~ snow.sno : falltmp ~~~
    ## Snowfall temperature (ºC).
    ## Mean air temperature at which precipitation is equally likely
    ## to be rain as snow/freezing rain. The snowfall temperature
    ## should be between –5 ºC and 5 ºC.
    ## A default recommended for this variable is SFTMP = 1.0.
    ## Required in watersheds where snowfall is significant.

    ##      name     file pstart                                                     description
    ## 1 falltmp snow.sno    176 Snowfall temperature (ºC). Mean air temperature at which pre...

In this case the first argument (`pattern`) is our search keyword, and
we have passed the filename in the second argument (`fname`).
`rswat_docs` will automatically detect when `pattern` is a filename and
return the full table of variables for the file as it did above, with
the exception of description-only queries (where `descw=1`); an example
of this can be found in the next section.

## searching for keywords

Some SWAT+ variable names are unique enough to turn up a single match in
searches. For example we can repeat the previous call without `fname`
and get the same unique result:

``` r
# demonstrate unique single match among all files
rswat_docs('falltmp')
```

    ## 1 exact result(s) for "falltmp" in 1 file(s)
    ## 
    ## ~~~ snow.sno : falltmp ~~~
    ## Snowfall temperature (ºC).
    ## Mean air temperature at which precipitation is equally likely
    ## to be rain as snow/freezing rain. The snowfall temperature
    ## should be between –5 ºC and 5 ºC.
    ## A default recommended for this variable is SFTMP = 1.0.
    ## Required in watersheds where snowfall is significant.

    ##      name     file pstart                                                     description
    ## 1 falltmp snow.sno    176 Snowfall temperature (ºC). Mean air temperature at which pre...

By default, when exact (substring) matches are found, `rswat_docs`
returns only them. If no exact matches are found, it returns approximate
matches. This can be pretty effective for tracking down variable names
when you have a rough idea of the name but you’re not sure about the
exact abbreviation in use.

For example we can find entries related to the Hargreaves-Samani PET
model like this:

``` r
# search for "Hargreaves" in descriptions and names
rswat_docs('Hargreaves')
```

    ## 3 exact result(s) for "Hargreaves" in 3 file(s)

    ##       name          file pstart                                                     description
    ## 1 harg_pet hydrology.hyd    126    Coefficient related to radiation used in Hargreaves equation
    ## 2     ipet   hru-lte.hru     88 Potential evapotranspiration (PET) method (character): ‘harg...
    ## 3      pet     codes.bsn     18 Potential evapotranspiration (PET) method. There are four op...

Three parameters are matched (exactly) because the literal string
‘Hargreaves’ appears in their descriptions. By default `rswat_docs`
searches both names and descriptions, but this can be changed via
parameter `descw`. This is a number from 0 to 1 (inclusive) giving the
weight to assign to matches in descriptions (versus matches in names)
when ordering results.

For example with `descw=0` descriptions are ignored and only names are
searched:

``` r
# search names only
rswat_docs('Hargreaves', descw=0)
```

    ## No exact matches for "Hargreaves". Repeating search at fuzzy level 1 
    ## 1 approximate result(s) for "Hargreaves" in 1 file(s)
    ## 
    ## ~~~ hydrology.hyd : harg_pet ~~~
    ## Coefficient related to radiation used in Hargreaves equation

    ##       name          file pstart                                                  description
    ## 1 harg_pet hydrology.hyd    126 Coefficient related to radiation used in Hargreaves equation

In this case no exact matches are found so the function reverts to
approximate matching and finds a unique best result (“harg\_pet”, which
turned up first in the last search). This match is based on character
order and length. The other two results from before, “pet” and “ipet”,
have little in common with “Hargreaves” in that sense, so they are not
matched.

Description-only searching (`descw=1`) is useful for finding links
between tables:

``` r
# demonstrate description-only search 
rswat_docs('snow.sno', descw=1)
```

    ## 1 exact result(s) for "snow.sno" in 1 file(s)
    ## 
    ## ~~~ hru-data.hru : snow ~~~
    ## Snow database name (points to snow.sno)

    ##   name         file pstart                             description
    ## 1 snow hru-data.hru     86 Snow database name (points to snow.sno)

In this case `pattern` is a filename, but since `descw=1` it is
interpreted as a search query. So instead of returning the 10 variables
in the file “snow.sno” (as before), the function searches for the
keyword “snow.sno” in the description text, and it finds one exact
match. This match is to the description of variable“snow” from another
file, “hru-data.hru”, which provides a key for joining to the rows in
“snow.sno”

The default `descw=0.5` provides equal weight to descriptions and names.
This works well for matching broad keywords to variable names that may
or may not be an abbreviation of those keywords. For example a search
for “tile” turns up a number of variables related to tile drainage on
the basis of their descriptions

``` r
# demonstrate mixture of name and description matches
rswat_docs('tile')
```

    ## 9 exact result(s) for "tile" in 6 file(s)

    ##        name              file pstart                                                     description
    ## 1 tiledrain       landuse.lum    188                  Tile drain file name (points to tiledrain.str)
    ## 2       lag     tiledrain.str    129                                             Drain tile lag time
    ## 3       tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff
    ## 4      dist     tiledrain.str    129 Distance between two drain tubes or tiles (mm) Range (7600 –...
    ## 5  drain_co     tiledrain.str    129 Daily drainage coefficient (mm day-1). Tile drainage routine...
    ## 6      tdrn         codes.bsn     21 Tile drainage equations flag/code Tile drainage routines fla...
    ## 7       typ        septic.str    130 The type of septic system Type Definition 1 Generic type con...
    ## 8     sepnm        septic.sep    173 Abridged name of a septic system sptname Definition GCON Gen...
    ## 9        cn         codes.bsn     20 Daily curve number calculation method: 0 calculate daily CN ...

## multiple keywords and fuzzy matching

Two of the results in the last call (from file “septic.sep”) were
matched only because of the word “tex**tile**” in their descriptions. To
avoid these kinds of false positives you can turn off substring matching
with `fuzzy=-1`

``` r
# example of exact (no substring) matching
rswat_docs('tile', fuzzy=-1)
```

    ## 5 exact result(s) for "tile" in 4 file(s)

    ##        name              file pstart                                                     description
    ## 1      tdrn         codes.bsn     21 Tile drainage equations flag/code Tile drainage routines fla...
    ## 2       lag     tiledrain.str    129                                             Drain tile lag time
    ## 3  drain_co     tiledrain.str    129 Daily drainage coefficient (mm day-1). Tile drainage routine...
    ## 4 tiledrain       landuse.lum    188                  Tile drain file name (points to tiledrain.str)
    ## 5       tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff

The redundant results are gone but we are now missing the “cn” result
(in file “codes.bsn”), because it uses the term “tiled-drained”. If you
need to match more than one keyword in one search, you can separate
multiple words in `pattern` by a pipe:

``` r
# example of multiple keywords with OR operation
rswat_docs('tile|tiled', fuzzy=-1)
```

    ## 6 exact result(s) for "tile|tiled" in 4 file(s)

    ##        name              file pstart                                                     description
    ## 1        cn         codes.bsn     20 Daily curve number calculation method: 0 calculate daily CN ...
    ## 2      tdrn         codes.bsn     21 Tile drainage equations flag/code Tile drainage routines fla...
    ## 3       lag     tiledrain.str    129                                             Drain tile lag time
    ## 4  drain_co     tiledrain.str    129 Daily drainage coefficient (mm day-1). Tile drainage routine...
    ## 5 tiledrain       landuse.lum    188                  Tile drain file name (points to tiledrain.str)
    ## 6       tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff

Pipes in `pattern` are interpreted as a logical OR operator. Similarly,
whitespace works like an approximate OR operator; whitespace-delimited
elements of `pattern` are searched separately, and their string
distances averaged to rank results:

``` r
# example of whitespace delimited keywords
rswat_docs('tile runoff ratio')
```

    ## 2 exact result(s) for "tile runoff ratio" in 2 file(s)

    ##   name              file pstart                                                     description
    ## 1  tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff
    ## 2   cn         codes.bsn     20 Daily curve number calculation method: 0 calculate daily CN ...

This works pretty well for identifying a small number of “best” matches
to a set of keywords. In this case we dropped the `fuzzy` argument, so
the function uses the default 0 (exact substring matching). If you want
more results, set `fuzzy` to have a value \>= 1:

``` r
# example of higher fuzziness level
rswat_docs('tile runoff ratio', fuzzy=1)
```

    ## 3 approximate result(s) for "tile runoff ratio" in 3 file(s)

    ##       name              file pstart                                                     description
    ## 1      tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff
    ## 2       cn         codes.bsn     20 Daily curve number calculation method: 0 calculate daily CN ...
    ## 3 vfsratio   filterstrip.str    134 Ratio of field area to filter strip area (unitless). Ranges ...

One additional result is added to the list. With positive `fuzzy`, the
function ranks all approximate matches by their “distance” to the search
pattern, bins the results into groups of (nearly) equal rank, and
returns the first `ceiling(fuzzy)` groups.

More simply, this just means `fuzzy` controls the number of results
indirectly. Note that you may get more than one additional result by
setting `fuzzy=1`, and more than two with `fuzzy=2`, etc. To request a
fixed number of results, say `n`, simply set `fuzzy=Inf` to return
everything, and pipe the results to `head(n)`:

``` r
# example of setting a fixed number of results (10)
rswat_docs('tile runoff ratio', fuzzy=Inf) %>% head(10)
```

    ## 1066 approximate result(s) for "tile runoff ratio" in 108 file(s)

    ##         name              file pstart                                                     description
    ## 1        tfr water_balance.sft    208                        Tile flow ratio – tile flow/total runoff
    ## 2         cn         codes.bsn     20 Daily curve number calculation method: 0 calculate daily CN ...
    ## 3   vfsratio   filterstrip.str    134 Ratio of field area to filter strip area (unitless). Ranges ...
    ## 4  tiledrain       landuse.lum    188                  Tile drain file name (points to tiledrain.str)
    ## 5     nperco    parameters.bsn     27 Nitrate percolation coefficient. NPERCO controls the amount ...
    ## 6     percop    parameters.bsn     30 Pesticide percolation coefficient. PERCOP controls the amoun...
    ## 7    urbcoef         urban.urb    169 Wash-off coefficient for removal of constituents from imperv...
    ## 8     erorgn     hydrology.hyd    125 Organic N enrichment ratio for loading with sediment. As sur...
    ## 9   pst_wsol     pesticide.pst    165 Solubility of the chemical in water (mg/L or ppm) The water ...
    ## 10     title      delratio.del    113                              The title of the delratio.del file

Here, `rswat_docs` returns a dataframe with all 1066 possible matches,
and `head(10)` extracts the top 10. The dataframe rows are ordered from
best to worst, so eg. the first three matches in this case are the same
as what we got with `fuzzy=1`

## thoughts and development plans

This is my first attempt at writing a text search tool, so it’s pretty
simple and ad-hoc in many ways. But I think it does the job well enough.
Feel free to use it in your own project. If there’s any interest in an R
package based on this code, please let me know and I’ll start tidying it
up for CRAN.

Note that I make no guarantees that this tool will find and parse every
table in the PDF correctly. It relies on what I *assume* are consistent
patterns in the formatting of the document (after rendering by
`pdftools::pdf_text`). eg. all-caps variable names, left-justified table
entries, filename headers preceeding the table on a line of their own.
This appears to work for all of the tables that I have so far been
interested in, but it’s possible I am missing others.

Note also that the PDF itself has some typos and errors. For example the
‘channel.cha’ table has names and descriptions offset by one row, and
most of the names for ‘nutrients.cha’ have not been updated to their
SWAT+ versions. `rswat_docs` can’t detect or repair these kinds of
problems, it simply renders whatever is in the PDF.

In future it would be good to improve support for boolean queries and
maybe do some indexing on startup to make it snappier in `fuzzy=-1`
mode. I also plan to work on extensions that support the SWAT2012
documentation files.
