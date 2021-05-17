demo\_rswat\_docs.R
================
Dean Koch
2021-05-17

**Mitacs UYRW project**

**demo\_rswat\_docs.R**: Access variable definition tables in SWAT+
documentation PDF from R

This script demonstrates the `rswat_docs` function, which reads and
parses the SWAT+ inputs PDF (“CHAPTER FILE.CIO: SWAT+ INPUT DATA”)
returning its name/definition table entries as dataframes, or printing
them to the console.

## introduction

Users of the SWAT+ model may find the number of built-in process
submodels and associated parameters a bit overwhelming. There are a LOT
of variables to keep track of. The latest configuration file
documentation PDF (“inputs\_swatplus\_rev60\_5.pdf”, [available
here](https://swatplus.gitbook.io/docs/user/io)) runs over 250 pages\!
And that’s just variable name definitions.

As I learn the model, I find I’m spending a lot of time scrolling this
PDF to look up definitions, units, and coding schemes. This is fine at
the first, as you don’t want to just skim through and miss something
important when you’re learning. But later on, when using the doc as a
reference, it gets tedious. Variable names are often mentioned multiple
times in the document before we get to their full definitions, and it
can be hard to track down the SWAT+ version of a SWAT name (or names you
can’t remember exactly) using just ctrl-F.

To streamline things I’ve written some helper functions that load the
PDF and build a big list of all its “Variable Name” - “Definition”
entries. The list can be searched using approximate matching (allowing
character substitutions, etc). R users may find this a quicker and more
direct route to accessing the variable definitions they need, and for
exploring the model.

## libraries

[pdftools](https://cran.r-project.org/web/packages/pdftools/index.html)
is required to render the PDF textboxes as character vectors in R,
[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
load some required libraries, global variables, and some helper
functions.

``` r
library(here)
library(pdftools)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))
```

Note that “rswat.R” is sourced in the line above to initialize the
`.rswat` environment in your session. The relevant functions definitions
in that file are `rswat_docs`, `rswat_pdf_open`, `rswat_pdf_parse`, and
`rswat_match`. Other functions are not needed at this point.

## getting started

Users will need to download “inputs\_swatplus\_rev60\_5.pdf” from the
SWAT+ [I/O docs](https://swatplus.gitbook.io/docs/user/io) page and
change the PDF path variable below to point to their local copy.
`rswat_pdf_open` will then load and parse the PDF for you:

``` r
# set the PDF path
pdfpath = 'D:/UYRW_data/development/inputs_swatplus_rev60_5.pdf'

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
    ## 24 channel-lte.cha   34     3        67
    ## 25      metals.cha    2     1        67

``` r
rswat_docs() %>% nrow
```

    ## [1] 105

The parser finds 105 different name-definition tables in the document,
each associated with a different SWAT+ configuration file. The `ndef`
field counts the number of definitions for each table, `startpage`
indicates the page (in the PDF) on which this table starts, and `npage`
counts the number of pages it runs.

Let’s look at the example of the “snow.sno” file, which controls
snowfall/snowmelt processes:

``` r
rswat_docs() %>% filter(file=='snow.sno')
```

    ##       file ndef npage startpage
    ## 1 snow.sno   10     3       176

This starts on page 176 and runs for three pages. To view the full text
of one of these pages in R, simply pass a page number or a vector of
them to `rswat_docs`:

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
# if you like tibbles, use `full=TRUE` and pipe to `tibble` to copy the full descriptions
rswat_docs('snow.sno', full=TRUE) %>% tibble %>% print
```

    ## 10 result(s) in file(s) snow.sno 
    ## # A tibble: 10 x 4
    ##    name    file    pstart description                                                                           
    ##    <chr>   <chr>    <int> <chr>                                                                                 
    ##  1 title   snow.s~    176 "The first line is reserved for user comments. This line is not\nprocessed by the mod~
    ##  2 name    snow.s~    176 "Name of the snow parameters"                                                         
    ##  3 falltmp snow.s~    176 "Snowfall temperature (ºC).\nMean air temperature at which precipitation is equally l~
    ##  4 melttmp snow.s~    176 "Snow melt base temperature (ºC).\nThe snow pack will not melt until the snow pack te~
    ##  5 meltmx  snow.s~    177 "Melt factor for snow on June 21 (mm H2O/ºC-day).\nIf the watershed is in the Norther~
    ##  6 meltmn  snow.s~    178 "Melt factor for snow on December 21 (mm H2O/ºC-day).\nIf the watershed is in the Nor~
    ##  7 timp    snow.s~    178 "Snow pack temperature lag factor.\nThe influence of the previous day’s snow pack tem~
    ##  8 covmx   snow.s~    178 "Minimum snow water content (mm H20)"                                                 
    ##  9 cov50   snow.s~    178 "Fraction of COVMX"                                                                   
    ## 10 init_mm snow.s~    178 "Initial snow water content at start of simulation"

Notice the *slash-“n”’s*, which indicate where a newline happened in the
source file. Since R is printing the literal strings here, the output
looks ugly unless we pipe it to a `cat` call. `rswat_docs` will do this
by default (instead of returning the dataframe) when it finds one single
match for a search query.

``` r
# eg. search for the pattern "title" in filename "snow.sno" like this:
rswat_docs('title', fname='snow.sno')
```

    ## 1 exact match result(s) among names for "title" in 1 file(s)
    ## 
    ## 
    ## ~~~ snow.sno : title ~~~
    ## The first line is reserved for user comments. This line is not
    ## processed by the model and may be left blank.
    ## Optional.

The result is a printout of the full description as it would appear in
the PDF (including newlines).

## searching names

The first argument of `rswat_docs` can be filename (as in the first two
calls above), or a search string for some keyword (as in the last
function call). When the argument `fname` is missing, all filenames are
included in the search:

``` r
# eg. repeat the last call without specifying "snow.sno" and get 99 matches (print the first few)
rswat_docs('title') %>% head
```

    ## 99 exact match result(s) among names for "title" in 99 file(s)

    ##    name            file pstart                                                     description
    ## 1 title        file.cio      2 The first line of ‘file.cio’ is reserved for a description o...
    ## 2 title       print.prt      5                               Description of the print.prt file
    ## 3 title      object.prt      7                            Description of the object print file
    ## 4 title      object.cnt      8                            Description of the object count file
    ## 5 title constituents.cs     17 The first line of the file is reserved for user comments. Th...
    ## 6 title       codes.bsn     18 The first line is reserved for user comments. This line is n...

By default, when exact matches are found, `rswat_docs` returns (only)
them. If no exact matches are found, it then looks for substring matches
and returns those. If still nothing is found, it then tries approximate
matching with increasing fuzziness until it gets at least one match.

This can be pretty effective for tracking down variable names when you
have a rough idea of the name but you’re not sure about the exact
abbreviation in use. eg. a search for the Hargreaves PET coefficient
finds ‘harg\_pet’ right away:

``` r
rswat_docs('Hargreaves')
```

    ## No matches among names for "Hargreaves".  Reverting to approximate matching...
    ## 1 approximate match result(s) among names for "Hargreaves" in 1 file(s)
    ## 
    ## 
    ## ~~~ hydrology.hyd : harg_pet ~~~
    ## Coefficient related to radiation used in Hargreaves equation

It’s not a very sopthisticated search tool, however, so you will get
false positives with less-unique letter combinations. eg. a search for
(snow) “melt” parameters turns up some things that have nothing to do
with melting

``` r
rswat_docs('melt')
```

    ## No matches among names for "melt".  Reverting to approximate matching...
    ## 8 approximate match result(s) among names for "melt" in 6 file(s)

    ##         name            file pstart                                                     description
    ## 1     meltmx        snow.sno    177 Melt factor for snow on June 21 (mm H2O/ºC-day). If the wate...
    ## 2     meltmn        snow.sno    178 Melt factor for snow on December 21 (mm H2O/ºC-day). If the ...
    ## 3    melttmp        snow.sno    176 Snow melt base temperature (ºC). The snow pack will not melt...
    ## 4   timestep        atmo.cli     48 There are three different timesteps for the file to be read ...
    ## 5    sno_mlt temperature.cha     69      Coefficient influencing snowmelt temperature contributions
    ## 6   cha_hmet channel-lte.cha     68         Channel lte heavy metals file (points to hmet.cha file)
    ## 7 num_metals constituents.cs     17                                Number of heavy metals simulated
    ## 8  op_method  management.sch    192                                         Plant name in community

and a search for “snowmelt” omits some relevant entries from file
“snow.sno” because their names are less similar to the search pattern
than a match found in “temperature.cha”:

``` r
rswat_docs('snowmelt')
```

    ## No matches among names for "snowmelt".  Reverting to approximate matching...
    ## 1 approximate match result(s) among names for "snowmelt" in 1 file(s)
    ## 
    ## 
    ## ~~~ temperature.cha : sno_mlt ~~~
    ## Coefficient influencing snowmelt temperature contributions

## searching descriptions

To avoid the problem above with vague keywords, it’s best to search the
variable *descriptions* text. Specify this search mode using the
`indesc=TRUE` argument:

``` r
# a better snow melt parameter search
rswat_docs('melt', indesc=TRUE)
```

    ## 4 approximate match result(s) among descriptions for "melt" in 2 file(s)

    ##      name            file pstart                                                     description
    ## 1 sno_mlt temperature.cha     69      Coefficient influencing snowmelt temperature contributions
    ## 2 melttmp        snow.sno    176 Snow melt base temperature (ºC). The snow pack will not melt...
    ## 3  meltmx        snow.sno    177 Melt factor for snow on June 21 (mm H2O/ºC-day). If the wate...
    ## 4  meltmn        snow.sno    178 Melt factor for snow on December 21 (mm H2O/ºC-day). If the ...

this turns up two places where a code must be set to enable the
Hargreaves model

``` r
rswat_docs('Hargreaves', indesc=TRUE)
```

    ## 3 approximate match result(s) among descriptions for "Hargreaves" in 3 file(s)

    ##       name          file pstart                                                     description
    ## 1 harg_pet hydrology.hyd    126    Coefficient related to radiation used in Hargreaves equation
    ## 2     ipet   hru-lte.hru     88 Potential evapotranspiration (PET) method (character): ‘harg...
    ## 3      pet     codes.bsn     18 Potential evapotranspiration (PET) method. There are four op...
