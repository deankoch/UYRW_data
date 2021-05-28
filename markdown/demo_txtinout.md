demo\_txtinout.R
================
Dean Koch
2021-05-28

**Mitacs UYRW project**

**demo\_txtinout.R**: managing SWAT+ config and output files with
`rswat`

This script demonstrates some of the core functionality of the `rswat`
helper functions: accessing/changing parameters and settings for the
SWAT+ simulator, running simulations, and loading outputs.

## libraries

[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[rswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat.md)
load required libraries, global variables, and some helper functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))
```

## project data

load the SWAT+ project info and gage data from the previous script
([demo\_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md))

``` r
# load some info about the SWAT+ model
qswat.meta = my_metadata('demo_qswat', data.dir=demo.subdir)
dir.qswat = qswat.meta['dir_qswat', 'file']
nm = qswat.meta['example_name', 'file']
print(nm)
```

    ## [1] "big_c_nr_emigrant"

``` r
# load the gage data for this catchment
gage = readRDS(here(qswat.meta['gage', 'file']))
head(gage)
```

    ##         date             flow
    ## 1 1973-09-01 1.302575 [m^3/s]
    ## 2 1973-09-02 1.160991 [m^3/s]
    ## 3 1973-09-03 1.132674 [m^3/s]
    ## 4 1973-09-04 1.076040 [m^3/s]
    ## 5 1973-09-05 1.019406 [m^3/s]
    ## 6 1973-09-06 1.019406 [m^3/s]

``` r
# set up the SWAT+ project config folder location to use in the demo
zip.path = here( qswat.meta['txtinout', 'file'] )
demo.dir = file.path( demo.subdir, gsub( '.zip', '', basename(zip.path) ) )

# store filenames created by this script
files.towrite = list(
  
  # directory for SWAT+ model files
  c(name='txtinout',
    file=demo.dir,
    type='string',
    description='directory for SWAT+ config files to use in demo'),
  
  # directory for SWAT+ model files
  c(name='txtinout_bak',
    file=file.path(demo.dir, 'backup_harg'),
    type='string',
    description='directory for SWAT+ config files to use in demo')
)
  
# write this filename metadata to disk
txtinout.meta = my_metadata('demo_txtinout', files.towrite, overwrite=TRUE, data.dir=demo.subdir)
```

    ## [1] "> writing metadata to: data/demo/demo_txtinout_metadata.csv"

``` r
# unzip a fresh copy of "TxtInOut" to the demo folder
print(demo.dir)
```

    ## [1] "data/demo/demo_big_c_nr_emigrant_txtinout"

``` r
unlink(here(demo.dir), recursive=TRUE)
unzip(zip.path, exdir=here(demo.dir), overwrite=TRUE)
```

## managing SWAT+ configuration files in R

The SWAT+ executable runs simulations according to the settings written
in a directory of text files, usually called ‘TxtInOut’. This is a large
and complicated set of files that define all parameters of model.
`rswat` is a set of tools for managing and cataloging these files.

If you are new to SWAT, check out the
[I/O](https://swatplus.gitbook.io/docs/user/io) and
[theory](https://swat.tamu.edu/media/99192/swat2009-theory.pdf) PDFs.
The I/O descriptions are relatively new but the theory document is
older, from 2009. This is the most recent, as far as I am aware, and
although core aspects of the the model have not changed much since then,
note that many variable and parameter names are slightly different in
SWAT+.

Once a SWAT+ model has been created (see
[demo\_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md)),
make it known to `rswat` by passing the ‘TxtInOut’ path to `rswat_cio`.
This will set the project directory and read in the master watershed
file “file.cio”. This should work with any up-to-date SWAT+ project
regardless of how it was created (ie. you don’t have to use `qswat_run`)

``` r
# assign the SWAT+ project directory. This builds a list of files in memory
cio = rswat_cio( here(demo.dir) )
```

    ## setting `ciopath` to D:/UYRW_data/data/demo/demo_big_c_nr_emigrant_txtinout/file.cio 
    ## file.cio: written by SWAT+ editor v2.0.0 on 2021-05-28 15:49

Subsequent calls to rswat\_cio() will list all config files in the
project directory. eg here are the first few entries:

``` r
# print the first few rows of the files dataframe
cio %>% head
```

    ##              file      group              size            modified
    ## 1        time.sim simulation 0.168 [kilobytes] 2021-05-28 16:00:02
    ## 2       print.prt simulation 3.389 [kilobytes] 2021-05-28 16:00:02
    ## 3      object.cnt simulation 0.577 [kilobytes] 2021-05-28 16:00:02
    ## 4       codes.bsn      basin 0.599 [kilobytes] 2021-05-28 16:00:02
    ## 5  parameters.bsn      basin 1.296 [kilobytes] 2021-05-28 16:00:02
    ## 6 weather-sta.cli    climate 3.622 [kilobytes] 2021-05-28 16:00:02

Each row of `cio` is a file containing a group of model parameters.
Before changing anything it’s a good idea to have a backup. `rswat_copy`
with argument `fname='.'` will copy the entire contents of the config
files directory (excluding subdirectories) to a backup subdirectory

``` r
# the return value is a vector of file paths to the backups
path.backup = rswat_copy(fname='.', quiet=TRUE)
dir.backup = dirname(path.backup[1])
print(dir.backup)
```

    ## [1] "D:/UYRW_data/data/demo/demo_big_c_nr_emigrant_txtinout/_rswat_backup_filee68212a2596"

to start over later, restore this copy by passing the backup directory
path back to `rswat_copy`

``` r
# restore the backup we just made
opath = rswat_copy(from=dir.backup, overwrite=TRUE, quiet=TRUE)

# delete the backup (we already have a zipped copy elsewhere)
unlink(dir.backup, recursive=TRUE)
```

To load a config file, pass its filename to `rswat_open` eg. the code
below displays the first few rows from the SWAT+ aquifer parameters
file, “aquifer.aqu”

``` r
# find aquifer-related tables
cio %>% filter( grepl('aqu', file) ) %>% print
```

    ##              file   group               size            modified
    ## 1     aquifer.con connect 13.793 [kilobytes] 2021-05-28 16:00:02
    ## 2     initial.aqu aquifer  0.321 [kilobytes] 2021-05-28 16:00:02
    ## 3     aquifer.aqu aquifer 13.481 [kilobytes] 2021-05-28 16:00:02
    ## 4 aqu_catunit.ele regions  5.165 [kilobytes] 2021-05-28 16:00:02

``` r
# this one contains the main process model parameters 
rswat_open('aquifer.aqu', quiet=TRUE) %>% str
```

    ## 'data.frame':    51 obs. of  18 variables:
    ##  $ id       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ name     : chr  "aqu011" "aqu012" "aqu021" "aqu022" ...
    ##  $ init     : chr  "initaqu1" "initaqu1" "initaqu1" "initaqu1" ...
    ##  $ gw_flo   : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ dep_bot  : num  10 10 10 10 10 10 10 10 10 10 ...
    ##  $ dep_wt   : num  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ no3_n    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sol_p    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ carbon   : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ flo_dist : num  50 50 50 50 50 50 50 50 50 50 ...
    ##  $ bf_max   : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ alpha_bf : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ revap    : num  0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 ...
    ##  $ rchg_dp  : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ spec_yld : num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ hl_no3n  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_min  : num  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ revap_min: num  5 5 5 5 5 5 5 5 5 5 ...

After rswat loads a SWAT+ config file into R, it caches the contents,
and the file becomes searchable and faster to open/write in subsequent
calls. `rswat_cio()` will also return some additional summary info:

``` r
# print summary info about the file 'aquifer.aqu'
rswat_cio() %>% filter( file=='aquifer.aqu' )
```

    ##          file   group               size            modified nline nskip ntab nvar
    ## 1 aquifer.aqu aquifer 13.481 [kilobytes] 2021-05-28 16:00:03    51     0    1  936

The ‘nline’ column counts the total number of data rows in the file;
‘nskip’ indicates lines not understood by the parser (probably
comments, but possibly bugs - best to check this manually); ‘ntab’ is
the number of distinct tables in the file (either having different
headers, or being separated by comments), and ‘nvar’ indicates how many
distinct fields there are in the file (nrow \* ncol, summed over all of
the tables, including headers);

To pre-load all files, use the `loadall` flag with `rswat_cio`. This
will take a few seconds to parse all the plaintext and detect headers,
types, and spacing rules. The code below excludes decision table files,
which contain many distinct tables (slow to load) and are not needed for
now.

``` r
# load most of the config files into memory for convenience.
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
print(cio)
```

    ##                 file          group                size            modified nline nskip ntab  nvar
    ## 1           time.sim     simulation   0.168 [kilobytes] 2021-05-28 16:00:03     1     0    1    10
    ## 2          print.prt     simulation   3.389 [kilobytes] 2021-05-28 16:00:03    46     0    5   223
    ## 3         object.cnt     simulation   0.577 [kilobytes] 2021-05-28 16:00:03     1     0    1    42
    ## 4          codes.bsn          basin   0.599 [kilobytes] 2021-05-28 16:00:03     1     0    1    48
    ## 5     parameters.bsn          basin   1.296 [kilobytes] 2021-05-28 16:00:03     1     0    1    88
    ## 6    weather-sta.cli        climate   3.622 [kilobytes] 2021-05-28 16:00:03    16     0    1   153
    ## 7    weather-wgn.cli        climate  42.549 [kilobytes] 2021-05-28 16:00:03   223    16   16  2992
    ## 8            pcp.cli        climate   0.359 [kilobytes] 2021-05-28 16:00:03    16     0    1    17
    ## 9            tmp.cli        climate   0.357 [kilobytes] 2021-05-28 16:00:03    16     0    1    17
    ## 10           hmd.cli        climate   0.363 [kilobytes] 2021-05-28 16:00:03    16     0    1    17
    ## 11           wnd.cli        climate   0.356 [kilobytes] 2021-05-28 16:00:03    16     0    1    17
    ## 12           hru.con        connect   8.425 [kilobytes] 2021-05-28 16:00:03    50     0    1   663
    ## 13     rout_unit.con        connect  16.283 [kilobytes] 2021-05-28 16:00:03    50     0    1  1479
    ## 14       aquifer.con        connect  13.793 [kilobytes] 2021-05-28 16:00:03    51     0    1  1092
    ## 15        recall.con        connect   5.680 [kilobytes] 2021-05-28 16:00:03    25     0    1   442
    ## 16       chandeg.con        connect   5.629 [kilobytes] 2021-05-28 16:00:03    25     0    1   442
    ## 17       initial.cha        channel   0.321 [kilobytes] 2021-05-28 16:00:03     1     0    1    14
    ## 18     nutrients.cha        channel   1.166 [kilobytes] 2021-05-28 16:00:03     1     0    1    80
    ## 19   channel-lte.cha        channel   2.721 [kilobytes] 2021-05-28 16:00:03    25     0    1   156
    ## 20   hyd-sed-lte.cha        channel   8.712 [kilobytes] 2021-05-28 16:00:03    25     0    1   624
    ## 21     rout_unit.def   routing_unit   2.617 [kilobytes] 2021-05-28 16:00:03    50     0    1   204
    ## 22     rout_unit.ele   routing_unit   4.453 [kilobytes] 2021-05-28 16:00:03    50     0    1   306
    ## 23     rout_unit.rtu   routing_unit   5.269 [kilobytes] 2021-05-28 16:00:03    50     0    1   306
    ## 24      hru-data.hru            hru   8.940 [kilobytes] 2021-05-28 16:00:03    50     0    1   510
    ## 25          exco.exc           exco   2.922 [kilobytes] 2021-05-28 16:00:03    25     0    1   156
    ## 26       exco_om.exc           exco   7.137 [kilobytes] 2021-05-28 16:00:03    25     0    1   494
    ## 27        recall.rec         recall   1.572 [kilobytes] 2021-05-28 16:00:03    25     0    1   104
    ## 28       initial.aqu        aquifer   0.321 [kilobytes] 2021-05-28 16:00:03     1     0    1    14
    ## 29       aquifer.aqu        aquifer  13.481 [kilobytes] 2021-05-28 16:00:03    51     0    1   936
    ## 30     hydrology.hyd      hydrology  11.083 [kilobytes] 2021-05-28 16:00:03    50     0    1   765
    ## 31    topography.hyd      hydrology   9.158 [kilobytes] 2021-05-28 16:00:03   100     0    1   606
    ## 32         field.fld      hydrology   3.225 [kilobytes] 2021-05-28 16:00:03    50     0    1   204
    ## 33     tiledrain.str     structural   0.331 [kilobytes] 2021-05-28 16:00:03     1     0    1    18
    ## 34        septic.str     structural   1.211 [kilobytes] 2021-05-28 16:00:03     2     0    1    84
    ## 35   filterstrip.str     structural   0.341 [kilobytes] 2021-05-28 16:00:03     2     0    1    18
    ## 36     grassedww.str     structural   0.560 [kilobytes] 2021-05-28 16:00:03     3     0    1    36
    ## 37       bmpuser.str     structural   0.304 [kilobytes] 2021-05-28 16:00:03     1     0    1    18
    ## 38        plants.plt    hru_parm_db 194.373 [kilobytes] 2021-05-28 16:00:03   256     0    1 13878
    ## 39    fertilizer.frt    hru_parm_db   7.216 [kilobytes] 2021-05-28 16:00:03    59     0    1   480
    ## 40       tillage.til    hru_parm_db   8.311 [kilobytes] 2021-05-28 16:00:03    78     0    1   553
    ## 41     pesticide.pes    hru_parm_db  49.945 [kilobytes] 2021-05-28 16:00:03   233     0    1  3510
    ## 42         urban.urb    hru_parm_db   1.832 [kilobytes] 2021-05-28 16:00:03     9     0    1   130
    ## 43        septic.sep    hru_parm_db   4.644 [kilobytes] 2021-05-28 16:00:03    26     0    1   324
    ## 44          snow.sno    hru_parm_db   0.326 [kilobytes] 2021-05-28 16:00:03     1     0    1    18
    ## 45          harv.ops            ops   1.273 [kilobytes] 2021-05-28 16:00:03    14     0    1    90
    ## 46         graze.ops            ops   1.626 [kilobytes] 2021-05-28 16:00:03    12     0    1    91
    ## 47           irr.ops            ops   0.662 [kilobytes] 2021-05-28 16:00:03     4     0    1    45
    ## 48      chem_app.ops            ops   1.897 [kilobytes] 2021-05-28 16:00:03    12     0    1   130
    ## 49          fire.ops            ops   0.265 [kilobytes] 2021-05-28 16:00:03     3     0    1    16
    ## 50         sweep.ops            ops   0.170 [kilobytes] 2021-05-28 16:00:03     1     0    1     8
    ## 51       landuse.lum            lum   1.613 [kilobytes] 2021-05-28 16:00:03     5     0    1    84
    ## 52       cntable.lum            lum  11.089 [kilobytes] 2021-05-28 16:00:03    52     0    1   424
    ## 53 cons_practice.lum            lum   3.243 [kilobytes] 2021-05-28 16:00:03    38     0    1   156
    ## 54     ovn_table.lum            lum   1.755 [kilobytes] 2021-05-28 16:00:03    20     0    1   105
    ## 55     cal_parms.cal            chg  15.543 [kilobytes] 2021-05-28 16:00:03   184     1    1   921
    ## 56         plant.ini           init   1.151 [kilobytes] 2021-05-28 16:00:03    10     9    1   106
    ## 57    soil_plant.ini           init   0.316 [kilobytes] 2021-05-28 16:00:03     1     0    1    14
    ## 58      om_water.ini           init   0.638 [kilobytes] 2021-05-28 16:00:03     1     0    1    40
    ## 59         soils.sol          soils  41.053 [kilobytes] 2021-05-28 16:00:03   150     0    1  3171
    ## 60     nutrients.sol          soils   0.426 [kilobytes] 2021-05-28 16:00:03     1     0    1    26
    ## 61           lum.dtl decision_table  23.015 [kilobytes] 2021-05-28 16:00:03    NA    NA   NA    NA
    ## 62       res_rel.dtl decision_table 317.488 [kilobytes] 2021-05-28 16:00:03    NA    NA   NA    NA
    ## 63       scen_lu.dtl decision_table   9.514 [kilobytes] 2021-05-28 16:00:03    NA    NA   NA    NA
    ## 64       flo_con.dtl decision_table  10.361 [kilobytes] 2021-05-28 16:00:03    NA    NA   NA    NA
    ## 65       ls_unit.ele        regions   5.063 [kilobytes] 2021-05-28 16:00:03    50     0    1   357
    ## 66       ls_unit.def        regions   3.333 [kilobytes] 2021-05-28 16:00:03    51     1    1   256
    ## 67   aqu_catunit.ele        regions   5.165 [kilobytes] 2021-05-28 16:00:03    51     0    1   364

The summary info is now available for most of the files (get even more
detail by toggling `trim=FALSE`) and any variable names appearing among
the table headers of these files now becomes searchable using
`rswat_find`. This can be useful for tracking down a SWAT+ parameter
using keywords or SWAT2012 names. This uses fuzzy case-insensitive
matching (see R’s `?agrep` doc), which catches many of the name changes
in the SWAT2012 -\> SWAT+ updates.

eg. the following code finds the PET estimation method parameter ‘pet’,
which was called ‘IPET’ in SWAT2012:

``` r
# fuzzy > 0 allows inexact matches
rswat_find('IPET', fuzzy=1) %>% filter(name != 'description') %>% print
```

    ##       name string     class dim          file table  i  j
    ## 1 pet_file   null character   1     codes.bsn     1  1  1
    ## 2      pet      1   integer   1     codes.bsn     1  1  3
    ## 3 harg_pet   <NA>   numeric  50 hydrology.hyd     1 NA 14

We get three matches for ‘IPET’, located in two files. The ‘string’
field (second column) shows the plaintext representation of a parameter
in the SWAT+ config file (fifth column).

We can see that ‘pet’ (in ‘codes.bsn’) is set to 1. This codes for the
Penman-Monteith model for potential evapotranspiration (PET). The other
two matches, ‘pet\_file’ and ‘harg\_pet’, are an input file for observed
PET, and a solar radiation coefficient used with the Hargreaves-Samani
model; Neither is currently used so we don’t worry about them for now.

## adjusting a SWAT+ model

To change a parameter, open its container file with `rswat_open`, modify
it in R, then write the change with `rswat_write`. eg. switching to the
Hargreaves-Samani model for PET (coded as ‘pet’ = 2) can be done like
this:

``` r
# open the file and put its contents into an R dataframe called `codes`
codes = rswat_open('codes.bsn')
codes %>% str
```

    ## 'data.frame':    1 obs. of  24 variables:
    ##  $ pet_file  : chr ""
    ##  $ wq_file   : chr ""
    ##  $ pet       : int 1
    ##  $ event     : int 0
    ##  $ crack     : int 0
    ##  $ rtu_wq    : int 1
    ##  $ sed_det   : int 0
    ##  $ rte_cha   : int 0
    ##  $ deg_cha   : int 0
    ##  $ wq_cha    : int 1
    ##  $ nostress  : int 0
    ##  $ cn        : int 0
    ##  $ c_fact    : int 0
    ##  $ carbon    : int 0
    ##  $ baseflo   : int 0
    ##  $ uhyd      : int 1
    ##  $ sed_cha   : int 0
    ##  $ tiledrain : int 0
    ##  $ wtable    : int 0
    ##  $ soil_p    : int 0
    ##  $ abstr_init: int 0
    ##  $ atmo_dep  : chr "a"
    ##  $ stor_max  : int 0
    ##  $ headwater : int 0

By default, SWAT+ estimates PET with Penman-Monteith (‘pet’ = 1), so we
have to change the PET code:

``` r
# change PET method to Hargreaves-Samani 
codes$pet = 2
```

The default behaviour of `rswat_write` is to preview the requested
change, so the following code doesn’t overwrite anything on disk:

``` r
# preview changes
rswat_write(codes) %>% str
```

    ## 'data.frame':    1 obs. of  7 variables:
    ##  $ file         : chr "codes.bsn"
    ##  $ table        : num 1
    ##  $ i            : int 1
    ##  $ j            : int 3
    ##  $ name         : chr "pet"
    ##  $ current_value: chr "1"
    ##  $ replacement  : chr "2"

The ‘current\_value’ and ‘replacement’ fields are as expected, so we can
go ahead and overwrite the file on disk with argument `preview=FALSE`.

``` r
# write the changes
rswat_write(codes, preview=FALSE, quiet=TRUE)
```

“codes.bsn” is now configured such that SWAT+ simulations will now use
Hargreaves-Samani for PET. This model has a parameter ‘harg\_pet’, that
turned up earlier in the search for ‘IPET’. Since it’s no longer
inactive, we need to assign it a sensible value.

``` r
# open the container file for 'harg_pet' and print a summary
hydro = rswat_open('hydrology.hyd')
hydro %>% str
```

    ## 'data.frame':    50 obs. of  15 variables:
    ##  $ name       : chr  "hyd01" "hyd02" "hyd03" "hyd04" ...
    ##  $ lat_ttime  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lat_sed    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ can_max    : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ esco       : num  0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 ...
    ##  $ epco       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ orgn_enrich: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orgp_enrich: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cn3_swf    : num  0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 ...
    ##  $ bio_mix    : num  0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 ...
    ##  $ perco      : num  0.9 0.5 0.9 0.5 0.5 0.9 0.9 0.5 0.9 0.5 ...
    ##  $ lat_orgn   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lat_orgp   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ harg_pet   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ latq_co    : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 ...

The parameters in this file are all length-50 column vectors. This is
because there are 50 channels in this model, and SWAT+ allows distinct
PET parameters for the HRUs associated with each of them. From the table
above we can see that SWAT+Editor has assigned them all the default
value 0, which means the model currently assumes no evapotranspiration
is happening.

For now we just set all HRUs to the same default value appearing in the
[I/O docs](https://swatplus.gitbook.io/docs/user/io), which is an
empirical estimate from Hargreaves and Samani
[(1985)](https://elibrary.asabe.org/abstract.asp?aid=26773) based on
Alta Fescue grass in California. Local calibration (if needed) can be
done later.

``` r
# assign the default 'harg_pet' value in all HRUs, then write to disk
hydro$harg_pet = 0.0023
rswat_write(hydro, preview=FALSE, quiet=TRUE)
```

Hargreaves-Samani may be the best choice for this project since we lack
the detailed data on humidity, wind, and solar energy required with
Penman-Monteith. SWAT+ can generate those missing data using a
stochastic process, but the result is imprecise at the daily scale.

## running a SWAT+ simulation

In computational model design and fitting there is a lot of back and
forth between parameter settings and simulations. We adjust parameters,
run a simulation, look for changes in state variables of interest, then
repeat (many times). `rswat` has utilities to streamline this process
from R.

`rswat_exec` runs a simulation by making a system call to run the SWAT+
executable. This reads in parameters from the config files in the
current project directory. These include the time period to simulate
over (specified in the file ‘time.sim’), and the time period to include
in output files (fifth table of ‘print.prt’). These can be adjusted
manually, or using a helper function as shown below:

``` r
# `rswat_time` without arguments prints the current settings in 'time.sim'
rswat_time()
```

    ##        start          end 
    ## "1945-01-01" "1945-01-02"

If the model was created with `qswat_run` (as it was here), then these
dates currently specify a one-day simulation at the very beginning of
the supplied weather time series. The code below changes them to match
the time series in `gage` (adjusting ‘print.prt’ to match), then calls
the SWAT+ executable to run a simulation with daily timesteps:

``` r
# pass a range of dates or dataframe with a 'date' field to set up simulation start/end dates
rswat_time(gage, daily=TRUE)
```

    ##        start          end 
    ## "1973-09-01" "1979-10-29"

The time series in `gage` is around seven years long, and the daily
simulation takes about 18-20 seconds to complete on my PC

``` r
# run a simulation
fout = rswat_exec()
```

    ## 
    ## >> finished (17.89 seconds runtime)

``` r
print(fout)
```

    ##  [1] "hru_nb_day.txt"            "hru_ls_day.txt"            "hru_pw_day.txt"            "lsunit_wb_day.txt"        
    ##  [5] "lsunit_nb_day.txt"         "lsunit_ls_day.txt"         "lsunit_pw_day.txt"         "basin_wb_day.txt"         
    ##  [9] "basin_nb_day.txt"          "basin_ls_day.txt"          "basin_pw_day.txt"          "crop_yld_aa.txt"          
    ## [13] "aquifer_day.txt"           "channel_sd_day.txt"        "channel_sdmorph_day.txt"   "basin_crop_yld_yr.txt"    
    ## [17] "basin_crop_yld_aa.txt"     "hydout_day.txt"            "hydin_day.txt"             "deposition_day.txt"       
    ## [21] "wetland_day.txt"           "basin_aqu_day.txt"         "basin_res_day.txt"         "recall_day.txt"           
    ## [25] "basin_cha_day.txt"         "basin_sd_cha_day.txt"      "basin_sd_chamorph_day.txt" "basin_psc_day.txt"        
    ## [29] "ru_day.txt"

the return value is a vector of output files generated. The next section
shows how to read these files with R.

## viewing simulation output data

The SWAT+ executable produces output in the form of .txt tables
containing simulated state variables. There can be many such output
tables (100+) in a given simulation, depending on the settings in
‘print.prt’ and ‘object.prt’.

The helper function `rswat_output` will catalog available output
variables and filenames, and import them into R as dataframes.

``` r
# get a dataframe with info on the available SWAT+ output files in the project directory
odf = rswat_output()
```

    ## parsing 81 SWAT+ output files...

``` r
# print the first few lines of the dataframe, omitting paths for tidyness
odf %>% select(-path) %>% head
```

    ##                file      name type step activated         group oid                  size            modified
    ## 1   aquifer_day.txt   aquifer  prt  day      TRUE       AQUIFER  NA 36146.989 [kilobytes] 2021-05-28 16:00:28
    ## 2  basin_ls_day.txt  basin_ls  prt  day      TRUE         BASIN  NA   378.442 [kilobytes] 2021-05-28 16:00:28
    ## 3  basin_nb_day.txt  basin_nb  prt  day      TRUE         BASIN  NA   540.702 [kilobytes] 2021-05-28 16:00:28
    ## 4  basin_pw_day.txt  basin_pw  prt  day      TRUE         BASIN  NA   675.708 [kilobytes] 2021-05-28 16:00:28
    ## 5  basin_wb_day.txt  basin_wb  prt  day      TRUE         BASIN  NA   999.996 [kilobytes] 2021-05-28 16:00:28
    ## 6 basin_aqu_day.txt basin_aqu  prt  day      TRUE BASIN_AQUIFER  NA   709.489 [kilobytes] 2021-05-28 16:00:28

Output files can be loaded as R dataframes by specifying their filename

``` r
# load an example file. Dates and units are added automatically 
fname.eg = 'aquifer_day.txt'
aqu.day = rswat_output(fname.eg)
aqu.day %>% str
```

    ## 'data.frame':    114750 obs. of  21 variables:
    ##  $ date   : Date, format: "1973-09-01" "1973-09-01" "1973-09-01" "1973-09-01" ...
    ##  $ unit   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ gis_id : int  11 12 21 22 31 32 41 42 51 52 ...
    ##  $ name   : chr  "aqu011" "aqu012" "aqu021" "aqu022" ...
    ##  $ flo    : Units: [mm] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dep_wt : Units: [m] num  3 3 3 3 3 ...
    ##  $ stor   : Units: [mm] num  350 350 350 350 350 ...
    ##  $ rchrg  : Units: [mm] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ seep   : Units: [mm] num  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 ...
    ##  $ revap  : Units: [mm] num  0.03 0.03 0.019 0.019 0.03 0.03 0.019 0.019 0.024 0.024 ...
    ##  $ no3_st : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ minp   : Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orgn   : Units: [kg/ha] num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
    ##  $ orgp   : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rchrgn : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ nloss  : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no3gw  : Units: [kg/ha] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ seepno3: Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_cha: Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_res: Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ flo_ls : Units: [m^3] num  0 0 0 0 0 0 0 0 0 0 ...

Specify a subset of output variables with argument `vname`. Some
columns, like ‘gis\_id’, are always loaded by default. They are indexing
fields needed to identify a row with a specific spatial object in the
model. However, this functionality - along with date-parsing - can be
switched off to get faster load times, or for debugging purposes:

``` r
# print the first few lines for two particular variables (recharge and lateral flow to/from aquifer)
vname.eg = c('flo', 'rchrg')
rswat_output(fname.eg, vname=vname.eg, makedates=FALSE, showidx=FALSE) %>% head
```

    ##      flo  rchrg
    ## 1 0 [mm] 0 [mm]
    ## 2 0 [mm] 0 [mm]
    ## 3 0 [mm] 0 [mm]
    ## 4 0 [mm] 0 [mm]
    ## 5 0 [mm] 0 [mm]
    ## 6 0 [mm] 0 [mm]

When an output file is scanned in by `rswat` its headers and units are
added to a database of known SWAT+ outputs. Variable names in this
database can then be searched by calling `rswat_output` without the
`fname` argument:

``` r
# search for output variables named "rchrg". The one loaded above is identified, and a few others
rswat_output(vname='rchrg') %>% print
```

    ##    name units type              file step
    ## 1 rchrg    mm  prt    aquifer_aa.txt year
    ## 2 rchrg    mm  prt   aquifer_day.txt  day
    ## 3 rchrg    mm  prt    aquifer_yr.txt year
    ## 4 rchrg    mm  prt  basin_aqu_aa.txt year
    ## 5 rchrg    mm  prt basin_aqu_day.txt  day
    ## 6 rchrg    mm  prt  basin_aqu_yr.txt year

Right now the database only includes the files currently found in the
SWAT+ project folder (“TxtInOut”). To get an more exhaustive list of
possible outputs, rswat can optionally run a short (1-day) simulation,
requesting all outputs, in order to parse all output file headers
(before restoring the original state of the project folder)

``` r
# build database of SWAT+ outputs using `loadall` flag
odf = rswat_output(loadall=TRUE)
```

    ## running SWAT+ to generate all output files...
    ## parsing 108 SWAT+ output files...

``` r
#  print the first few lines (omit paths for tidiness)
odf %>% select(-path) %>% head
```

    ##               file     name type  step activated   group oid                  size            modified
    ## 1   aquifer_aa.txt  aquifer  prt  year     FALSE AQUIFER  NA     0.739 [kilobytes] 2021-05-28 16:00:40
    ## 2  aquifer_day.txt  aquifer  prt   day      TRUE AQUIFER  NA 36146.989 [kilobytes] 2021-05-28 16:00:40
    ## 3  aquifer_mon.txt  aquifer  prt month     FALSE AQUIFER  NA        NA [kilobytes]                <NA>
    ## 4   aquifer_yr.txt  aquifer  prt  year     FALSE AQUIFER  NA     0.739 [kilobytes] 2021-05-28 16:00:40
    ## 5  basin_ls_aa.txt basin_ls  prt  year     FALSE   BASIN  NA     0.442 [kilobytes] 2021-05-28 16:00:40
    ## 6 basin_ls_day.txt basin_ls  prt   day      TRUE   BASIN  NA   378.442 [kilobytes] 2021-05-28 16:00:40

Notice the filenames list now includes entries with NA fields for
‘size’, ‘modified’ (and ‘path’, though it is not shown here). These
are files not currently found in ‘TxtInOut’ but which can be enabled in
SWAT+ simulations. Since the above function call cached their headers
they are now searchable:

``` r
# repeat the search for "rchrg" and find a new (monthly outputs) matches
rswat_output(vname='rchrg') %>% print
```

    ##    name units type              file  step
    ## 1 rchrg    mm  prt    aquifer_aa.txt  year
    ## 2 rchrg    mm  prt   aquifer_day.txt   day
    ## 3 rchrg    mm  prt   aquifer_mon.txt month
    ## 4 rchrg    mm  prt    aquifer_yr.txt  year
    ## 5 rchrg    mm  prt  basin_aqu_aa.txt  year
    ## 6 rchrg    mm  prt basin_aqu_day.txt   day
    ## 7 rchrg    mm  prt basin_aqu_mon.txt month
    ## 8 rchrg    mm  prt  basin_aqu_yr.txt  year

SWAT+ potentially generates lot of data in a given simulation depending
on the output settings. This slows things down, and we need to avoid
that in model fitting. The next script in this series,
[demo\_objective](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_objective.md),
uses an alternative output mode in SWAT+ to speed up simulations, then
demonstrates some basics of SWAT+ model fitting in R.

Make a backup of the “TxtInOut” folder to pick up from later

``` r
fout = rswat_copy(to=here( txtinout.meta['txtinout_bak', 'file'] ), fname='.', quiet=TRUE)
```
