demo\_objective.R
================
Dean Koch
2021-05-28

**Mitacs UYRW project**

**demo\_objective.R**: Parameter fitting for a SWAT+ model with `rswat`

This script demonstrates the use of OHG files to quickly get simulated
SWAT+ channel flow values, and methods for building objective functions
to optimize in parameter-fitting.

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

    ## Warning: package 'pdftools' was built under R version 4.0.5

    ## Using poppler version 21.04.0

## project data

load the SWAT+ project info and gage data fro previous scripts
([demo\_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md),
and
[demo\_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md))

``` r
# load some info about the SWAT+ model
qswat.meta = my_metadata('demo_qswat', data.dir=demo.subdir)
txtinout.meta = my_metadata('demo_txtinout', data.dir=demo.subdir)
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
# copy the directory of the project folder and its backup
demo.dir = here( txtinout.meta['txtinout', 'file'] )
demobak.dir = here( txtinout.meta['txtinout_bak', 'file'] )
```

## load the SWAT+ project

Start by loading a SWAT+ project folder

``` r
# point `rswat` to the demo "TxtInOut" directory used earlier 
demo.dir = here( txtinout.meta['txtinout', 'file'] )
print(demo.dir)
```

    ## [1] "D:/UYRW_data/data/demo/demo_big_c_nr_emigrant_txtinout"

``` r
rswat_cio(demo.dir)
```

    ## setting `ciopath` to D:/UYRW_data/data/demo/demo_big_c_nr_emigrant_txtinout/file.cio 
    ## file.cio: written by SWAT+ editor v2.0.0 on 2021-05-28 15:49

    ##                 file          group                size            modified
    ## 1           time.sim     simulation   0.168 [kilobytes] 2021-05-28 16:16:21
    ## 2          print.prt     simulation   3.389 [kilobytes] 2021-05-28 16:16:21
    ## 3         object.cnt     simulation   0.577 [kilobytes] 2021-05-28 16:14:17
    ## 4          codes.bsn          basin   0.599 [kilobytes] 2021-05-28 16:14:14
    ## 5     parameters.bsn          basin   1.296 [kilobytes] 2021-05-28 16:14:17
    ## 6    weather-sta.cli        climate   3.622 [kilobytes] 2021-05-28 16:14:17
    ## 7    weather-wgn.cli        climate  42.549 [kilobytes] 2021-05-28 16:14:17
    ## 8            pcp.cli        climate   0.359 [kilobytes] 2021-05-28 16:14:17
    ## 9            tmp.cli        climate   0.357 [kilobytes] 2021-05-28 16:14:17
    ## 10           hmd.cli        climate   0.363 [kilobytes] 2021-05-28 16:14:14
    ## 11           wnd.cli        climate   0.356 [kilobytes] 2021-05-28 16:14:17
    ## 12           hru.con        connect   8.425 [kilobytes] 2021-05-28 16:14:14
    ## 13     rout_unit.con        connect  16.283 [kilobytes] 2021-05-28 16:14:17
    ## 14       aquifer.con        connect  13.793 [kilobytes] 2021-05-28 16:14:14
    ## 15        recall.con        connect   5.680 [kilobytes] 2021-05-28 16:14:17
    ## 16       chandeg.con        connect   5.629 [kilobytes] 2021-05-28 16:14:14
    ## 17       initial.cha        channel   0.321 [kilobytes] 2021-05-28 16:14:16
    ## 18     nutrients.cha        channel   1.166 [kilobytes] 2021-05-28 16:14:17
    ## 19   channel-lte.cha        channel   2.721 [kilobytes] 2021-05-28 16:14:14
    ## 20   hyd-sed-lte.cha        channel   8.712 [kilobytes] 2021-05-28 16:14:15
    ## 21     rout_unit.def   routing_unit   2.617 [kilobytes] 2021-05-28 16:14:17
    ## 22     rout_unit.ele   routing_unit   4.453 [kilobytes] 2021-05-28 16:14:17
    ## 23     rout_unit.rtu   routing_unit   5.269 [kilobytes] 2021-05-28 16:14:17
    ## 24      hru-data.hru            hru   8.940 [kilobytes] 2021-05-28 16:14:14
    ## 25          exco.exc           exco   2.922 [kilobytes] 2021-05-28 16:14:14
    ## 26       exco_om.exc           exco   7.137 [kilobytes] 2021-05-28 16:14:14
    ## 27        recall.rec         recall   1.572 [kilobytes] 2021-05-28 16:14:17
    ## 28       initial.aqu        aquifer   0.321 [kilobytes] 2021-05-28 16:14:16
    ## 29       aquifer.aqu        aquifer  13.481 [kilobytes] 2021-05-28 16:14:14
    ## 30     hydrology.hyd      hydrology  11.083 [kilobytes] 2021-05-28 16:31:30
    ## 31    topography.hyd      hydrology   9.158 [kilobytes] 2021-05-28 16:14:17
    ## 32         field.fld      hydrology   3.225 [kilobytes] 2021-05-28 16:14:14
    ## 33     tiledrain.str     structural   0.331 [kilobytes] 2021-05-28 16:14:17
    ## 34        septic.str     structural   1.211 [kilobytes] 2021-05-28 16:14:17
    ## 35   filterstrip.str     structural   0.341 [kilobytes] 2021-05-28 16:14:14
    ## 36     grassedww.str     structural   0.560 [kilobytes] 2021-05-28 16:14:14
    ## 37       bmpuser.str     structural   0.304 [kilobytes] 2021-05-28 16:14:14
    ## 38        plants.plt    hru_parm_db 194.373 [kilobytes] 2021-05-28 16:14:17
    ## 39    fertilizer.frt    hru_parm_db   7.216 [kilobytes] 2021-05-28 16:14:14
    ## 40       tillage.til    hru_parm_db   8.311 [kilobytes] 2021-05-28 16:14:17
    ## 41     pesticide.pes    hru_parm_db  49.945 [kilobytes] 2021-05-28 16:14:17
    ## 42         urban.urb    hru_parm_db   1.832 [kilobytes] 2021-05-28 16:14:17
    ## 43        septic.sep    hru_parm_db   4.644 [kilobytes] 2021-05-28 16:14:17
    ## 44          snow.sno    hru_parm_db   0.326 [kilobytes] 2021-05-28 16:41:50
    ## 45          harv.ops            ops   1.273 [kilobytes] 2021-05-28 16:14:14
    ## 46         graze.ops            ops   1.626 [kilobytes] 2021-05-28 16:14:14
    ## 47           irr.ops            ops   0.662 [kilobytes] 2021-05-28 16:14:16
    ## 48      chem_app.ops            ops   1.897 [kilobytes] 2021-05-28 16:14:14
    ## 49          fire.ops            ops   0.265 [kilobytes] 2021-05-28 16:14:14
    ## 50         sweep.ops            ops   0.170 [kilobytes] 2021-05-28 16:14:17
    ## 51       landuse.lum            lum   1.613 [kilobytes] 2021-05-28 16:14:16
    ## 52       cntable.lum            lum  11.089 [kilobytes] 2021-05-28 16:14:14
    ## 53 cons_practice.lum            lum   3.243 [kilobytes] 2021-05-28 16:14:14
    ## 54     ovn_table.lum            lum   1.755 [kilobytes] 2021-05-28 16:14:17
    ## 55     cal_parms.cal            chg  15.543 [kilobytes] 2021-05-28 16:14:14
    ## 56         plant.ini           init   1.151 [kilobytes] 2021-05-28 16:14:17
    ## 57    soil_plant.ini           init   0.316 [kilobytes] 2021-05-28 16:14:17
    ## 58      om_water.ini           init   0.638 [kilobytes] 2021-05-28 16:14:17
    ## 59         soils.sol          soils  41.053 [kilobytes] 2021-05-28 16:14:17
    ## 60     nutrients.sol          soils   0.426 [kilobytes] 2021-05-28 16:14:17
    ## 61           lum.dtl decision_table  23.015 [kilobytes] 2021-05-28 16:14:17
    ## 62       res_rel.dtl decision_table 317.488 [kilobytes] 2021-05-28 16:14:17
    ## 63       scen_lu.dtl decision_table   9.514 [kilobytes] 2021-05-28 16:14:17
    ## 64       flo_con.dtl decision_table  10.361 [kilobytes] 2021-05-28 16:14:14
    ## 65       ls_unit.ele        regions   5.063 [kilobytes] 2021-05-28 16:14:16
    ## 66       ls_unit.def        regions   3.333 [kilobytes] 2021-05-28 16:14:16
    ## 67   aqu_catunit.ele        regions   5.165 [kilobytes] 2021-05-28 16:14:14

restore the backup that was created after running the previous demo
script

``` r
# restore model backup, which has Hargreaves-Samani PET method activated
print(demobak.dir)
```

    ## [1] "D:/UYRW_data/data/demo/demo_big_c_nr_emigrant_txtinout/backup_harg"

``` r
fout = rswat_copy(from=demobak.dir, fname='.', overwrite=TRUE, quiet=TRUE)

# pre-load all output and config files (except decision tables)
cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
odf = rswat_output(loadall=TRUE)
```

    ## running SWAT+ to generate all output files...
    ## parsing 109 SWAT+ output files...

The helper functions `rswat_copy`, `rswat_cio`, and `rswat_output` are
described in the previous demo script
[demo\_txtinout](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md).

## Comparing the simulated and observed data

Printing simulation data to the .txt output files is a bottleneck for
SWAT+. To speed things up it is better to request only the outputs you
need, and omit printing the others. These settings are found in
‘print.prt’ (for the normal outputs) and ‘object.prt’ (object
hydrograph outputs). Outputs that are currently toggled on are indicated
by the ‘activated’ field in the dataframe returned by `rswat_output`

``` r
# display the output files that are currently activated in SWAT+
rswat_output() %>% filter(activated) %>% pull(file)
```

    ##  [1] "aquifer_day.txt"           "basin_ls_day.txt"          "basin_nb_day.txt"          "basin_pw_day.txt"         
    ##  [5] "basin_wb_day.txt"          "basin_aqu_day.txt"         "basin_cha_day.txt"         "basin_psc_day.txt"        
    ##  [9] "basin_res_day.txt"         "basin_sd_chamorph_day.txt" "basin_sd_cha_day.txt"      "deposition_day.txt"       
    ## [13] "hru_ls_day.txt"            "hru_nb_day.txt"            "hru_pw_day.txt"            "hydin_day.txt"            
    ## [17] "hydout_day.txt"            "recall_day.txt"            "wetland_day.txt"           "lsunit_ls_day.txt"        
    ## [21] "lsunit_nb_day.txt"         "lsunit_pw_day.txt"         "lsunit_wb_day.txt"         "ru_day.txt"               
    ## [25] "channel_sd_day.txt"        "channel_sdmorph_day.txt"

All daily output files are active (this setting was applied by a call to
`rswat_time` above). There are quite a few of them, so execution is
relatively slow. If we turn off all off the standard output files, the
SWAT+ simulation will still run, and it completes much faster.

``` r
# open fifth table of 'print.prt', disable all output files and write the changes
print.prt = rswat_open('print.prt')[[5]]
print.prt[, names(print.prt) != 'objects'] = 'n'
rswat_write(print.prt, preview=F, quiet=TRUE)

# call the SWAT+ executable
fout = rswat_exec()
```

    ## 
    ## >> finished (7.98 seconds runtime)

On my machine the process completes in less than half the time (7-9
seconds versus 17-20). This time cost reduction may not matter when
running one-off simulations like we do here, but later on when fitting
parameters it becomes very significant, because we will need to run
thousands of simulations.

Note that two files related to crop yields were generated in spite of
the settings in ‘print.prt’. It is not clear how to inactivate them. But
since they are small, yearly tables, they likely don’t impact runtimes
very much.

In parameter fitting we need to generate daily outputs of discharge at
our gaged channel(s) to evaluate errors, ie to evaluate the objective
function. As we can see in the above example, requesting these outputs
via ‘print.prt’ is not ideal because the returned files print the data
for every channel, but we only need it for a few (usually one). SWAT+
has a special type of output file for the purpose of channel-specific
outputs, the object hydrograph (OHG).

``` r
# activate the object hydrograph for the outlet channel (id number 1)
id.outlet = 1
rswat_ohg(overwrite=TRUE, oid=id.outlet)
```

    ##   NUMB OBTYP OBTYPNO HYDTYP      FILENAME
    ## 1    1   sdc       1    tot sdc_1_tot.ohg

this function modifies “file.cio” and “object.prt” so that SWAT+
generates the plaintext output file “sdc\_1\_tot.ohg” in addition to any
others specified in “print.prt”. The `oid` argument specifies that we
only want data on the channel with ID code 1 (AKA ‘cha01’, usually the
main outlet of the catchment).

``` r
# call the SWAT+ executable
fout = rswat_exec()
```

    ## 
    ## >> finished (7.79 seconds runtime)

Note that this new file output seems not to affect runtimes

``` r
# check that the expected file has been generated
rswat_output() %>% filter(type=='ohg') %>% select(-path)
```

    ##            file name type step activated group oid                size            modified
    ## 1 sdc_1_tot.ohg  tot  ohg  day      TRUE   sdc   1 776.591 [kilobytes] 2021-05-28 16:46:48

``` r
# open the output 
rswat_output('sdc_1_tot.ohg') %>% str
```

    ## 'data.frame':    2250 obs. of  21 variables:
    ##  $ date: Date, format: "1973-09-01" "1973-09-02" "1973-09-03" "1973-09-04" ...
    ##  $ name: chr  "chandeg" "chandeg" "chandeg" "chandeg" ...
    ##  $ type: chr  "cha01" "cha01" "cha01" "cha01" ...
    ##  $ flo : Units: [m3/d] num  651416 2481 0 0 0 ...
    ##  $ sed : Units: [tons] num  1.28e+04 9.97e+01 3.04e-02 3.72e-02 2.98e-02 ...
    ##  $ orgn: Units: [kg] num  7.08e+01 1.52e-01 1.37e-06 2.12e-07 1.78e-07 ...
    ##  $ sedp: Units: [kg] num  4.31 9.27e-03 1.73e-07 6.13e-07 5.36e-07 ...
    ##  $ no3 : Units: [kg] num  0.00 1.09e-05 7.87e-07 5.38e-07 4.84e-07 ...
    ##  $ solp: Units: [kg] num  0.00 4.03e-05 1.35e-05 7.07e-06 5.04e-06 ...
    ##  $ chla: Units: [kg] num  0.00 0.00 4.59e-07 7.83e-08 3.93e-08 ...
    ##  $ nh3 : Units: [kg] num  2.11e-02 4.55e-04 5.99e-07 0.00 0.00 ...
    ##  $ no2 : Units: [kg] num  0.00 1.34e-04 2.17e-06 0.00 0.00 ...
    ##  $ cbod: Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dox : Units: [kg] num  4.52e+02 1.02e+01 6.21e-03 1.19e-01 8.85e-02 ...
    ##  $ san : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sil : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cla : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sag : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ lag : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ grv : Units: [tons] num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ null: num  9.13 10.19 11.75 13.59 14.9 ...

This is a subset of the outflow variables that would normally appear in
the “SWAT-DEG\_CHANNEL” group. Note that the variable names in OHG files
omit the suffix "\_out". For example variable ‘flo’ in the OHG file
corresponds to variable ‘flo\_out’ in ‘channel\_sd\_day.txt’.

The units in these files are not necessarily the same - in particular,
the OHG discharge values are in per-day units whereas the normal output
files use per-second units. Different units means we can expect some
post-conversion differences due to numerical imprecision (see also \#32
and \#75
[here](https://infiniteundo.com/post/25509354022/more-falsehoods-programmers-believe-about-time)):

``` r
# load the OHG output file
ohg.out = rswat_output('sdc_1_tot.ohg')

# load the normal output file, trimming to only include values from the main outlet
prt.out = rswat_output('channel_sd_day.txt') %>% filter(gis_id==id.outlet) 

# join them and check the level of discrepancy
left_join(prt.out, ohg.out, by=c('date')) %>% 
  mutate( absdiff = abs( (flo_out - flo) ) ) %>% 
  pull(absdiff) %>% max
```

    ## 0.004930556 [m^3/s]

The errors appear to be small enough here to ignore for the purposes of
parameter fitting.

``` r
# OHG outputs can be switched off for now
rswat_ohg(overwrite=TRUE, delete=TRUE)
```

    ## NULL

The helper function `rswat_flo` is for quickly getting the OHG output
for a simulation. It handles all of the required settings adjustments
laid out above, and by default will restore all config files to their
original state afterwards). Simply pass it a range of dates and get back
the simulated discharge values:

``` r
# `oid=1` is set by default and 'dates' can be range of dates or a dataframe containing a 'date' column
ohg.out = rswat_flo(dates=gage, quiet=TRUE)
ohg.out %>% str
```

    ## 'data.frame':    2250 obs. of  2 variables:
    ##  $ date: Date, format: "1973-09-01" "1973-09-02" "1973-09-03" "1973-09-04" ...
    ##  $ flo : Units: [m3/d] num  651416 2481 0 0 0 ...

If ‘dates’ is a dataframe containing observations (columns names
starting with ‘flo’ or ‘obs’), `rswat_flo` can optionally be passed an
(anonymous) error function `errfn(x,y)`. In that case instead of
returning the discharge simulation values it passes them directly to
`errfn` (as `x`) along with the observed data (as `y`)

eg. this code passes the function `my_nse` (defined in
[helper\_main](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md))
which computes [Nash-Sutcliffe
efficiency](https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient)

``` r
# define `my_nse_2` to get a function of two variables only (`my_nse` has four)
my_nse_2 = function(x, y) {my_nse(x, y, L=2, normalized=TRUE)}

# run simulation and return NSE (normalized , with exponent 2)
rswat_flo(dates=gage, errfn=my_nse_2, quiet=TRUE) %>% print
```

    ## [1] 0.5069249

This is useful for linking up with optimization algorithms offered in
other R packages. For example if we want to maximize NSE for our
simulated hydrograph with respect to the parameter ‘harg\_pet’
(discussed earlier), we can construct an objective function like this:

``` r
# define an anonymous function that modifies parameters then evaluates NSE of resulting simulation 
obj.example = function(x)
{
  # open the container file for 'harg_pet', 
  hydro = rswat_open('hydrology.hyd')
  
  # assign value `x` to all HRUs then write to disk
  hydro$harg_pet = x
  rswat_write(hydro, preview=FALSE, quiet=TRUE)
  
  # run simulation then return the resulting NSE
  return( rswat_flo(dates=gage, errfn=my_nse_2, quiet=TRUE) )
}
```

A grid search now becomes very simple to program:

``` r
# run the objective function for a range of `harg_pet` values
harg.test = seq(0, 0.005, length=8)
harg.nse = sapply(harg.test, obj.example)

# identify and print the best one
idx.opt = which.max( harg.nse ) 
nse.opt = harg.nse[idx.opt]
harg.opt = harg.test[idx.opt]
print( paste0( 'NSE is maximized (', round(nse.opt, 3),') at harg_pet=', round(harg.opt, 3)) )
```

    ## [1] "NSE is maximized (0.559) at harg_pet=0.001"

`obj.example` could be altered to include multiple parameters in `x` (a
vector). In this way an objective function of any number of SWAT+
parameters can be constructed and passed to an optimizer. The new
function would do a series of `rswat_open` calls, new parameter
assignments, then a series of `rswat_write` calls - one for each file
corresponding to one of the parameters in `x`

To simplify this type of code, we have the function `rswat_amod`. It
returns an anonymous function, customized for quickly modifying a
selection of model parameters. This provides a shortcut for when we need
to repeatedly read/write certain parameters, such as when defining an
objective function like `obj.example`.

To specify the desired selection of parameters, simply pass the output
of `rswat_find` to `rswat_amod`. This can be done with (`magittr`)
pipes, if you’re into that sort of thing:

``` r
# make sure that we get the variable of interest from the `rswat_find` call
rswat_find('harg_pet')
```

    ##       name string   class dim          file table  i  j
    ## 1 harg_pet   <NA> numeric  50 hydrology.hyd     1 NA 14

``` r
# pipe it to `rswat_amod` to define the function, which we are calling "harg_pet"
harg_pet = rswat_find('harg_pet') %>% rswat_amod

# calling the function without arguments produces the current value and some metadata
harg_pet()
```

    ##   value     name   class dim          file table  i  j
    ## 1 0.005 harg_pet numeric  50 hydrology.hyd     1 NA 14

``` r
# modify the parameter on disk with a one-liner (overwrites "hydrology.hyd"!)
harg_pet(0.0023)
```

    ##    value     name   class dim          file table  i  j
    ## 1 0.0023 harg_pet numeric  50 hydrology.hyd     1 NA 14

This example has one parameter, but any number of parameters (in any
number of files) can be specified. For example the code below shows how
to modify all of the (numeric) parameters in “snow.sno”:

``` r
# define a function for quickly querying/modifying parameters in the file "snow.sno" 
snow.sno = rswat_find(include='snow.sno') %>% rswat_amod

# print info about the current values 
snow.sno()
```

    ##   value      name   class dim     file table i j
    ## 1   1.0  fall_tmp numeric   1 snow.sno     1 1 2
    ## 2   0.5  melt_tmp numeric   1 snow.sno     1 1 3
    ## 3   4.5  melt_max numeric   1 snow.sno     1 1 4
    ## 4   4.5  melt_min numeric   1 snow.sno     1 1 5
    ## 5   1.0   tmp_lag numeric   1 snow.sno     1 1 6
    ## 6   1.0  snow_h2o numeric   1 snow.sno     1 1 7
    ## 7   0.5     cov50 numeric   1 snow.sno     1 1 8
    ## 8   0.0 snow_init numeric   1 snow.sno     1 1 9

``` r
# make a small adjustment and write the change
snow.newvals = snow.sno()$value + 1e-2 
snow.sno(snow.newvals)
```

    ##   value      name   class dim     file table i j
    ## 1  1.01  fall_tmp numeric   1 snow.sno     1 1 2
    ## 2  0.51  melt_tmp numeric   1 snow.sno     1 1 3
    ## 3  4.51  melt_max numeric   1 snow.sno     1 1 4
    ## 4  4.51  melt_min numeric   1 snow.sno     1 1 5
    ## 5  1.01   tmp_lag numeric   1 snow.sno     1 1 6
    ## 6  1.01  snow_h2o numeric   1 snow.sno     1 1 7
    ## 7  0.51     cov50 numeric   1 snow.sno     1 1 8
    ## 8  0.01 snow_init numeric   1 snow.sno     1 1 9

``` r
# restore the old values
snow.sno(snow.newvals - 1e-2)
```

    ##   value      name   class dim     file table i j
    ## 1   1.0  fall_tmp numeric   1 snow.sno     1 1 2
    ## 2   0.5  melt_tmp numeric   1 snow.sno     1 1 3
    ## 3   4.5  melt_max numeric   1 snow.sno     1 1 4
    ## 4   4.5  melt_min numeric   1 snow.sno     1 1 5
    ## 5   1.0   tmp_lag numeric   1 snow.sno     1 1 6
    ## 6   1.0  snow_h2o numeric   1 snow.sno     1 1 7
    ## 7   0.5     cov50 numeric   1 snow.sno     1 1 8
    ## 8   0.0 snow_init numeric   1 snow.sno     1 1 9

`rswat_amod` becomes very useful when we move beyond these toy examples
and starting selecting larger subsets of parameters for tuning. This,
along with parallel processing, is illustrated in the next vignette
