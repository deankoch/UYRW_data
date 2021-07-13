demo\_API.R
================
Dean Koch
2021-07-12

**Mitacs UYRW project**

**demo\_API.R**: (in development) documents inputs outputs for
forecasting system

## dependencies

Load some R libraries and saved datasets

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))
```

    ## Using poppler version 21.04.0

``` r
# rswat_docs(4, doc='i')
# 
# rswat_docs(4, doc='o')
# 
# rswat_docs('channel_sd_day.txt')
# rswat_docs('channel_sdmorph_day.txt')
# 
# rswat_docs('precip', doc='o')
# 
# pattern='channel_sdmorph_day.txt'
# fname=NULL
# fuzzy=0
# descw=0.5
# full=FALSE
# quiet=FALSE
# doc='b'




# load an example QSWAT+ project 
nm = 'big_c_nr_emigrant'
qswat.metadata = my_metadata('run_qswat')
cal.metadata = my_metadata('calibrate_initial')
qswat.dir = here( qswat.metadata[nm, 'file'] )
qswat = qswat_read(qswat.dir)

# load the SWAT+ model files (calibrated)
textio = here(cal.metadata[nm, 'file'])
cio = rswat_cio(textio, reload=TRUE, ignore='decision_table', quiet=TRUE)
```

## file specifications

SWAT+ has two major output types - the channel and the HRU, represented
as line and polygon geometries, respectively. QSWAT+ (the SWAT+ ) For
the demo here I’ve saved copies of these geometry collections as geoJSON
files:

-   bigcreek\_channels.geojson
-   bigcreek\_hrus.geojson

Each feature in these collections corresponds to a physical region in
the watershed

a large number of different hydrograph variables

-   bigcreek\_channels.json
-   bigcreek\_hrus\_vars.json

These are mapped to line and polygon geometries saved to the following
files (described below)

## SWAT+ HRUs

The atomic spatial object in a SWAT model is the HRU. This is a parcel
of land - represented as a polygon geometry - that drains to a common
point (a stream reach). During model construction we use the DEM to
divide the watershed into many HRUs (on the order of 10s to 1000s),
where the exact number is a parameter chosen to strike a balance between
realism and computational complexity.

This example has 50 HRUs. I save their geometry data in a single geoJSON
below:

``` r
# save the HRUs polygon collection as geoJSON
unlink('bigcreek_hrus.geojson')
st_write(qswat$hru, 'bigcreek_hrus.geojson')
```

    ## Writing layer `bigcreek_hrus' to data source `bigcreek_hrus.geojson' using driver `GeoJSON'
    ## Writing 50 features with 20 fields and geometry type Multi Polygon.

``` r
# plot the polygons and colour by average elevation
qswat$hru[,'elev'] %>% plot(border=NA, key.pos=1, main='elevation by HRU (m)')
```

![](D:/UYRW_data/markdown/demo_API_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

More HRUs means more spatial detail, but slower simulations. This is
because SWAT+ simplifies computations by assuming the entire area of the
HRU has spatially uniform properties. For example, slope, temperature
and soil profile are all assumed equal *throughout* an HRU. This is
obviously inaccurate but it works well as a kind of average of the
actual physical state, and we have to draw the line somewhere on spatial
detail.

HRUs come in pairs. Areas close to a stream reach make up the floodplain
HRU, and the rest is the upslope HRU.

``` r
# plot the upslope/floodplain areas
qswat$hru[,'Landscape'] %>% plot(key.pos=1, reset=FALSE, main='HRU type')
```

![](D:/UYRW_data/markdown/demo_API_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## SWAT+ Channels

Each pair of HRUs is associated with exactly one stream reach -
represented by a line geometry - which drains both the upslope and
floodplain areas. These reaches, AKA channels, are connected in a
tree-like structure and have their own attributes. Just like with HRUs,
channel attributes are assumed by the model to be spatially uniform
along their entire length.

I save the channel geometries in a geoJSON below

``` r
# save the HRUs polygon collection as geoJSON
unlink('bigcreek_channels.geojson')
st_write(qswat$cha, 'bigcreek_channels.geojson')
```

    ## Writing layer `bigcreek_channels' to data source `bigcreek_channels.geojson' using driver `GeoJSON'
    ## Writing 25 features with 16 fields and geometry type Line String.

The plot below overlays the channel geometry with (exaggerated) line
widths scaled according to physical stream width.

``` r
# plot the stream line geometries with width scaled to match physical average (attribute 'Wid2')
st_geometry(qswat$hru) %>% plot(col='grey', border=NA, main='stream widths (exaggerated)', reset=FALSE)
st_geometry(qswat$cha) %>% plot(main='width (ft)', lwd=qswat$cha$Wid2/2, add=TRUE)
```

![](D:/UYRW_data/markdown/demo_API_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

We have many HRU-level and channel-level attributes in the model (100s),
some of which are very important during calibration or scenario
simulations. A small number of these attributes (like ‘elev’ and ‘Wid2’,
in the plots above) are already copied to the geometry collections and
so you’ll see them in the geoJSON files.

Later on we can join the full set of attributes and define everything
but for now lets just worry about the geometries and associated
simulation outputs.

## SWAT+ outputs

There are around 660 different daily output variables in SWAT+. These
are time series data (hydrographs) representing the daily averages for
water cycle process aggregated to a particular spatial level:
basin-wide, HRU-wide, channel-wide, etc.

The SWAT+ executable writes these data to a set of about 30 output
files, organized by process type (eg aquifer vs surface) and spatial
level (eg. basin vs subbasin vs HRU).

Files can be toggled on/off to speed up simulations. The code below
generates all of them, parses the variable names and
[udunits2](https://www.unidata.ucar.edu/software/udunits/udunits-2.0.4/udunits2lib.html)
units strings, and counts the variables

``` r
# scan outputs and filter to daily, channel/HRU-level outputs
outputs = rswat_output(loadall=T)
```

    ## running SWAT+ to generate all output files...
    ## restoring file.cio, time.sim, print.prt, object.prt ...
    ## parsing 109 SWAT+ output files...

``` r
daily.outputs = outputs %>% filter(step=='day') 

# count the number of variables
rswat_ofind() %>% filter(file %in% daily.outputs$file) %>% filter( !is.na(units) ) %>% nrow()
```

    ## [1] 660

We can trim the list a bit by omitting redundant variables - some are
duplicates and most are aggregated values (such as basin-wide averages)
which we can easily reconstruct ourselves from the daily HRU/channel
values. The code below filters the redundant outputs and splits the
remainder into two groups: HRU and channel level outputs, leaving us
with about 150 output variables total, spread among six files.

``` r
# make a list of the daily HRU-level outputs
HRU.fn = daily.outputs %>% filter( group %in% c('AQUIFER', 'HRU') ) %>% pull(file)
HRU.info = rswat_ofind() %>% 
  filter(file %in% HRU.fn) %>% 
  filter( !is.na(units) ) %>% 
  distinct(name, .keep_all=TRUE) %>%
  select(name, units, file)

# join with descriptions and documentation reference page, and add object type field
HRU.info = do.call( rbind, lapply( HRU.fn, function(fn) rswat_docs(fn) ) ) %>% 
  right_join(HRU.info, by=c('file', 'name')) %>%
  select(name, units, description, file, pstart) %>%
  mutate(obj='hru')
```

    ## reading section FILE.CIO...                                 0.383 %      reading section FILE.CIO...                                 0.766 %      reading section FILE.CIO...                                 1.149 %      reading section PRINT.PRT...                                1.533 %      reading section PRINT.PRT...                                1.916 %      reading section PRINT.PRT...                                2.299 %      reading section PRINT.PRT...                                2.682 %      reading section OBJECT.PRT...                               3.065 %      reading section OBJECT.CNT...                               3.448 %      reading section CONSTITUENTS.CS...                          3.831 %      reading section CONSTITUENTS.CS...                          4.215 %      reading section PEST_HRU.INI...                             4.598 %      reading section PEST_HRU.INI...                             4.981 %      reading section PEST_WATER.INI...                           5.364 %      reading section MANAGEMENT.SCH...                           5.747 %      reading section LUM.DTL...                                  6.13 %      reading section LUM.DTL...                                  6.513 %      reading section CODES.BSN...                                6.897 %      reading section CODES.BSN...                                7.28 %      reading section CODES.BSN...                                7.663 %      reading section CODES.BSN...                                8.046 %      reading section PARAMETERS.BSN...                           8.429 %      reading section PARAMETERS.BSN...                           8.812 %      reading section PARAMETERS.BSN...                           9.195 %      reading section PARAMETERS.BSN...                           9.579 %      reading section PARAMETERS.BSN...                           9.962 %      reading section PARAMETERS.BSN...                           10.345 %      reading section PARAMETERS.BSN...                           10.728 %      reading section PARAMETERS.BSN...                           11.111 %      reading section PARAMETERS.BSN...                           11.494 %      reading section PARAMETERS.BSN...                           11.877 %      reading section PARAMETERS.BSN...                           12.261 %      reading section PARAMETERS.BSN...                           12.644 %      reading section WEATHER-STA.CLI...                          13.027 %      reading section WEATHER-WGN.CLI...                          13.41 %      reading section WEATHER-WGN.CLI...                          13.793 %      reading section WEATHER-WGN.CLI...                          14.176 %      reading section WEATHER-WGN.CLI...                          14.559 %      reading section WEATHER-WGN.CLI...                          14.943 %      reading section WEATHER-WGN.CLI...                          15.326 %      reading section WEATHER-WGN.CLI...                          15.709 %      reading section WIND-DIR.CLI...                             16.092 %      reading section PCP.CLI...                                  16.475 %      reading section TMP.CLI...                                  16.858 %      reading section SLR.CLI...                                  17.241 %      reading section HMD.CLI...                                  17.625 %      reading section WND.CLI...                                  18.008 %      reading section ATMO.CLI...                                 18.391 %      reading section ATMO.CLI...                                 18.774 %      reading section ATMO.CLI...                                 19.157 %      reading section ATMO.CLI...                                 19.54 %      reading section HRU.CON...                                  19.923 %      reading section HRU.CON...                                  20.307 %      reading section CHANNEL.CHA...                              20.69 %      reading section HYDROLOGY.CHA...                            21.073 %      reading section SEDIMENT.CHA...                             21.456 %      reading section SEDIMENT.CHA...                             21.839 %      reading section NUTRIENTS.CHA...                            22.222 %      reading section NUTRIENTS.CHA...                            22.605 %      reading section NUTRIENTS.CHA...                            22.989 %      reading section NUTRIENTS.CHA...                            23.372 %      reading section NUTRIENTS.CHA...                            23.755 %      reading section NUTRIENTS.CHA...                            24.138 %      reading section NUTRIENTS.CHA...                            24.521 %      reading section NUTRIENTS.CHA...                            24.904 %      reading section NUTRIENTS.CHA...                            25.287 %      reading section PATHOGENS.CHA...                            25.67 %      reading section CHANNEL-LTE.CHA...                          26.054 %      reading section HYD-SED-LTE.CHA...                          26.437 %      reading section TEMPERATURE.CHA...                          26.82 %      reading section INITIAL.RES...                              27.203 %      reading section RESERVOIR.RES...                            27.586 %      reading section HYDROLOGY.RES...                            27.969 %      reading section HYDROLOGY.RES...                            28.352 %      reading section NUTRIENTS.RES...                            28.736 %      reading section NUTRIENTS.RES...                            29.119 %      reading section NUTRIENTS.RES...                            29.502 %      reading section PATHOGENS.RES...                            29.885 %      reading section SEDIMENT.RES...                             30.268 %      reading section SEDIMENT.RES...                             30.651 %      reading section WEIR.RES...                                 31.034 %      reading section WETLAND.WET...                              31.418 %      reading section HYDROLOGY.WET...                            31.801 %      reading section ROUT_UNIT.DEF...                            32.184 %      reading section ROUT_UNIT.RTU...                            32.567 %      reading section ROUT_UNIT.DR...                             32.95 %      reading section HRU-DATA.HRU...                             33.333 %      reading section HRU-LTE.HRU...                              33.716 %      reading section HRU-LTE.HRU...                              34.1 %      reading section HRU-LTE.HRU...                              34.483 %      reading section HRU-LTE.HRU...                              34.866 %      reading section HRU-LTE.HRU...                              35.249 %      reading section HRU-LTE.HRU...                              35.632 %      reading section HRU-LTE.HRU...                              36.015 %      reading section HRU-LTE.HRU...                              36.398 %      reading section HRU-LTE.HRU...                              36.782 %      reading section HRU-LTE.HRU...                              37.165 %      reading section HRU-LTE.HRU...                              37.548 %      reading section HRU-LTE.HRU...                              37.931 %      reading section HRU-LTE.HRU...                              38.314 %      reading section HRU-LTE.HRU...                              38.697 %      reading section HRU-LTE.HRU...                              39.08 %      reading section HRU-LTE.HRU...                              39.464 %      reading section HRU-LTE.HRU...                              39.847 %      reading section HRU-LTE.HRU...                              40.23 %      reading section HRU-LTE.HRU...                              40.613 %      reading section HRU-LTE.HRU...                              40.996 %      reading section HRU-LTE.HRU...                              41.379 %      reading section EXCO.EXC...                                 41.762 %      reading section EXCO_PEST.EXC...                            42.146 %      reading section EXCO_SALT.EXC...                            42.529 %      reading section RECALL.REC...                               42.912 %      reading section recall_day.rec...                           43.295 %      reading section DELRATIO.DEL...                             43.678 %      reading section DR_PEST.DEL...                              44.061 %      reading section DR_HMET.DEL...                              44.444 %      reading section INITIAL.AQU...                              44.828 %      reading section AQUIFER.AQU...                              45.211 %      reading section AQUIFER.AQU...                              45.594 %      reading section AQUIFER.AQU...                              45.977 %      reading section HERD.HRD...                                 46.36 %      reading section CHAN-SURF.LIN...                            46.743 %      reading section HYDROLOGY.HYD...                            47.126 %      reading section HYDROLOGY.HYD...                            47.51 %      reading section HYDROLOGY.HYD...                            47.893 %      reading section HYDROLOGY.HYD...                            48.276 %      reading section HYDROLOGY.HYD...                            48.659 %      reading section FIELD.FLD...                                49.042 %      reading section TILEDRAIN.STR...                            49.425 %      reading section SEPTIC.STR...                               49.808 %      reading section SEPTIC.STR...                               50.192 %      reading section SEPTIC.STR...                               50.575 %      reading section SEPTIC.STR...                               50.958 %      reading section SEPTIC.STR...                               51.341 %      reading section FILTERSTRIP.STR...                          51.724 %      reading section BMPUSER.STR...                              52.107 %      reading section PLANTS.PLT...                               52.49 %      reading section PLANTS.PLT...                               52.874 %      reading section PLANTS.PLT...                               53.257 %      reading section PLANTS.PLT...                               53.64 %      reading section PLANTS.PLT...                               54.023 %      reading section PLANTS.PLT...                               54.406 %      reading section PLANTS.PLT...                               54.789 %      reading section PLANTS.PLT...                               55.172 %      reading section PLANTS.PLT...                               55.556 %      reading section PLANTS.PLT...                               55.939 %      reading section PLANTS.PLT...                               56.322 %      reading section PLANTS.PLT...                               56.705 %      reading section PLANTS.PLT...                               57.088 %      reading section PLANTS.PLT...                               57.471 %      reading section PLANTS.PLT...                               57.854 %      reading section PLANTS.PLT...                               58.238 %      reading section PLANTS.PLT...                               58.621 %      reading section PLANTS.PLT...                               59.004 %      reading section PLANTS.PLT...                               59.387 %      reading section PLANTS.PLT...                               59.77 %      reading section PLANTS.PLT...                               60.153 %      reading section PLANTS.PLT...                               60.536 %      reading section PLANTS.PLT...                               60.92 %      reading section FERTILIZER.FRT...                           61.303 %      reading section TILLAGE.TIL...                              61.686 %      reading section TILLAGE.TIL...                              62.069 %      reading section PESTICIDE.PST...                            62.452 %      reading section PESTICIDE.PST...                            62.835 %      reading section PESTICIDE.PST...                            63.218 %      reading section PATHOGENS.PTH...                            63.602 %      reading section PATHOGENS.PTH...                            63.985 %      reading section URBAN.URB...                                64.368 %      reading section URBAN.URB...                                64.751 %      reading section URBAN.URB...                                65.134 %      reading section SEPTIC.SEP...                               65.517 %      reading section SEPTIC.SEP...                               65.9 %      reading section SEPTIC.SEP...                               66.284 %      reading section SEPTIC.SEP...                               66.667 %      reading section SEPTIC.SEP...                               67.05 %      reading section SEPTIC.SEP...                               67.433 %      reading section SNOW.SNO...                                 67.816 %      reading section SNOW.SNO...                                 68.199 %      reading section SNOW.SNO...                                 68.582 %      reading section HARV.OPS...                                 68.966 %      reading section GRAZE.OPS...                                69.349 %      reading section IRR.OPS...                                  69.732 %      reading section FIRE.OPS...                                 70.115 %      reading section SWEEP.OPS...                                70.498 %      reading section SWEEP.OPS...                                70.881 %      reading section LANDUSE.LUM...                              71.264 %      reading section LANDUSE.LUM...                              71.648 %      reading section LANDUSE.LUM...                              72.031 %      reading section MANAGEMENT.SCH...                           72.414 %      reading section MANAGEMENT.SCH...                           72.797 %      reading section MANAGEMENT.SCH...                           73.18 %      reading section MANAGEMENT.SCH...                           73.563 %      reading section MANAGEMENT.SCH...                           73.946 %      reading section MANAGEMENT.SCH...                           74.33 %      reading section CNTABLE.LUM...                              74.713 %      reading section CNTABLE.LUM...                              75.096 %      reading section CONS_PRACTICE.LUM...                        75.479 %      reading section CONS_PRACTICE.LUM...                        75.862 %      reading section OVN_TABLE.LUM...                            76.245 %      reading section OVN_TABLE.LUM...                            76.628 %      reading section CAL_PARMS.CAL...                            77.011 %      reading section CAL_PARMS.CAL...                            77.395 %      reading section CAL_PARMS.CAL...                            77.778 %      reading section CALIBRATION.CAL...                          78.161 %      reading section CALIBRATION.CAL...                          78.544 %      reading section CALIBRATION.CAL...                          78.927 %      reading section CODES.SFT...                                79.31 %      reading section WATER_BALANCE.SFT...                        79.693 %      reading section CH_SED_BUDGET.SFT...                        80.077 %      reading section CH_SED_PARMS.SFT...                         80.46 %      reading section PLANT_PARMS.SFT...                          80.843 %      reading section PLANT.INI...                                81.226 %      reading section PLANT.INI...                                81.609 %      reading section PLANT.INI...                                81.992 %      reading section PEST_HRU.INI...                             82.375 %      reading section PEST_HRU.INI...                             82.759 %      reading section HMET_WATER.INI...                           83.142 %      reading section SOILS.SOL...                                83.525 %      reading section SOILS.SOL...                                83.908 %      reading section SOILS.SOL...                                84.291 %      reading section SOILS.SOL...                                84.674 %      reading section SOILS.SOL...                                85.057 %      reading section SOILS.SOL...                                85.441 %      reading section SOILS.SOL...                                85.824 %      reading section SOILS.SOL...                                86.207 %      reading section SOILS.SOL...                                86.59 %      reading section NUTRIENTS.SOL...                            86.973 %      reading section NUTRIENTS.SOL...                            87.356 %      reading section NUTRIENTS.SOL...                            87.739 %      reading section D_TABLE.DTL...                              88.123 %      reading section D_TABLE.DTL...                              88.506 %      reading section D_TABLE.DTL...                              88.889 %      reading section D_TABLE.DTL...                              89.272 %      reading section D_TABLE.DTL...                              89.655 %      reading section D_TABLE.DTL...                              90.038 %      reading section D_TABLE.DTL...                              90.421 %      reading section D_TABLE.DTL...                              90.805 %      reading section D_TABLE.DTL...                              91.188 %      reading section D_TABLE.DTL...                              91.571 %      reading section D_TABLE.DTL...                              91.954 %      reading section D_TABLE.DTL...                              92.337 %      reading section D_TABLE.DTL...                              92.72 %      reading section LS_UNIT.DEF...                              93.103 %      reading section CH_REG.DEF...                               93.487 %      reading section AQU_CATUNIT.ELE...                          93.87 %      reading section AQU_REG.DEF...                              94.253 %      reading section RES_REG.DEF...                              94.636 %      reading section REC_CATUNIT.ELE...                          95.019 %      reading section REC_REG.DEF...                              95.402 %      reading section REC_REG.DEF...                              95.785 %      reading section REC_REG.DEF...                              96.169 %      reading section REC_REG.DEF...                              96.552 %      reading section REC_REG.DEF...                              96.935 %      reading section REC_REG.DEF...                              97.318 %      reading section REC_REG.DEF...                              97.701 %      reading section REC_REG.DEF...                              98.084 %      reading section REC_REG.DEF...                              98.467 %      reading section REC_REG.DEF...                              98.851 %      reading section REC_REG.DEF...                              99.234 %      reading section REC_REG.DEF...                              99.617 %      reading section REC_REG.DEF...                              100 %      done
    ## reading outputs PDF...done
    ## 17 result(s) in file(s) aquifer_day.txt 
    ## 9 result(s) in file(s) hru_ls_day.txt 
    ## 15 result(s) in file(s) hru_nb_day.txt 
    ## 20 result(s) in file(s) hru_pw_day.txt 
    ## 30 result(s) in file(s) hru_wb_day.txt

``` r
# make a list of the daily channel-level outputs
channel.fn = daily.outputs %>% filter(group %in% c('SWAT-DEG_CHANNEL', 'SWAT-DEG_CHANNEL_MORPH')) %>% pull(file)
channel.info = rswat_ofind() %>% 
  filter(file %in% channel.fn) %>% 
  filter( !is.na(units) ) %>% 
  distinct(name, .keep_all=TRUE) %>%
  select(name, units, file)
  
# join with descriptions and documentation reference page, and add object type field
channel.info = do.call( rbind, lapply( channel.fn, function(fn) rswat_docs(fn) ) ) %>% 
  right_join(channel.info, by=c('file', 'name')) %>%
  select(name, units, description, file, pstart) %>%
  mutate(obj='channel')
```

    ## 58 result(s) in file(s) channel_sd_day.txt 
    ## 21 result(s) in file(s) channel_sdmorph_day.txt

``` r
# join the HRU and channel variables info into a single table and save as JSON
joined.info = rbind(HRU.info, channel.info)
jsonlite::toJSON(joined.info, dataframe='rows', pretty=TRUE) %>% write('bigcreek_variables.json')
```

To access one of these output variables, the user will need to provide
the variable name and either: the filename where the variable is stored;
or the object type (‘channel’, or ‘hru’). In most cases, we can look up
the object type and filename based on the variable name, but this is not
always possible because not all output variables have unique names (eg.
‘seep’ appears in both ‘aquifer\_day.txt’ and ‘channel\_sd\_day.txt’,
where it has different meanings and units).

``` r
joined.info %>% filter(name=='seep')
```

    ##   name units                       description               file pstart     obj
    ## 1 seep    mm    seepage from bottom of aquifer    aquifer_day.txt      5     hru
    ## 2 seep  ha m seepage from bottom of water body channel_sd_day.txt      6 channel

To access a given output variable, we configure SWAT+ to write the
associated file, call the simulator executable and when it’s finished,
open the file and extract the data. The code chunk below shows an
example of how this works for the variable ‘flo’ in file
‘aquifer\_day.txt’ This produces

Note that while it is straightforward to configure SWAT+ to write ALL
its output files at once, this can be several times slower than
requesting a particular file.

Usually we will be interested in a particular date-range, and a
particular location, so in addition to the variable name we can accept

For example the following code shows this for the variables
