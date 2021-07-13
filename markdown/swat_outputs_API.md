swat\_outputs\_API.R
================
Dean Koch
2021-07-13

**Mitacs UYRW project**

**swat\_outputs\_API.R**: (in development) documents SWAT+ model outputs
for EPIIC

This documents the outputs of the SWAT+ model and how we can use my R
code to manage them. In the first three sections (this page and next) I
describe examples of geoJSON and JSON outputs. Subsequent sections show
how this works in R using my `rswat` package, with some detail how SWAT+
manages its input/output files.

## output geometries

SWAT+ produces output at two spatial levels, the channel and the HRU.
These correspond to line and polygon geometries that divide the
watershed into subregions. Outputs are served at the level of these
subregions. For the Big Creek demo I’ve saved copies of these geometry
collections as geoJSON files:

-   [**bigcreek\_channels.geojson**](https://raw.githubusercontent.com/deankoch/UYRW_data/master/data/epiic/demo_api/bigcreek_channels.geojson) (136kb, geoJSON LineString Feature
    collection)
-   [**bigcreek\_hrus.geojson**](https://raw.githubusercontent.com/deankoch/UYRW_data/master/data/epiic/demo_api/bigcreek_hrus.geojson) (1MB, geoJSON MultiPolygons Feature
    collection)

Each feature in these collections corresponds to a physical region in
the watershed over which we can request model output. This is the
highest spatial resolution for outputs from SWAT+. We can control this
resolution somewhat during model construction but not afterwards.
Individual features in these files are indexed by the attribute
‘**geometry\_id**’, included in both geoJSON files above.

## output variables and requests

Many different hydrograph variables are available as SWAT+ outputs.
Their names, definitions and units are summarized in the file:

-   [**bigcreek\_output\_definitions.json**](https://raw.githubusercontent.com/deankoch/UYRW_data/master/data/epiic/demo_api/bigcreek_output_definitions.json) (28kb, JSON table with 6
    attributes)

The most important attributes here are ‘name’ and ‘type’ (“channel” or
“hru”), as both are needed to uniquely identify an output variable. For
each output variable users can request data on a particular HRU or
channel by specifying its geometry ID. Example: the following input…

``` r
date_range = c('1973-09-01', '1974-09-01')
geometry_id = 7
swat_variable = 'flo'
swat_file = 'aquifer_day.txt'
dest_file = 'bigcreek_aquifer_day_flo_7_data.json'
```

…produces the following file:

-   [**bigcreek\_aquifer\_day\_flo\_7\_data.json**](https://raw.githubusercontent.com/deankoch/UYRW_data/master/data/epiic/demo_api/bigcreek_aquifer_day_flo_7_data.json) (8kb, JSON time series,
daily)

This contains the ‘flo’ output for the HRU with ID key 7, for a one year
period starting in fall 1973. It also contains a ‘date’ (string) and
‘geometry\_id’ (integer) field. The size of this file will obviously
increase with the length of the time period simulated.

## output API

It will be straightforward for me to write a self-contained Rscript or R
package that produces an output file like the JSON example above for any
variable and location. The inputs to this process are:

-   **date\_range**: vector of start/end dates in whatever format is
    convenient, eg (‘1973-09-01’, ‘1974-09-01’)
-   **type**: string, either ‘hru’ or ‘channel’
-   **geometry\_id**: integer, key for the desired location (must appear
    in the corresponding geometries geoJSON file)
-   **swat\_variable**: string, the variable name (must appear in the
    output definitions JSON file)
-   **dest\_file**: string, the path to the desired output JSON file

If we want, `geometry_id` could also be a GIS point to snap to the
nearest geometry. The entire process could happen in R with minimal
resource requirements, except for a system call to the SWAT+ executable.
See the example sections below for an R implementation.

Resource requirements for the SWAT+ executable vary depending on the
number of HRUs, the length of the simulation period, and the
length/resolution of the weather input files.

If it is convenient we can also support multiple variables (by adding
new fields to the JSON file) or multiple `geometry_id`s (by adding new
entries with identical dates) in the same JSON file. As an example, the
following output file includes the first 10 geometry IDs:

-   [**bigcreek\_aquifer\_day\_flo\_1-10\_data.json**](https://raw.githubusercontent.com/deankoch/UYRW_data/master/data/epiic/demo_api/bigcreek_aquifer_day_flo_1-10_data.json) (78kb, JSON time
    series, daily)

The inputs to produce this file are the same as the first example except
with `geometry_id = c(1,2,...10)`, and the new filename for `dest_file`.
The process specifications for this vectorized version are the same as
above, except with `type`, `geometry_id`, and `swat_variable` being
equal-length vectors.

## R dependencies

Load some R libraries

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))

# load the SWAT+ documentation files
invisible(rswat_docs(quiet=T))

# load an example QSWAT+ project for the demo 
nm = 'big_c_nr_emigrant'
qswat.metadata = my_metadata('run_qswat')
cal.metadata = my_metadata('calibrate_initial')
qswat.dir = here( qswat.metadata[nm, 'file'] )
qswat = qswat_read(qswat.dir)

# copy the (calibrated) SWAT+ model files to a temporary demo directory
temp.dir = tempdir()
src.dir = here(cal.metadata[nm, 'file'])
rswat_copy(from=src.dir, to=temp.dir, fname='.', quiet=TRUE) %>% invisible

# load this project directory in R
cio = rswat_cio(temp.dir, reload=TRUE, ignore='decision_table', quiet=TRUE)

# make a directory for output files
output.dir = here(data.dir, 'epiic/demo_api')
my_dir(output.dir)
```

## HRUs

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
unlink(file.path(output.dir, 'bigcreek_hrus.geojson'))
qswat$hru = qswat$hru %>% rename(geometry_id = id)
st_write(qswat$hru, file.path(output.dir, 'bigcreek_hrus.geojson'))
```

    ## Writing layer `bigcreek_hrus' to data source `D:/UYRW_data/data/epiic/demo_api/bigcreek_hrus.geojson' using driver `GeoJSON'
    ## Writing 50 features with 20 fields and geometry type Multi Polygon.

``` r
# plot the polygons and colour by average elevation
qswat$hru[,'elev'] %>% plot(border=NA, key.pos=1, main='elevation by HRU (m)')
```

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_outputs_API/unnamed-chunk-3-1.png)<!-- -->

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

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_outputs_API/unnamed-chunk-4-1.png)<!-- -->

## channels

Each pair of HRUs is associated with exactly one stream reach -
represented by a line geometry - which drains both the upslope and
floodplain areas. These reaches, AKA channels, are connected in a
tree-like structure and have their own attributes. Just like with HRUs,
channel attributes are assumed by the model to be spatially uniform
along their entire length.

I save the channel geometries in a geoJSON below

``` r
# save the HRUs polygon collection as geoJSON
unlink(file.path(output.dir, 'bigcreek_channels.geojson'))
qswat$cha = qswat$cha %>% rename(geometry_id = Channel)
st_write(qswat$cha, file.path(output.dir, 'bigcreek_channels.geojson'))
```

    ## Writing layer `bigcreek_channels' to data source `D:/UYRW_data/data/epiic/demo_api/bigcreek_channels.geojson' using driver `GeoJSON'
    ## Writing 25 features with 16 fields and geometry type Line String.

The plot below overlays the channel geometry with (exaggerated) line
widths scaled according to physical stream width.

``` r
# plot the stream line geometries with width scaled to match physical average (attribute 'Wid2')
st_geometry(qswat$hru) %>% plot(col='grey', border=NA, main='stream widths (exaggerated)', reset=FALSE)
st_geometry(qswat$cha) %>% plot(main='width (ft)', lwd=qswat$cha$Wid2/2, add=TRUE)
```

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_outputs_API/unnamed-chunk-6-1.png)<!-- -->

We have many HRU-level and channel-level attributes in the model (100s),
some of which are very important during calibration or scenario
simulations. A small number of these attributes (like ‘elev’ and ‘Wid2’,
in the plots above) are already copied to the geometry collections and
so you’ll see them in the geoJSON files.

Later on we can join the full set of attributes and define everything
but for now lets just worry about the geometries and associated
simulation outputs.

## output variables

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
rswat_ofind() %>% 
  filter(file %in% daily.outputs$file) %>% 
  filter( !is.na(units) ) %>% 
  nrow()
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

    ## 17 result(s) in file(s) aquifer_day.txt 
    ## 9 result(s) in file(s) hru_ls_day.txt 
    ## 15 result(s) in file(s) hru_nb_day.txt 
    ## 20 result(s) in file(s) hru_pw_day.txt 
    ## 30 result(s) in file(s) hru_wb_day.txt

``` r
# make a list of the daily channel-level outputs
channel.groups = c('SWAT-DEG_CHANNEL', 'SWAT-DEG_CHANNEL_MORPH')
channel.fn = daily.outputs %>% filter(group %in% channel.groups) %>% pull(file)
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
jsonlite::toJSON(joined.info, dataframe='rows', pretty=TRUE) %>% 
  write(file.path(output.dir, 'bigcreek_output_definitions.json'))
```

## requesting SWAT+ outputs

To access one of these output variables, the user will need to provide
the variable name and either: the filename where the variable is stored;
or the object type (‘channel’, or ‘hru’). In most cases, we can look up
the object type and filename based on the variable name, but this is not
always possible because not all output variables have unique names. For
example, ‘seep’ appears in both ‘aquifer\_day.txt’ and
‘channel\_sd\_day.txt’, where it has different interpretations and
units).

``` r
# print info about 'seep'
joined.info %>% filter(name=='seep')
```

    ##   name units                       description               file pstart     obj
    ## 1 seep    mm    seepage from bottom of aquifer    aquifer_day.txt      5     hru
    ## 2 seep  ha m seepage from bottom of water body channel_sd_day.txt      6 channel

Users can also optionally specify a date range and a geometry ID for the
spatial object of interest. The geometry ID is an integer key
identifying individual HRUs/channels, and can be found in the geometry
geoJSON files as attribute ‘geometry\_id’. If no geometry ID is
specified, SWAT+ returns data for all of them.

Ideally the date range will be a subset of the dates covered by a set of
weather inputs specified by the user at this stage, but any physically
valid date range should acceptable, since SWAT+ has a weather generator
module that kicks in for missing data. If no input dates are specified,
SWAT+ uses the previously saved settigns.

Here is the process that happens in R for an output variable request:

-   edit SWAT+ configuration files for the requested dates and outputs
-   call the simulator executable and wait for it to finish
-   open the output file and extract the relevant data columns
-   parse dates and other ID fields, filter to relevant rows
-   write to output JSON time series file

## example 1

The code chunk below shows an example of how this works for the variable
‘flo’ in file ‘aquifer\_day.txt’. We specify a date range to coincide
with the main USGS gage record and request the data for the aquifer in
the HRU with geometry ID 7 (these parameters defined at the beginning of
the script)

``` r
# plot the HRU of interest
st_geometry(qswat$hru) %>% 
  plot(col='grey', border=NA, main=paste('HRU with id =', geometry_id), reset=FALSE)
qswat$hru %>% filter(geometry_id == !!geometry_id) %>% 
  st_geometry() %>% 
  plot(col='black', add=TRUE)
```

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_outputs_API/unnamed-chunk-10-1.png)<!-- -->

``` r
# print info about requested variable
output.info = joined.info %>% filter(name == swat_variable)

# set the time range and toggle output files (edits 'print.prt', 'time.sim')
rswat_time(date_range)
```

    ##        start          end 
    ## "1973-09-01" "1974-09-01"

``` r
rswat_print(swat_file)

# run the executable (ignore the basin_cop* output files, which can't be turned off)
rswat_exec()
```

    ## 
    ## >> finished (2.27 seconds runtime)

    ## [1] "aquifer_day.txt"       "basin_crop_yld_yr.txt" "basin_crop_yld_aa.txt"

``` r
# load the output data, filter to requested
output.data = rswat_output(swat_file, vname=swat_variable) %>% 
  filter(unit==geometry_id) %>% 
  rename(geometry_id = unit) %>%
  select(-c(gis_id, name))
 
# convert units as needed, then drop the attribute for the JSON
output.data[[swat_variable]] = output.data[[swat_variable]] %>% 
  set_units(output.info$units, mode='standard') %>% 
  drop_units

# write to JSON time series file and print the first few rows
write_json(output.data, file.path(output.dir, dest_file), dataframe='columns', Date='ISO8601', pretty=TRUE)
output.data %>% head
```

    ##         date geometry_id   flo
    ## 1 1973-09-01           7 0.154
    ## 2 1973-09-02           7 0.122
    ## 3 1973-09-03           7 0.076
    ## 4 1973-09-04           7 0.042
    ## 5 1973-09-05           7 0.022
    ## 6 1973-09-06           7 0.011

This output JSON has three fields: ‘date’, a string in
[ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html)
format; ‘geometry\_id’, an integer (necessary for cases where multiple
IDs are requested), and the requested variable (in this case ‘flo’),
which is always a floating point number having units as listed in
‘bigcreek\_variables.json’

## example 2

This next example shows how data for multiple geometry IDs can be
requested in the same JSON file:

``` r
# use the same inputs as above, except request multiple geometry IDs and write to a new JSON file
geometry_id = 1:10
dest_file = 'bigcreek_aquifer_day_flo_1-10_data.json'

# plot the catchment
st_geometry(qswat$hru) %>% 
  plot(col='grey', 
       border=NA, 
       main=paste('HRUs with ids =', paste(geometry_id, collapse=', ')), 
       reset=FALSE)

# colour the HRUs of interest
qswat$hru %>% 
  filter(geometry_id %in% !!geometry_id) %>% 
  st_geometry() %>% plot(col='black', border='grey', add=TRUE)
```

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_outputs_API/unnamed-chunk-11-1.png)<!-- -->

``` r
# the simulation settings and output file haven't changed, so no need to run the executable

# load the output data, filter to requested
output.data = rswat_output(swat_file, vname=swat_variable) %>% 
  filter(unit %in% geometry_id) %>% 
  rename(geometry_id = unit) %>%
  select(-c(gis_id, name))

# convert units as needed, then drop the attribute for the JSON
output.data[[swat_variable]] = output.data[[swat_variable]] %>% 
  set_units(output.info$units, mode='standard') %>% 
  drop_units

# write to JSON time series file and print the first few rows
write_json(output.data, file.path(output.dir, dest_file), dataframe='columns', Date='ISO8601', pretty=TRUE)
output.data %>% head
```

    ##         date geometry_id   flo
    ## 1 1973-09-01           1 0.134
    ## 2 1973-09-01           2 0.021
    ## 3 1973-09-01           3 0.139
    ## 4 1973-09-01           4 0.021
    ## 5 1973-09-01           5 0.138
    ## 6 1973-09-01           6 0.021

``` r
# delete the temporary folder 
unlink(temp.dir, recursive=TRUE)
```
