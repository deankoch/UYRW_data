get\_snow.R
================
Dean Koch
2021-03-05

**Mitacs UYRW project**

**get\_snow**: finds climatic sensor stations located in the UYRW and
downloads their time series

Retrieve snow related datasets from various sources: SNOTEL time series
and other snow data from the USDA NRCS National Water and Climate Centre
website; SNODAS grids from the National Snow and Ice Data Center FTP
Archives

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
which creates some required directories and project config files, should
be run before this script.

## libraries

Start by sourcing two helper scripts
([helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_get\_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))
```

The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package
fetches [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from
the USDA.

``` r
library(snotelr)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
snow.meta = my_metadata('get_snow', files.towrite, overwrite=TRUE)
```

    ## [1] "> writing metadata to: data/get_snow_metadata.csv"

``` r
print(snow.meta[, c('file', 'type')])
```

    ##                                       file          type
    ## nwcc_src                  data/source/nwcc     directory
    ## nwcc_sites data/source/nwcc/nwcc_sites.csv           CSV
    ## nwcc_sf       data/prepared/nwcc_sites.rds   R sf object
    ## nwcc_data      data/prepared/nwcc_data.rds R list object
    ## metadata        data/get_snow_metadata.csv           CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_snow_metadata.csv)
in the `/data` directory.

Load some of the data prepared earlier

``` r
# load metadata csv, CRS info list and watershed geometries from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.poly.padded = readRDS(here(basins.meta['boundary_padded', 'file']))
```

## List relevant NWCC sites

Find list of all NWCC sites, saving as a CSV file and recasting as
`sfc`. Mask to URYW area

``` r
# make the destination directory
snow.dir = here(snow.meta['nwcc_src', 'file'])
my_dir(snow.dir)

# download the sites table if we haven't already
snow.csv = here(snow.meta['nwcc_sites', 'file'])
if(!file.exists(snow.csv))
{
  # a helper function downloads and parses the table
  nwcc.sites = my_nwcc_list(snow.csv)

} else {
  
  nwcc.sites = read.csv(snow.csv)
  
}


# extract site coordinates, convert to sf object, transform to UTM
coords.mat = as.matrix(nwcc.sites[, c('longitude', 'latitude')])
nwcc.sfc = st_sfc(lapply(1:nrow(coords.mat), function(site) st_point(coords.mat[site,])), crs=crs.list$epsg.geo)
nwcc.sfc = st_transform(nwcc.sfc, crs=crs.list$epsg)

# find subset of sites within UYRW area
nwcc.uyrw.idx = st_intersects(nwcc.sfc, uyrw.poly.padded, sparse=FALSE)
uyrw.sites = nwcc.sites[nwcc.uyrw.idx,]
head(uyrw.sites)
```

    ##      ntwk state   ts latitude longitude     elev      county site_id            site_nm       huc_id                 huc_nm
    ## 631  SNOW    MT <NA>    45.08   -111.38 2377.440     Madison   11D23       Cashe Creek  100200080106            Sage Creek 
    ## 705  SNOW    WY <NA>    44.15   -110.22 2816.352       Teton   10E17 Two Ocean Plateau  170401010201         De Lacy Creek 
    ## 896  SNOW    WY <NA>    44.45   -110.82 2255.520       Teton   10E18      Old Faithful  100200070102 Little Firehole River 
    ## 1041 SNOW    MT <NA>    45.43   -110.05 2880.360 Sweet Grass   10D21      Picket Pin D  100700050202       Limestone Creek 
    ## 1052 SNOW    WY <NA>    44.80   -109.66 2331.720        Park   09E08         Wolverine  100700060104           Crazy Creek 
    ## 1255 SNOW    WY <NA>    44.73   -109.91 2865.120        Park   09E07       Parker Peak  100700010502            Sour Creek 
    ##      start_mo start_yr end_mo end_yr    dc
    ## 631         2     1984     12   1991  TRUE
    ## 705         4     1982      4   1988  TRUE
    ## 896         2     1975      1   2021 FALSE
    ## 1041        2     1970     12   1990  TRUE
    ## 1052        2     1970      6   1991  TRUE
    ## 1255        3     1965      5   1986  TRUE

## download and import NWCC data

This chunk selects all available variables at daily to monthly level at
the sites identified above, and executes NWCC requests to download the
data as CSV then import it into R, using helper functions based on
`snotelr`, saving copies to disk as rds

``` r
# print the first few variable descriptions
nwcc.descriptions = my_nwcc_get()
nwcc.varnames = names(nwcc.descriptions)
head(nwcc.descriptions)
```

    ##                         TMAX                         TMIN                         TOBS                         PREC 
    ##    "air temperature maximum"    "air temperature minimum"   "air temperature observed" "precipitation accumulation" 
    ##                         PRCP                         RESC 
    ##    "precipitation increment"   "reservoir storage volume"

``` r
# define the RDS files to be written here
uyrw.files.path = here(snow.meta['nwcc_data', 'file'])
uyrw.sf.path = here(snow.meta['nwcc_sf', 'file'])

# proceed only if they don't exist
if(any(!file.exists(c(uyrw.files.path, uyrw.sf.path))))
{
  # run the helper to download data files to `snow.dir` - two per site (daily and semimonthly)
  uyrw.files = my_nwcc_get(nwcc.varnames, uyrw.sites, snow.dir, reuse=TRUE, retry=2)
  head(uyrw.files %>% select(site_id, state, ntwk, freq, path))
  
  # open everything as list of dataframes and omit empty datasets (sites to discard)
  uyrw.data = my_nwcc_import(uyrw.files, nwcc.varnames)
  idx.empty = sapply(uyrw.data, is.null)
  uyrw.data = uyrw.data[!idx.empty]
  
  # construct an `sf` dataframe of site locations with geometries computed earlier
  uyrw.sfc = nwcc.sfc[nwcc.uyrw.idx][!idx.empty]
  uyrw.sf = st_sf(uyrw.sites[!idx.empty,], geometry = uyrw.sfc)
  
  # replace start/end dates with these verified values, add row counts and a primary key:
  uyrw.sf = uyrw.sf %>% 
    mutate(table_id = 1:nrow(uyrw.sf)) %>% 
    mutate(n_daily = sapply(uyrw.data, function(x) sum(x$period=='daily'))) %>% 
    mutate(n_semimonthly = sapply(uyrw.data, function(x) sum(x$period=='semimonthly'))) %>% 
    mutate(start = do.call(c, lapply(uyrw.data, function(x) min(x$date)))) %>%
    mutate(end = do.call(c, lapply(uyrw.data, function(x) max(x$date)))) %>%
    select(-c(start_yr, start_mo, end_yr, end_mo))
  
  # add site_id field to each data frame for convenience later
  uyrw.data = mapply(function(x, y) cbind(x, data.frame(site_id=y)), uyrw.data, uyrw.sf$site_id)

  # save to disk
  saveRDS(uyrw.data, uyrw.files.path)
  saveRDS(uyrw.sf, uyrw.sf.path)
  
} else {
  
  # load from disk
  uyrw.data = readRDS(uyrw.files.path)
  uyrw.sf = readRDS(uyrw.sf.path)
} 

# report the variables found in these records of the UYRW area
uyrw.varnames = unique(unlist(sapply(uyrw.data, function(x) names(x))))
print(nwcc.descriptions[names(nwcc.descriptions) %in% uyrw.varnames])
```

    ##                                 TMAX                                 TMIN                                 TOBS 
    ##            "air temperature maximum"            "air temperature minimum"           "air temperature observed" 
    ##                                 PREC                                 PRCP                                 SNWD 
    ##         "precipitation accumulation"            "precipitation increment"                         "snow depth" 
    ##                                 WTEQ                               PRCPSA                                SRDOO 
    ##              "snow water equivalent" "precipitation increment - snow-adj"      "river discharge observed mean" 
    ##                                WTEQX                                SRVOO                                 SNDN 
    ##      "snow water equivalent maximum"            "stream volume, observed"                       "snow density" 
    ##                                 SNRR                                WDIRV                                WSPDV 
    ##                    "snow rain ratio"             "wind direction average"                 "wind speed average" 
    ##                                WSPDX 
    ##                 "wind speed maximum"
