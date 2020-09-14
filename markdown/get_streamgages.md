get\_streamgages.R
================
Dean Koch
August 22, 2020

**MITACS UYRW project**

**get\_streamgages**: finds stream sensor stations located in the UYRW

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
which creates some required directories and project config files, should
be run before this script.

## libraries

`dataRetrieval` is used to fetch the USGS data. See the
[get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
for other required libraries

``` r
library(here)
source(here('R/get_helperfun.R'))
library(dataRetrieval)
#?library(waterData)
```

## project data

``` r
# This list describes all of the files created by this script:
files.towrite = list(
  
  # table of site locations from USGS Site web service
  c(name='USGS_sites_rdb',
    file=file.path(src.subdir, 'USGS_sites.rdb'),
    type='USGS rdb file', 
    description='results of USGS Site web service search for sites in UYRW'),
  
  # site locations with metadata from USGS in sfc format
  c(name='USGS_sites',
    file=file.path(out.subdir, 'USGS_sites.rds'), 
    type='R sf object', 
    description='sfc object with USGS sensor locations in UYRW'),
  
  # parameter code descriptions for NWIS time-series data
  c(name='USGS_paramcodes',
    file=file.path(data.dir, 'USGS_paramcodes.csv'), 
    type='CSV', 
    description='description of codes used in parameter_cd column of NWIS dataframe on time-series'),
  
  # graphic showing SNOTEL and GHCND site locations on the UYRW
  c(name='img_streamgage',
    file=file.path(graphics.dir, 'streamgage_sites.png'),
    type='png graphic', 
    description='image of stream gage locations in the UYRW')
  
)

# write this information to disk
my_metadata('get_streamgages', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/get_streamgages_metadata.csv"

    ##                                              file          type
    ## USGS_sites_rdb         data/source/USGS_sites.rdb USGS rdb file
    ## USGS_sites           data/prepared/USGS_sites.rds   R sf object
    ## USGS_paramcodes          data/USGS_paramcodes.csv           CSV
    ## img_streamgage      graphics/streamgage_sites.png   png graphic
    ## metadata        data/get_streamgages_metadata.csv           CSV
    ##                                                                                       description
    ## USGS_sites_rdb                          results of USGS Site web service search for sites in UYRW
    ## USGS_sites                                          sfc object with USGS sensor locations in UYRW
    ## USGS_paramcodes description of codes used in parameter_cd column of NWIS dataframe on time-series
    ## img_streamgage                                         image of stream gage locations in the UYRW
    ## metadata                                         list files of files written by get_streamgages.R

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_streamgage_metadata.csv)
in the `/data` directory. Load some of the data prepared earlier

``` r
# load metadata csv, CRS info list and watershed geometries from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
uyrw.waterbody = readRDS(here(my_metadata('get_basins')['waterbody', 'file']))
uyrw.mainstem = readRDS(here(my_metadata('get_basins')['mainstem', 'file']))
uyrw.flowline = readRDS(here(my_metadata('get_basins')['flowline', 'file']))
```

## Find sites

The instructions at the [URL Generation Tool
page](https://waterservices.usgs.gov/rest/Site-Test-Tool.html), show how
to constrct a URL that downloads a copy of the site info list from the
USGS Site Web Service. Information about the output format (USGS RDB) is
[available here](https://waterservices.usgs.gov/rest/Site-Service.html)
and, in more detail,
[here](https://pubs.usgs.gov/of/2003/ofr03123/6.4rdb_format.pdf).

``` r
if(!file.exists(here(my_metadata('get_streamgages')['USGS_sites_rdb', 'file'])))
{
  # find a bounding box in geographical coordinates
  bbox.geo = st_bbox(st_transform(uyrw.poly, crs=crs.list$epsg.geo))
  
  # set up URLs and URL arguments for querying site records inside this box
  urlargs.domain = 'https://waterservices.usgs.gov/nwis/site/'
  urlargs.list = list(format = '?format=rdb',
                      bbox = paste0('bBox=', paste(bbox.geo, collapse=',')), 
                      verbosity = 'seriesCatalogOutput=true',
                      status = 'siteStatus=all')
  
  # build the URL and query the USGS Site Web Service
  download.file(paste0(urlargs.domain, paste(urlargs.list, collapse='&')), here(my_metadata('get_streamgages')['USGS_sites_rdb', 'file']))
  
}
```

Load the RDB file, omitting stations not in UYRW, and convert it to a
`sf` object

``` r
if(!file.exists(here(my_metadata('get_streamgages')['USGS_sites', 'file'])))
{
  # load the RDB file as a tab-delimited data frame, omit first row (which indicates string lengths) 
  usgs.df = read.csv(here(my_metadata('get_streamgages')['USGS_sites_rdb', 'file']), comment.char='#', sep='\t')
  usgs.df = usgs.df[-1,]
  
  # extract coordinates, coercing to numeric
  sites.coords.matrix = sapply(usgs.df[, c('dec_long_va', 'dec_lat_va')], as.numeric) 

  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  usgs.sfc = st_sfc(lapply(1:nrow(usgs.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  usgs.sf = st_sf(cbind(usgs.df, usgs.sfc))
  
  # transform to UTM and clip to UYRW (8,500 data records identified, most of which are one-time)
  usgs.sf = st_transform(usgs.sf, crs=crs.list$epsg)
  usgs.sf = st_intersection(usgs.sf, uyrw.poly)
  
  # save to disk
  saveRDS(usgs.sf, here(my_metadata('get_streamgages')['USGS_sites', 'file']))

} else {
  
  # load from disk 
  usgs.sf = readRDS(here(my_metadata('get_streamgages')['USGS_sites', 'file']))
  
}
```

This dataset is quite large, with over 8500 location-times indexed.
Mostly these are one-time measurements. Time-series data at regular
(daily) intervals will be more useful, when it comes to fitting a
hydrology model. Parse the USGS data to find 88 time-series entries:

``` r
# 
idx.ts = usgs.sf$data_type_cd %in% c('dv', 'iv', 'id')
sum(idx.ts)
```

    ## [1] 88

(see the [USGS water
Services](https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd)
for more information about what these `data_type` codes mean).

Information on parameter codes can also be downloaded using the Water
Services REST interface

``` r
if(!file.exists(here(my_metadata('get_streamgages')['USGS_paramcodes', 'file'])))
{
  # query the meaning of the parameter column codes corresponding to time series in our area
  uyrw.paramcodes = unique(usgs.sf[idx.ts,]$parm_cd)
  paramcodes.url = 'https://help.waterdata.usgs.gov/code/parameter_cd_nm_query'
  paramcodes.query = paste0(paramcodes.url, '?parm_nm_cd=', uyrw.paramcodes, '&fmt=rdb')
  paramcodes.list = lapply(paramcodes.query, function(urlstring) read.csv(url(urlstring), comment.char='#', sep='\t')[-1,])
  paramcodes.df = do.call(rbind, paramcodes.list)
  
  # save to disk
  write.csv(paramcodes.df, here(my_metadata('get_streamgages')['USGS_paramcodes', 'file']), row.names=FALSE)
  
} else {
  
  # load from disk 
  paramcodes.df = read.csv(here(my_metadata('get_streamgages')['USGS_paramcodes', 'file']), colClasses='character')
}
```

## visualization

plot the locations of stream gage locations as a png file

``` r
# make a copy of the time-series data
usgs.ts.sf = usgs.sf[idx.ts,]

# these correspond to 21 unique locations
uyrw.sitecodes = unique(usgs.ts.sf$site_no)
length(uyrw.sitecodes)
```

    ## [1] 21

``` r
# find all entries corresponding to streamflow
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']
idx.streamflow = usgs.ts.sf$parm_cd == paramcode.streamflow

# find all entries corresponding to turbidity and suspended sediment
paramcode.turbidity = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Turbidity']
paramcode.sediment = paramcodes.df$parameter_cd[paramcodes.df$SRSName %in% c('Suspended sediment concentration (SSC)', 'Suspended sediment discharge')]
idx.turbidity = usgs.ts.sf$parm_cd == paramcode.turbidity
idx.sediment = usgs.ts.sf$parm_cd %in%  paramcode.sediment

# 32 entries: 20 are of streamflow, 6 are of turbidity, 6 are of suspended sediment
idx.all = idx.streamflow | idx.turbidity | idx.sediment
sum(idx.all)
```

    ## [1] 32

``` r
sum(idx.streamflow)
```

    ## [1] 20

``` r
sum(idx.turbidity)
```

    ## [1] 6

``` r
sum(idx.sediment)
```

    ## [1] 6

``` r
# find the end-years and durations as integers
usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
usgs.ts.startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.startyear

# add dummy columns for indicating the variable recorded
usgs.ts.sf$plotlabel_sf = 'streamflow'
usgs.ts.sf$plotlabel_tb = 'turbidity'
usgs.ts.sf$plotlabel_ss = 'suspended sediment'

# add columns for duration and end-year of time series for precipitation
usgs.ts.sf$endyear[usgs.ts.sf$endyear == 2020] = NA
```

Set up the aesthetics and make the plot

``` r
# load the plotting parameters used in get_weatherstations.R
tmap.pars = readRDS(here(my_metadata('get_weatherstations')['pars_tmap', 'file']))

# adjust with a better highlight colour for the blue background
tmap.pars$dots$tm_symbols$colorNA = 'orange'
  
# create the plot grob and write to disk
if(!file.exists(here(my_metadata('get_streamgages')['img_streamgage', 'file'])))
{
  # build the tmap plot object
  tmap.streamgage = tm_shape(uyrw.poly) +
                      tm_polygons(col='skyblue', border.col='yellowgreen') +
                    tm_shape(uyrw.waterbody) +
                      tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
                    tm_shape(uyrw.mainstem) +
                      tm_lines(col='dodgerblue4', lwd=2) +
                    tm_shape(uyrw.flowline) +
                      tm_lines(col='dodgerblue3') +
                    tm_shape(usgs.ts.sf[idx.streamflow,]) +
                      tm_dots(col='plotlabel_sf', palette='black', size=0.5, shape=1, title='') +
                    tm_shape(usgs.ts.sf[idx.sediment,]) +
                      tm_dots(col='plotlabel_ss', palette='black', size=0.5, shape=2, title='') +
                    tm_shape(usgs.ts.sf[idx.turbidity,]) +
                      tm_dots(col='plotlabel_tb', palette='black', size=0.5, shape=6, title='') +
                    tm_shape(usgs.ts.sf[idx.all,]) +
                      tmap.pars$dots +
                    tmap.pars$layout +
                    tm_layout(main.title='NWIS daily discharge records in the UYRW')
  
  # render the plot
  tmap_save(tm=tmap.streamgage, 
            here(my_metadata('get_streamgages')['img_streamgage', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}
```

![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/streamgage_sites.png)
