---
title: "get_streamgages.R"
author: "Dean Koch"
date: "August 22, 2020"
output: github_document
---

**MITACS UYRW project**

**get_streamgages**: finds stream sensor stations located in the UYRW


[get_basins.R](https://github.com/deankoch/URYW_data/blob/master/get_basins.knit.md)
which creates some required directories and project config files, should be run before this script.

## libraries


```r
library(sf)
library(tmap)
library(here)
library(dataRetrieval)
library(waterData)
#library(nhdplusTools)
library('readtext')
```


## project data


```r
# project directory names
graphics.dir = 'graphics'
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# load metadata csv, CRS info list and watershed geometries from disk
# basins.metadata.df = read.csv(here('data/basins_metadata.csv'), header=TRUE, row.names=1)
crs.list = readRDS(here(basins.metadata.df['crs', 'file']))
uyrw.poly = readRDS(here(basins.metadata.df['boundary', 'file']))
# uyrw.waterbody = readRDS(here(basins.metadata.df['waterbody', 'file']))
# uyrw.mainstem = readRDS(here(basins.metadata.df['mainstem', 'file']))
# uyrw.flowline = readRDS(here(basins.metadata.df['flowline', 'file']))



# this CSV file will serve as a guide for all files written to the project folder
streamgage.metadata.file = 'data/streamgage_metadata.csv'
if(!file.exists(here(streamgage.metadata.file)))
{
  
  # filename for table of site locations from USGS Site web service
  sensor.sites.rdb.file = c(name='USGS_sites.rdb',
                            file=file.path(src.subdir, 'USGS_sites.rdb'),
                            type='USGS rdb file', 
                            description='results of USGS Site web service search for sites in UYRW')
  
  # filename for site locations with metadata from USGS in sfc format
  sensor.sites.sfc.file = c(name='USGS_sites.sfc',
                            file=file.path(out.subdir, 'USGS_sites.rds'), 
                            type='R sf object', 
                            description='sfc object with USGS sensor locations in UYRW')
  
  # filename for graphic showing SNOTEL and GHCND site locations on the UYRW
  sensor.sites.png.file = c(name='img_streamgage',
                            file=file.path(graphics.dir, 'streamgage_sites.png'),
                            type='png graphic', 
                            description='image of stream gage locations in the UYRW')

  
  # bind all the individual filename info vectors into a data frame
  streamgage.metadata.df = data.frame(rbind(sensor.sites.rdb.file,
                                            sensor.sites.sfc.file,
                                            sensor.sites.png.file), row.names='name')
  
  # save the data frame
  write.csv(streamgage.metadata.df, here(streamgage.metadata.file))
  
} else {
  
  # load the data frame
  streamgage.metadata.df = read.csv(here(streamgage.metadata.file), header=TRUE, row.names=1)
  
}
```

This list of files and descriptions is now stored as a
[.csv file](https://github.com/deankoch/URYW_data/blob/master/data/streamgage_metadata.csv)
in the `/data` directory.

## Find sites

The instructions at the [URL Generation Tool page](https://waterservices.usgs.gov/rest/Site-Test-Tool.html), 
show how R's `download.file` function can be used to download a copy of the site info list from the USGS Site
Web Service. Information about the output format (USGS RDB) is 
[available here](https://waterservices.usgs.gov/rest/Site-Service.html) and, in more detail,
[here](https://pubs.usgs.gov/of/2003/ofr03123/6.4rdb_format.pdf).


```r
if(!file.exists(here(streamgage.metadata.df['USGS_sites.rdb', 'file'])))
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
  download.file(paste0(urlargs.domain, paste(urlargs.list, collapse='&')), here(streamgage.metadata.df['USGS_sites.rdb', 'file']))
  
}
```

Load the RDB file, omit stations not in UYRW, and convert it to a `sf` object, then save to disk


```r
if(!file.exists(here(streamgage.metadata.df['USGS_sites.rdb', 'file'])))
{

  # load the RDB file as a tab-delimited data frame, omit first row (which indicates string lengths) 
  usgs.df = read.csv(here(streamgage.metadata.df['USGS_sites.rdb', 'file']), comment.char='#', sep='\t')
  usgs.df = usgs.df[-1,]
  
  # extract coordinates, coercing to numeric
  sites.coords.matrix = sapply(usgs.df[, c('dec_long_va', 'dec_lat_va')], as.numeric) 

  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  usgs.sfc = st_sfc(lapply(1:nrow(usgs.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  usgs.sf = st_sf(cbind(usgs.df, usgs.sfc))
  
  # transform to UTM and clip to URYW (11,000 data records identified, most of which are one-time)
  usgs.sf = st_transform(usgs.sf, crs=crs.list$epsg)
  usgs.sf = st_intersection(usgs.sf, uyrw.poly)
  
  # save to disk
  saveRDS(usgs.sf, here(streamgage.metadata.df['USGS_sites.sfc', 'file']))

  
} else {
  
  # load from disk 
  usgs.sf = readRDS(here(streamgage.metadata.df['USGS_sites.sfc', 'file']))
  
}

usgs.sf$begin_date
```

```
##    [1] "1970-09-09" "1970-09-09" "1970-09-09" "1977-09-12" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09"
##   [12] "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09"
##   [23] "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1977-09-12" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1977-09-12"
##   [34] "1977-09-12" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09"
##   [45] "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09"
##   [56] "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-09" "1970-09-10" "1970-09-10"
##   [67] "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10"
##   [78] "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10"
##   [89] "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10"
##  [100] "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10"
##  [111] "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10" "1970-09-10"
##  [122] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22"
##  [133] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22"
##  [144] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-19"
##  [155] "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19"
##  [166] "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19"
##  [177] "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1969-05-19" "1971-09-22" "1971-09-22"
##  [188] "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22"
##  [199] "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22"
##  [210] "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22"
##  [221] "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22"
##  [232] "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1971-09-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22"
##  [243] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22"
##  [254] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22"
##  [265] "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-05-22" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [276] "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [287] "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [298] "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [309] "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [320] "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19" "1969-08-19"
##  [331] "1969-08-19" "1969-08-19" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16"
##  [342] "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16"
##  [353] "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "1964-06-16" "2006"       "1983-10-01" "1983-10-01" "1983-10-01" "1926-10-01" "1983-10-01"
##  [364] "1983-10-01" "1983-10-01" "1923-07-06" "1970-10-19" "1970-10-19" "1983-10-13" "1983-10-13" "1984-02-29" "1984-02-29" "1984-02-29" "1970-10-19"
##  [375] "1976-09-18" "1990-10-02" "1970-10-19" "1970-10-19" "1970-10-19" "1976-09-18" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19"
##  [386] "1976-09-18" "1976-09-18" "1976-09-18" "1976-09-18" "1976-09-18" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19"
##  [397] "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1976-09-18" "1990-10-02" "1970-10-19"
##  [408] "1976-09-18" "1976-09-18" "1970-10-19" "1970-10-19" "1970-10-19" "1970-10-19" "1997-10-22" "1984-02-29" "1913-06-20" "1991-10-01" "2007-10-01"
##  [419] "1913-06-21" "1913-06"    "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23"
##  [430] "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23"
##  [441] "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23" "1969-06-23"
##  [452] "1951-10-02" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29"
##  [463] "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29"
##  [474] "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1967-08-22" "1967-08-22" "1969-07-18" "1969-07-18"
##  [485] "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22"
##  [496] "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1967-08-22" "1969-07-18"
##  [507] "1969-07-18" "1967-08-22" "1967-08-22" "1969-07-18" "1967-08-22" "1967-08-22" "1967-08-22" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29"
##  [518] "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29"
##  [529] "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29" "1968-07-29"
##  [540] "1968-07-29" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17"
##  [551] "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17"
##  [562] "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17" "1969-07-17"
##  [573] "1922-09-01" "1923-06-12" "1983-10-01" "1983-10-01" "1983-10-01" "1983-10-01" "1983-10-01" "1983-10-01" "1983-10-01" "1984-07-07" "1983-10-13"
##  [584] "1983-10-13" "1983-10-13" "1983-10-13" "1984-03-01" "1984-03-01" "1984-03-01" "1983-11-02" "1983-10-13" "1984-03-01" "1984-10-03" "1983-11-02"
##  [595] "1984-03-01" "1983-11-02" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [606] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [617] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [628] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [639] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [650] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [661] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [672] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [683] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [694] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [705] "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23" "1969-07-23"
##  [716] "1969-07-23" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22"
##  [727] "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22"
##  [738] "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-22" "1969-07-24" "1969-07-24"
##  [749] "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24"
##  [760] "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24"
##  [771] "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1969-07-24" "1974-09-01" "1975-09-01" "1996-04-03" "2006"       "2003-04-24"
##  [782] "2003-04-24" "2003-04-24" "1998-10-01" "2008-10-09" "2008-10-09" "2008-10-09" "2009-05-29" "2009-05-29" "2009-05-29" "1999-06-19" "1999-01-21"
##  [793] "1999-10-06" "1999-01-21" "1999-01-21" "1999-01-21" "2000-10-23" "1999-10-06" "2000-10-23" "1999-10-06" "2000-10-23" "1999-01-21" "1999-10-06"
##  [804] "2000-10-23" "1999-10-06" "2000-03-14" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-10-06" "1999-01-21" "1999-11-02"
##  [815] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21"
##  [826] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "2001-01-11" "1999-01-21" "1999-01-21" "2000-02-08" "1999-01-21"
##  [837] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21"
##  [848] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-10-06" "1999-01-21" "1999-01-21" "2000-03-14" "1999-01-21" "1999-01-21"
##  [859] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21"
##  [870] "1999-01-21" "2000-03-14" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "2000-03-14" "1999-01-21" "2000-03-14"
##  [881] "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "2000-03-14" "1999-01-21" "1999-01-21" "1999-01-21" "2000-05-25"
##  [892] "1999-01-21" "1999-04-05" "2000-10-23" "1999-10-06" "2000-10-23" "1999-01-21" "2000-06-27" "2000-06-27" "1999-01-21" "2001-01-11" "2000-10-23"
##  [903] "1999-10-06" "2000-10-23" "1999-01-21" "1999-01-21" "1999-01-21" "1999-01-21" "2000-10-23" "1999-01-21" "1999-01-21" "1999-01-21" "1999-02-09"
##  [914] "1999-01-21" "2000-10-23" "1999-10-06" "1999-10-06" "2000-10-23" "1999-01-21" "1999-01-21" "1999-10-06" "1999-10-06" "1999-10-06" "2000-05-16"
##  [925] "1999-10-06" "1999-10-06" "1999-10-06" "1999-10-06" "1996-08-21" "1998-10-01" "2007-10-01" "2010-08-11" "1969-05-20" "1969-05-20" "1969-05-20"
##  [936] "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20"
##  [947] "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20"
##  [958] "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-05-20" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24"
##  [969] "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24"
##  [980] "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24"
##  [991] "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "1969-06-24" "2006"       "2005-04-21" "2005-04-21" "2005-04-21"
##  [ reached getOption("max.print") -- omitted 10329 entries ]
```

```r
# find 115 time-series entries (see https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd) 
idx.ts = usgs.sf$data_type_cd %in% c('dv', 'iv', 'id')
sum(idx.ts)
```

```
## [1] 115
```

```r
# these correspond to 37 locations
uyrw.sitecodes = unique(usgs.sf[idx.ts,]$site_no)
length(uyrw.sitecodes)
```

```
## [1] 37
```

```r
# query the meaning of these parameter codes
uyrw.paramcodes = unique(usgs.sf[idx.ts,]$parm_cd)
paramcodes.url = 'https://help.waterdata.usgs.gov/code/parameter_cd_nm_query'
paramcodes.query = paste0(url.paramcodes, '?parm_nm_cd=', uyrw.paramcodes, '&fmt=rdb')
paramcodes.list = lapply(paramcodes.query, function(urlstring) read.csv(url(urlstring), comment.char='#', sep='\t')[-1,])
paramcodes.df = do.call(rbind, paramcodes.list)

# find all entries corresponding to streamflow
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']
idx.streamflow = usgs.sf$parm_cd == paramcode.streamflow

# find all entries corresponding to groundwater time series
paramcode.groundwater = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Height, gage']
idx.groundwater = usgs.sf$parm_cd == paramcode.groundwater

# 35 entries: 34 are time series of streamflow and one of groundwater
idx.streamflow.ts = idx.streamflow & idx.ts 
idx.groundwater.ts = idx.groundwater & idx.ts 
sum(idx.streamflow.ts)
```

```
## [1] 34
```

```r
sum(idx.groundwater.ts)
```

```
## [1] 1
```

```r
# make a copy of the time series data
usgs.ts.sf = usgs.sf[idx.streamflow.ts | idx.groundwater.ts, ]

# find the end-years and durations as integers
usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
usgs.ts.startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.startyear

# add a dummy column for plotting groundwater station(s)
usgs.ts.sf$constant = 'groundwater time series'

# find all intermittent groundwater data
idx.gw = usgs.sf$data_type_cd == 'gw'
sum(idx.gw)
```

```
## [1] 185
```


## visualization



```r
# add columns for duration and end-year of time series for precipitation
usgs.ts.sf$endyear[usgs.ts.sf$endyear == 2020] = NA

# plot streamflow sensor station locations as a png file
if(!file.exists(here(streamgage.metadata.df['img_streamgage', 'file'])))
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
                tm_shape(usgs.ts.sf[usgs.ts.sf$parm_cd == paramcode.groundwater,]) +
                  tm_dots(col='constant', palette='black', size=0.5, shape=6, title='') +
                tm_shape(usgs.ts.sf) +
                  tm_dots(size='duration',
                          col='endyear',
                          style='cont',
                          shape=16,
                          palette='magma',
                          alpha=0.7, 
                          contrast=0.7, 
                          title.size='duration (years)',
                          legend.size.is.portrait = TRUE,
                          shapes.legend.fill='grey',
                          perceptual = TRUE,
                          sizes.legend=c(5,25,50,75,125),
                          title='decomissioned', 
                          textNA='currently operational',
                          colorNA='red2') +
                tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
                tm_scale_bar(breaks=c(0, 20, 40), position=c('left', 'bottom'), text.size=0.5) +
                tm_layout(main.title='NWIS daily discharge records in the UYRW',
                          main.title.size=1,
                          main.title.position='center',
                          legend.title.size=0.7,
                          legend.text.size=0.5,
                          frame=FALSE,
                          legend.format=list(fun=function(x) formatC(x, digits=0, format='d')),
                          legend.outside=TRUE,
                          legend.outside.position='right',
                          title.snap.to.legend=FALSE)
  tmap.streamgage
  
  # render/write the plot
  tmap_save(tm=tmap.streamgage, here(streamgage.metadata.df['img_streamgage', 'file']), width=2000, height=2400, pointsize=16)
}
```

![weather stations in the UYRW](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/streamgage_sites.png)




