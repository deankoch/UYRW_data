#' ---
#' title: "get_streamgages.R"
#' author: "Dean Koch"
#' date: "August 22, 2020"
#' output: github_document
#' ---
#'
#' **MITACS UYRW project**
#' 
#' **get_streamgages**: finds stream sensor stations located in the UYRW
#' 
#' 
#' [get_basins.R](https://github.com/deankoch/URYW_data/blob/master/get_basins.knit.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
library(sf)
library(tmap)
library(here)
library(dataRetrieval)
library(waterData)
#library(nhdplusTools)
library('readtext')


#'
#' ## project data

#+ results='hide'
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
#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/URYW_data/blob/master/data/streamgage_metadata.csv)
#' in the `/data` directory.


#'
#' ## Find sites
#' 
#' The instructions at the [URL Generation Tool page](https://waterservices.usgs.gov/rest/Site-Test-Tool.html), 
#' show how R's `download.file` function can be used to download a copy of the site info list from the USGS Site
#' Web Service. Information about the output format (USGS RDB) is 
#' [available here](https://waterservices.usgs.gov/rest/Site-Service.html) and, in more detail,
#' [here](https://pubs.usgs.gov/of/2003/ofr03123/6.4rdb_format.pdf).
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

#' Load the RDB file, omit stations not in UYRW, and convert it to a `sf` object, then save to disk
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
  saveRDS(usgs.sf, here(streamgage.metadata.df['usgs.sfc', 'file']))

  
} else {
  
  # load from disk 
  usgs.sf = readRDS(here(streamgage.metadata.df['usgs', 'file']))
  
}

usgs.sf$begin_date

# find 115 time-series entries (see https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd) 
idx.ts = usgs.sf$data_type_cd %in% c('dv', 'iv', 'id')
sum(idx.ts)

# these correspond to 37 locations
uyrw.sitecodes = unique(usgs.sf[idx.ts,]$site_no)
length(uyrw.sitecodes)

# query the meaning of these parameter codes
uyrw.paramcodes = unique(usgs.sf[idx.ts,]$parm_cd)
paramcodes.url = 'https://help.waterdata.usgs.gov/code/parameter_cd_nm_query'
paramcodes.query = paste0(url.paramcodes, '?parm_nm_cd=', uyrw.paramcodes, '&fmt=rdb')
paramcodes.list = lapply(paramcodes.query, function(urlstring) read.csv(url(urlstring), comment.char='#', sep='\t')[-1,])
paramcodes.df = do.call(rbind, paramcodes.list)

# find all entries corresponding to streamflow
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']
idx.streamflow = usgs.sf$parm_cd == paramcode.streamflow

# 34 entries are time series of streamflow: make a copy of this dataset
idx.streamflow.ts = idx.streamflow & idx.ts
sum(idx.streamflow.ts)
usgs.streamflow.ts.sf = usgs.sf[idx.streamflow.ts, ]

# find the end-years and durations as integers
endyear.streamflow.ts = as.integer(sapply(strsplit(usgs.streamflow.ts.sf$end_date,'-'), function(xx) xx[1]))
startyear.streamflow.ts = as.integer(sapply(strsplit(usgs.streamflow.ts.sf$begin_date,'-'), function(xx) xx[1]))
duration.streamflow.ts = endyear.streamflow.ts - startyear.streamflow.ts

#'
#' ## visualization
#' 

# add columns for duration and end-year of time series for precipitation
usgs.streamflow.ts.sf$duration = duration.streamflow.ts
usgs.streamflow.ts.sf$endyear = endyear.streamflow.ts
usgs.streamflow.ts.sf$endyear[usgs.streamflow.ts.sf$endyear == 2020] = NA

# plot streamflow sensor station locations as a png file
if(!file.exists(here(streamgage.metadata.df['img_streamgage', 'file'])))
{
  # build the tmap plot object
  tmap.streamgage = tm_shape(uyrw.poly) +
                  tm_polygons(col='greenyellow', border.col='yellowgreen') +
                #tm_shape(uyrw.waterbody) +
                #  tm_polygons(col='yellowgreen', border.col='yellowgreen') +
                #tm_shape(uyrw.mainstem) +
                #  tm_lines(col='yellowgreen', lwd=2) +
                #tm_shape(uyrw.flowline) +
                #  tm_lines(col='yellowgreen') +
                #tm_shape(precip.sf[!is.na(precip.sf$snowtel_id),]) +
                #  tm_dots(col='constant', palette='black', size=0.5, shape=6, title='') +
                tm_shape(usgs.streamflow.ts.sf) +
                  tm_dots(size='duration',
                          col='endyear',
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
                tm_layout(main.title='GHCN (daily) streamflow records in the UYRW',
                          main.title.size=1,
                          main.title.position='center',
                          legend.title.size=0.7,
                          legend.text.size=0.5,
                          frame=FALSE,
                          legend.outside=TRUE,
                          legend.outside.position='right',
                          title.snap.to.legend=FALSE)
  
  #tmap.streamgage
  
  #formatC(endyear.streamflow.ts, digits=0, format='d')
  
  # render/write the plot
  tmap_save(tm=tmap.streamgage, here(streamgage.metadata.df['img_streamgage', 'file']), width=2000, height=2400, pointsize=16)
}

#' ![weather stations in the UYRW](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/streamgage_sites.png)


#+ include=FALSE
# Development code

#+ include=FALSE
# Convert to markdown by running the following line uncommented
# rmarkdown::render(here('get_streamgages.R'), run_pandoc=FALSE, clean=TRUE)
