#' ---
#' title: "get_streamgages.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_streamgages**: finds USGS stream sensor stations located in the UYRW
#' 
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
#' [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html) is used to fetch the USGS data. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(dataRetrieval)
#?library(waterData)


#'
#' ## project data
#+ echo=FALSE
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

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
streamgages.meta = my_metadata('get_streamgages', files.towrite, overwrite=TRUE)
print(streamgages.meta[, c('file', 'type')])


#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_streamgage_metadata.csv)
#' in the `/data` directory.
#' 
#' Load some of the data prepared earlier 
# load CRS info list and watershed geometries from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))

#'
#' ## Find sites
#' 
#' The instructions at the [URL Generation Tool page](https://waterservices.usgs.gov/rest/Site-Test-Tool.html), 
#' show how to constrct a URL that downloads a copy of the site info list from the USGS Site Web Service.
#' Information about the output format (USGS RDB) is 
#' [available here](https://waterservices.usgs.gov/rest/Site-Service.html) and, in more detail,
#' [here](https://pubs.usgs.gov/of/2003/ofr03123/6.4rdb_format.pdf).
if(!file.exists(here(streamgages.meta['USGS_sites_rdb', 'file'])))
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
  download.file(paste0(urlargs.domain, paste(urlargs.list, collapse='&')), here(streamgages.meta['USGS_sites_rdb', 'file']))
  
}

#' Load the RDB file, omitting stations not in UYRW, and convert it to a `sf` object
if(!file.exists(here(streamgages.meta['USGS_sites', 'file'])))
{
  # load the RDB file as a tab-delimited data frame, omit first row (which indicates string lengths) 
  usgs.df = read.csv(here(streamgages.meta['USGS_sites_rdb', 'file']), comment.char='#', sep='\t')
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
  saveRDS(usgs.sf, here(streamgages.meta['USGS_sites', 'file']))

} else {
  
  # load from disk 
  usgs.sf = readRDS(here(streamgages.meta['USGS_sites', 'file']))
  
}

#' This dataset is quite large, with over 8500 location-times indexed. Mostly these are one-time measurements.
#' Time-series data at regular (daily) intervals will be more useful, when it comes to fitting a hydrology model.
#' Parse the USGS data to find 88 time-series entries: 
# 
idx.ts = usgs.sf$data_type_cd %in% c('dv', 'iv', 'id')
sum(idx.ts)

#' (see the [USGS water Services](https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd) for 
#' more information about what these `data_type` codes mean). 
#' 

#' Information on parameter codes can also be downloaded using the Water Services REST interface
if(!file.exists(here(streamgages.meta['USGS_paramcodes', 'file'])))
{
  # query the meaning of the parameter column codes corresponding to time series in our area
  uyrw.paramcodes = unique(usgs.sf[idx.ts,]$parm_cd)
  paramcodes.url = 'https://help.waterdata.usgs.gov/code/parameter_cd_nm_query'
  paramcodes.query = paste0(paramcodes.url, '?parm_nm_cd=', uyrw.paramcodes, '&fmt=rdb')
  paramcodes.list = lapply(paramcodes.query, function(urlstring) read.csv(url(urlstring), comment.char='#', sep='\t')[-1,])
  paramcodes.df = do.call(rbind, paramcodes.list)
  
  # save to disk
  write.csv(paramcodes.df, here(streamgages.meta['USGS_paramcodes', 'file']), row.names=FALSE)
  
} else {
  
  # load from disk 
  paramcodes.df = read.csv(here(streamgages.meta['USGS_paramcodes', 'file']), colClasses='character')
}

#'
#' ## Download streamflow time series
#' 


#'
#' ## visualization
#' 

#' Some data-preparation work will allow us to plot information about both the locations and the time
#' periods associated with each station dataset
#' 
# make a copy of the time-series data
usgs.ts.sf = usgs.sf[idx.ts,]

# these correspond to 21 unique locations
uyrw.sitecodes = unique(usgs.ts.sf$site_no)
length(uyrw.sitecodes)

# find all entries corresponding to streamflow
paramcode.streamflow = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Stream flow, mean. daily']
idx.streamflow = usgs.ts.sf$parm_cd == paramcode.streamflow

# TO DO : find temperature time series

# find all entries corresponding to turbidity and suspended sediment
paramcode.turbidity = paramcodes.df$parameter_cd[paramcodes.df$SRSName == 'Turbidity']
paramcode.sediment = paramcodes.df$parameter_cd[paramcodes.df$SRSName %in% c('Suspended sediment concentration (SSC)', 'Suspended sediment discharge')]
idx.turbidity = usgs.ts.sf$parm_cd == paramcode.turbidity
idx.sediment = usgs.ts.sf$parm_cd %in%  paramcode.sediment

# 32 entries: 20 are of streamflow, 6 are of turbidity, 6 are of suspended sediment
idx.all = idx.streamflow | idx.turbidity | idx.sediment
sum(idx.all)
sum(idx.streamflow)
sum(idx.turbidity)
sum(idx.sediment)

# find the end-years and durations as integers
usgs.ts.sf$endyear = as.integer(sapply(strsplit(usgs.ts.sf$end_date,'-'), function(xx) xx[1]))
usgs.ts.startyear = as.integer(sapply(strsplit(usgs.ts.sf$begin_date,'-'), function(xx) xx[1]))
usgs.ts.sf$duration = usgs.ts.sf$endyear - usgs.ts.startyear

#' Set up aesthetic parameters
#' 

# add dummy columns for indicating the variable recorded
usgs.ts.sf$plotlabel_sf = 'streamflow'
usgs.ts.sf$plotlabel_tb = 'turbidity'
usgs.ts.sf$plotlabel_ss = 'suspended sediment'

# add columns for duration and end-year of time series for precipitation
usgs.ts.sf$endyear[usgs.ts.sf$endyear == 2020] = NA

# load the plotting parameters used in get_weatherstations.R
tmap.pars = readRDS(here(my_metadata('get_weatherstations')['pars_tmap', 'file']))

# adjust with a better highlight colour for the blue background
tmap.pars$dots$tm_symbols$colorNA = 'orange'

#' Plot the streamgage data, using shapes to indicate the variable type, sizes to indicate
#' duration of the time series, and colours to indicate their end-dates:
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/streamgage_sites.png)
if(!file.exists(here(streamgages.meta['img_streamgage', 'file'])))
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
            here(streamgages.meta['img_streamgage', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}


#' example data  
#' 

# grab the mill creek data
idx.millcreek =grepl('mill creek', usgs.ts.sf$station_nm, ignore.case=TRUE)
print(usgs.ts.sf[idx.millcreek,])

# inputs to downloader function
siteNumber = usgs.ts.sf$site_no[idx.millcreek]
parameterCd = usgs.ts.sf$parm_cd[idx.millcreek]
startDate = usgs.ts.sf$begin_date[idx.millcreek]
endDate = usgs.ts.sf$end_date[idx.millcreek]

# see https://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html&inline=true for stats codes
statCd = paste0('0000', 1:9)
statCd = paste0('0000', 3)

# download and parse data
flow.millcreek = renameNWISColumns(readNWISdv(siteNumber, parameterCd, startDate, endDate, statCd))
flow.attr = attr(flow.millcreek, 'variableInfo')


#+ include=FALSE
# Development code
#my_markdown('get_streamgages')