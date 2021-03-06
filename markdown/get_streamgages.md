get\_streamgages.R
================
Dean Koch
2021-03-05

**Mitacs UYRW project**

**get\_streamgages**: finds and retrieves data from USGS stream sensor
stations located in the UYRW

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
which creates some required directories and project config files, should
be run before this script.

## libraries

Start by sourcing two helper scripts (
[helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_get\_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))
```

[`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html)
is used to fetch the USGS data and
[`RColorBrewer`](https://colorbrewer2.org/) for colour palettes.

``` r
library(dataRetrieval)
library(RColorBrewer)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
streamgages.meta = my_metadata('get_streamgages', files.towrite, overwrite=TRUE)
```

    ## [1] "> writing metadata to: data/get_streamgages_metadata.csv"

``` r
print(streamgages.meta[, c('file', 'type')])
```

    ##                                              file          type
    ## USGS_sites_rdb         data/source/USGS_sites.rdb USGS rdb file
    ## USGS_sites           data/prepared/USGS_sites.rds   R sf object
    ## USGS_data             data/prepared/USGS_data.rds R list object
    ## USGS_paramcodes          data/USGS_paramcodes.csv           CSV
    ## img_streamgage      graphics/streamgage_sites.png   png graphic
    ## metadata        data/get_streamgages_metadata.csv           CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_streamgage_metadata.csv)
in the `/data` directory.

Load some of the data prepared earlier

``` r
# load CRS info list and watershed geometries from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))
```

The dates associated with daily mean USGS data are provided in the local
time zone. However, for non-daily data (which have an associated time of
day), time/date info is provided in UTM by default (eg. for the UYRW,
this is an offset of +7 hours). For convenience, we override this
default and request all data in US/Mountain time:

``` r
tz.uyrw = 'US/Mountain'
```

Some USGS records of discharge are given in imperial units, others in
metric. After downloading the raw data, we will convert all measurements
to a common unit (the one used in SWAT), defined here:

``` r
unit.discharge = 'm^3/s'
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
```

Load the RDB file, omitting stations not in UYRW, and convert it to a
`sf` object

``` r
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
```

This dataset is quite large, with over 8500 location-times indexed.
However these are mostly one-time measurements, and the number of
distinct locations is \<400. Time-series data at regular (daily)
intervals will be most useful when it comes to fitting a hydrology
model, and there are 88 such records.

``` r
# 
idx.ts = usgs.sf$data_type_cd %in% c('dv', 'iv', 'id')
sum(idx.ts)
```

    ## [1] 88

(see the [USGS water
Services](https://waterservices.usgs.gov/rest/Site-Service.html#outputDataTypeCd)
for more information about what these `data_type` codes mean).

Information on parameter codes (‘parm\_nm’) can also be downloaded using
the Water Services REST interface or from [the USGS
website](https://help.waterdata.usgs.gov/codes-and-parameters/parameters).

``` r
if(!file.exists(here(streamgages.meta['USGS_paramcodes', 'file'])))
{
  # query the meaning of the parameter column codes relevant to our area
  uyrw.paramcodes = unique(usgs.sf$parm_cd)
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
```

## Download streamflow time series data

This chunk uses the `DataRetrieval` package, which downloads from the
[USGS Water Services](https://waterservices.usgs.gov/). We will likely
come back to this web service for other data types since they also
offer: time series of sediment discharge, temperature, turbidity, snow
depth, and precipitation, among others. For now we download only the
streamflow discharge data.

Aim to run this chunk at night to minimize your impact on the
waterdata.usgs.gov (see their best practices FAQ
[here](https://help.waterdata.usgs.gov/faq/automated-retrievals)). Big
requests may lead to timeout errors if the server is busy, in which case
the code will fail with `discharge.dat` incomplete.

If this happens, don’t re-run the script. Wait an hour or so then resume
the script by adjusting the indexing vector of the `for` loop to start
at whichever value of `idx.site` you left off (instead of `1`). Earlier
sites should still be there in `discharge.dat` unless you wipe it after
the error.

``` r
if(!file.exists(here(streamgages.meta['USGS_data', 'file'])))
{
  # determine which parameter codes correspond to streamflow:
  discharge.df = paramcodes.df %>% filter(group=='Physical', grepl('[Dd]ischarge', parm_nm))
  
  # TODO: find temperature measurements 
  # temperature.df = paramcodes.df %>% filter(group=='Physical', grepl('[Tt]emperature, water', parm_nm))
  # usgs.temperature.sf = usgs.sf %>% filter(parm_cd %in% temperature.df$parameter_cd)
  # print(unique(usgs.temperature.sf$data_type_cd))
  # site.codes = unique(usgs.temperature.sf$site_no)
  # n.sites = length(site.codes)
  # n.obs = usgs.temperature.sf %>% 
  #   group_by(site_no) %>% 
  #   summarize(n=sum(as.integer(count_nu))) 
  # plot(uyrw.poly, main= 'USGS water temperature gages with > 1000 days in record')
  # plot(st_geometry(uyrw.flowline), add=TRUE, col='grey90')
  # plot(st_geometry(n.obs[n.obs$n < 1001,'n']), add=TRUE)
  # plot(n.obs[n.obs$n > 1000,'n'] %>% arrange(n), pch=16, cex=2, add=TRUE)
  # plot(st_geometry(n.obs[n.obs$n > 1000,'n']), cex=2, add=TRUE)
  
  # TODO: find conductivity measurements 
  # conductance.df = paramcodes.df %>% filter(group=='Physical', grepl('[Cc]onduct|[Cc]olor', parm_nm))
  # usgs.conductance.sf = usgs.sf %>% filter(parm_cd %in% conductance.df$parameter_cd)
  # print(unique(usgs.conductance.sf$data_type_cd))
  # site.codes = unique(usgs.conductance.sf$site_no)
  # n.sites = length(site.codes)
  # n.obs = usgs.conductance.sf %>% 
  #   group_by(site_no) %>% 
  #   summarize(n=sum(as.integer(count_nu))) 
  # plot(uyrw.poly, main= 'USGS water conductance gages with > 1000 days in record')
  # plot(st_geometry(uyrw.flowline), add=TRUE, col='grey90')
  # plot(st_geometry(n.obs[n.obs$n < 1001,'n']), add=TRUE)
  # plot(n.obs[n.obs$n > 1000,'n'] %>% arrange(n), pch=16, cex=2, add=TRUE)
  # plot(st_geometry(n.obs[n.obs$n > 1000,'n']), cex=2, add=TRUE)

  
  # note that '30208', '30209' appear to duplicate the times/dates in '00060', '00061', yet they
  # consistently underestimate the flow. This might be a unit conversion problem? 
  # Keep only the former:
  discharge.df = discharge.df %>% filter(parameter_cd %in% c('00060', '00061'))

  # find the subset of usgs records containing these data
  usgs.discharge.sf = usgs.sf %>% filter(parm_cd %in% discharge.df$parameter_cd)
  
  print(unique(usgs.discharge.sf$data_type_cd))
  # we have three data types in this region:
  # 'qw' (water-quality related)
  # 'dv' (daily)
  # 'uv' (regular but not dv, often sub-daily)

  # find all relevant site codes, and the total number of days in the record at each site
  site.codes = unique(usgs.discharge.sf$site_no)
  n.sites = length(site.codes)
  n.obs = usgs.discharge.sf %>% 
    group_by(site_no) %>% 
    summarize(n=sum(as.integer(count_nu))) %>% 
    pull(n)
  
  # download the data and store as nested list of dataframes - one top level entry per site
  discharge.dat = setNames(vector(mode='list', length=n.sites), site.codes)
  print(paste0('downloading ', sum(n.obs), ' days of records (across ', n.sites, ' sites)...'))
  for(idx.site in 60:n.sites)
  {
    # pull site-specific info
    site.no = site.codes[idx.site]
    site.sf = usgs.discharge.sf %>% filter(site_no==site.no)
    site.string = paste0(site.sf$station_nm[1], ' (#', site.no, ') ...')
    
    # create data list for this site and loop to download records by data type
    progress.string = paste0('[progress: ', idx.site, '/', n.sites, '] ')
    job.string = paste0('downloading ', n.obs[idx.site], ' streamflow record(s) from: ', site.string)
    print(paste0(progress.string, job.string))
    discharge.dat.entry = vector(mode='list', length=nrow(site.sf))
    for(idx.entry in 1:nrow(site.sf))
    {
      # determine data type (this determines which web service to use)
      site.dtype = site.sf$data_type_cd[idx.entry]
      site.pcode = site.sf$parm_cd[idx.entry]
      site.start = site.sf$begin_date[idx.entry]
      
      # in the code below I use the default endDate argument '', shorthand for maximum, whereas
      # startDate is specified based on the metadata in `usgs.sf`. This should download the complete
      # time series of each site/paramcode
      
      # water quality
      if(site.dtype == 'qw')
      {
        # download the raw data from water quality service (specifying MDT time zone)
        nwis.raw = renameNWISColumns(readNWISqw(siteNumbers=site.no, 
                                                parameterCd=site.pcode,
                                                expanded=FALSE,
                                                startDate=site.start,
                                                endDate='',
                                                tz=tz.uyrw))
        
        # much of the data frame has NA or irrelevant fields - tidy up
        nwis.varname = paste0('p', site.pcode)  
        names.tidier = c('date', 'time', 'flow')
        nwis.tidier = setNames(nwis.raw[, c('sample_dt', 'sample_tm', nwis.varname)], names.tidier)
      }
      
      # daily values
      if(site.dtype == 'dv')
      {
        # download the raw data (daily mean) from water quality service 
        nwis.raw = renameNWISColumns(readNWISdv(siteNumbers=site.no, 
                                                parameterCd=site.pcode,
                                                startDate=site.start,
                                                endDate='',))
        
        # tidy up
        names.tidier = c('date', 'flow')
        nwis.tidier = setNames(nwis.raw[, c('Date', 'Flow')], names.tidier)
      }
      
      # unit values
      if(site.dtype == 'uv')
      {
        # download the raw data (daily mean) from water quality service 
        nwis.raw = renameNWISColumns(readNWISuv(siteNumbers=site.no, 
                                                parameterCd=site.pcode,
                                                startDate=site.start,
                                                endDate='',
                                                tz=tz.uyrw))
        
        # tidy up, separating dates and (UTM) times, for consistency with 'qw' values 
        nwis.tidier = nwis.raw %>% 
          mutate(date=format(dateTime, '%Y-%m-%d')) %>%
          mutate(time=format(dateTime, '%H:%M:%S')) %>%
          select(date, time, Flow_Inst) %>%
          as.data.frame()
        
        # tidy up
        names.tidier = c('date', 'time', 'flow')
        nwis.tidier = setNames(nwis.tidier[, c('date', 'time', 'Flow_Inst')], names.tidier)
      }
      
      # convert units to standard metric (if necessary)
      units.src = attr(nwis.raw, 'variableInfo')[['unit']]
      nwis.tidier$flow = units::set_units(nwis.tidier$flow, units.src, mode='standard') %>%
        units::set_units(unit.discharge, mode='standard')

      # copy to list 
      discharge.dat.entry[[idx.entry]] = nwis.tidier
    }
    
    # add completed list entry to full storage list along with corresponding slice of sf object
    discharge.dat[[idx.site]] = list(sf=site.sf, dat=discharge.dat.entry)
  }
  
  # put data in a list along with point feature geometry and metadata, save to disk
  discharge.list = list(sf=usgs.discharge.sf, dat=discharge.dat)
  saveRDS(discharge.list, here(streamgages.meta['USGS_data', 'file']))
  
} else {
  
  # load from disk
  discharge.list = readRDS(here(streamgages.meta['USGS_data', 'file']))
  
} 
```

## visualization

plot information about the locations and the time periods associated
with each record

``` r
# add some fields to use in plotting
discharge.sf = discharge.list$sf %>% 
  mutate(startyear = as.integer(format(as.Date(begin_date), '%Y'))) %>%
  mutate(endyear = as.integer(format(as.Date(end_date), '%Y'))) %>%
  mutate(duration = 1 + as.integer(as.Date(end_date) - as.Date(begin_date))) %>%
  mutate(singleton = duration == 1) %>%
  mutate(singleton_text = 'sub-daily or one-time')

# load the plotting parameters used in get_weatherstations.R
tmap.pars = readRDS(here(my_metadata('get_weatherstations')['pars_tmap', 'file']))

# configuration for plotting the locations of time series data
tmap.pars$dots = tm_dots(size='duration',
                         col='endyear',
                         shape=16,
                         palette=brewer.pal('YlOrRd', n=9)[3:9],
                         scale=3,
                         style='cont',
                         alpha=0.7, 
                         contrast=0.7, 
                         title.size='duration (days)',
                         legend.size.is.portrait=TRUE,
                         shapes.legend.fill='grey20',
                         shapes.legend=1,
                         sizes.legend=c(1, 500, 5000, 25000),
                         perceptual=TRUE,
                         title='daily values until')
```

Plot the streamgage data, using shapes to indicate the variable type,
sizes to indicate duration of the time series, and colours to indicate
their end-dates:
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/streamgage_sites.png)

``` r
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
    tm_shape(discharge.sf[discharge.sf$singleton,]) +
      tm_dots(col='singleton_text', palette='purple', size=0.3, shape=17, title='') +
    tm_shape(discharge.sf) +
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


#my_markdown('get_streamgages', 'R/get_data')
```
