#' ---
#' title: "get_weatherstations.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_weatherstations**: finds climatic sensor stations located in the UYRW and downloads their time series
#' 
#' The weather variables available through GHCN vary by station and time period. This script downloads everything,
#' however for the SWAT model we will mostly be interested in the variables 'tmin', 'tmax', 'prcp', 'snow', 'awnd'   
#' 

#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
#' The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package fetches
#' [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA; and the
#' [`rnoaa`](https://github.com/ropensci/rnoaa) package fetches
#' [GHCN Daily](https://www.ncdc.noaa.gov/ghcn-daily-description) data (see documentation 
#' [here](https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt) and 
#' [here](https://docs.ropensci.org/rnoaa/)).
#' We use them to build a map of climatic sensor stations in the UYRW, and to query historical data
#' for model training. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(snotelr)
library(rnoaa)

#'
#' ## project data
#+ echo=FALSE
files.towrite = list(
  
  # metadata table downloaded from SNOTEL website
  c(name='csv_snotel',
    file=file.path(src.subdir, 'snotel_sites.csv'), 
    type='CSV', 
    description='metadata list for SNOTEL sites (unchanged)'), 
  
  # SNOTEL metadata table as an sfc object
  c(name='snotel',
    file=file.path(out.subdir, 'snotel_sites.rds'), 
    type='R sf object', 
    description='sfc object with SNOTEL sensor locations in UYRW'),
  
  # GHCND metadata table downloaded from NOAA
  c(name='csv_ghcnd',
    file=file.path(src.subdir, 'ghcnd_sites.csv'), 
    type='CSV', 
    description='metadata list for GHCND sites (unchanged)'),
  
  # GHCND metadata table as an sfc object
  c(name='ghcnd',
    file=file.path(out.subdir, 'ghcnd_sites.rds'),
    type='R sf object',
    description='sfc object with GHCN Daily sensor locations in UYRW'),
  
  # GHCND data tables 
  c(name='ghcnd_data',
    file=file.path(out.subdir, 'ghcnd_data.rds'),
    type='R list object',
    description='list of data frames with GHCN Daily data in UYRW'),
  
  # aesthetic parameters for plotting
  c(name='pars_tmap',
    file=file.path(data.dir, 'tmap_get_weatherstations.rds'), 
    type='R list object', 
    description='parameters for writing png plots using tmap and tm_save'),
  
  # graphic showing SNOTEL and GHCND site locations on the UYRW
  c(name='img_weatherstation',
    file=file.path(graphics.dir, 'weatherstation_sites.png'),
    type='png graphic', 
    description='image of SNOTEL and GHCND site locations in the UYRW')
  
)

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
weatherstations.meta = my_metadata('get_weatherstations', files.towrite, overwrite=TRUE)
print(weatherstations.meta[, c('file', 'type')])

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_weatherstations_metadata.csv)
#' in the `/data` directory.
#' 
#' Load some of the data prepared earlier 
# load metadata csv, CRS info list and watershed geometries from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.poly.padded = readRDS(here(basins.meta['boundary_padded', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))


#'
#' ## Find SNOTEL sites
#' 
#' the `snotel_info` function in `snotelr` downloads a CSV containing site IDs and coordinates
if(!file.exists(here(weatherstations.meta['csv_snotel', 'file'])))
{
  # download the metadata csv to the folder specified in `path`. This writes the file "snotel_metadata.csv"
  snotel_info(path=here(src.subdir))
  
  # rename the csv to avoid confusion with identically-named file in the parent folder (my list of project files)
  file.rename(from=here(src.subdir, 'snotel_metadata.csv'), to=here(weatherstations.meta['csv_snotel', 'file']))
  
}

#' Load this CSV, omit stations not in UYRW, and convert it to a `sf` object, then save to disk
if(!file.exists(here(weatherstations.meta['snotel', 'file'])))
{
   # load the site info table into a data frame and extract coordinates
  snotel.df = read.csv(here(weatherstations.meta['csv_snotel', 'file']), header=TRUE)
  sites.coords.matrix = as.matrix(snotel.df[, c('longitude', 'latitude')])
  
  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  snotel.sfc = st_sfc(lapply(1:nrow(snotel.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  snotel.sf = st_sf(cbind(snotel.df, snotel.sfc))
  
  # transform to UTM and clip to extended UYRW area (30 stations identified)
  snotel.sf = st_transform(snotel.sf, crs=crs.list$epsg)
  snotel.sf = st_intersection(snotel.sf, uyrw.padded.poly)
  
  # save to disk
  saveRDS(snotel.sf, here(weatherstations.meta['snotel', 'file']))
  
} else {
  
  # load from disk 
  snotel.sf = readRDS(here(weatherstations.meta['snotel', 'file']))
  
}


#'
#' ## Find NOAA Global Historical Climatology Network (GHCN) Daily sites
#' the `ghcnd_stations` function in `rnoaa` downloads a table of site IDs and coordinates
if(!file.exists(here(weatherstations.meta['csv_ghcnd', 'file'])))
{
  # download the metadata table and load into R (slow, 1-2min)
  ghcnd.df = ghcnd_stations()

  # save a copy as csv in the /data/source folder
  write.csv(ghcnd.df, here(weatherstations.meta['csv_ghcnd', 'file']))

}

#' Load this CSV. It indexed over 100,000 stations worldwide! This chunk transforms the coordinates to UTM,
#' omits stations not in UYRW area (leaving 138), converts the result to an `sf` object with one feature
#' per station, and saves the result to disk.
#' 
#' Note that when clipping to the URYW area, we use a polygon that is padded by several kilometers
#' from the outer boundary of the watershed. This allows us to fetch nearby but out-of-watershed station
#' data to better inform the SWAT+ weather generator (which uses spatial interpolation).
if(!file.exists(here(weatherstations.meta['ghcnd', 'file'])))
{

  # load the site info table into a data frame
  ghcnd.df = read.csv(here(weatherstations.meta['csv_ghcnd', 'file']), header=TRUE)
  
  # find all unique station IDs, extracting coordinates from the first entry in the table for each station 
  ghcnd.IDs = unique(ghcnd.df$id)
  idx.duplicateID = duplicated(ghcnd.df$id)
  sum(!idx.duplicateID) 
  ghcnd.coords.matrix = as.matrix(ghcnd.df[!idx.duplicateID, c('longitude', 'latitude')])
  
  # create sfc object from points, appending only the id field
  ghcnd.sfc = st_sfc(lapply(1:sum(!idx.duplicateID), function(xx) st_point(ghcnd.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  ghcnd.sf = st_sf(cbind(data.frame(id=ghcnd.df[!idx.duplicateID, 'id']), ghcnd.sfc))
  
  # transform to our reference system
  ghcnd.sf = st_transform(ghcnd.sf, crs=crs.list$epsg)
  
  # some (polar area) points are undefined in the UTM transformation, remove them
  ghcnd.sf = ghcnd.sf[!st_is_empty(ghcnd.sf),]
  
  # clip to UYRW watershed region (95 stations) then join with the other attributes that are constant across "id"
  ghcnd.sf = st_intersection(ghcnd.sf, uyrw.padded.poly)
  idx.ghcnd.uyrw = which(!idx.duplicateID)[ghcnd.df$id[!idx.duplicateID] %in% ghcnd.sf$id]
  ghcnd.sf = cbind(ghcnd.sf, ghcnd.df[idx.ghcnd.uyrw, c('longitude', 'latitude', 'elevation', 'name')])
  nrow(ghcnd.sf)
  
  # "element" attribute varies by site "id". There are 45 possibilities in the UYRW area
  ghcnd.elem = unique(ghcnd.df[ghcnd.df$id %in% ghcnd.sf$id, 'element'])
  length(ghcnd.elem)
  
  # double sapply call to this function builds a table indicating which elements are available in which year
  ghcnd.elem.df = t(sapply(ghcnd.sf$id, function(idval) sapply(ghcnd.elem, function(elemval) my_ghcnd_reshape(idval, elemval))))
  
  # join this data to the sfc object (reordering to emphasize most populated fields)
  ghcnd.sf = cbind(ghcnd.sf, ghcnd.elem.df[,order(apply(ghcnd.elem.df, 2, function(xx) sum(!is.na(xx))), decreasing=TRUE)])
  
  # There is some overlap with SNOTEL. Identify the GHCND sites that are indexed by SNOTEL
  ghcnd.sf$snowtel_id = apply(st_distance(ghcnd.sf, snotel.sf), 1, function(xx) ifelse(!any(xx<1), NA, snotel.sf$site_id[xx<1]))
  
  # save to disk
  saveRDS(ghcnd.sf, here(weatherstations.meta['ghcnd', 'file']))
  
  
} else {
  
  # load from disk
  ghcnd.sf = readRDS(here(weatherstations.meta['ghcnd', 'file']))
  
} 

#' 
#' ## download station data
#' see
#' [here](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html)
#' for documentation on the dataset and its variable names.
#' 
if(!file.exists(here(weatherstations.meta['ghcnd_data', 'file'])))
{
  # this call may take some time to download all station data listed in `ghcnd.sf`
  ghcnd.list = lapply(setNames(nm=ghcnd.sf$id), meteo_pull_monitors)
  
  # save to disk
  saveRDS(ghcnd.list, here(weatherstations.meta['ghcnd_data', 'file']))
  
} else {
  
  # load from disk
  ghcnd.list = readRDS(here(weatherstations.meta['ghcnd_data', 'file']))
  
} 

#'
#' ## download soil and water hub dataset
#' 
#' This chunk in development. Downloads and imports 1900-2013 time series of weather
# come back to this after gridded weather is tested



#'
#' ## visualization
#' 

#' Set up the aesthetics to use for these types of plots
if(!file.exists(here(weatherstations.meta['pars_tmap', 'file'])))
{
  # load the plotting parameters used in get_basins.R
  tmap.pars = readRDS(here(basins.meta['pars_tmap', 'file']))
  
  # adjust them to suit these wider plots (with legends)
  tmap.pars$png['w'] = 1800 
  
  # configuration for plotting the locations of time series data
  tmap.pars$dots = tm_dots(size='duration',
                           col='endyear',
                           shape=16,
                           palette='magma',
                           style='cont',
                           alpha=0.7, 
                           contrast=0.7, 
                           title.size='duration (years)',
                           legend.size.is.portrait=TRUE,
                           shapes.legend.fill='grey20',
                           shapes.legend=1,
                           perceptual=TRUE,
                           sizes.legend=c(5,25,50,75,125),
                           title='decomissioned', 
                           textNA='currently operational',
                           colorNA='red2')
  
  # parameters related to the layout for building legends
  tmap.pars$layout = tmap.pars$layout + 
    tm_layout(legend.format=list(fun=function(x) formatC(x, digits=0, format='d')),
              legend.outside=TRUE,
              legend.outside.position='right',
              legend.text.size=tmap.pars$label.txt.size)
  
  # save to disk
  saveRDS(tmap.pars, here(weatherstations.meta['pars_tmap', 'file']))
  
} else {
  
  # load from disk
  tmap.pars = readRDS(here(weatherstations.meta['pars_tmap', 'file']))
  
} 


#' Plot precipitation sensor station locations
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/weatherstation_sites.png)
if(!file.exists(here(weatherstations.meta['img_weatherstation', 'file'])))
{
  # make a copy of the points datasets, omitting stations with only temperature data
  idx.onlytemp = is.na(ghcnd.sf$PRCP) & is.na(ghcnd.sf$SNWD) & is.na(ghcnd.sf$SNOW)
  precip.sf = ghcnd.sf[!idx.onlytemp,]
  
  # add columns for duration and end-year of time series for precipitation
  years.PRCP = strsplit(precip.sf$PRCP,'-')
  endyear.PRCP = sapply(years.PRCP, function(xx) as.numeric(xx[2]))
  duration.PRCP = endyear.PRCP - sapply(years.PRCP, function(xx) as.numeric(xx[1]))
  precip.sf$duration = duration.PRCP
  precip.sf$endyear = endyear.PRCP
  precip.sf$endyear[precip.sf$endyear == 2020] = NA
  
  # add a dummy column (containing a plot label) for indicating SNOTEL stations
  precip.sf$constant = 'SNOTEL station'
  
  # build the tmap plot object
  tmap.precip = tm_shape(uyrw.padded.poly) +
                  tm_polygons(col='gray', border.col=NA) +
                tm_shape(uyrw.poly) +
                  tm_polygons(col='greenyellow', border.col='yellowgreen') +
                tm_shape(uyrw.waterbody) +
                  tm_polygons(col='yellowgreen', border.col='yellowgreen') +
                tm_shape(uyrw.mainstem) +
                  tm_lines(col='yellowgreen', lwd=2) +
                tm_shape(uyrw.flowline) +
                  tm_lines(col='yellowgreen') +
                tm_shape(precip.sf[!is.na(precip.sf$snowtel_id),]) +
                  tm_dots(col='constant', palette='black', size=0.5, shape=6, title='') +
                tm_shape(precip.sf) +
                  tmap.pars$dots + 
                tmap.pars$layout +
                tm_layout(main.title='GHCN (daily) precipitation records in the UYRW')
  
  # render/write the plot
  tmap_save(tm=tmap.precip, 
            here(weatherstations.meta['img_weatherstation', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}

#+ include=FALSE
# Development code

# Data downloads look like this:
# xx = meteo_pull_monitors('US1MTPK0001')
# yy = snotel_download(site_id = 363, internal=TRUE)

#my_markdown('get_weatherstations')