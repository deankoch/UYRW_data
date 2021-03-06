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
#' however for the SWAT model we will mostly be interested in the variables 'tmin', 'tmax', 'prcp', 'snow', 'awnd'.
#' TODO: remove the SNOTEL sections as we fetch that data in a different script now.   
#' 

#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
#' Start by sourcing two helper scripts
#' ([helper_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md) and
#' [helper_get_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
#' which set up required libraries and directories and define some utility functions.
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))

#' The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package fetches
#' [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA; and the
#' [`rnoaa`](https://github.com/ropensci/rnoaa) package fetches
#' [GHCN Daily](https://www.ncdc.noaa.gov/ghcn-daily-description) data (see documentation 
#' [here](https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt) and 
#' [here](https://docs.ropensci.org/rnoaa/)).
#' We use them to build a map of climatic sensor stations in the UYRW, and to query historical data
#' for model training.
library(snotelr)
library(rnoaa)

#'
#' ## project data
#+ echo=FALSE
{
files.towrite = list(
  
  # metadata table downloaded from SNOTEL website
  c(name='snotel_sites',
    file=file.path(src.subdir, 'snotel_sites.csv'), 
    type='CSV', 
    description='metadata list for SNOTEL sites (unchanged)'), 
  
  # SNOTEL metadata table as an sfc object
  c(name='snotel_sf',
    file=file.path(out.subdir, 'snotel_sites.rds'), 
    type='R sf object', 
    description='sfc object with SNOTEL sensor locations in UYRW'),
  
  # directory to save snotel downloads  
  c(name='snotel_src',
    file=file.path(src.subdir, 'snotel'),
    type='directory',
    description='directory to use for storage of SNOTEL CSV downloads from NOAA'),
  
  # snotel processed data tables 
  c(name='snotel_data',
    file=file.path(out.subdir, 'snotel_data.rds'),
    type='R list object',
    description='list of data frames with SNOTEL time series in UYRW'),
  
  # GHCND metadata table downloaded from NOAA
  c(name='ghcnd_sites',
    file=file.path(src.subdir, 'ghcnd_sites.csv'), 
    type='CSV', 
    description='metadata list for GHCND sites (unchanged)'),
  
  # GHCND metadata table as an sfc object
  c(name='ghcnd_sf',
    file=file.path(out.subdir, 'ghcnd_sites.rds'),
    type='R sf object',
    description='sfc object with GHCN Daily sensor locations in UYRW'),
  
  # GHCND raw data tables 
  c(name='ghcnd_src',
    file=file.path(src.subdir, 'ghcnd'),
    type='directory',
    description='directory to use for storage of GHCND CSV downloads from NOAA'),
  
  # GHCND processed data tables 
  c(name='ghcnd_data',
    file=file.path(out.subdir, 'ghcnd_data.rds'),
    type='R list object',
    description='list of data frames with GHCN Daily data in UYRW'),
  
  # aesthetic parameters for plotting
  c(name='pars_tmap',
    file=file.path(data.dir, 'tmap_get_weatherstations.rds'), 
    type='R list object', 
    description='parameters for writing png plots using tmap and tm_save'),
  
  # graphic showing GHCND site locations on the UYRW
  c(name='img_weatherstation',
    file=file.path(graphics.dir, 'weatherstation_sites.png'),
    type='png graphic', 
    description='image of GHCND site locations in the UYRW'),
  
  # graphic showing SNOTEL site locations on the UYRW
  c(name='img_weatherstation_snotel',
    file=file.path(graphics.dir, 'weatherstation_sites_snotel.png'),
    type='png graphic', 
    description='image of SNOTEL site locations in the UYRW')
  
)
}

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
if(!file.exists(here(weatherstations.meta['snotel_sites', 'file'])))
{
  # download the metadata csv to the folder specified in `path`. This writes the file "snotel_metadata.csv"
  snotel_info(path=here(src.subdir))

  # rename the csv to avoid confusion with identically-named file in the parent folder (my list of project files)
  file.rename(from=here(src.subdir, 'snotel_metadata.csv'), to=here(weatherstations.meta['snotel_sites', 'file']))
}

#' Load this CSV, omit stations not in UYRW, and convert it to a `sf` object, then save to disk
if(!file.exists(here(weatherstations.meta['snotel_sf', 'file'])))
{
  # load the site info table into a data frame and extract coordinates
  snotel.df = read.csv(here(weatherstations.meta['snotel_sites', 'file']), header=TRUE)
  sites.coords.matrix = as.matrix(snotel.df[, c('longitude', 'latitude')])
  
  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  snotel.sfc = st_sfc(lapply(1:nrow(snotel.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  snotel.sf = st_sf(cbind(snotel.df, snotel.sfc))
  
  # transform to UTM and clip to extended UYRW area (30 stations identified)
  snotel.sf = st_transform(snotel.sf, crs=crs.list$epsg)
  snotel.sf = st_intersection(snotel.sf, uyrw.padded.poly)
  
  # save to disk
  saveRDS(snotel.sf, here(weatherstations.meta['snotel_sf', 'file']))
  
} else {
  
  # load from disk 
  snotel.sf = readRDS(here(weatherstations.meta['snotel_sf', 'file']))
  
}


#' 
#' ## download SNOTEL data
#' 
#' The `snotelr` package uses the USDA NRCS [Report Generator](https://wcc.sc.egov.usda.gov/reportGenerator/)
#' to fetch SNOTEL network data.
#' 
#' SNOTEL contains data on snow, as well as precipitation and temperature. Some of these time series
#' appear to be cross-listed on GHCND, however in many cases we have quite different measurements despite
#' matching locations and timestamps (perhaps due to different instruments, or different data-cleaning
#' methodologies)
#' 
#' Note: If running this on windows, you may need to click through a firewall warning to allow
#' `snotelr` to proceed

# create the storage directory
snotel.src.dir = here(weatherstations.meta['snotel_src', 'file'])
my_dir(snotel.src.dir)

# build list of files to write
snotel.id = snotel.sf$site_id
snotel.fn = file.path(snotel.src.dir, paste0('snotel_', snotel.id, '.csv'))

# proceed only with files not already downloaded
if(!all(file.exists(snotel.fn)))
{
  # build list of missing files
  idx.todl = !file.exists(snotel.fn)
  
  # download all missing station data to disk in a loop
  pb = txtProgressBar(max=sum(idx.todl), style=3)
  for(idx.dl in 1:sum(idx.todl))
  {
    idx.fn = which(idx.todl)[idx.dl]
    snotel_download(site_id=snotel.id[idx.fn], path=snotel.src.dir, internal=FALSE)
    setTxtProgressBar(pb, idx.dl)
  }
  close(pb)
  
} 

#' 
#' ## process SNOTEL data
#' 
#' These data should already be in metric units (degC, mm), and we label them as such in the chunk below.
#' Afterwards, function `snotel_phenology` can be applied to the (data frame) elements of `snotel.list` to
#' compute dates of first/last snowmelt, etc
#' 
snotel.path = here(weatherstations.meta['snotel_data', 'file'])
if(!file.exists(snotel.path))
{
  # load all station data into memory
  snotel.all = setNames(lapply(snotel.fn, read.csv), basename(snotel.fn))
  
  # remove the first few fields, which are identical across all rows. This info is already in `snotel.sf`
  field.rm = apply(snotel.all[[1]], 2, function(colvals) all(duplicated(colvals)[-1])) 
  snotel.list = lapply(snotel.all, function(df) df[,!field.rm])
  
  # coerce dates column to Date type, add units to each measurement field
  for(idx.df in 1:length(snotel.list))
  {
    # copy the data frame
    snotel.df = snotel.list[[idx.df]]
    
    # make the unit/type changes
    snotel.df$date = as.Date(snotel.df$date)
    snotel.df$snow_water_equivalent = set_units(snotel.df$snow_water_equivalent, 'mm')
    snotel.df$precipitation_cumulative = set_units(snotel.df$precipitation_cumulative, 'mm')
    snotel.df$temperature_max = set_units(snotel.df$temperature_max, '°C')
    snotel.df$temperature_min = set_units(snotel.df$temperature_min, '°C')
    snotel.df$temperature_mean = set_units(snotel.df$temperature_mean, '°C')
    snotel.df$precipitation = set_units(snotel.df$precipitation, 'mm')
    
    # overwrite the list entry with updated values
    snotel.list[[idx.df]] = snotel.df
  }
  
  # save to disk
  saveRDS(snotel.list, here(weatherstations.meta['snotel_data', 'file']))
  
} else {
  
  # load from disk
  snotel.list = readRDS(here(weatherstations.meta['snotel_data', 'file']))
  
} 

#'
#' ## Find NOAA Global Historical Climatology Network (GHCN) Daily sites
#' the `ghcnd_stations` function in `rnoaa` downloads a table of site IDs and coordinates
if(!file.exists(here(weatherstations.meta['ghcnd_sites', 'file'])))
{
  # download the metadata table and load into R (slow, 1-2min)
  ghcnd.df = ghcnd_stations()
  
  # save a copy as csv in the /data/source folder
  write.csv(ghcnd.df, here(weatherstations.meta['ghcnd_sites', 'file']))
  
}

#' Load this CSV. It indexed over 100,000 stations worldwide! This chunk transforms the coordinates to UTM,
#' omits stations not in UYRW area (leaving 138), converts the result to an `sf` object with one feature
#' per station, and saves the result to disk.
#' 
#' Note that when clipping to the URYW area, we use a polygon that is padded by several kilometers
#' from the outer boundary of the watershed. This allows us to fetch nearby but out-of-watershed station
#' data to better inform the SWAT+ weather generator (which uses spatial interpolation).
if(!file.exists(here(weatherstations.meta['ghcnd_sf', 'file'])))
{
  
  # load the site info table into a data frame
  ghcnd.df = read.csv(here(weatherstations.meta['ghcnd_sites', 'file']), header=TRUE)
  
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
  saveRDS(ghcnd.sf, here(weatherstations.meta['ghcnd_sf', 'file']))
  
  
} else {
  
  # load from disk
  ghcnd.sf = readRDS(here(weatherstations.meta['ghcnd_sf', 'file']))
  
} 

#' 
#' ## download GHCN station data
#' See
#' [here](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html)
#' for documentation on the dataset and its variable names. This chunk downloads each station dataset
#' into memory then saves a copy to disk in the source data directory (as CSV spreadsheet). 

# create the storage directory if needed
ghcnd.dir = here(weatherstations.meta['ghcnd_src', 'file'])
my_dir(ghcnd.dir)

# make a list of files to write
ghcnd.fn = paste0(ghcnd.sf$id, '.csv')
ghcnd.path = file.path(ghcnd.dir, ghcnd.fn) 
idx.missing = !file.exists(ghcnd.path)

# download only those files missing from the storage directory
if(any(idx.missing))
{
  # loop over stations not found in storage
  for(idx.station in which(idx.missing))
  {
    # download from NOAA then write to CSV
    ghcnd.df = meteo_pull_monitors(ghcnd.sf$id[idx.station])
    write.csv(ghcnd.df, ghcnd.path[idx.station], row.names=FALSE)
  }
  
}

#' 
#' ## process GHCN station data
#' documentation on units can be found [here](https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
#' 
ghcnd.out.path = here(weatherstations.meta['ghcnd_data', 'file'])
if(!file.exists(ghcnd.out.path))
{
  # load all station data into memory
  ghcnd.list = setNames(lapply(ghcnd.path, read.csv), basename(ghcnd.path))
  
  # units for some important volume fields (see link above for info on others)
  ghcnd.units.mm = c('prcp', # all precipitation (10ths of mm)
                     'snow', # snowfall (mm)
                     'snwd', # snow depth (mm)
                     'wesf', # water equivalent of snowfall (10ths of mm)
                     'wesd', # water equivalent of snow on ground (10ths of mm)
                     'mdpr', # multiday precipitation total (10ths of mm, see DAPR)
                     'mdsf') # multiday snowfall total (10ths of mm, see DASF) 
  
  # units for temperature fields
  ghcnd.units.degC = c('tmax', # temperature maximum (10ths of degC)
                       'tobs', # observed temperature (10ths of degC)
                       'tmin') # temperature minimum (10ths of degC)
  
  # units for day counts
  ghcnd.units.days = c('dapr', # number of days counted in MDPR
                       'dasf') # number of days counted in MDSF
  
  # all the fields to copy, in addition to 'date'
  ghcnd.colnames = c('date', ghcnd.units.mm, ghcnd.units.degC, ghcnd.units.days)
  
  # coerce dates column to Date type, add units for important measurement fields
  for(idx.df in 1:length(ghcnd.list))
  {
    # copy the data frame, keeping only the relevant fields above 
    ghcnd.df = ghcnd.list[[idx.df]]
    ghcnd.df = ghcnd.df[, colnames(ghcnd.df) %in% c(ghcnd.colnames)]
    
    # drop any empty rows (no non-NA values except date)
    ghcnd.df = ghcnd.df[!apply(is.na(ghcnd.df[, colnames(ghcnd.df)!='date']), 1, all), ]
    
    # make the date type change
    ghcnd.df$date = as.Date(ghcnd.df$date)
    
    # add units for volume measurements (most are integer, in units of 10ths of a mm)
    idx.mm = setNames(colnames(ghcnd.df) %in% ghcnd.units.mm, colnames(ghcnd.df))
    for(idx.vn in which(idx.mm)) { ghcnd.df[,idx.vn] = set_units(ghcnd.df[,idx.vn], 'mm')/10 }
    
    # change back the two variables not given in 10ths
    idx.whole = setNames(colnames(ghcnd.df) %in% c('snow', 'snwd'), colnames(ghcnd.df))
    for(idx.vn in which(idx.whole)) { ghcnd.df[,idx.vn] = ghcnd.df[,idx.vn]*10 }
    
    # add units for temperatures (supplied as integer, in units of 10ths of a degree)
    idx.degC = setNames(colnames(ghcnd.df) %in% ghcnd.units.degC, colnames(ghcnd.df))
    for(idx.vn in which(idx.degC)) { ghcnd.df[,idx.vn] = set_units(ghcnd.df[,idx.vn], '°C')/10 }
    
    # add units for day counts
    idx.days = setNames(colnames(ghcnd.df) %in% ghcnd.units.days, colnames(ghcnd.df))
    for(idx.vn in which(idx.days)) { ghcnd.df[,idx.vn] = set_units(ghcnd.df[,idx.vn], 'days') }
    
    # overwrite the list entry with updated values
    ghcnd.list[[idx.df]] = ghcnd.df
  }
  
  # save to disk
  saveRDS(ghcnd.list, ghcnd.out.path)
  
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


#my_markdown('get_weatherstations', 'R/get_data')