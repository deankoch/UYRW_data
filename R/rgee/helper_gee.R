#' ---
#' title: "helper_GEE.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **helper_gee**: helper functions for google earth engine downloads
#' 
#' This uses the `rgee` package, which depends on `reticulate`, an R wrapper for python calls.
#' We use the native python API for GEE, so a working python installation is required.
#' 
#' Some useful web resouces related to `rgee`:
#' 
#' https://github.com/r-spatial/rgee/blob/master/README.md
#' https://r-spatial.github.io/rgee/articles/BestPractices.html
#' https://r-spatial.github.io/rgee/reference/eedate_to_rdate.html
#' https://github.com/r-spatial/rgee/issues/58
#' 
#' Useful web resouces related to GEE
#' 
#' https://gis.stackexchange.com/questions/281752/reducing-image-collection-to-get-daily-sum-from-hourly-precipitation-and-extract
#' https://developers.google.com/earth-engine/guides/objects_methods_overview?hl=en

# relative paths for working directory
library(here)

# we use `stars` to open the GeoTIFFs because `raster` is very slow on multiband projects
library(stars)

# `googledrive` and `future` are required by `rgee` to download raster collections via gdrive
library(googledrive)
library(future)

# define directories for dependencies
gee.dir = here('rgee')
conda.dir = file.path(gee.dir, 'conda')
conda.env = file.path(conda.dir, 'envs/r-reticulate')

# set an environmental variable to ensure reticulate finds the right environment
Sys.setenv(RETICULATE_PYTHON = conda.env)
library('reticulate')
library('rgee')


#' 
#' ## general
#' 

# returns info and useful objects related to a google earth engine ImageCollection
rgee_info = function(path, dates=TRUE, webpage=TRUE)
{
  # ARGUMENTS:
  # 
  # `path`: character, the name of the collection (eg "NOAA/GFS0P25")
  # `dates`: logical, whether to request the date range of the collection
  # `webpage`: logical, whether to load the GEE webpage for the collection
  #
  # RETURN:
  #
  # a list with elements:
  #
  # 'ic': the `rgee` ImageCollection object
  # 'crs': the EPSG code for the first image (as character)
  # 'pnames': the property names of the first image
  # 'nband': the number of bands in the first image
  # 'dates': the minimum and maximum dates in the collection (optional)
  # 
  
  # `rgee` representations of the GEE imagecollection and its first image 
  ee.ic = ee$ImageCollection(path)
  ee.first = ee.ic$first()
  
  # retrieve band/property info for the image collection based on first image
  cat('collecting info about first image...\n')
  crs = ee.first$projection()$getInfo()$crs
  pnames = ee.ic$first()$propertyNames()$getInfo()
  ee.bandinfo = ee.ic$first()$get('system:bands')
  bandinfo = ee.bandinfo$getInfo()
  
  # find the minimum and maximum dates in the collection if requested 
  earliest = latest = NA
  if( dates )
  {
    cat('collecting info about dates...')
    ee.daterange = ee.ic$reduceColumns(ee$Reducer$minMax(), list('system:time_start'))
    earliest = ee$Date(ee.daterange$get('min')) %>% eedate_to_rdate %>% as.Date
    latest = ee$Date(ee.daterange$get('max')) %>% eedate_to_rdate %>% as.Date
  }
  
  # compile output as list
  cat('done... \n')
  list(path = path,
       ic = ee.ic,
       crs = crs,
       pnames = pnames,
       nband = length(bandinfo),
       bandnames = names(bandinfo),
       dates = c(earliest, latest))
}

# returns an `rgee` geometry object usable as 'region' in getDownloadURL calls
rgee_region = function(boundary, crs=4326)
{
  # ARGUMENTS:
  # 
  # `boundary`: sf polygon object, the boundary of the region of interest
  # `crs`: integer, the EPGS code for transforming the output
  #
  # RETURN:
  #
  # An `rgee` object of class "ee.geometry.Geometry"
  #
  # DETAILS:
  #
  # returns the bounding box of `boundary` in the target reference system `crs`,
  # as an EE geometry, such that it can be passed as input 'region' in calls to
  # Image$getDownloadURL
  
  return( sf_as_ee(st_transform(boundary, crs))$bounds() )
}


#' 
#' ## NOAA Global Forecast System (GFS) 
#' 
#' These are frequently-updated forecasts from a gridded weather model at a resolution
#' of about 25km - for the URYW area this translates to a 3x3 grid of points. See
#' the function `rgee_gfs_forecast_date` below for details on the variables predicted.
#' 
#' For each date in the GFS collection there are (up to) four publication times - midnight,
#' 6am, 12pm, 6pm - when new forecasts are posted. Each of these releases includes numerous
#' forecast times, corresponding to target times of between 0 and 384 hours into the future.
#' On most days there are hourly data available up to 3 days into the future, with 3-hour
#' intervals for times beyond that. Early dates in the time series only include day 0 and 1.
#' 
#' Our approach is to take all of the forecasts released on a given day, group them by
#' their forecast target day (0-10 days into the future), then reduce by taking temperature
#' minima/maxima, average total precipitation for the day, and medians for all other variables.
#' These represent an averaged value of the forecasts released on a particular day, aggregated
#' into daily form (by reducing over hours) so it can be ingested by SWAT+ 
#' 
#' Note the GFS changed its data specifications in the fall of 2019; prior dates counted
#' precipitation at the target time of the forecast whereas later ones counted
#' cumulatively with respect to the 0-hour forecast. This is described on the GEE
#' page for this collection. The functions below normalize everything by always returning
#' the daily total in the band '*_total_precipitation_surface'.
#' 
#' Note also that this collection has a few hiccups that aren't clear from the product
#' description:
#' 
#' 1. Certain images (eg most 0-hour forecasts) are missing 3 of the 9 bands
#' 2. Occasionally a date will be missing all data (eg. 2018-03-16 to 2018-03-18)
#' 3. Occasionally an image property is missing (eg. 'forecast_hours' on 2017-10-31)
#' 
#' These missing data lead to errors from GEE when we try to reduce the collection
#' to a single image (raster) by taking medians etc. There doesn't seem to be an easy way of
#' replacing these missing bands with a nodata value that is handled appropriately by the
#' reducer functions (min, max, median, etc), so we either need a clever way of imputing the
#' data or else we need to change the GEE requests in these special cases to avoid the
#' incomplete images.
#' 
#' Issue 1 is dealt with by replacing the missing bands with those of the closest complete
#' forecast hour (usually 1hr or 3hr ahead or behind) from the same release time. For 2, I
#' simply skip the downloads for that date. We can impute those missing days very easily
#' in R later on. And for 3, I overwrite the forecast hours property at the beginning
#' (using the property 'forecast_time', which is found in all images)
#' 



# wrapper for rgee_gfs_forecast_date
rgee_gfs_forecast = function(dates, boundary, destdir, info=NULL)
{
  # ARGUMENTS:
  # 
  # `dates`: character or Date vector, to be downloaded in a loop
  # `boundary`: sf polygon object, the boundary of the region of interest
  # `destdir`: character, path to a directory for storing the downloaded files
  # `info`: list, (optional) return value of rgee_info('NOAA/GFS0P25')
  #
  # RETURN:
  #
  # A list of metadata about the GeoTIFF file(s) and contents (see )
  #
  # DETAILS:
  #
  # This function reduces (aggregates) GFS collection images into daily forecast(s)
  # for the supplied `dates`. It is a wrapper to vectorize and simplify calls to
  # `rgee_gfs_forecast_date`.
  # 
  # Data are downloaded for each element in `dates` separately, in a loop,
  # so the input dates need not be sequential. When `dest` is NA the function downloads
  # the file to a temporary location before opening it with the stars package.
  #
  
  # collect summary info about the collection (unless already supplied)
  if( is.null(info) ) info = rgee_info('NOAA/GFS0P25')
  
  # verify user-supplied dates lie in the correct range
  dates = as.Date(dates)
  idx.validdate = dates %in% seq.Date(info$dates[1], info$dates[2], by='day')
  if( any( !idx.validdate) ) stop('invalid input date(s)')
  ndates = length(dates)
  
  # set up destination paths for the zip files and metadata
  zipdest = file.path(destdir, paste0(dates, '.zip'))
  jsondest = file.path(destdir, paste0(dates, '.json'))
  completed = rep(FALSE, ndates)
  
  # loop over input dates and print progress bar as we go
  pb = txtProgressBar(1, ndates+1, style=3)
  for(idx.file in seq(ndates))
  {
    # wrap the download call in an error-catcher
    rdate = dates[idx.file]
    out.list = try(rgee_gfs_forecast_date(rdate, 
                                          boundary, 
                                          zipdest[idx.file], 
                                          info), TRUE)
    
    # create empty file info list for downloads that fail
    if( class(out.list) != 'try-error' ) out.list = list(date=rdate, status='incomplete')
    
    # write output metadata to disk
    completed[idx.file] = out.list$status == 'complete'
    jsonlite::toJSON(out.list, pretty=TRUE) %>% write(jsondest[idx.file])
        
    # print progress message 
    setTxtProgressBar(pb, idx.file)
    cat('\n')
  }
  close(pb)
  
  return(list(zip=zipdest, json=jsondest, completed=completed))
}

# reduce and download daily GFS forecast data as multiband GeoTIFF
rgee_gfs_forecast_date = function(rdate, boundary, zipdest, info)
{
  # ARGUMENTS:
  # 
  # `rdate`: Date object, the date to download (assumed valid)
  # `boundary`: sf polygon object, the boundary of the region of interest
  # `dest`: character or NA, the desired download path (should end in '.zip')
  # `info`: list, return value of rgee_info('NOAA/GFS0P25')
  #
  # RETURN:
  #
  # A list of metadata about the GeoTIFF file(s) and contents:
  #
  #   'url' : the (temporary) download URL used
  #   'status' : either 'complete' or 'incomplete'
  #   'zip' : path to the downloaded zip file (if 'status' is complete)
  #   'date' : the timestamp for this output, ie the date the forecast was released
  #   'bandnames' : band names of the zipped GeoTIFF
  #   'vnames' : variable names associated with each band
  #   'forecast_day' : integer, for each band, the number of days ahead into the future
  #   'units' : udunits2 strings for each variable name
  #   'jdate', 'rhour' 'fhour', 'fday', 'nband' : integers (used for debugging)
  #
  # The function also has the side-effect (if successful) of downloading the requested
  # data as a zipped GeoTIFF in `zipdest`
  #
  # DETAILS:
  #
  # This function reduces (aggregates) GFS collection images into daily forecast(s)
  # for the supplied `dates` by reducing over forecast target times (as described
  # in the section introduction above). 
  #
  # Different forecast targets are stored as different bands in a single GeoTIFF -
  # a prefix of the form "[0-9]_" indicates the target day, as days into the future.
  # Note that GEE seems to drop bandnames from rasters returned with getDownloadURL,
  # so the names in the output list entry 'bandnanmes' will need to be copied when
  # it's time to open the rasters.

  # hard-coded band names
  bandnames = c('downward_shortwave_radiation_flux', 
                'total_precipitation_surface', 
                'total_cloud_cover_entire_atmosphere',
                'specific_humidity_2m_above_ground',
                'precipitable_water_entire_atmosphere',   
                'v_component_of_wind_10m_above_ground', 
                'temperature_2m_above_ground', 
                'u_component_of_wind_10m_above_ground', 
                'relative_humidity_2m_above_ground')
  
  # names of bands that are missing from certain images
  n.bands = length(bandnames)
  bandnames.missing = bandnames[1:3]
  
  # new band names for temperature data (over which we take min/max)
  temp.srcname = 'temperature_2m_above_ground'
  temp.bandnames = setNames(paste0(c('min', 'max'), '_', temp.srcname), c('min', 'max'))
  
  # band name for precipitation data (handled differently in different eras)
  precip.srcname = 'total_precipitation_surface'
  precip.changedate = as.Date('2019-11-07')
  
  # for all other bands we take medians 
  median.bandnames = bandnames[ !(bandnames %in% c(temp.srcname, precip.srcname)) ] 
  
  # units and names of output bands (not all the same as above!)
  units.out = c(precip.srcname, temp.bandnames, median.bandnames)
  names(units.out) = c('kg/m2', 'degC', 'degC', 'kg/kg', 'kg/m2', 'W/m2', 'm/s', '%', 'm/s', '%')
  
  # message and era check for precipitation spec
  cat(paste0('fetching forecasts from ', as.character(rdate), '... \n'))
  is.precip.cumulative = !( rdate < precip.changedate )
  
  # filter collection to `rdate` and create some new image properties for later filtering
  ee.ic = info$ic$filterDate(rdate_to_eedate(rdate), rdate_to_eedate(rdate+1))$map(function(img) {
    
    # rgee has trouble importing these dates image collections
    ee.start = ee$Date(img$get('system:time_start'))
    ee.target = ee$Date(img$get('forecast_time'))
                       
    # define some derived properties (all integers)
    rhour = ee.start$getRelative('hour', 'day') # hour of day of this publication
    jdate = ee.target$getRelative('day', 'year')$add(1) # julian date of forecast target
    fhour = ee.target$getRelative('hour', 'day') # hour of the day of forecast target
    fday = ee.target$difference(ee.start, 'day')$int() # forecast target days into future
    forecast_hours = fday$multiply(ee$Number(24))$add(fhour) # this property sometimes missing!
    nband = img$bandNames()$length() # number of bands in the image (not all have 9!)
    
    # set these properties for later use
    img$set('jdate', jdate)$
      set('rhour', rhour)$
      set('fhour', fhour)$
      set('fday', fday)$
      set('forecast_hours', forecast_hours)$
      set('nband', nband)
  })
  
  # retrieve these properties for all images in the current date selection
  cat('collecting metadata about this subset ...\n')
  jdate.all = ee.ic$aggregate_array('jdate')$getInfo()
  rhour.all = ee.ic$aggregate_array('rhour')$getInfo()
  fhour.all = ee.ic$aggregate_array('fhour')$getInfo()
  fday.all = ee.ic$aggregate_array('fday')$getInfo()
  nband.all = ee.ic$aggregate_array('nband')$getInfo()
  
  # new field 'fday' counts the number of days between the forecast target and the current day
  fday.available = unique(fday.all)
  
  # throw an error if there is nothing left after filtering
  if( length(fday.available) == 0 ) stop('no forecast data found')
  
  # Normally 17 forecast days are available, but we only request (at most) the first 10
  fday = na.omit( fday.available[1:10] )
  ee.fday = ee$List(fday)
  # this keeps us in compliance with GEE 100 band limit for direct downloads (10 lyr*10day)
  
  # handle missing bands (if any)
  idx.missing = which(nband.all < n.bands)
  if( length(idx.missing) > 0 )
  {
    cat('imputing missing bands...\n')
    
    # filter image collection to separate the problem images
    ee.complete = ee.ic$filterMetadata('nband', 'equals', n.bands)
    ee.missing = ee.ic$filterMetadata('nband', 'not_equals', n.bands)
    
    # replace missing bands with an adjacent forecast
    ee.missing = ee.missing$map(function(img) {
      
      # identify the nearest forecast image from the same release time
      img.surrogate = ee.ic$filterMetadata('nband', 'equals', n.bands)$
        filterMetadata('rhour', 'equals', img$get('rhour'))$
        map(function(img2) { 
          
          # compute difference in hours with all eligible images
          img2$set('surrogate_diff', ee$Number(img2$get('forecast_hours'))$
                     subtract(img$get('forecast_hours'))$abs() )
          
          })$sort('surrogate_diff')$first()
      
      # replace the missing bands with these surrogate values
      img$addBands(img.surrogate, bandnames.missing)
      
    })
    
    # recombine with complete images
    ee.ic = ee.complete$merge(ee.missing)
  }
  
  # group images with like forecast target day and take pixel-wise medians (reduces to 7 bands)
  ee.median = ee.fday$map(ee_utils_pyfunc(function(d) {
    img.median = ee.ic$select(median.bandnames)$filterMetadata('fday', 'equals', d)$median() 
    return( img.median )
  })) %>% ee$ImageCollection()
  
  # pixel-wise precipitation daily sum (creates image collection with 1 band)
  if(!is.precip.cumulative)
  {
    # normalize pre-2019 values, which hold hourly rather than cumulative data
    ee.precip = ee.fday$map(ee_utils_pyfunc(function(d) {
      
      # identify images up to and including the day d
      ic.tosum = ee.ic$select(precip.srcname)$filterMetadata('fday', 'not_greater_than', d)
      
      # sum them together and scale to get mean of predictions for 24hr total
      scale.constant = ee$Number(d)$add(1)$multiply( ee$Number(24) )$divide(ic.tosum$size())
      ic.tosum$sum()$multiply(scale.constant)
      
    })) %>% ee$ImageCollection()
    
  } else {
    
    # for cumulative data we simply take the daily max (this should be the final hour prediction) 
    ee.precip = ee.fday$map(ee_utils_pyfunc(function(d) {
      
      # maximum of forecasts for this target day
      ee.ic$select(precip.srcname)$filterMetadata('fday', 'equals', d)$max()
      
    })) %>% ee$ImageCollection()
  }
  
  # pixel-wise temperature minimum (creates image collection with 1 band)
  ee.min = ee.fday$map(ee_utils_pyfunc(function(d) {
    img.median = ee.ic$select(temp.srcname)$filterMetadata('fday', 'equals', d)$min()
    return( img.median$rename(temp.bandnames['min']) )
  })) %>% ee$ImageCollection()
  
  # pixel-wise temperature maximum (creates image collection with 1 band)
  ee.max = ee.fday$map(ee_utils_pyfunc(function(d) {
    img.median = ee.ic$select(temp.srcname)$filterMetadata('fday', 'equals', d)$max()
    return( img.median$rename(temp.bandnames['max']) )
  })) %>% ee$ImageCollection()
  
  # combine the three ImageCollections then flatten by duplicating bands with image ID prefix
  ee.out = ee.precip$combine(ee.min)$combine(ee.max)$combine(ee.median)$toBands()
  bandnames.out = ee.out$bandNames()$getInfo()
  
  # extract forecast day target and variable names from these output band names
  vnames.out = gsub('[0-9]+\\_', '', bandnames.out, perl=TRUE)
  units.out = names(units.out)[match(vnames.out, units.out)]
  len.prefix = nchar(bandnames.out) - nchar(vnames.out) - 1
  fday.out = as.integer( substr(bandnames.out, 1, len.prefix) )
  
  # get temporary download URL (spatial masking region is supplied here)
  cat('submitting request to google earth engine...\n')
  ee.url = ee.out$getDownloadUrl(list(filePerBand=FALSE, region=rgee_region(boundary, crs=4326)))
  
  # download the data, handling network issues with 10 retries with a pause of 10 seconds between
  cat(paste0('downloading forecast days ', paste(fday, collapse=', '), '...'))
  attempts = 0
  attempts.max = 10
  pause.sec = 10
  download.status = 'incomplete'
  while(attempts < attempts.max)
  {
    # wait ten seconds between attempts
    if(attempts > 0) 
    {
      cat(paste(attempts, 'attempt(s) failed. retrying in 10s...\n'))
      Sys.sleep(pause.sec) 
    }
    attempts = attempts + 1
    
    # wrap the download call in an error-catcher
    try.dl = try(download.file(ee.url, zipdest, mode='wb', quiet=TRUE), TRUE)
    if( class(try.dl) != 'try-error' ) download.status = 'complete'
    
    # break from the while loop if the download is successful 
    if(download.status=='complete') attempts = attempts.max + 1
  }
  
  # build output list
  cat('done\n\n')
  list(url = ee.url,
       status = download.status,
       zip = zipdest,
       date = rdate,
       bandnames = bandnames.out,
       vnames = vnames.out,
       forecast_day = fday.out,
       units = units.out,
       jdate = jdate.all,
       rhour = rhour.all,
       fhour = fhour.all,
       fday = fday.all,
       nband = nband.all)
}





#+ include=FALSE
#library(here)
#source(here('R/helper_main.R'))
#my_markdown('helper_gee', 'R/rgee')