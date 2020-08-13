---
title: "get_weatherstations.R"
author: "Dean Koch"
date: "August 13, 2020"
output: github_document
---

**MITACS UYRW project**

**get_weatherstations**: finds climatic sensor stations located in the UYRW


The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package fetches
[SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA; and the
[`rnoaa`](https://github.com/ropensci/rnoaa ) package fetches
[GHCN Daily](https://www.ncdc.noaa.gov/ghcn-daily-description) data (see documentation 
[here](https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)).
We use them to build a map of climatic sensor stations in the UYRW, and to query historical data for model training.

[get_basins.R](https://github.com/deankoch/URYW_data/blob/master/get_basins.knit.md)
which creates some required directories and project config files, should be run before this script.

## libraries


```r
library(snotelr)
library(rnoaa)
library(sf)
library(tmap)
library(here)
```

Data on geographical landmarks and highways are available from OSM using the overpass API via `osmdata`


```r
library(osmdata)
```


## project data


```r
# project directory names
graphics.dir = 'graphics'
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# load metadata csv, CRS info list and watershed geometries from disk
uyrw.metadata.df = read.csv(here('data/basins_metadata.csv'), header=TRUE, row.names=1)
crs.list = readRDS(here(uyrw.metadata.df['crs', 'file']))
uyrw.poly = readRDS(here(uyrw.metadata.df['boundary', 'file']))
uyrw.waterbody = readRDS(here(uyrw.metadata.df['waterbody', 'file']))
uyrw.mainstem = readRDS(here(uyrw.metadata.df['mainstem', 'file']))
uyrw.flowline = readRDS(here(uyrw.metadata.df['flowline', 'file']))

# this CSV file will serve as a guide for all files written to the project folder
weatherstation.metadata.file = 'data/weatherstation_metadata.csv'
if(!file.exists(here(weatherstation.metadata.file)))
{
  # filename for metadata table downloaded from SNOTEL website
  snotel.csv.file = c(name='snotel.csv',
                      file=file.path(src.subdir, 'snotel_sites.csv'), 
                      type='CSV table', 
                      description='metadata list for SNOTEL sites (unchanged)')
  
  # filename for SNOTEL metadata table as an sfc object
  snotel.sfc.file = c(name='snotel',
                      file=file.path(out.subdir, 'snotel_sites.rds'), 
                      type='R sf object', 
                      description='sfc object with SNOTEL sensor locations in UYRW')
  
  # filename for GHCND metadata table downloaded from NOAA
  ghcnd.csv.file = c(name='ghcnd.csv',
                     file=file.path(src.subdir, 'ghcnd_sites.csv'), 
                     type='CSV table', 
                     description='metadata list for GHCND sites (unchanged)')
  
  # filename for GHCND metadata table as an sfc object
  ghcnd.sfc.file = c(name='ghcnd',
                     file=file.path(out.subdir, 'ghcnd_sites.rds'),
                     type='R sf object',
                     description='sfc object with GHCN Daily sensor locations in UYRW')
  
  # filename for sfc object representing a padded (outer buffer) watershed boundary 
  uyrw.poly.pad.sfc.file = c(name='boundary_padded',
                             file=file.path(out.subdir, 'uyrw_boundary_padded.rds'),
                             type='R sf object',
                             description='padded watershed boundary polygon for querying nearby weather stations')
  
  # filename for graphic showing SNOTEL and GHCND site locations on the UYRW
  sensor.sites.png.file = c(name='img_weatherstation',
                            file=file.path(graphics.dir, 'weatherstation_sites.png'),
                            type='png graphic', 
                            description='image of SNOTEL and GHCND site locations in the UYRW')

  
  # bind all the individual filename info vectors into a data frame
  weatherstation.metadata.df = data.frame(rbind(snotel.csv.file,
                                                snotel.sfc.file,
                                                ghcnd.csv.file,
                                                ghcnd.sfc.file,
                                                uyrw.poly.pad.sfc.file,
                                                sensor.sites.png.file), row.names='name')
  
  # save the data frame
  write.csv(weatherstation.metadata.df, here(weatherstation.metadata.file))
  
} else {
  
  # load the data frame
  weatherstation.metadata.df = read.csv(here(weatherstation.metadata.file), header=TRUE, row.names=1)
  
}
```

This list of files and descriptions is now stored as a
[.csv file](https://github.com/deankoch/URYW_data/blob/master/data/weatherstation_metadata.csv)
in the `/data` directory.
Climatic data near the boundaries of the watershed will be useful for interpolation.
Define a padded boundary polygon to search inside for station data


```r
if(!file.exists(here(weatherstation.metadata.df['boundary_padded', 'file'])))
{
  # for now I use a 25km buffer
  uyrw.padded.poly = st_buffer(uyrw.poly, dist = 25e3)
  saveRDS(uyrw.padded.poly, here(weatherstation.metadata.df['boundary_padded', 'file']))
  
} else {
  
  # load from disk 
  uyrw.padded.poly = readRDS(here(weatherstation.metadata.df['boundary_padded', 'file']))
  
}
```


## Find SNOTEL sites

the `snotel_info` function in `snotelr` downloads a CSV containing site IDs and coordinates


```r
if(!file.exists(here(weatherstation.metadata.df['snotel.csv', 'file'])))
{
  # download the metadata csv to the folder specified in `path`. This writes the file "snotel_metadata.csv"
  snotel_info(path=here(src.subdir))
  
  # rename the csv to avoid confusion with identically-named file in the parent folder (my list of project files)
  file.rename(from=here(src.subdir, 'snotel_metadata.csv'), to=here(weatherstation.metadata.df['snotel.csv', 'file']))
  
}
```

Load this CSV, omit stations not in UYRW, and convert it to a `sf` object, then save to disk


```r
if(!file.exists(here(weatherstation.metadata.df['snotel', 'file'])))
{
 
   # load the site info table into a data frame and extract coordinates
  snotel.df = read.csv(here(weatherstation.metadata.df['snotel.csv', 'file']), header=TRUE)
  sites.coords.matrix = as.matrix(snotel.df[, c('longitude', 'latitude')])
  
  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  snotel.sfc = st_sfc(lapply(1:nrow(snotel.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  snotel.sf = st_sf(cbind(snotel.df, snotel.sfc))
  
  # transform to UTM and clip to URYW area (30 stations identified)
  snotel.sf = st_transform(snotel.sf, crs=crs.list$epsg)
  snotel.sf = st_intersection(snotel.sf, uyrw.padded.poly)
  
  # save to disk
  saveRDS(snotel.sf, here(weatherstation.metadata.df['snotel', 'file']))

  
} else {
  
  # load from disk 
  snotel.sf = readRDS(here(weatherstation.metadata.df['snotel', 'file']))
  
}
```


## Find NOAA Global Historical Climatology Network (GHCN) Daily sites
the `ghcnd_stations` function in `rnoaa` downloads a table of site IDs and coordinates


```r
if(!file.exists(here(weatherstation.metadata.df['ghcnd.csv', 'file'])))
{
  # download the metadata table and load into R (slow, 1-2min)
  ghcnd.df = ghcnd_stations()

  # save a copy as csv in the /data/source folder
  write.csv(ghcnd.df, here(weatherstation.metadata.df['ghcnd.csv', 'file']))

}
```

Load this CSV. It indexed over 100,000 stations worldwide! This chunk transforms the coordinates to UTM,
omits stations not in UYRW (leaving 138), converts the result to an `sf` object with one feature per station,
and saves the result to disk


```r
if(!file.exists(here(weatherstation.metadata.df['ghcnd', 'file'])))
{

  # load the site info table into a data frame
  ghcnd.df = read.csv(here(weatherstation.metadata.df['ghcnd.csv', 'file']), header=TRUE)
  
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
  
  # clip to UYRW watershed region (65 stations) then join with the other attributes that are constant across "id"
  ghcnd.sf = st_intersection(ghcnd.sf, uyrw.padded.poly)
  idx.ghcnd.uyrw = which(!idx.duplicateID)[ghcnd.df$id[!idx.duplicateID] %in% ghcnd.sf$id]
  ghcnd.sf = cbind(ghcnd.sf, ghcnd.df[idx.ghcnd.uyrw, c('longitude', 'latitude', 'elevation', 'name')])
  nrow(ghcnd.sf)
  
  # "element" attribute varies by site "id". There are 76 possibilities in the UYRW area
  ghcnd.elem = unique(ghcnd.df[ghcnd.df$id %in% ghcnd.sf$id, 'element'])

  # this helper function will convert start/end year columns to a single (string) column for each "element"
  my.ghcnd.reshape = function(idval, elemval)
  {
    # query this combination of station id and element string in the full GHCND table
    idx.row = (ghcnd.df$id == idval) & (ghcnd.df$element == elemval)
    
    # if it exists, return a string of form "start-end", otherwise NA
    return(ifelse(!any(idx.row), NA, paste(ghcnd.df[idx.row, c('first_year', 'last_year')], collapse='-')))
  }
  
  # double sapply call to this function builds a table indicating which elements are available in which year
  ghcnd.elem.df = t(sapply(ghcnd.sf$id, function(idval) sapply(ghcnd.elem, function(elemval) my.ghcnd.reshape(idval, elemval))))
  
  # join this data to the sfc object (reordering to emphasize most populated fields)
  ghcnd.sf = cbind(ghcnd.sf, ghcnd.elem.df[,order(apply(ghcnd.elem.df, 2, function(xx) sum(!is.na(xx))), decreasing=TRUE)])
  
  # There is some overlap with SNOTEL. Identify the GHCND sites that are indexed by SNOTEL
  ghcnd.sf$snowtel_id = apply(st_distance(ghcnd.sf, snotel.sf), 1, function(xx) ifelse(!any(xx<1), NA, snotel.sf$site_id[xx<1]))
  
  # save to disk
  saveRDS(ghcnd.sf, here(weatherstation.metadata.df['ghcnd', 'file']))
  
  
} else {
  
  # load from disk
  ghcnd.sf = readRDS(here(weatherstation.metadata.df['ghcnd', 'file']))
  
} 
  
  












# define a new attribute: the number of years in the record
year.start = sapply(strsplit(snotel.sf$start, '-'), function(xx) as.numeric(xx[[1]]))
year.end = sapply(strsplit(snotel.sf$end, '-'), function(xx) as.numeric(xx[[1]]))
# snotel.sf$duration = year.end - year.start 


# split the points into upper and lower, for plotting text labels that don't overlap
idx.lower = snotel.sf$site_name %in% c('s fork shields ', 'sacajawea ', 'monument peak ')
```


## visualization



```r
# define a padded bounding box for plotting
# cex.xlim = 1.8
# cex.ylim = 1.1
# uyrw.xlim.larger = crs.list$dims$xlim + (cex.xlim-1)*c(-1,1)*diff(crs.list$dims$xlim)/2
# uyrw.ylim.larger = crs.list$dims$ylim + (cex.ylim-1)*c(0,1)*diff(crs.list$dims$ylim)/2

# determine some reasonable dimensions (in pixels) for output
# flowlines.png.res = round(c(diff(uyrw.xlim.larger), diff(uyrw.ylim.larger))/100)

# plot the SNOTEL stations as a png file
if(!file.exists(here(weatherstation.metadata.df['img_weatherstation', 'file'])))
{
  # render/write the plot
  # png(here(weatherstation.metadata.df['img_snotel', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)
  png(here(weatherstation.metadata.df['img_weatherstation', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)

    # print(tm_shape(uyrw.padded.poly, xlim=uyrw.xlim.larger, ylim=uyrw.ylim.larger) + 
    #         tm_polygons(col='greenyellow', border.col='yellowgreen') +
    #       tm_shape(uyrw.flowline) +
    #         tm_lines(col='dodgerblue3') +
    #       tm_shape(uyrw.mainstem) +
    #         tm_lines(col='dodgerblue4', lwd=2) +
    #       tm_shape(uyrw.waterbody) + 
    #         tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
    #       tm_shape(snotel.sf[idx.lower,]) +
    #         tm_dots(size=0.5, col='red') +
    #         tm_text('site_name', just='bottom', ymod=-0.8, size=0.8) +
    #       tm_shape(snotel.sf[!idx.lower,]) +
    #         tm_dots(size=0.5, col='red') +
    #         tm_text('site_name', just='top', ymod=0.8, size=0.8) +
    #       tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
    #       tm_scale_bar(breaks=c(0, 20, 40), position=c('center', 'bottom'), text.size=0.5) +
    #       tm_layout(title='SNOTEL stations in the UYRW', title.position=c('center', 'TOP'), frame=FALSE))
    
    print(tm_shape(uyrw.padded.poly) +
      tm_polygons(border.col=NA) +
    tm_shape(uyrw.poly) +
      tm_polygons(col='greenyellow', border.col='yellowgreen') +
    tm_shape(uyrw.flowline) +
      tm_lines(col='yellowgreen') +
    tm_shape(uyrw.mainstem) +
      tm_lines(col='yellowgreen', lwd=2) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='yellowgreen', border.col='yellowgreen') +
    tm_shape(ghcnd.sf[!is.na(ghcnd.sf$SNOW),]) +
      tm_dots(size=0.1, col='red') +
    tm_shape(snotel.sf) +
      tm_dots(size=0.1, col='purple') +
      tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
      tm_scale_bar(breaks=c(0, 20, 40), position=c('center', 'bottom'), text.size=0.5) +
      tm_layout(title='climatic sensor stations in the UYRW', title.position=c('center', 'TOP'), frame=FALSE))

    
  dev.off()
}
```

![SNOTEL stations in the UYRW](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/weatherstation_sites.png)



Data downloads look like this:



```r
# xx = meteo_pull_monitors('US1MTPK0001')
# yy = snotel_download(site_id = 806, internal=TRUE)
```


