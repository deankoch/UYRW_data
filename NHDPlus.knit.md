---
title: "NHDPlus.R"
author: "Dean Koch"
date: "August 12, 2020"
output: github_document
---

**MITACS UYRW project**

A basic example script for getting started working with watershed data in R.

For now, this simply follows the example on the `nhdplusTools` github page with some minor modifications.
A user guide for the NHDPlus dataset is
<a href="https://pubs.er.usgs.gov/publication/ofr20191096" target="_blank">available here</a>, and a 
<a href="https://tinyurl.com/y54rlqja" target="_blank">data dictionary here</a>.

The `nhdplusTools` package can be used to fetch NHDPlus products from the web without having to navigate the USGS
website. We use it to assemble some basic info on the hydrology of the UYRW upstream of Big Timber, Montana. This script
transforms that data into a more convenient format, and produces some plots giving an overview of the watershed.

## libraries
If any of these CRAN packages are not installed on your machine, run `install.packages(...)` to get them


```r
library(nhdplusTools)
library(sf)
library(smoothr)
library(tmap)
```

The `sf` package is needed for handling GIS data; The `smoothr` package simplifies complex spatial features; and the `tmap`
package constructs nice ggplot2-based thematic map graphics.

To define the watershed, we need a starting location. Here we use Big Timber, MT, and define the UYRW to include all catchments upstream.
We find the coordinates of Big Timber in R using the `AOI` package, which interfaces with OpenStreetMaps (OSM) to get the latitude/longitude
pair corresponding to a placename. `AOI` is loaded automatically by Mike Johnson's `HydroData` package, which we use later on to fetch more
hydrology datasets.


```r
# uncomment two lines below to install from github using devtools
#library(devtools)
#install_github('mikejohnson51/HydroData')
library(HydroData)
```

Data on geographical landmarks and highways are available from OSM using the overpass API via `osmdata`


```r
library(osmdata)
```

The `here` package is helpful for defining working directories on portable code


```r
library(here)
```


## project data
To avoid downloading things over and over again, we'll use a permanent storage location on disk ("/data").
This is where we store large data files and R object binaries, which are not suitable for git.

The `if(!file.exists(...))` conditionals preceding each code chunk indicate which files will be written in that section.
If the files are detected in the local data storage directory, then code chunk can be skipped (to avoid redundant downloads,
*etc*), and the files are loaded from disk instead. 

We start by defining a project directory tree and a list of files and metadata to download


```r
# figures directory 'graphics' will be created in the RStudio project folder
graphics.dir = 'graphics'

# subdirectories of `data` (in the RStudio project folder) for source files and processed output 
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# a helper function for creating folders
my_dir = function(path) { if(!dir.exists(path)) {dir.create(path, recursive=TRUE)} }

# create folders as needed
lapply(here(c(src.subdir, out.subdir, graphics.dir)), my_dir)

# this CSV file will serve as a guide for all files written to the project folder
uyrw.metadata.file = 'data/uyrw_metadata.csv'
if(!file.exists(here(uyrw.metadata.file)))
{
  # filename for points-of-interest, their comids, and plotting labels
  uyrw.poi.file = c(name='poi',
                    file='data/uyrw_poi.rds', 
                    type='R list object', 
                    description='points of interest in the watershed')
  
  # filename for coordinate reference system details 
  uyrw.crs.file = c(name='crs',
                    file='data/uyrw_crs.rds', 
                    type='R list object', 
                    description='details of projection/extent for the project')
  
  # filename for the hydrology GeoPackage (gpkg) file from NHD (source)
  uyrw.nhd.file = c(name='nhd',
                    file=file.path(src.subdir, 'uyrw_nhd.gpkg'), 
                    type='geopackage', 
                    description='source geometries from NHDPlus')
  
  # filename for derived watershed boundary
  uyrw.poly.file = c(name='boundary',
                     file=file.path(out.subdir, 'uyrw_nhd_boundary.rds'), 
                     type='R sfc object', 
                     description='UYRW watershed boundary polygon derived from NHDPlus catchements')
  
  # filename for derived main stem course
  uyrw.mainstem.file = c(name='mainstem',
                         file=file.path(out.subdir, 'uyrw_nhd_mainstem.rds'), 
                         type='R sfc object', 
                         description='UYR main stem line geometry derived from NHDPlus flowlines')
  
  # filename for reprojected NHD data on catchments...
  uyrw.catchment.file = c(name='catchment',
                          file=file.path(out.subdir, 'uyrw_nhd_catchment.rds'), 
                          type='R sfc object', 
                          description='reprojected/repaired NHDPlus polygons')
  
  # ... water bodies ...
  uyrw.waterbody.file = c(name='waterbody',
                          file=file.path(out.subdir, 'uyrw_nhd_waterbody.rds'), 
                          type='R sfc object', 
                          description='reprojected/repaired NHDPlus polygons')
  
  # ... and flowlines
  uyrw.flowline.file = c(name='flowline',
                         file=file.path(out.subdir, 'uyrw_nhd_flowline.rds'), 
                         type='R sfc object', 
                         description='reprojected/repaired NHDPlus flowline geometries')
  
  # filename for graphic showing flowlines in study area
  uyrw.flowline.png.file = c(name='img_flowlines',
                             file=file.path(graphics.dir, 'uyrw_flowlines.png'),
                             type='png graphic', 
                             description='image of flowlines in the UYRW with placenames')
  
  # filename for graphic showing flowlines in study area...
  uyrw.basin.png.file = c(name='img_basins',
                          file=file.path(graphics.dir, 'uyrw_basins.png'),
                          type='png graphic', 
                          description='image of some 4000 drainage basins in the UYRW')
  
  # bind all the individual filename info vectors into a data frame
  uyrw.metadata.df = data.frame(rbind(uyrw.poi.file,
                                      uyrw.crs.file,
                                      uyrw.nhd.file,
                                      uyrw.poly.file, 
                                      uyrw.mainstem.file, 
                                      uyrw.catchment.file,
                                      uyrw.waterbody.file,
                                      uyrw.flowline.file,
                                      uyrw.flowline.png.file,
                                      uyrw.basin.png.file), row.names='name')
  
  # save the data frame
  write.csv(uyrw.metadata.df, here(uyrw.metadata.file))

} else {
  
  # load the data frame
  uyrw.metadata.df = read.csv(here(uyrw.metadata.file), header=TRUE, row.names=1)
  
}
```

This list of files and descriptions is now stored as a
[.csv file](https://github.com/deankoch/URYW_data/blob/master/data/uyrw_metadata.csv)
in the `/data` directory. Since github is not meant for hosting large binaries, some of these files are not
shared in this repository (see my 
[.gitignore](https://raw.githubusercontent.com/deankoch/URYW_data/master/.gitignore) file). 
However you can reproduce all of them by running this script.

## starting location
Now we define a source outlet from which to explore upstream. Later on we can load this information from disk instead of
querying OSM and USGS and computing things all over again.


```r
if(!file.exists(here(uyrw.metadata.df['poi', 'file'])))
{
  # define some points of interest in the watershed
  poi.name = c(bigtimber = 'Big Timber, MT',
               livingston = 'Livingston, MT',
               emigrant = 'Emigrant, MT',
               corwinsprings = 'Corwin Springs, MT',
               canyonvillage = 'Canyon Village, WY',
               yellowstonelake = 'Yellowstone Lake, WY')
  
  # look up their coordinates on OSM
  poi.pt = lapply(poi.name, function(locstring) geocode(location=locstring, pt=TRUE))
    
  # look up their COMIDs
  poi.comid = lapply(poi.pt, discover_nhdplus_id)
  
  # compile into a list and save to disk
  poi.list = list(name = poi.name, pt = poi.pt, comid = poi.comid)
  saveRDS(poi.list, here(uyrw.metadata.df['poi', 'file']))

} else {
  
  # load from disk 
  poi.list = readRDS(here(uyrw.metadata.df['poi', 'file']))
  
}
```


## downloading the data
package `nhdplusTools` uses the NHD's common identifier number (COMID) from Big Timber to delineate the watershed and
download the relevant data. These next few lines use the outlet location `poi.list$pt$bigtimber` to find and download
relevant watershed geometries.



```r
if(!file.exists(here(uyrw.metadata.df['nhd', 'file'])))
{
  # download a line geometry defining flowlines upstream of Big Timber, MT
  uyrw.flowlines = navigate_nldi(list(featureSource='comid', featureID=poi.list$comid$bigtimber), mode='upstreamTributaries', data_source = '')
  
  # notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
  print(uyrw.flowlines$nhdplus_comid)
  
  # download geometries defining catchements, water bodies, and the full flowline network
  subset_nhdplus(comids=uyrw.flowlines$nhdplus_comid, output_file=here(uyrw.metadata.df['nhd', 'file']), nhdplus_data='download')
}
```


## watershed boundary and projection
Once the data are downloaded, we load them into R as sfc objects for processing. This code chunk reprojects the watershed
boundary polygon to a reference system more appropriate for hydrology modeling, then computes the bounding box extent.



```r
if(any(!file.exists(here(c(uyrw.metadata.df['boundary', 'file'], uyrw.metadata.df['crs', 'file'])))))
{
  # load the watershed catchments. There are several thousand
  uyrw.catchment = read_sf(here(uyrw.metadata.df['nhd', 'file']), 'CatchmentSP')
  print(nrow(uyrw.catchment))
  
  # Their union delineates the entire UYR watershed as a single polygon
  uyrw.poly = fill_holes(st_union(uyrw.catchment, by_feature=FALSE), threshold=1e6)
  
  # determine the extent of the watershed in terms of long/lat coordinates
  uyrw.geo.xlim = st_bbox(uyrw.poly)[c(1,3)]
  uyrw.geo.ylim = st_bbox(uyrw.poly)[c(2,4)]
  
  #' We'll use the Transverse Mercator (UTM) projection, as recommended in the QSWAT+ manual. This is regarded as "close enough"
  #' to an equal area projection for modeling purposes.
  
  # determine the EPSG code for the UTM zone (12N) on which our study area is centred
  uyrw.UTM.epsg = 32700 - round((45+mean(uyrw.geo.ylim))/90)*100 + round((183+mean(uyrw.geo.xlim))/6)
  
  # and for plotting purposes, we may want the epsg code for latitude/longitude
  latlong.epsg = 4269
  
  #' Reproject the watershed boundary polygon from latitude/longitude to UTM
  uyrw.poly = st_transform(uyrw.poly, uyrw.UTM.epsg)
  
  # determine the extent in terms of the new (projected) x/y coordinates
  uyrw.xlim = st_bbox(uyrw.poly)[c(1,3)]
  uyrw.ylim = st_bbox(uyrw.poly)[c(2,4)]
  
  # define CRS info list
  crs.list = list(epsg = uyrw.UTM.epsg,
                  epsg.geo = latlong.epsg,
                  dims.geo = list(xlim=uyrw.geo.xlim, ylim=uyrw.geo.ylim),
                  dims = list(xlim=uyrw.xlim, ylim=uyrw.ylim))
  
  # save to disk
  saveRDS(crs.list, here(uyrw.metadata.df['crs', 'file']))
  saveRDS(uyrw.poly, here(uyrw.metadata.df['boundary', 'file']))
  
} else {
  
  # load CRS info list and watershed boundary from disk
  crs.list = readRDS(here(uyrw.metadata.df['crs', 'file']))
  uyrw.poly = readRDS(here(uyrw.metadata.df['boundary', 'file']))
}
```

Note that holes in this watershed boundary polygon can emerge, when the catchement boundaries don't perfectly align - *eg.* try
plotting `st_union(uyrw.catchment)`. These are filled using the *fill_holes* function in the `smoothr`package.

## data prep

With the watershed boundaries and projection so defined, we can now transform the rest of the data. Files fetched by `nhdplusTools`
may include some invalid geometries (self-intersections) and features lying outside this watershed, so we clean up the sfc objects
before continuing.


```r
if(any(!file.exists(here(uyrw.metadata.df[c('catchment', 'waterbody', 'flowline', 'mainstem'), 'file']))))
{
  # load and reproject all geometries to from latitude/longitude to UTM
  uyrw.catchment = st_transform(read_sf(here(uyrw.metadata.df['nhd', 'file']), 'CatchmentSP'), crs.list$epsg)
  uyrw.flowline = st_transform(read_sf(here(uyrw.metadata.df['nhd', 'file']), 'NHDFlowline_Network'), crs.list$epsg)
  uyrw.waterbody = st_transform(read_sf(here(uyrw.metadata.df['nhd', 'file']), 'NHDWaterbody'), crs.list$epsg)
  
  # fix invalid geometries and mask with watershed boundary
  uyrw.waterbody = st_intersection(st_make_valid(uyrw.waterbody), uyrw.poly)
  uyrw.flowline = st_intersection(st_make_valid(uyrw.flowline), uyrw.poly)
  uyrw.catchment = st_intersection(st_make_valid(uyrw.catchment), uyrw.poly)
  
  # find and join all line segments labeled as the 'Yellowstone River', then fix self-intersection issues
  uyrw.mainstem = uyrw.flowline[uyrw.flowline$gnis_name == 'Yellowstone River',]
  uyrw.mainstem = st_make_valid(st_union(uyrw.mainstem, by_feature=FALSE))
  
  # save to disk
  saveRDS(uyrw.catchment, here(uyrw.metadata.df['catchment', 'file']))
  saveRDS(uyrw.flowline, here(uyrw.metadata.df['flowline', 'file']))
  saveRDS(uyrw.waterbody, here(uyrw.metadata.df['waterbody', 'file']))
  saveRDS(uyrw.mainstem, here(uyrw.metadata.df['mainstem', 'file']))
  
} else {
  
  # load from disk
  uyrw.catchment = readRDS(here(uyrw.metadata.df['catchment', 'file']))
  uyrw.flowline = readRDS(here(uyrw.metadata.df['flowline', 'file']))
  uyrw.waterbody = readRDS(here(uyrw.metadata.df['waterbody', 'file']))
  uyrw.mainstem = readRDS(here(uyrw.metadata.df['mainstem', 'file']))
  
}
```


## visualization
All of the data needed to create the flowlines plot in our readme are now loaded into R. The following code creates that plot
(and a few others) using the `tmap` package


```r
# define a padded bounding box for plotting
cex.xlim = 1.8
cex.ylim = 1.1
uyrw.xlim.larger = crs.list$dims$xlim + (cex.xlim-1)*c(-1,1)*diff(crs.list$dims$xlim)/2
uyrw.ylim.larger = crs.list$dims$ylim + (cex.ylim-1)*c(0,1)*diff(crs.list$dims$ylim)/2

# determine some reasonable dimensions (in pixels) for output
flowlines.png.res = round(c(diff(uyrw.xlim.larger), diff(uyrw.ylim.larger))/100)

# plot the watershed flowlines and water bodies as a png file
if(!file.exists(here(uyrw.metadata.df['img_flowline', 'file'])))
{
  # render/write the plot
  png(here(uyrw.metadata.df['img_flowline', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)

    print(tm_shape(uyrw.poly, xlim=uyrw.xlim.larger, ylim=uyrw.ylim.larger) + 
            tm_polygons(col='greenyellow', border.col='yellowgreen') +
          #tm_shape(uyrw.flowline) +
          #  tm_lines(col='dodgerblue3') +
          tm_shape(uyrw.mainstem) +
            tm_lines(col='dodgerblue4', lwd=2) +
          tm_shape(uyrw.waterbody) + 
            tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
          tm_shape(poi.list$pt[['bigtimber']]) +   
            tm_dots(size=0.2, col='red') +
            tm_text('request', just='top', ymod=0.5, xmod=2.5, size=0.8) +
          tm_shape(do.call(rbind, poi.list$pt[c('emigrant','livingston')])) +   
            tm_dots(size=0.2, col='red') +
            tm_text('request', just='bottom', xmod=-2.5, size=0.8) +
          tm_shape(poi.list$pt[['corwinsprings']]) +   
            tm_dots(size=0.2, col='red') +
            tm_text('request', just='bottom', ymod=-0.5, xmod=-3.5, size=0.8) +
          tm_shape(poi.list$pt[['yellowstonelake']]) +   
            tm_text('request', just='top', xmod=-4, size=0.8) +
          tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
          tm_scale_bar(breaks=c(0, 20, 40), position=c('center', 'bottom'), text.size=0.7) +
          tm_layout(title='major watercourses in the UYRW', title.position=c('center', 'TOP'), frame=FALSE))
    
  dev.off()
}
```

![flowlines of the Upper Yellowstone and tributaries](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/uyrw_flowlines.png)


```r
# plot the watershed drainage basins and water bodies as a png file
if(!file.exists(here(uyrw.metadata.df['img_basins', 'file'])))
{
  # render/write the plot
  png(here(uyrw.metadata.df['img_basins', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)
    
    print(tm_shape(uyrw.catchment, xlim=uyrw.xlim.larger, ylim=uyrw.ylim.larger) + 
            tm_polygons(col='MAP_COLORS', border.col=NA) +
          tm_shape(uyrw.mainstem) +
            tm_lines(col=adjustcolor('dodgerblue4', alpha=0.8), lwd=2) +
          tm_shape(uyrw.waterbody) + 
            tm_polygons(col=adjustcolor('deepskyblue3', alpha=0.8), border.col='deepskyblue4') +
          tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
            tm_scale_bar(breaks=c(0, 20, 40), position=c('center', 'bottom'), text.size=0.7) +
          tm_layout(title=paste0('drainage basins of the UYRW (n=', nrow(uyrw.catchment), ')'),
                    title.position=c('center', 'TOP'),
                    frame=FALSE))
    
  dev.off()
}
```

![Drainage basins of the Upper Yellowstone and tributaries](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/uyrw_basins.png)





