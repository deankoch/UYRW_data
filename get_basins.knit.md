---
title: "get_basins.R"
author: "Dean Koch"
date: "August 12, 2020"
output: github_document
---

**MITACS UYRW project** 

**get_basins**: getting started working with watershed data in R

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
```

```
## USGS Support Package: https://owi.usgs.gov/R/packages.html#support
```

```r
library(sf)
```

```
## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

```r
library(smoothr)
```

```
## 
## Attaching package: 'smoothr'
```

```
## The following object is masked from 'package:stats':
## 
##     smooth
```

```r
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

```
## Loading required package: AOI
```

```
## Loading required package: leaflet
```

Data on geographical landmarks and highways are available from OSM using the overpass API via `osmdata`


```r
library(osmdata)
```

```
## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright
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
uyrw.metadata.file = 'data/basins_metadata.csv'
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
                          description='reprojected/repaired NHDPlus catchment polygons')
  
  # ... water bodies ...
  uyrw.waterbody.file = c(name='waterbody',
                          file=file.path(out.subdir, 'uyrw_nhd_waterbody.rds'), 
                          type='R sfc object', 
                          description='reprojected/repaired NHDPlus water body polygons')
  
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
  basins.metadata.df = data.frame(rbind(uyrw.poi.file,
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
  write.csv(basins.metadata.df, here(uyrw.metadata.file))

} else {
  
  # load the data frame
  basins.metadata.df = read.csv(here(uyrw.metadata.file), header=TRUE, row.names=1)
  
}
```

This list of files and descriptions is now stored as a
[.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/basins_metadata.csv)
in the `/data` directory. Since github is not meant for hosting large binaries, some of these files are not
shared in this repository (see my 
[.gitignore](https://raw.githubusercontent.com/deankoch/UYRW_data/master/.gitignore) file). 
However you can reproduce all of them by running this script.

## starting location
Now we define a source outlet from which to explore upstream. Later on, we can load this information from disk instead of
querying OSM and USGS and computing things all over again.


```r
if(!file.exists(here(basins.metadata.df['poi', 'file'])))
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
  saveRDS(poi.list, here(basins.metadata.df['poi', 'file']))

} else {
  
  # load from disk 
  poi.list = readRDS(here(basins.metadata.df['poi', 'file']))
  
}
```


## downloading the data
package `nhdplusTools` uses the NHD's common identifier number (COMID) from Big Timber to delineate the watershed and
download the relevant data. These next few lines use the outlet location `poi.list$pt$bigtimber` to find and download
relevant watershed geometries.



```r
if(!file.exists(here(basins.metadata.df['nhd', 'file'])))
{
  # download a line geometry defining flowlines upstream of Big Timber, MT
  uyrw.flowlines = navigate_nldi(list(featureSource='comid', featureID=poi.list$comid$bigtimber), mode='upstreamTributaries', data_source = '')
  
  # notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
  print(uyrw.flowlines$nhdplus_comid)
  
  # download geometries defining catchements, water bodies, and the full flowline network
  subset_nhdplus(comids=uyrw.flowlines$nhdplus_comid, output_file=here(basins.metadata.df['nhd', 'file']), nhdplus_data='download')
}
```


## watershed boundary and projection
Once the data are downloaded, we load them into R as sfc objects for processing. This code chunk reprojects the watershed
boundary polygon to a reference system more appropriate for hydrology modeling, then computes the bounding box extent.



```r
if(any(!file.exists(here(c(basins.metadata.df['boundary', 'file'], basins.metadata.df['crs', 'file'])))))
{
  # load the watershed catchments. There are several thousand
  uyrw.catchment = read_sf(here(basins.metadata.df['nhd', 'file']), 'CatchmentSP')
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
  
  # and for plotting purposes, we may want the EPSG code for latitude/longitude in WGS84 datum
  latlong.epsg = 4326
  
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
  saveRDS(crs.list, here(basins.metadata.df['crs', 'file']))
  saveRDS(uyrw.poly, here(basins.metadata.df['boundary', 'file']))
  
} else {
  
  # load CRS info list and watershed boundary from disk
  crs.list = readRDS(here(basins.metadata.df['crs', 'file']))
  uyrw.poly = readRDS(here(basins.metadata.df['boundary', 'file']))
}
```

Note that holes in this watershed boundary polygon can emerge, when the catchement boundaries don't perfectly align - *eg.* try
plotting `st_union(uyrw.catchment)`. These are filled using the *fill_holes* function in the `smoothr`package.

## data prep

With the watershed boundaries and projection so defined, we can now transform the rest of the data. Files fetched by `nhdplusTools`
may include some invalid geometries (self-intersections) and features lying outside this watershed, so we clean up the sfc objects
before continuing.


```r
if(any(!file.exists(here(basins.metadata.df[c('catchment', 'waterbody', 'flowline', 'mainstem'), 'file']))))
{
  # load and reproject all geometries to from latitude/longitude to UTM
  uyrw.catchment = st_transform(read_sf(here(basins.metadata.df['nhd', 'file']), 'CatchmentSP'), crs.list$epsg)
  uyrw.flowline = st_transform(read_sf(here(basins.metadata.df['nhd', 'file']), 'NHDFlowline_Network'), crs.list$epsg)
  uyrw.waterbody = st_transform(read_sf(here(basins.metadata.df['nhd', 'file']), 'NHDWaterbody'), crs.list$epsg)
  
  # fix invalid geometries and mask with watershed boundary
  uyrw.waterbody = st_intersection(st_make_valid(uyrw.waterbody), uyrw.poly)
  uyrw.flowline = st_intersection(st_make_valid(uyrw.flowline), uyrw.poly)
  uyrw.catchment = st_intersection(st_make_valid(uyrw.catchment), uyrw.poly)
  
  # find and join all line segments labeled as the 'Yellowstone River', then fix self-intersection issues
  uyrw.mainstem = uyrw.flowline[uyrw.flowline$gnis_name == 'Yellowstone River',]
  uyrw.mainstem = st_make_valid(st_union(uyrw.mainstem, by_feature=FALSE))
  
  # save to disk
  saveRDS(uyrw.catchment, here(basins.metadata.df['catchment', 'file']))
  saveRDS(uyrw.flowline, here(basins.metadata.df['flowline', 'file']))
  saveRDS(uyrw.waterbody, here(basins.metadata.df['waterbody', 'file']))
  saveRDS(uyrw.mainstem, here(basins.metadata.df['mainstem', 'file']))
  
} else {
  
  # load from disk
  uyrw.catchment = readRDS(here(basins.metadata.df['catchment', 'file']))
  uyrw.flowline = readRDS(here(basins.metadata.df['flowline', 'file']))
  uyrw.waterbody = readRDS(here(basins.metadata.df['waterbody', 'file']))
  uyrw.mainstem = readRDS(here(basins.metadata.df['mainstem', 'file']))
  
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

# find some of the major watercourses and prepare labels for them
uyrw.flowline[rev(order(uyrw.flowline$lengthkm)),]$gnis_name
```

```
##    [1] "Pelican Creek"                      "Pelican Creek"                      "Broad Creek"                       
##    [4] "Raven Creek"                        "Little Timber Creek"                "North Fork Willow Creek"           
##    [7] "North Fork Sixmile Creek"           "Deep Creek"                         "Tower Creek"                       
##   [10] "Alum Creek"                         "Tobin Creek"                        " "                                 
##   [13] "Cottonwood Creek"                   "Pebble Creek"                       "Fawn Creek"                        
##   [16] "Ferry Creek"                        "Indian Creek"                       "Bridge Creek"                      
##   [19] "Bangtail Creek"                     "West Fork Duck Creek"               "Soda Butte Creek"                  
##   [22] "Fairy Creek"                        "Solution Creek"                     " "                                 
##   [25] "Wrong Creek"                        "Lupine Creek"                       "Grouse Creek"                      
##   [28] "Cold Creek"                         "Panther Creek"                      "Burnt Creek"                       
##   [31] " "                                  "South Fork McDonald Creek"          "Dog Creek"                         
##   [34] "West Chippy Creek"                  "Sour Creek"                         "Wolverine Creek"                   
##   [37] "Middle Fork Willow Creek"           " "                                  "Clear Creek"                       
##   [40] "Rocky Creek"                        "Yellowstone River"                  " "                                 
##   [43] "North Fork Elk Creek"               "Bear Creek"                         "Poison Creek"                      
##   [46] "North Fork Horse Creek"             "Calfee Creek"                       " "                                 
##   [49] "Strawberry Creek"                   "Yellowstone River"                  "Carnelian Creek"                   
##   [52] "Amphitheater Creek"                 "Slough Creek"                       "Yellowstone River"                 
##   [55] "Senecio Creek"                      "Shallow Creek"                      "Hornaday Creek"                    
##   [58] "Miles Creek"                        "Alum Creek"                         "Lost Creek"                        
##   [61] "Sheep Creek"                        "Falls Creek"                        "South Fork Yellowstone River"      
##   [64] " "                                  " "                                  "Bangtail Creek"                    
##   [67] "Middle Fork Horse Creek"            "Muddy Creek"                        "Mist Creek"                        
##   [70] "Cache Creek"                        "Boulder River"                      "Rose Creek"                        
##   [73] " "                                  "Dry Creek"                          "Blacktail Deer Creek"              
##   [76] "Miller Creek"                       "Woody Creek"                        " "                                 
##   [79] "Jay Creek"                          " "                                  " "                                 
##   [82] "Lowell Creek"                       "Columbine Creek"                    "Dry Creek"                         
##   [85] " "                                  "Escarpment Creek"                   "Astringent Creek"                  
##   [88] "Great Falls Creek"                  " "                                  "Yellowstone River"                 
##   [91] "Little Buffalo Creek"               "Falcon Creek"                       "Sedge Creek"                       
##   [94] "Cort Creek"                         " "                                  " "                                 
##   [97] "Cache Creek"                        "Shields River"                      " "                                 
##  [100] "Tucker Creek"                       "Tower Creek"                        "Yellowstone River"                 
##  [103] " "                                  " "                                  " "                                 
##  [106] "Stoughten Creek"                    "Cliff Creek"                        "East Fork Duck Creek"              
##  [109] "Glen Creek"                         " "                                  "East Fork Hellroaring Creek"       
##  [112] " "                                  "Cottonwood Creek"                   "Willow Creek"                      
##  [115] "Buffalo Fork"                       " "                                  "Moss Creek"                        
##  [118] "Oxbow Creek"                        "Rock Creek"                         " "                                 
##  [121] "Little Rock Creek"                  " "                                  "Looking Glass Creek"               
##  [124] "Area Creek"                         "Pass Creek"                         "Cub Creek"                         
##  [127] " "                                  "Beaverdam Creek"                    "Little Lamar River"                
##  [130] " "                                  " "                                  " "                                 
##  [133] "Yellowstone River"                  "Lamar River"                        "Passage Creek"                     
##  [136] "Amethyst Creek"                     " "                                  "Noel Creek"                        
##  [139] " "                                  "Cottonwood Creek"                   "Lewis Creek"                       
##  [142] "Elbow Creek"                        "East Boulder River"                 " "                                 
##  [145] " "                                  "Badger Creek"                       "South Fork Pine Creek"             
##  [148] "Suce Creek"                         "Yellowstone River"                  "Willow Creek"                      
##  [151] "Sour Creek"                         "Buffalo Creek"                      "North Fork Butte Creek"            
##  [154] "Grassy Creek"                       "Slough Creek"                       " "                                 
##  [157] " "                                  "Bridgman Creek"                     "Little Thumb Creek"                
##  [160] " "                                  "Elk Tongue Creek"                   "Yellowstone River"                 
##  [163] " "                                  "Pebble Creek"                       " "                                 
##  [166] "Perkins Creek"                      "Grizzly Creek"                      "Potter Creek"                      
##  [169] "Yellowstone River"                  " "                                  "Yellowstone River"                 
##  [172] "Mill Creek"                         "Dry Creek"                          " "                                 
##  [175] "Cascade Creek"                      " "                                  "Lamar River"                       
##  [178] "Slaughterhouse Creek"               "Hellroaring Creek"                  "Daisy Dean Creek"                  
##  [181] " "                                  "Chicken Creek"                      "Arrow Canyon Creek"                
##  [184] " "                                  "Speculator Creek"                   " "                                 
##  [187] "Winter Creek"                       "Thorofare Creek"                    "Basin Creek"                       
##  [190] " "                                  " "                                  "West Fork Mendenhall Creek"        
##  [193] "Rice Creek"                         "Big Creek"                          " "                                 
##  [196] "Lava Creek"                         " "                                  "Mist Creek"                        
##  [199] "Cache Creek"                        " "                                  "Miller Creek"                      
##  [202] "East Fork Boulder River"            "Little Donahue Creek"               "Cottonwood Creek"                  
##  [205] " "                                  " "                                  "Tumble Creek"                      
##  [208] "Rock Creek"                         "Slough Creek"                       " "                                 
##  [211] " "                                  "Crazy Head Creek"                   "Bark Cabin Creek"                  
##  [214] "Donahue Creek"                      " "                                  " "                                 
##  [217] "South Fork Horse Creek"             " "                                  " "                                 
##  [220] "Meadow Creek"                       "Straight Creek"                     " "                                 
##  [223] " "                                  " "                                  "Rock Creek"                        
##  [226] " "                                  " "                                  "Hellroaring Creek"                 
##  [229] "Boulder River"                      " "                                  "Trespass Creek"                    
##  [232] "East Fork Spring Creek"             "Elk Antler Creek"                   " "                                 
##  [235] " "                                  " "                                  " "                                 
##  [238] " "                                  "Hunters Creek"                      " "                                 
##  [241] "Colley Creek"                       "Palmer Creek"                       "Bluff Creek"                       
##  [244] "Shields River"                      "Antelope Creek"                     " "                                 
##  [247] " "                                  " "                                  "Meatrack Creek"                    
##  [250] "Work Creek"                         "West Boulder River"                 "Lamar River"                       
##  [253] " "                                  "Yellowstone River"                  "South Fork Elk Creek"              
##  [256] "Crandall Creek"                     "West Boulder River"                 " "                                 
##  [259] " "                                  "Horse Creek"                        "Canyon Creek"                      
##  [262] "Lava Creek"                         "Hellroaring Creek"                  "Bassett Creek"                     
##  [265] " "                                  " "                                  " "                                 
##  [268] " "                                  " "                                  " "                                 
##  [271] " "                                  "Eagle Creek"                        "Donahue Creek"                     
##  [274] "Lamar River"                        "Oxbow Creek"                        " "                                 
##  [277] "Unnamed Creek"                      " "                                  "Lynx Creek"                        
##  [280] " "                                  "Yellowstone River"                  "South Fork Daisy Dean Creek"       
##  [283] "Pebble Creek"                       "Spring Creek"                       "Yellowstone River"                 
##  [286] " "                                  "Shields River"                      " "                                 
##  [289] "Yellowstone River"                  " "                                  "Falls Creek"                       
##  [292] "Cascade Creek"                      "North Fork McDonald Creek"          " "                                 
##  [295] "Monitor Creek"                      " "                                  "Antelope Creek"                    
##  [298] "Electric Creek"                     " "                                  "Middle Fork Flathead Creek"        
##  [301] "Horse Creek"                        " "                                  " "                                 
##  [304] "Pine Creek"                         "Obsidian Creek"                     "Sedge Creek"                       
##  [307] " "                                  "Cache Creek"                        "Potter Creek"                      
##  [310] " "                                  " "                                  " "                                 
##  [313] " "                                  " "                                  " "                                 
##  [316] "Horse Creek"                        "Bull Creek"                         "Atlantic Creek"                    
##  [319] " "                                  "Big Creek"                          " "                                 
##  [322] " "                                  "Soda Butte Creek"                   "South Fork Shields River"          
##  [325] " "                                  "Mission Creek"                      "Hammond Creek"                     
##  [328] "Canyon Creek"                       "Republic Creek"                     "Lamar River"                       
##  [331] "Clause Creek"                       " "                                  "Lynx Creek"                        
##  [334] "Dike Creek"                         " "                                  "Elk Creek"                         
##  [337] "East Chippy Creek"                  "McDonald Creek"                     "Slip and Slide Creek"              
##  [340] " "                                  "Yellowstone River"                  "Grouse Creek"                      
##  [343] "Opal Creek"                         "Silvertip Creek"                    "Frazier Creek"                     
##  [346] "Little Trail Creek"                 "Yellowstone River"                  "Middle Creek"                      
##  [349] "Wild Creek"                         "Twin Peaks Creek"                   "Spring Creek"                      
##  [352] "North Fork Flathead Creek"          "Open Creek"                         "East Boulder River"                
##  [355] " "                                  "Davis Creek"                        " "                                 
##  [358] "Geode Creek"                        "Arrastra Creek"                     " "                                 
##  [361] "Grouse Creek"                       "Agate Creek"                        " "                                 
##  [364] " "                                  "Burnt Creek"                        "Dry Creek"                         
##  [367] "Hole-In-The-Rock Creek"             "Butte Creek"                        "Trappers Creek"                    
##  [370] "Smokey Creek"                       " "                                  "Lake Abundance Creek"              
##  [373] " "                                  "Cow Creek"                          "East Branch Hellroaring Creek"     
##  [376] "Howell Creek"                       " "                                  "North Fork Brackett Creek"         
##  [379] " "                                  " "                                  "Hidden Creek"                      
##  [382] "Elk Creek"                          "East Fork Mill Creek"               "Yellowstone River"                 
##  [385] "Cottonwood Creek"                   "Passage Creek"                      "Suce Creek"                        
##  [388] " "                                  " "                                  " "                                 
##  [391] " "                                  "Middle Fork Muddy Creek"            "Pool Creek"                        
##  [394] " "                                  "East Boulder River"                 "Mill Fork Creek"                   
##  [397] " "                                  "Fleshman Creek"                     "West Fork Little Timber Creek"     
##  [400] "Thistle Creek"                      "North Fork Clover Creek"            "West Boulder River"                
##  [403] "Cedar Creek"                        " "                                  " "                                 
##  [406] "Needle Creek"                       " "                                  "Little Indian Creek"               
##  [409] "Yellowstone River"                  "Castle Creek"                       "Lamar River"                       
##  [412] "Buck Creek"                         "Chalcedony Creek"                   " "                                 
##  [415] "Middle Fork Rock Creek"             "Lost Creek"                         " "                                 
##  [418] " "                                  "Falls Creek"                        " "                                 
##  [421] "North Fork Elbow Creek"             "Crystal Creek"                      " "                                 
##  [424] "Porcupine Creek"                    "Sheep Creek"                        " "                                 
##  [427] "Clear Creek"                        " "                                  " "                                 
##  [430] "Thompson Creek"                     "Elbow Creek"                        "Thorofare Creek"                   
##  [433] "Basin Creek"                        " "                                  " "                                 
##  [436] "Shallow Creek"                      "North Two Ocean Creek"              " "                                 
##  [439] "Soldier Creek"                      " "                                  "Flint Creek"                       
##  [442] "North Fork Elk Creek"               " "                                  " "                                 
##  [445] "Beaverdam Creek"                    "Falls Creek"                        " "                                 
##  [448] " "                                  "Thorofare Creek"                    "Sulphur Creek"                     
##  [451] "Grouse Creek"                       "Trappers Creek"                     "South Fork Shields River"          
##  [454] " "                                  " "                                  " "                                 
##  [457] " "                                  "Silver Creek"                       "West Fork Duck Creek"              
##  [460] "Emigrant Creek"                     "Carrol Creek"                       " "                                 
##  [463] "Sunlight Creek"                     " "                                  "Bear Creek"                        
##  [466] "Henry Creek"                        "North Fork Daisy Dean Creek"        " "                                 
##  [469] "Obsidian Creek"                     "South Fork Dry Creek"               " "                                 
##  [472] " "                                  "Gold Prize Creek"                   " "                                 
##  [475] " "                                  "Dugout Creek"                       "Yellowstone River"                 
##  [478] "Glen Creek"                         "Mountain Creek"                     "Kay Creek"                         
##  [481] " "                                  " "                                  "Boulder River"                     
##  [484] "Sheep Creek"                        " "                                  "Indian Creek"                      
##  [487] "Sheep Creek"                        " "                                  "Bailey Creek"                      
##  [490] "Lamar River"                        "Antelope Creek"                     " "                                 
##  [493] "Greeley Creek"                      " "                                  "Pelican Creek"                     
##  [496] " "                                  "Indian Creek"                       " "                                 
##  [499] "Pass Creek"                         "Bailey Creek"                       "Fridley Creek"                     
##  [502] "North Fork Deep Creek"              " "                                  "Frenchy Creek"                     
##  [505] " "                                  "North Fork Cedar Creek"             "Fleshman Creek"                    
##  [508] "Wicked Creek"                       "South Fork Carrol Creek"            "Elk Creek"                         
##  [511] " "                                  " "                                  " "                                 
##  [514] "Davis Creek"                        "Little Indian Creek"                "Potter Creek"                      
##  [517] "Little Cottonwood Creek"            "Mission Creek"                      "Shields River"                     
##  [520] " "                                  " "                                  " "                                 
##  [523] " "                                  "Rock Creek"                         " "                                 
##  [526] " "                                  "West Fork Little Timber Creek"      " "                                 
##  [529] " "                                  "Jarrett Creek"                      " "                                 
##  [532] "Obsidian Creek"                     "Cliff Creek"                        " "                                 
##  [535] " "                                  "Upper Sage Creek"                   " "                                 
##  [538] "Brackett Creek"                     "Shed Creek"                         "Shields River"                     
##  [541] "Little Timber Creek"                "Falls Creek"                        " "                                 
##  [544] " "                                  "Lambert Creek"                      " "                                 
##  [547] " "                                  " "                                  " "                                 
##  [550] "Yellowstone River"                  " "                                  " "                                 
##  [553] "Little Cottonwood Creek"            "Cache Creek"                        " "                                 
##  [556] "Emigrant Creek"                     "Jay Creek"                          "Sixmile Creek"                     
##  [559] " "                                  "Graham Creek"                       "Little Mission Creek"              
##  [562] "Indian Creek"                       " "                                  "Canyon Creek"                      
##  [565] " "                                  " "                                  " "                                 
##  [568] "Adair Creek"                        "Bullrun Creek"                      "Skully Creek"                      
##  [571] "Flathead Creek"                     "Cedar Creek"                        " "                                 
##  [574] "Dry Creek"                          " "                                  "East Fork West Boulder River"      
##  [577] " "                                  "Yellowstone River"                  "Cascade Creek"                     
##  [580] " "                                  "Miller Creek"                       "Eightmile Creek"                   
##  [583] " "                                  " "                                  "Wigwam Creek"                      
##  [586] "Deep Creek"                         "East Dam Creek"                     "Sixmile Creek"                     
##  [589] "North Fork Muddy Creek"             "Hidden Creek"                       "Falls Creek"                       
##  [592] "Walsh Creek"                        " "                                  "Green Canyon Creek"                
##  [595] "Middle Fork Cottonwood Creek"       "North Fork Bear Creek"              "Tobin Creek"                       
##  [598] "Steele Creek"                       "South Fork Elk Creek"               " "                                 
##  [601] "Jasper Creek"                       "South Fork Bridge Creek"            "Duck Creek"                        
##  [604] "Horse Creek"                        " "                                  " "                                 
##  [607] " "                                  "Pass Creek"                         "Lamar River"                       
##  [610] "Counts Creek"                       "Hoppe Creek"                        "North Fork Carrol Creek"           
##  [613] "Mission Creek"                      " "                                  "North Fork Butte Creek"            
##  [616] "South Cache Creek"                  " "                                  " "                                 
##  [619] " "                                  "Balm of Gilead Creek"               "South Fork Daisy Dean Creek"       
##  [622] "Little Cottonwood Creek"            "Anderson Creek"                     " "                                 
##  [625] " "                                  " "                                  " "                                 
##  [628] "Contact Creek"                      "South Fork Elk Creek"               " "                                 
##  [631] "Yellowstone River"                  "North Fork Deep Creek"              "Buck Creek"                        
##  [634] "Bitter Creek"                       "Buffalo Creek"                      " "                                 
##  [637] "Indian Creek"                       " "                                  "East Fork Boulder River"           
##  [640] " "                                  " "                                  " "                                 
##  [643] " "                                  " "                                  "West Fork Little Timber Creek"     
##  [646] " "                                  "Blakely Creek"                      " "                                 
##  [649] " "                                  "Billman Creek"                      " "                                 
##  [652] "Lost Creek"                         "Shields River"                      "Siggins Fork"                      
##  [655] "Bullrun Creek"                      "Skunk Creek"                        "Fiddle Creek"                      
##  [658] "Hammond Creek"                      " "                                  " "                                 
##  [661] "Deaf Jim Creek"                     " "                                  " "                                 
##  [664] "South Fork Flathead Creek"          " "                                  " "                                 
##  [667] "Jungle Creek"                       " "                                  "West Fork Buffalo Creek"           
##  [670] "Canyon Creek"                       " "                                  "Placer Basin Creek"                
##  [673] "Buffalo Creek"                      " "                                  "Slough Creek"                      
##  [676] "Crevice Creek"                      " "                                  " "                                 
##  [679] " "                                  " "                                  " "                                 
##  [682] " "                                  " "                                  " "                                 
##  [685] " "                                  " "                                  "Slippery Creek"                    
##  [688] " "                                  " "                                  "Trail Creek"                       
##  [691] " "                                  "Greeley Creek"                      " "                                 
##  [694] " "                                  "Deep Creek"                         "Horse Creek"                       
##  [697] " "                                  "Winter Creek"                       " "                                 
##  [700] "Spring Creek"                       " "                                  "Rock Creek"                        
##  [703] " "                                  " "                                  "Shields River"                     
##  [706] "Fridley Creek"                      "Cold Creek"                         "Arrow Canyon Creek"                
##  [709] " "                                  " "                                  "Rescue Creek"                      
##  [712] "Turkey Pen Creek"                   "Pine Creek"                         "Thompson Creek"                    
##  [715] "Passage Creek"                      " "                                  " "                                 
##  [718] " "                                  " "                                  "Lava Creek"                        
##  [721] "Eagle Creek"                        "Little Trail Creek"                 " "                                 
##  [724] "Shorty Creek"                       "Potter Creek"                       " "                                 
##  [727] " "                                  " "                                  " "                                 
##  [730] " "                                  " "                                  " "                                 
##  [733] "South Fork Carrol Creek"            "Middle Fork Muddy Creek"            " "                                 
##  [736] "Yellowstone River"                  "Hidden Creek"                       " "                                 
##  [739] " "                                  "Cabin Creek"                        " "                                 
##  [742] "Little Pine Creek"                  "South Fork Eightmile Creek"         "Grizzly Creek"                     
##  [745] "West Fork Horse Creek"              " "                                  "Daniels Creek"                     
##  [748] "Fleshman Creek"                     " "                                  " "                                 
##  [751] "North Fork Lena Creek"              " "                                  " "                                 
##  [754] "Elk Creek"                          " "                                  " "                                 
##  [757] "East Fork Duck Creek"               "Sour Creek"                         " "                                 
##  [760] " "                                  " "                                  " "                                 
##  [763] "Bull Creek"                         "Copper Creek"                       " "                                 
##  [766] " "                                  "West Fork Duck Creek"               "Beaverdam Creek"                   
##  [769] " "                                  "South Fork Bull Creek"              "Whistle Creek"                     
##  [772] " "                                  "South Fork Shields River"           " "                                 
##  [775] " "                                  "Trail Creek"                        "Mendenhall Creek"                  
##  [778] " "                                  " "                                  " "                                 
##  [781] " "                                  "Rock Creek"                         "Mill Creek"                        
##  [784] "Spring Creek"                       " "                                  "Goat Creek"                        
##  [787] "Yellowstone River"                  " "                                  " "                                 
##  [790] "Cottonwood Creek"                   "West Boulder River"                 "Meadow Creek"                      
##  [793] " "                                  " "                                  " "                                 
##  [796] " "                                  " "                                  "Little Timber Creek"               
##  [799] "Cub Creek"                          "Lava Creek"                         " "                                 
##  [802] "Yellowstone River"                  " "                                  "Lowell Creek"                      
##  [805] "West Fork Spring Creek"             "Three Creeks"                       " "                                 
##  [808] " "                                  " "                                  "Slough Creek"                      
##  [811] " "                                  "Hellroaring Creek"                  "Second Fork West Fork Buffalo Fork"
##  [814] "Arnica Creek"                       " "                                  "Open Creek"                        
##  [817] " "                                  "Flathead Creek"                     " "                                 
##  [820] " "                                  "Rapid Creek"                        " "                                 
##  [823] " "                                  "West Fork Crevice Creek"            " "                                 
##  [826] "Eightmile Creek"                    " "                                  " "                                 
##  [829] "Obsidian Creek"                     "Brundage Creek"                     " "                                 
##  [832] "Beaver Creek"                       "Flathead Creek"                     "Sixmile Creek"                     
##  [835] " "                                  "Castle Creek"                       " "                                 
##  [838] "South Fork Brackett Creek"          "Brackett Creek"                     "Middle Fork Dry Creek"             
##  [841] "Yellowstone River"                  " "                                  "Dry Creek"                         
##  [844] " "                                  "Bennett Creek"                      "South Cache Creek"                 
##  [847] "Yellowstone River"                  " "                                  " "                                 
##  [850] "Meatrack Creek"                     " "                                  " "                                 
##  [853] " "                                  " "                                  "Sunlight Creek"                    
##  [856] "Pine Creek"                         "Thorofare Creek"                    " "                                 
##  [859] "Bear Creek"                         " "                                  " "                                 
##  [862] "Yellowstone River"                  " "                                  "Alkali Creek"                      
##  [865] "Boulder River"                      " "                                  " "                                 
##  [868] " "                                  " "                                  "East Fork Boulder River"           
##  [871] "Brackett Creek"                     " "                                  "Cache Creek"                       
##  [874] "Fourmile Creek"                     "Rock Creek"                         " "                                 
##  [877] "Dixon Creek"                        "Cole Creek"                         "East Fork Emigrant Creek"          
##  [880] " "                                  "Coyote Creek"                       " "                                 
##  [883] " "                                  " "                                  "Clear Creek"                       
##  [886] "Falls Creek"                        " "                                  " "                                 
##  [889] " "                                  " "                                  " "                                 
##  [892] " "                                  "Lake Abundance Creek"               "East Fork Rock Creek"              
##  [895] "Hellroaring Creek"                  " "                                  "Valley Fork"                       
##  [898] " "                                  " "                                  "Rock Creek"                        
##  [901] "Smith Creek"                        " "                                  " "                                 
##  [904] "Mendenhall Creek"                   " "                                  "Specimen Creek"                    
##  [907] " "                                  "West Boulder River"                 "North Fork Frazier Creek"          
##  [910] "Elk Creek"                          " "                                  " "                                 
##  [913] " "                                  "Mill Creek"                         "Chicken Creek"                     
##  [916] "Potter Creek"                       "Tom Miner Creek"                    " "                                 
##  [919] " "                                  " "                                  " "                                 
##  [922] "Deep Creek"                         " "                                  " "                                 
##  [925] " "                                  " "                                  " "                                 
##  [928] " "                                  "Phelps Creek"                       "North Fork Bull Creek"             
##  [931] " "                                  "Weasel Creek"                       " "                                 
##  [934] " "                                  " "                                  " "                                 
##  [937] " "                                  " "                                  "Cutoff Creek"                      
##  [940] "Wallace Creek"                      " "                                  "Cole Creek"                        
##  [943] " "                                  " "                                  "Snowslide Creek"                   
##  [946] "Davis Creek"                        " "                                  " "                                 
##  [949] " "                                  "Winter Creek"                       "Miller Creek"                      
##  [952] " "                                  "Phelps Creek"                       " "                                 
##  [955] " "                                  " "                                  " "                                 
##  [958] " "                                  "Yellowstone River"                  " "                                 
##  [961] "Serrett Creek"                      " "                                  "Trail Creek"                       
##  [964] "West Fork Mill Creek"               " "                                  "North Fork Pine Creek"             
##  [967] " "                                  "East Fork Buffalo Fork"             " "                                 
##  [970] " "                                  "Slough Creek"                       " "                                 
##  [973] "Howell Fork"                        " "                                  " "                                 
##  [976] "Little Indian Creek"                " "                                  "Boulder River"                     
##  [979] "Spring Creek"                       "Lava Creek"                         " "                                 
##  [982] "Pole Creek"                         "Tom Miner Creek"                    "Speculator Creek"                  
##  [985] " "                                  "Dry Creek"                          "Middle Fork Hellroaring Creek"     
##  [988] " "                                  " "                                  " "                                 
##  [991] " "                                  "Chicken Creek"                      "Emigrant Creek"                    
##  [994] " "                                  " "                                  "Reeder Creek"                      
##  [997] "Bailey Creek"                       " "                                  " "                                 
## [1000] " "                                 
##  [ reached getOption("max.print") -- omitted 3335 entries ]
```

```r
# 
# 
# uyrw.flowline[rev(order(uyrw.flowline$surfarea)),]$gnis_name
# 
# names(uyrw.flowline)
# unique(uyrw.flowline$surfarea)
# which(uyrw.flowline$gnis_name == 'Mill Creek')

# plot the watershed flowlines and water bodies as a png file
if(!file.exists(here(basins.metadata.df['img_flowline', 'file'])))
{
  # render/write the plot
  png(here(basins.metadata.df['img_flowline', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)

    print(tm_shape(uyrw.poly, xlim=uyrw.xlim.larger, ylim=uyrw.ylim.larger) + 
            tm_polygons(col='greenyellow', border.col='yellowgreen') +
          tm_shape(uyrw.flowline) +
            tm_lines(col='dodgerblue3') +
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

![flowlines of the Upper Yellowstone and tributaries](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_flowlines.png)


```r
# plot the watershed drainage basins and water bodies as a png file
if(!file.exists(here(basins.metadata.df['img_basins', 'file'])))
{
  # render/write the plot
  png(here(basins.metadata.df['img_basins', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)
    
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

![Drainage basins of the Upper Yellowstone and tributaries](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_basins.png)





