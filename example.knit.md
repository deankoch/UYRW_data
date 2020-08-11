---
title: "example.R"
author: "Dean Koch"
date: "June 19, 2020"
output: github_document
---

**MITACS UYRW project**

A basic example script for getting started working with watershed data in R.

For now, this simply follows the example on the `nhdplusTools` github page with some minor modifications.
A user guide for the NHDPlus dataset is
<a href="https://tinyurl.com/y54rlqja" target="_blank">available here</a>, and a 
<a href="https://tinyurl.com/y54rlqja" target="_blank">data dictionary here</a>.

I'll start by downloading some GIS data on the hydrology of the UYR upstream of Big Timber, MT. The `nhdplusTools` 
package can be used to fetch this data without having to navigate the USGS website.
@details # libraries
If any of these CRAN packages are not already installed on your machine, run `install.packages('packagename')` to get them


```r
library(nhdplusTools)
```

The `sf` package is needed for handling GIS data; The `smoothr` package is handy for simplifying complex spatial features
and generating polygons from rasters; and the `tmap` package constructs nice ggplot2-style thematic map graphics.


```r
library(sf)
library(smoothr)
library(tmap)
```

To define the watershed, we need a starting location. I will use Big Timber, Montana, and define the UYRW to 
include all catchments upstream. The `AOI` package interfaces with OpenStreetMaps (OSM) to get lat/long info 
from placenames. This package is loaded automatically with package `HydroData`, which we will use later on to
pull more hydrology datasets (uncomment two lines below to install from github using devtools).


```r
#library(devtools)
#install_github('mikejohnson51/HydroData')
library(HydroData)
```

Data on geographical landmarks and highways can be downloaded from OSM using the overpass API via `osmdata`


```r
library(osmdata)
```

The `here` package is helpful for defining working directories on portable code


```r
library(here)
```

@details # starting location


```r
# define a source outlet from which to explore upstream 
BigTimber.pt = geocode(location='Big Timber, MT', pt=TRUE)
BigTimber.pt$name = 'Big Timber, MT'

# define some other points of interest in the watershed
Livingston.pt = geocode(location='Livingston, MT', pt=TRUE)
Livingston.pt$name = 'Livingston, MT'
Emigrant.pt = geocode(location='Emigrant, MT', pt=TRUE)
Emigrant.pt$name = 'Emigrant, MT'
CorwinSprings.pt = geocode(location='Corwin Springs, MT', pt=TRUE)
CorwinSprings.pt$name = 'Corwin Springs, MT'
TowerRoosevelt.pt = geocode(location='Tower-Roosevelt, WY', pt=TRUE) 
TowerRoosevelt.pt$name = 'Tower-Roosevelt, WY'
CanyonVillage.pt = geocode(location='Canyon Village, WY', pt=TRUE) 
CanyonVillage.pt$name = 'Canyon Village, WY'
YellowstoneLake.pt = geocode(location='Yellowstone Lake, WY', pt=TRUE) 
YellowstoneLake.pt$name = 'Yellowstone Lake, WY'

# get a COMID for the source outlet
BigTimber.comid = discover_nhdplus_id(BigTimber.pt)
```

@details # downloading the data
package nhdplusTools will use the COMID from Big Timber to delineate the watershed and download the relevant data.
To avoid downloading things over and over again, first define a permanent storage location on disk:


```r
# use the `here` package to define a subdirectory of the RStudio project folder, and create it as needed
data.dir = here('bigfiles') # These are large binaries, not suitable for git
if(!dir.exists(data.dir))
{
  dir.create(data.dir, recursive=TRUE)
}

# define a filename for the hydrology GeoPackage (gpkg) file from NHD
BigTimber.nhdfile = file.path(data.dir, 'nhd_basic.gpkg')
```

These next few lines will use the source outlet location (BigTimber.pt) to find/download relevant watershed geometries.
I've wrapped them in a "if file.exists" conditional to skip this step after the data are downloaded once. 


```r
if(!file.exists(BigTimber.nhdfile))
{
  # download a line geometry defining flowlines upstream of Big Timber, MT
  BigTimber.flowlines = navigate_nldi(list(featureSource='comid', featureID=BigTimber.comid), mode='upstreamTributaries', data_source = '')
  
  # notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
  print(BigTimber.flowlines$nhdplus_comid)
  
  # download geometries defining catchements, water bodies, and the full flowline network
  subset_nhdplus(comids=BigTimber.flowlines$nhdplus_comid, output_file=BigTimber.nhdfile, nhdplus_data='download')
}
```

Note there is a warning message on the last call, indicating that the package has not been tested on such a large watershed 
(indicating I should go through by hand later to verify it fetched everything).
@details # data prep
Once the data are downloaded, we can load them into R as sfc objects


```r
# load the watershed data
UYRW.flowline = read_sf(BigTimber.nhdfile, 'NHDFlowline_Network')
UYRW.catchment = read_sf(BigTimber.nhdfile, 'CatchmentSP')
UYRW.waterbody = read_sf(BigTimber.nhdfile, 'NHDWaterbody')
```

There are many thousands of catchments ...


```r
print(nrow(UYRW.catchment))
```

```
## [1] 4162
```

... and their union delineates the entire UYR watershed as a single polygon. Holes in this polygon may emerge if the 
catchements boundaries don't perfectly align (try plotting `st_union(UYRW.catchment)`). These can be filled 
using the *fill_holes* function in the `smoothr` package.


```r
UYRW.poly = fill_holes(st_union(UYRW.catchment, by_feature=FALSE), threshold=1e6)
```

The water body geometries fetched by `nhdplusTools` may include some invalid geometries (self-intersections) and features 
lying outside this watershed. We should clean up this sfc object before continuing ...


```r
UYRW.waterbody = st_intersection(st_make_valid(UYRW.waterbody), UYRW.poly)
```

```
## although coordinates are longitude/latitude, st_intersection assumes that they are planar
```

```
## Warning: attribute variables are assumed to be spatially constant throughout all geometries
```

... and just to be sure, do the same for the flowlines


```r
UYRW.flowline = st_intersection(st_make_valid(UYRW.flowline), UYRW.poly)
```

```
## although coordinates are longitude/latitude, st_intersection assumes that they are planar
```

```
## Warning: attribute variables are assumed to be spatially constant throughout all geometries
```

It will be useful to have a line geometry representing the main stem and main tributaries of the river


```r
# find and join all line segments labeled as the 'Yellowstone River'
Yellowstone.flowline = UYRW.flowline[UYRW.flowline$gnis_name == 'Yellowstone River',]

# fix self-intersection issues
Yellowstone.flowline = st_make_valid(st_union(Yellowstone.flowline, by_feature=FALSE))
```

@details # coordinate reference system


```r
# determine the extent of the watershed in terms of long/lat coordinates
UYRW.geo.xlim = st_bbox(UYRW.poly)[c(1,3)]
UYRW.geo.ylim = st_bbox(UYRW.poly)[c(2,4)]
```

For now I use Transverse Mercator (UTM), as recommended in the QSWAT+ manual. This is regarded as "close enough"
to an equal area projection for modeling purposes.


```r
# determine the EPSG code for the UTM zone (12N) on which our study area is centred
UYRW.UTM.epsg = 32700 - round((45+mean(UYRW.geo.ylim))/90)*100 + round((183+mean(UYRW.geo.xlim))/6)

# and for plotting purposes, we may want the epsg code for latitude/longitude
latlong.epsg = 4269
```

Reproject all geometries to from latitude/longitude to UTM


```r
UYRW.catchment = st_transform(UYRW.catchment, UYRW.UTM.epsg)
UYRW.flowline = st_transform(UYRW.flowline, UYRW.UTM.epsg)
UYRW.waterbody = st_transform(UYRW.waterbody, UYRW.UTM.epsg)
UYRW.poly = st_transform(UYRW.poly, UYRW.UTM.epsg)
Yellowstone.flowline = st_transform(Yellowstone.flowline, UYRW.UTM.epsg)
```

@details # visualization
All of the data needed to create the flowlines plot in our readme are now loaded into R. The following code creates that plot


```r
# create a directory for storing graphics
graphics.dir = here('graphics')
if(!dir.exists(graphics.dir))
{
  dir.create(graphics.dir, recursive=TRUE)
}

# determine the extent in terms of x/y coordinates
UYRW.xlim = st_bbox(UYRW.poly)[c(1,3)]
UYRW.ylim = st_bbox(UYRW.poly)[c(2,4)]

# define a padded bounding box for plotting
cex.xlim = 1.8
cex.ylim = 1.1
UYRW.xlim.larger = UYRW.xlim + (cex.xlim-1)*c(-1,1)*diff(UYRW.xlim)/2
UYRW.ylim.larger = UYRW.ylim + (cex.ylim-1)*c(0,1)*diff(UYRW.ylim)/2

# plot the watershed flowlines and water bodies as a png file
```

```r
# determine resonable dimensions for output
flowlines.png.res = round(c(diff(UYRW.xlim.larger), diff(UYRW.ylim.larger))/100)

# render/write the plot
png(file.path(graphics.dir, 'UYRW_flowlines.png'), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)
  tm_shape(UYRW.poly, xlim=UYRW.xlim.larger, ylim=UYRW.ylim.larger) + 
    tm_polygons(col='greenyellow', border.col='yellowgreen') +
  tm_shape(UYRW.flowline) +
    tm_lines(col='dodgerblue3') +
  tm_shape(Yellowstone.flowline) +
    tm_lines(col='dodgerblue4', lwd=2) +
  tm_shape(UYRW.waterbody) + 
    tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
  tm_shape(BigTimber.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='right', xmod=5, size=0.8) +
  tm_shape(Livingston.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='left', xmod=-7, size=0.8) +
  tm_shape(Emigrant.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='left', xmod=-7, size=0.8) +
  tm_shape(CorwinSprings.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='left', xmod=-9, size=0.8) +
  tm_shape(TowerRoosevelt.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='right', xmod=12, size=0.8) +
  tm_shape(CanyonVillage.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='left', xmod=-7, size=0.8) +
  tm_shape(YellowstoneLake.pt) +   
    tm_dots(size=0.2, col='red') +
    tm_text('name', just='left', xmod=-8, size=0.8) +
  tm_grid(n.x=4, n.y=5, projection=latlong.epsg, alpha=0.5) +
  tm_layout(title='Upper Yellowstone River Watershed',
            title.position=c('center', 'TOP'),
            frame=FALSE) 
dev.off()
```


