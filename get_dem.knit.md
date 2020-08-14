---
title: "get_dem.R"
author: "Dean Koch"
date: "August 14, 2020"
output: github_document
---

**MITACS UYRW project**

**get_DEM**: download a DEM and warp to our reference coordinate system 

[get_basins.R](https://github.com/deankoch/URYW_data/blob/master/get_basins.knit.md) and 
[get_weatherstations.R](https://github.com/deankoch/URYW_data/blob/master/get_weatherstations.knit.md), 
should be run before this script.

## libraries


```r
library(raster)
library(gdalUtils)
library(FedData)
library(sf)
library(tmap)
library(here)
```


## project data


```r
# project directory names
graphics.dir = 'graphics'
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# load metadata csv, CRS info list and watershed geometries from disk
basins.metadata.df = read.csv(here('data/basins_metadata.csv'), header=TRUE, row.names=1)
weatherstation.metadata.df = read.csv(here('data/weatherstation_metadata.csv'), header=TRUE, row.names=1)
crs.list = readRDS(here(basins.metadata.df['crs', 'file']))
uyrw.poly = readRDS(here(basins.metadata.df['boundary', 'file']))
uyrw.padded.poly = readRDS(here(weatherstation.metadata.df['boundary_padded', 'file']))
uyrw.waterbody = readRDS(here(basins.metadata.df['waterbody', 'file']))
uyrw.mainstem = readRDS(here(basins.metadata.df['mainstem', 'file']))
uyrw.flowline = readRDS(here(basins.metadata.df['flowline', 'file']))


# this CSV file documents files written to the project data folder
dem.metadata.file = 'data/dem_metadata.csv'
if(!file.exists(here(dem.metadata.file)))
{
  # filename for DEM downloaded from the USGS website
  ned.file = c(name='ned',
               file=file.path(src.subdir, 'UYRW_NED_1.tif'), 
               type='GeoTIFF raster file',
               description='mosaic of elevation tiles from the NED (unchanged)')
  
  # filename for DEM after warp and crop
  dem.file = c(name='dem',
               file=file.path(out.subdir, 'dem.tif'), 
               type='GeoTIFF raster file',
               description='digital elevation map of the UYRW and surrounding area')
  
  # filename for graphic showing DEM for the UYRW
  dem.png.file = c(name='img_dem',
                   file=file.path(graphics.dir, 'dem.png'),
                   type='png graphic',
                   description='image of DEM for the UYRW')

  
  # bind all the individual filename info vectors into a data frame
  dem.metadata.df = data.frame(rbind(ned.file,
                                     dem.file,
                                     dem.png.file), row.names='name')
  
  # save the data frame
  write.csv(dem.metadata.df, here(dem.metadata.file))
  
} else {
  
  # load the data frame
  dem.metadata.df = read.csv(here(dem.metadata.file), header=TRUE, row.names=1)
  
}
```

This list of files and descriptions is now stored as a
[.csv file](https://github.com/deankoch/URYW_data/blob/master/data/dem_metadata.csv)
in the `/data` directory.

## Download the DEM raster

The *get_ned* function from `FedData` retrieves and merge all (12) required elevation tiles from the USGS NED


```r
if(!file.exists(here(dem.metadata.df['ned', 'file'])))
{
  # We will downloaded the DEM for an extended UYRW boundary, to allow modeling of nearby weather records 
  boundary.padded.sp = as_Spatial(uyrw.padded.poly)
  
  # This function call downloads tiles to "/RAW", extracts them, and writes the mosaic to "UYRW_NED_1" in "/data/source"
  dem.original.tif = get_ned(template=boundary.padded.sp, label='UYRW', extraction.dir=here(src.subdir))
  
  # remove the temporary files
  unlink(here('RAW'), recursive=TRUE)
  
} else {
  
  # load the NED raster from disk
  dem.original.tif = raster(here(dem.metadata.df['ned', 'file']))
}
```

Warp (gridded CRS transform) and clip the raster to our reference system and study area 


```r
if(!file.exists(here(dem.metadata.df['dem', 'file'])))
{
  # package 'gdalUtils' performs these kinds of operations much faster than `raster`
  gdalwarp(srcfile=here(dem.metadata.df['ned', 'file']), 
           dstfile=here(dem.metadata.df['dem', 'file']),
           s_srs=crs(dem.original.tif), 
           t_srs=paste0('EPSG:', crs.list$epsg),
           #tr=
           #te=raster::bbox(bc.mask.tif), 
           overwrite=TRUE,
           verbose=TRUE)
  
  # load the transformed raster (resolution is approx 30x30m)
  dem.tif = raster(here(dem.metadata.df['dem', 'file']))
  
  # clip to padded boundary
  dem.tif = mask(dem.tif, as_Spatial(uyrw.padded.poly))
  
  # overwrite on disk
  writeRaster(dem.tif, here(dem.metadata.df['dem', 'file']), overwrite=TRUE)

} else {
  
  # load from disk 
  dem.tif = raster(here(dem.metadata.df['dem', 'file']))
  
}
```


## visualization



```r
# create a copy of the dem clipped to exact boundary
dem.tight.tif = mask(dem.tif, as_Spatial(uyrw.poly))

# plot precipitation sensor station locations as a png file
if(!file.exists(here(dem.metadata.df['img_dem', 'file'])))
{
  tmap.dem = tm_shape(dem.tif) +
              tm_raster(palette=gray.colors(100), legend.show=FALSE) +
              tm_shape(dem.tight.tif, raster.downsample=F, style='cont') +
              tm_raster(palette='-viridis', title='elevation (meters)') +
    tm_layout(main.title='Digital elevation map of the UYRW',
              main.title.size=1,
              main.title.position='center',
              legend.title.size=0.7,
              legend.text.size=0.5,
              frame=FALSE,
              title.snap.to.legend=FALSE,
              legend.position=c('left', 'bottom'))
              
  # render/write the plot
  tmap_save(tm=tmap.dem, here(dem.metadata.df['img_dem', 'file']), width=2000, height=2400, pointsize=16)
}
```

![elevation map of the UYRW](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/dem.png)




