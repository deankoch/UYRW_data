get\_dem.R
================
Dean Koch
2020-10-01

**Mitacs UYRW project**

**get\_DEM**: download a DEM and warp to our reference coordinate system

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
should be run before this script.

## libraries

[`FedData`](https://cran.r-project.org/web/packages/FedData/index.html)
is used to fetch the USGS data,
[`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html)
provides a wrapper for GDAL calls to warp the DEM. See the
[get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
for other required libraries

``` r
library(here)
source(here('R/get_helperfun.R'))
library(FedData)
library(gdalUtilities)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
dem.meta = my_metadata('get_dem', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/get_dem_metadata.csv"

``` r
print(dem.meta[, c('file', 'type')])
```

    ##                                 file                type
    ## ned       data/source/UYRW_NED_1.tif GeoTIFF raster file
    ## dem            data/prepared/dem.tif GeoTIFF raster file
    ## pars_tmap      data/tmap_get_dem.rds       R list object
    ## img_dem             graphics/dem.png         png graphic
    ## metadata   data/get_dem_metadata.csv                 CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_dem_metadata.csv)
in the `/data` directory.

Load some of the data prepared earlier

``` r
# load CRS info list and watershed polygon from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
```

## Download the DEM raster

The `get_ned` function from `FedData` retrieves and merge all (12)
required elevation tiles from the USGS NED

``` r
if(!file.exists(here(dem.meta['ned', 'file'])))
{
  # We will downloaded the DEM for an extended UYRW boundary, to allow modeling of nearby weather records 
  boundary.padded.sp = as_Spatial(uyrw.padded.poly)
  
  # This function call downloads tiles to "/RAW", extracts them, and writes the mosaic to "UYRW_NED_1" in "/data/source"
  dem.original.tif = get_ned(template=boundary.padded.sp, label='UYRW', extraction.dir=here(src.subdir))
  
  # remove the temporary files
  unlink(here('RAW'), recursive=TRUE)
  
} else {
  
  # load the NED raster from disk
  dem.original.tif = raster(here(dem.meta['ned', 'file']))
}
```

Warp (gridded CRS transform) and clip the raster to our reference system
and study area

``` r
if(!file.exists(here(dem.meta['dem', 'file'])))
{
  # define a temporary file
  temp.tif = paste0(tempfile(), '.tif')
  
  # package 'gdalUtils' performs these kinds of operations much faster than `raster`
  gdalwarp(srcfile=here(dem.meta['ned', 'file']), 
           dstfile=temp.tif,
           t_srs=paste0('EPSG:', crs.list$epsg),
           overwrite=TRUE)
  
  # load the transformed raster (resolution is approx 30m x 30m)
  dem.tif = raster(temp.tif)
  
  # write to output directory
  writeRaster(dem.tif, here(dem.meta['dem', 'file']), overwrite=TRUE)

} else {
  
  # load from disk 
  dem.tif = raster(here(dem.meta['dem', 'file']))
}
```

## visualization

Set up the aesthetics to use for these types of plots

``` r
if(!file.exists(here(dem.meta['pars_tmap', 'file'])))
{
  # load the plotting parameters used in get_basins.R
  tmap.pars = readRDS(here(my_metadata('get_basins')['pars_tmap', 'file']))
  
  # shrink the vertical length
  tmap.pars$png['h'] = 1800
  
  # the background is too dark for black text
  tmap.pars$layout = tmap.pars$layout + 
    tm_layout(legend.position=c('right', 'top'),
              legend.text.color='white',
              legend.title.color='white')
  
  # save to disk
  saveRDS(tmap.pars, here(dem.meta['pars_tmap', 'file']))
  
} else {
  
  # load from disk
  tmap.pars = readRDS(here(dem.meta['pars_tmap', 'file']))
  
} 
```

plot the DEM raster ![elevation map of the
UYRW](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/dem.png)

``` r
if(!file.exists(here(dem.meta['img_dem', 'file'])))
{
  tmap.dem = tm_shape(dem.tif, raster.downsample=FALSE, bbox=st_bbox(uyrw.poly)) +
      tm_raster(palette='viridis', title='elevation (m)', style='cont') +
    tm_shape(uyrw.poly) +
      tm_borders(col='white') +
    tmap.pars$layout +
    tm_layout(main.title='Digital elevation map of the UYRW')
              
  # render the plot
  tmap_save(tm=tmap.dem, 
            here(dem.meta['img_dem', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}
```
