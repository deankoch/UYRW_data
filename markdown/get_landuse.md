get\_landuse.R
================
Dean Koch
2021-03-10

**Mitacs UYRW project**

**get\_landuse**: downloads the USGS
[GAP/LANDFIRE](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-overview)
National Terrestrial Ecosystems dataset, an approximately 30m x 30m
gridded land cover map of the US (see metadata
[here](https://www.sciencebase.gov/catalog/file/get/573cc51be4b0dae0d5e4b0c5?f=__disk__5d/11/f4/5d11f4366a3402f7e0d23ffa77258a4e12f04809&transform=1)).
Each pixel is mapped to a US National Vegetation Classification (NVC)
code, providing an inventory of typical plant assemblages that we can
use to assign SWAT+ plant growth paramaters.

This code fetches the data, warps to our reference coordinate system,
and prepares input files required by SWAT and SWAT+

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
should be run before this script.

## libraries

Start by sourcing two helper scripts
([helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_get\_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))
```

The base package
[`grid`](https://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid-package.html)
allows more control over plot layouts, and
[`colorspace`](https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html)
provides some predefined color palettes for plotting.

``` r
library(raster)
library(grid)
library(colorspace)
```

    ## 
    ## Attaching package: 'colorspace'

    ## The following object is masked from 'package:raster':
    ## 
    ##     RGB

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
landuse.meta = my_metadata('get_landuse', files.towrite, overwrite=TRUE)
```

    ## [1] "> writing metadata to: data/get_landuse_metadata.csv"

``` r
print(landuse.meta[, c('file', 'type')])
```

    ##                                                          file          type
    ## landuse_source                       data/source/GAP_LANDFIRE     directory
    ## landuse_csv                         data/prepared/landuse.csv           CSV
    ## landuse_tif                         data/prepared/landuse.tif       GeoTIFF
    ## swat_landuse_tif               data/prepared/swat_landuse.tif       GeoTIFF
    ## swat_landuse_lookup data/prepared/swatplus_landuse_lookup.csv           CSV
    ## pars_tmap                           data/tmap_get_landuse.rds R list object
    ## img_landuse                              graphics/landuse.png   png graphic
    ## img_swat_landuse                    graphics/swat_landuse.png   png graphic
    ## metadata                        data/get_landuse_metadata.csv           CSV

Filenames (with descriptions) are now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_landuse_metadata.csv)
in the `/data` directory.

Load some of the data prepared earlier

``` r
# load CRS info list and watershed polygon from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
dem.tif = raster(here(my_metadata('get_dem')['dem', 'file']))
```

## download the landuse raster and attribute table from USGS

The GAP/LANDFIRE data can be downloaded in a zip archive covering
various geographic extents from the [Land Cover Data
Download](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-download?qt-science_center_objects=0#qt-science_center_objects)
page. In our case, the watershed is covered by the [Great Northern
Landscape Conservation Cooperative
region](https://www.usgs.gov/media/images/conterminous-us-land-cover-data-map-lcc):

``` r
# define the USGS URL (find this by navigating the USGS data download site linked above)
landuse.domain = 'https://www.sciencebase.gov/catalog/file/get/592f00bce4b0e9bd0ea793c6'
landuse.prefix = '?f=__disk__ea%2F46%2F58%2Fea4658ba4c31bdcce62574dac174616cb612bf29'
landuse.url = paste0(landuse.domain, landuse.prefix)

# skip download if zip contents are already in the expected data subdirectory
if(!file.exists(here(landuse.meta['landuse_source', 'file'])))
{
  # define a temporary file for the zip and an output directory for zip contents
  temp.path = paste0(normalizePath(tempdir(), winslash='/'), '.zip')
  landuse.out.path = here(landuse.meta['landuse_source', 'file'])
  
  # download/extract the zip archive
  download.file(landuse.url, temp.path, mode='wb')
  ex.path = unzip(temp.path, exdir=landuse.out.path)
  
  # remove the zip file
  unlink(temp.path)
}
```

## process data

Some information on the NVC classifications is [available
here](http://usnvc.org/data-standard/natural-vegetation-classification/)

Warp/crop the land use raster, load integer lookup codes and attributes,
and save to disk

``` r
if(any(!file.exists(here(landuse.meta[c('landuse_tif', 'landuse_csv'), 'file']))))
{
  # define paths for temporary and output geotiff files
  temp.tif = paste0(tempfile(), '.tif')
  landuse.tif.path = here(landuse.meta['landuse_tif', 'file'])
  
  # identify the land-use raster on disk (it is the only geotiff in the zip)
  landuse.src.dir = here(landuse.meta['landuse_source', 'file'])
  landuse.src.fname = list.files(landuse.src.dir)[endsWith(list.files(landuse.src.dir), '.tif')]
  landuse.src.path = file.path(landuse.src.dir, landuse.src.fname)
  
  # reprojection and translate/resample warps are done separately: first warp to our CRS via tempfile...
  gdalwarp(srcfile=landuse.src.path, 
           dstfile=temp.tif,
           t_srs=paste0('EPSG:', crs.list$epsg),
           overwrite=TRUE)
  
  # ...then warp/crop to match target extent and resolution, and write final output geotiff
  gdalwarp(srcfile=temp.tif, 
           dstfile=landuse.tif.path,
           te=st_bbox(dem.tif),
           tr=res(dem.tif),
           overwrite=TRUE)
  
  # open the raster and extract its attributes table
  landuse.tif = raster(landuse.tif.path)
  landuse.rat = levels(landuse.tif)[[1]]
  
  # open raster attributes table .txt file and take subset corresponding to the cropped raster
  landuse.tab.fname = list.files(landuse.src.dir)[endsWith(list.files(landuse.src.dir), '_Attributes.txt')]
  landuse.tab.path = file.path(landuse.src.dir, landuse.tab.fname)
  landuse.tab = read.delim(landuse.tab.path, header=TRUE) %>% filter(Value %in% landuse.rat$ID)
  
  # verify that 'Value' matches raster values in correct order
  all(landuse.tab$Value == landuse.rat$ID)
  
  # add n_count column, counting number of pixels occupied by each `Value` code
  landuse.freq = setNames(data.frame(freq(landuse.tif)), c('Value', 'n_uyrw'))
  landuse.tab = left_join(landuse.tab, landuse.freq, by='Value') 
  
  # omit table entries not found on land use raster (after masking to UYRW)
  landuse.tab = landuse.tab %>% filter(!is.na(n_uyrw))
  
  # write to disk
  landuse.csv.path = here(landuse.meta['landuse_csv', 'file'])
  write.csv(landuse.tab, landuse.csv.path, row.names=FALSE)
  
} else {
  
  # load from disk 
  landuse.tif = raster(here(landuse.meta['landuse_tif', 'file']))
  landuse.tab = read.csv(here(landuse.meta['landuse_csv', 'file']))
}
```

## QSWAT+ input files

SWAT+ comes packaged with a plant growth database table, `plants_plt` in
“swatplus\_datasets.sqlite”. This includes a large number of growth
parameter sets for agricultural crops (eg. `alfalfa`), and a collection
of generic classes for wild plant assemblages (eg.
`deciduous_broadleaf_forest`).

We will use these codes to parametrize HRUs based on BVC information
about land use derived from the LANDFIRE/GAP dataset. This section maps
[NVC biogeography
classifications](http://usnvc.org/data-standard/natural-vegetation-classification/)
to entries of the SWAT+ plant growth database, generating the landuse
GeoTIFF raster and lookup table required by QSWAT+.

The raster will also work with SWAT, but the lookup table will not. At
this time, the plant growth parameter set that ships with SWAT is a
smaller subset of the one included in SWAT+, and is coded differently
(requiring 4-letter uppercase codes). The plant codes assigned by this
chunk will therefore not be recognized when running QSWAT. SWAT Users
will need to import the missing parameters from the `plants_plt`
database in SWAT+ (into ‘QSWATRef2012.mdb’), and convert the codes in
their lookup table. The script ‘make\_qswat’ does this automatically,
later on.

``` r
# define files to write in this chunk and proceed only if they don't exist
swat.lookup.path = here(landuse.meta['swat_landuse_lookup', 'file'])
swat.landuse.tif.path = here(landuse.meta['swat_landuse_tif', 'file'])
if(any(!file.exists(c(swat.lookup.path, swat.landuse.tif.path))))
{
  
  # DEBUGGING: Try adding lakes via WATR landuse tag
  
  # copy relevant fields of land use table, omitting 'Open Water' which is dealt with separately by SWAT+
  #nvc.df = landuse.tab %>% filter(NVC_DIV != 'Open Water') %>% select(Value, NVC_DIV, NVC_MACRO, NVC_GROUP)
  
  # append SWAT+ plant codes using a helper function
  #nvc.df = my_plants_plt(nvc.df)
  nvc.df = my_plants_plt(landuse.tab)
  
  # build and write the output CSV for QSWAT+
  swatcodes.unique = unique(nvc.df$swatcode)
  swat.lookup = data.frame(Value=1:length(swatcodes.unique), Landuse=swatcodes.unique)
  write.csv(swat.lookup, swat.lookup.path, row.names=FALSE)
  
  # build the reclassification matrix for the raster (mapping open water to NA)
  #waterval = landuse.tab$Value[landuse.tab$NVC_DIV=='Open Water']
  idx.landuse = match(nvc.df$swatcode, swat.lookup$Landuse)
  #rcl = cbind(c(nvc.df$Value, waterval), c(idx.landuse, NA))
  rcl = cbind(nvc.df$Value, idx.landuse)
  
  # build the raster, crop and mask to UYRW, and write the output GeoTIFF
  swat.landuse.tif = reclassify(landuse.tif, rcl)
  swat.landuse.tif = mask(crop(swat.landuse.tif, as(uyrw.poly, 'Spatial')), as(uyrw.poly, 'Spatial'))
  writeRaster(swat.landuse.tif, swat.landuse.tif.path, overwrite=TRUE, NAflag=tif.na.val)
  
} else {
  
  # load from disk 
  swat.landuse.tif = raster(swat.landuse.tif.path)
  swat.lookup = read.csv(swat.lookup.path)
}
```

## visualization

Set up the aesthetics to use for these types of plots

``` r
if(!file.exists(here(landuse.meta['pars_tmap', 'file'])))
{
  # load the plotting parameters used in get_dem.R, modify for this plot
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  
  # adjust graphic dimensions
  tmap.pars$png['h'] = 4800
  tmap.pars$png['w'] = 2400
  tmap.pars$png['pt'] = 32
  tmap.pars$layout = tmap.pars$layout + tm_layout(scale=3)
    
  # save to disk
  saveRDS(tmap.pars, here(landuse.meta['pars_tmap', 'file']))
  
} else {
  
  # load from disk
  tmap.pars = readRDS(here(landuse.meta['pars_tmap', 'file']))
} 
```

First we make a plot of the 10 or so NVC physiognomy classes in the
UYRW. In the code (below) we create the raster and legend plot objects
separately, then combine them together using (base) R’s `grid` package.
This works around a bug in which certain legend labels for factor levels
in a raster plot are scrambled or omitted.
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/landuse.png)

``` r
if(!file.exists(here(landuse.meta['img_landuse', 'file'])))
{
  # create a simplified table containing the NVC physiognomy classes in the UYRW area
  nvc.classes = unique(landuse.tab$NVC_CLASS)
  nvc.class.lookup = cbind(landuse.tab$Value, match(landuse.tab$NVC_CLASS, nvc.classes))
  nvc.class.tif = mask(reclassify(landuse.tif, nvc.class.lookup), as(uyrw.poly, 'Spatial'))
  
  # define a palette 
  nc = length(nvc.classes) 
  mypal = setNames(c(shrub='darkolivegreen1',
                     forest='darkolivegreen3', 
                     desert='gold1',
                     polar='turquoise1',
                     rock='tan1',
                     sparse='darkolivegreen4',
                     agriculture='darkorange1',
                     introduced='violetred1',
                     disturbed='pink',
                     water='steelblue1',
                     developed='firebrick1'), 1:nc)
  
  # build the raster plot, omit legend
  tmap.landuse = tm_shape(nvc.class.tif, raster.downsample=FALSE, bbox=st_bbox(uyrw.poly)) +
    tm_raster(style='cat', palette=mypal) +
    tm_shape(uyrw.poly, lwd=0.5) +
      tm_borders(col='black') +
    tmap.pars$layout +
    tm_layout(legend.show=FALSE)
  
  # build the legend grob separately, using a dummy raster
  legend.title = 'NVC physiognomy classifications in the UYRW'
  dummy.tif = raster(matrix(1:nc, nc, nc), crs=CRS(paste0('+init=epsg:', crs.list$epsg)))
  legend.tm = tm_shape(dummy.tif) +
    tm_raster('layer', style='cat', labels=nvc.classes, palette=mypal, title=legend.title) +
    tm_layout(legend.only=TRUE,
              legend.text.size=4,
              legend.title.size=6)
              
  # start the png graphics device
  png(filename=here(landuse.meta['img_landuse', 'file']), 
      width=tmap.pars$png['w'], 
      height=tmap.pars$png['h'], 
      pointsize=tmap.pars$png['pt'])

    # set up a grid layout for the raster and legend panels
    grid.newpage()
    page.layout = grid.layout(nrow=2, ncol=1, heights=c(3,1))
    pushViewport(viewport(layout=page.layout))
    
    # print the plot elements
    print(tmap.landuse, vp=viewport(layout.pos.row=1))
    print(legend.tm, vp=viewport(layout.pos.row=2))

  # save and close plot device
  dev.off()
}
```

Next plot the assigned SWAT+ plant codes in the UYRW (based on NVC
biogeography classes). We use the `colorspace` package to create
palettes for the large number of different land use types. Note that
areas of open water are assigned a no-data value in the SWAT+ land use
raster
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_landuse.png)

``` r
if(!file.exists(here(landuse.meta['img_swat_landuse', 'file'])))
{
  # create lookup table for SWAT+ plant code descriptions with legend labels
  plt = my_plants_plt(landuse.tab) %>% 
    mutate(Landuse=swatcode) %>% 
    mutate(label=gsub('_', ' ', swatdesc)) %>% 
    select(Landuse, label) %>% 
    distinct %>%
    right_join(swat.lookup, by='Landuse')
  plt.labels = paste0(plt$Value, '. ',  plt$Landuse, ' (', plt$label, ')') 
  
  # build sub-palettes for different vegetation types; try colorspace::hcl_palettes(plot = TRUE)
  mypal.list = list(
    
    #wetlands
    setNames(sequential_hcl(5, palette='Tealgrn')[4:5], c('wetf', 'wehb')),
    
    # mixed/deciduous forest
    setNames(sequential_hcl(7, palette='Emrld')[6:7], c('frst_tems', 'popl')),
    
    # coniferous forest
    setNames(sequential_hcl(7, palette='Emrld')[1:5], c('frse_tems', 'frse_test', 'pine', 'ldgp', 'wspr')),
    
    # brushland
    setNames(sequential_hcl(6, palette='BurgYl')[1:3], c('rngb_tems', 'rngb_test', 'migs')),
    
    # grassland
    setNames(sequential_hcl(6, palette='Peach')[1:3], c('rnge_tems', 'rnge_test', 'fesc')),
    
    # tundra
    setNames(sequential_hcl(3, palette='Purp')[1:2], c('tubg', 'tuhb')),
    
    # sparse/bare
    setNames(sequential_hcl(5, palette='BrwnYl')[4:5], c('barr', 'bsvg')),
    
    # agricultural
    setNames(sequential_hcl(2, palette='PinkYl')[1], c('agrl'))
    
  )
  
  # compile them into a single custom palette, in correct order
  nc = nrow(plt)
  mypal = setNames(unlist(my.pal.list)[match(plt$Landuse, names(unlist(my.pal.list)))], 1:nc)
  
  # the following code is useful for building and previewing custom palettes
  # print(plt)
  # hcl_palettes(plot = TRUE)
  # image(1:nc, 1, as.matrix(1:nc), col=mypal, xaxt='n', yaxt='n', xlab='', ylab='')
  # axis(side=1, at=(1:length(mypal)), labels=names(mypal), las=2)
  
  # build the raster plot, omit legend
  tmap.landuse = tm_shape(swat.landuse.tif, raster.downsample=FALSE, bbox=st_bbox(uyrw.poly)) +
    tm_raster(style='cat', palette=mypal) +
    tm_shape(uyrw.poly, lwd=0.5) +
    tm_borders(col='black') +
    tmap.pars$layout +
    tm_layout(legend.show=FALSE)
  
  # build the legend grob separately, using a dummy raster
  legend.title = 'SWAT+ plant codes in the UYRW'
  dummy.tif = raster(matrix(1:nc, nc, nc), crs=CRS(paste0('+init=epsg:', crs.list$epsg)))
  legend.tm = tm_shape(dummy.tif) +
    tm_raster('layer', style='cat', labels=plt.labels, palette=mypal, title=legend.title) +
    tm_layout(legend.only=TRUE,
              legend.text.size=4,
              legend.title.size=6)
  
  # start the png graphics device
  png(filename=here(landuse.meta['img_swat_landuse', 'file']), 
      width=tmap.pars$png['w'], 
      height=tmap.pars$png['h'], 
      pointsize=tmap.pars$png['pt'])
  
    # set up a grid layout for the raster and legend panels
    grid.newpage()
    page.layout = grid.layout(nrow=2, ncol=1, heights=c(3,1))
    pushViewport(viewport(layout=page.layout))
    
    # print the plot elements
    print(tmap.landuse, vp=viewport(layout.pos.row=1))
    print(legend.tm, vp=viewport(layout.pos.row=2))
  
  # save and close plot device
  dev.off()
}
```
