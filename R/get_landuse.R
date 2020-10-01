#' ---
#' title: "get_landuse.R"
#' author: "Dean Koch"
#' date: "September 22, 2020"
#' output: github_document
#' ---
#'
#' **MITACS UYRW project**
#' 
#' **get_landuse**: downloads the USGS
#' [GAP/LANDFIRE](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-overview)
#' National Terrestrial Ecosystems dataset, an approximately 30m x 30m gridded land cover map of the US (see metadata 
#' [here](https://www.sciencebase.gov/catalog/file/get/573cc51be4b0dae0d5e4b0c5?f=__disk__5d/11/f4/5d11f4366a3402f7e0d23ffa77258a4e12f04809&transform=1)),
#' warps to our reference coordinate system, and prepares SWAT+ AW input files (work in progress).
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' should be run before this script.

#'
#' ## libraries
#' [`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html) provides a wrapper
#' for GDAL calls to warp the land use raster, and the base package
#' [`grid`](https://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid-package.html) allows more control
#' over plot layouts. 
#' See the [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries 
library(here)
source(here('R/get_helperfun.R'))
library(raster)
library(gdalUtilities)
library(grid)
library(colorspace)


#'
#' ## project data

# This list describes all of the files created by this script:
files.towrite = list(
  
  # land use zip file contents downloaded from the USGS website
  c(name='landuse_source',
    file=file.path(src.subdir, 'GAP_LANDFIRE'), 
    type='directory',
    description='contents of GAP/LANDFIRE zip archive for "Great Northern" region'), 
  
  # attribute table for the GAP/LANDFIRE database in the UYRW
  c(name='landuse_csv',
    file=file.path(out.subdir, 'landuse.csv'), 
    type='CSV',
    description='attributes for each raster land use integer key in the UYRW'), 
  
  # GAP/LANDFIRE raster mapping land use in the watershed 
  c(name='landuse_tif',
    file=file.path(out.subdir, 'landuse.tif'), 
    type='GeoTIFF',
    description='GAP/LANDFIRE land classification raster, maps to landuse_csv'),
  
  # lookup table for integer codes in the SWAT+ land use raster
  c(name='swat_landuse_lookup',
    file=file.path(out.subdir, 'swat_landuse_lookup.csv'), 
    type='CSV',
    description='swat_landuse_tif integer code lookup table, maps to plants_plt SWAT+ dataset'), 
  
  # SWAT+ plant codes raster for the watershed, based on NVC biogeography class 
  c(name='swat_landuse_tif',
    file=file.path(out.subdir, 'swat_landuse.tif'), 
    type='GeoTIFF',
    description='SWAT+ land use classification, maps to swat_landuse_lookup'),
  
  # aesthetic parameters for plotting
  c(name='pars_tmap',
    file=file.path(data.dir, 'tmap_get_landuse.rds'), 
    type='R list object', 
    description='parameters for writing png plots using tmap and tm_save'),
  
  # graphic showing NVC physiognomy classifications derived from GAP/LANDFIRE for the UYRW
  c(name='img_landuse',
    file=file.path(graphics.dir, 'landuse.png'),
    type='png graphic',
    description='image of SWAT+ land use values in the UYRW'),
  
  # graphic showing SWAT+ land use classifications derived from NVC biogeography classes
  c(name='img_swat_landuse',
    file=file.path(graphics.dir, 'swat_landuse.png'),
    type='png graphic',
    description='image of SWAT+ land use values in the UYRW')
)

# write this information to disk
my_metadata('get_landuse', files.towrite, overwrite=TRUE)
 
#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_landuse_metadata.csv)
#' in the `/data` directory.

#' Load some of the data prepared earlier 
# load CRS info list and watershed polygons from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))

#'
#' ## Download the landuse raster and attribute table from USGS
#' The GAP/LANDFIRE data can be downloaded in a zip archive covering various geographic extents from the 
#' [Land Cover Data Download](https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-download?qt-science_center_objects=0#qt-science_center_objects)
#' page. In our case, the watershed is covered by the
#' [Great Northern Landscape Conservation Cooperative region](https://www.usgs.gov/media/images/conterminous-us-land-cover-data-map-lcc):

# define the USGS URL (find this by navigating the USGS data download site linked above)
landuse.domain = 'https://www.sciencebase.gov/catalog/file/get/592f00bce4b0e9bd0ea793c6'
landuse.prefix = '?f=__disk__ea%2F46%2F58%2Fea4658ba4c31bdcce62574dac174616cb612bf29'
landuse.url = paste0(landuse.domain, landuse.prefix)

# skip download if zip contents are already in the expected data subdirectory
if(!file.exists(here(my_metadata('get_landuse')['landuse_source', 'file'])))
{
  # define a temporary file for the zip and an output directory for zip contents
  temp.path = paste0(normalizePath(tempdir(), winslash='/'), '.zip')
  landuse.out.path = here(my_metadata('get_landuse')['landuse_source', 'file'])
  
  # download/extract the zip archive
  download.file(landuse.url, temp.path, mode='wb')
  ex.path = unzip(temp.path, exdir=landuse.out.path)
  
  # remove the zip file
  unlink(temp.path)
}

#'
#' ## Process data 
#' Some information on the NVC classifications is
#' [available here](http://usnvc.org/data-standard/natural-vegetation-classification/)
#' 
#' Warp/crop the land use raster, load integer lookup codes and attributes, and save to disk
if(any(!file.exists(here(my_metadata('get_landuse')[c('landuse_tif', 'landuse_csv'), 'file']))))
{
  # define paths for temporary and output geotiff files
  temp.tif = paste0(tempfile(), '.tif')
  landuse.tif.path = here(my_metadata('get_landuse')['landuse_tif', 'file'])
  
  # identify the land-use raster on disk (it is the only geotiff in the zip)
  landuse.src.dir = here(my_metadata('get_landuse')['landuse_source', 'file'])
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
  landuse.csv.path = here(my_metadata('get_landuse')['landuse_csv', 'file'])
  write.csv(landuse.tab, landuse.csv.path, row.names=FALSE)
  
} else {
  
  # load from disk 
  landuse.tif = raster(here(my_metadata('get_landuse')['landuse_tif', 'file']))
  landuse.tab = read.csv(here(my_metadata('get_landuse')['landuse_csv', 'file']))
}

#'
#' ## SWAT input files
#' 
#' 
#' SWAT+ comes packaged with a plant growth database (table `plants_plt` in "swatplus_datasets.sqlite"
#' which includes a large number of parameter sets for agricultural crops (eg. `alfalfa`), and a smaller
#' collection of generic classes for wild plant assemblages (eg. `deciduous_broadleaf_forest`). These are
#' used to parametrize HRUs based on information about land use, such as the classifications found in the
#' LANDFIRE/GAP dataset.
#' 
#' In this section, we map the
#' [NVC biogeography classifications](http://usnvc.org/data-standard/natural-vegetation-classification/)
#' to entries of the SWAT plant growth database, generating the landuse GeoTIFF raster and lookup table
#' required by SWAT+ AW.

if(any(!file.exists(here(my_metadata('get_landuse')[c('swat_landuse_tif', 'swat_landuse_lookup'), 'file']))))
{
  # copy relevant fields of land use table, omitting 'Open Water' which is dealt with separately by SWAT+
  nvc.df = landuse.tab %>% filter(NVC_DIV != 'Open Water') %>% select(Value, NVC_DIV, NVC_MACRO, NVC_GROUP)
  
  # append SWAT+ plant codes using a helper function
  nvc.df = my_plants_plt(nvc.df)
  
  # build and write the output CSV
  swatcodes.unique = unique(nvc.df$swatcode)
  swat.lookup = data.frame(Value=1:length(swatcodes.unique), Landuse=swatcodes.unique)
  swat.lookup.path = here(my_metadata('get_landuse')['swat_landuse_lookup', 'file'])
  write.csv(swat.lookup, swat.lookup.path, row.names=FALSE)
  
  # build the reclassification matrix for the raster (mapping open water to NA)
  waterval = landuse.tab$Value[landuse.tab$NVC_DIV=='Open Water']
  idx.landuse = match(nvc.df$swatcode, swat.lookup$Landuse)
  rcl = cbind(c(nvc.df$Value, waterval), c(idx.landuse, NA))
  
  # build the raster, crop and mask to UYRW, and write the output GeoTIFF
  swat.landuse.tif = reclassify(landuse.tif, rcl)
  swat.landuse.tif = mask(crop(swat.landuse.tif, as(uyrw.poly, 'Spatial')), as(uyrw.poly, 'Spatial'))
  writeRaster(swat.landuse.tif, here(my_metadata('get_landuse')['swat_landuse_tif', 'file']), overwrite=TRUE)
  
} else {
  
  # load from disk 
  swat.landuse.tif = raster(here(my_metadata('get_landuse')['swat_landuse_tif', 'file']))
  swat.lookup = read.csv(here(my_metadata('get_landuse')['swat_landuse_lookup', 'file']))
}

#'
#' ## visualization
#' 

#' Set up the aesthetics to use for these types of plots
if(!file.exists(here(my_metadata('get_landuse')['pars_tmap', 'file'])))
{
  # load the plotting parameters used in get_dem.R, modify for this plot
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  
  # adjust graphic dimensions
  tmap.pars$png['h'] = 4800
  tmap.pars$png['w'] = 2400
  tmap.pars$png['pt'] = 32
  tmap.pars$layout = tmap.pars$layout + tm_layout(scale=3)
    
  # save to disk
  saveRDS(tmap.pars, here(my_metadata('get_landuse')['pars_tmap', 'file']))
  
} else {
  
  # load from disk
  tmap.pars = readRDS(here(my_metadata('get_landuse')['pars_tmap', 'file']))
} 

# plot NVC classifications raster as a png file
if(!file.exists(here(my_metadata('get_landuse')['img_landuse', 'file'])))
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
  png(filename=here(my_metadata('get_landuse')['img_landuse', 'file']), 
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

#' ![land use of the UYRW](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/landuse.png)


# plot the SWAT+ plant codes in the UYRW
if(!file.exists(here(my_metadata('get_landuse')['img_swat_landuse', 'file'])))
{
  # create lookup table for SWAT+ plant code descriptions with legend labels
  plt = my_plants_plt(landuse.tab) %>% 
    mutate(Landuse=swatcode) %>% 
    mutate(label=gsub('_', ' ', swatdesc)) %>% 
    select(Landuse, label) %>% 
    distinct %>%
    right_join(swat.lookup, by='Landuse')
  plt.labels = paste0(plt$Value, '. ',  plt$Landuse, ' (', plt$label, ')') 
  
  # create a palette for these categories
  nc = nrow(plt)
  forest.pal = terrain_hcl(15)
  range.pal = heat_hcl(5)
  other.pal = rainbow_hcl(10)
  mypal = setNames(c(rnge_tems=range.pal[4],
                     frse_test=forest.pal[7],
                     frse_tems=forest.pal[6],
                     popl=forest.pal[1],
                     pine=forest.pal[5],
                     ldgp=forest.pal[4],
                     wspr=forest.pal[3],
                     frst_tems=forest.pal[2],
                     rngb_tems=range.pal[1],
                     wetf=other.pal[5],
                     rngb_test=range.pal[2],
                     wehb=other.pal[6],
                     migs=range.pal[3],
                     rnge_test=range.pal[5],
                     bsvg=other.pal[9],
                     tuhb=other.pal[7],
                     tubg=other.pal[8],
                     barr=other.pal[10],
                     agrl='darkorange1',
                     fesc='gold1'), 1:nc)
  
  
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
  png(filename=here(my_metadata('get_landuse')['img_swat_landuse', 'file']), 
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

#' ![SWAT plant codes](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/swat_landuse.png)





#+ include=FALSE
# Development code
#my_markdown('get_landuse')
