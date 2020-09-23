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
#' over plot layouts. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(raster)
library(gdalUtilities)
library(grid)

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
    description='GAP/LANDFIRE land classification raster, maps to landuse_csv and swat_landuse_lookup'), 
  
  # lookup table for integer codes in the SWAT+ land use raster
  c(name='swat_landuse_lookup',
    file=file.path(out.subdir, 'swat_landuse_lookup.csv'), 
    type='CSV',
    description='swat_tif integer code lookup table for SWAT+, a much simplified version of landuse_csv'), 
  
  # aesthetic parameters for plotting
  c(name='pars_tmap',
    file=file.path(data.dir, 'tmap_get_landuse.rds'), 
    type='R list object', 
    description='parameters for writing png plots using tmap and tm_save'),
  
  # graphic showing SWAT+ land use classifications derived from GAP/LANDFIRE for the UYRW
  c(name='img_landuse',
    file=file.path(graphics.dir, 'landuse.png'),
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
  
  # verify that 'Value' matches raster values in correct order then write to disk
  all(landuse.tab$Value == landuse.rat$ID)
  landuse.csv.path = here(my_metadata('get_landuse')['landuse_csv', 'file'])
  write.csv(landuse.tab, landuse.csv.path, row.names=FALSE)
  
} else {
  
  # load from disk 
  landuse.tif = raster(here(my_metadata('get_landuse')['landuse_tif', 'file']))
  landuse.tab = read.csv(here(my_metadata('get_landuse')['landuse_csv', 'file']))
}



#'
#' ## visualization
#' 

#' create a simplified table, showing only the 
#' 
#' nvc.vals = setNames(1:length(unique(landuse.attr$NVC_CLASS)), unique(landuse.attr$NVC_CLASS))

# create simplified raster showing only the NVC class over the UYRW area
nvc.classes = unique(landuse.tab$NVC_CLASS)
nvc.class.lookup = cbind(landuse.tab$Value, match(landuse.tab$NVC_CLASS, nvc.classes))
nvc.class.tif = mask(reclassify(landuse.tif, nvc.class.lookup), as(uyrw.poly, 'Spatial'))

# load the plotting parameters used in get_dem.R, modify for this plot
tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))

#' Set up the aesthetics to use for these types of plots
if(!file.exists(here(my_metadata('get_landuse')['pars_tmap', 'file'])))
{
  # define a palette 
  mypal = setNames(c(forest='darkolivegreen3', 
                     shrub='darkolivegreen1',
                     desert='gold1',
                     polar='turquoise1',
                     aquatic='seagreen1',
                     rock='tan1',
                     sparse='darkolivegreen4',
                     agriculture='darkorange1',
                     introduced='violetred1',
                     disturbed='pink',
                     water='steelblue1',
                     developed='firebrick1'), 1:nc)
  
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


# plot land use raster as a png file
if(!file.exists(here(my_metadata('get_landuse')['img_landuse', 'file'])))
{
  # build the raster plot, omit legend
  tmap.landuse = tm_shape(nvc.class.tif, raster.downsample=FALSE, bbox=st_bbox(uyrw.poly)) +
    tm_raster(title='NVC class', style='cat', palette=mypal) +
    tm_shape(uyrw.poly, lwd=0.5) +
      tm_borders(col='black') +
    tmap.pars$layout +
    tm_layout(legend.show=FALSE)
  
  # build the legend grob separately, using a dummy raster
  legend.title = 'LANDFIRE/GAP classifications in the UYRW'
  nc = length(nvc.classes) 
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


#+ include=FALSE
# Development code
#my_markdown('get_landuse')
