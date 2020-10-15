#' ---
#' title: "get_basins.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **get_basins**: loads watershed geometry data into R
#' 
#' This code is based on the
#' [`nhdplusTools` vignette](https://usgs-r.github.io/nhdplusTools/articles/nhdplusTools.html). It downloads
#' watershed geometry features from the USGS NHDPlus database (see the 
#' [user guide](https://pubs.er.usgs.gov/publication/ofr20191096) and 
#' [data dictionary](https://tinyurl.com/y54rlqja)), and establishes the coordinate reference system and watershed
#' boundary which are used extensively to define SWAT+ inputs. This is the first step in the project workflow.
#' 
#' The `nhdplusTools` package fetches [NHDPlus products](https://pubs.er.usgs.gov/publication/fs20203033)
#' from the web without having to navigate the
#' [USGS website](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products).
#' This script uses it to assemble some basic info on the hydrology of the UYRW upstream of
#' [Carter's Bridge, Montana](https://myfwp.mt.gov/fwpPub/landsMgmt/siteDetail.action?lmsId=39753508),
#' transforming that data into a more convenient format, and producing some plots giving an overview of the watershed.

#'
#' ## libraries

#' The `here` package defines working directories in a way that makes R code portable
library(here)

#' Start by sourcing the [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' which sets up required libraries, directories, and defines some utility functions
source(here('R/get_helperfun.R'))

#' Some additional packages are needed in this script:
#' [`nhdplusTools`](https://usgs-r.github.io/nhdplusTools/) fetches data from the USGS, 
#' [`smoothr`](https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html) simplifies complex spatial features,
#' and [`AOI`](https://github.com/mikejohnson51/AOI) interfaces with OpenStreetMaps (OSM) to get the latitude/longitude pair
#' corresponding to a placename. 
#' 
#' `AOI` is not on CRAN, so install it by uncommenting the two lines below to install from github using devtools
#library(devtools)
#install_github('mikejohnson51/AOI')
library(AOI)
library(nhdplusTools)
library(smoothr)


#' ## project data
#' To keep the project data folder organized, the list `files.towrite` is defined at the beginning of each
#' script describing all of the files created by that script. This list definition is hidden from the markdown
#' output for brevity (see "get_helperfun.R" source file for details).
#+ echo=FALSE

# descriptions of all files created by this script:
files.towrite = list(
  
   # list of points of interest in the watershed
   c(name='poi',
     file='data/uyrw_poi.rds',
     type='R list object', 
     description='points of interest in the watershed'),
   
   # hydrology GeoPackage (gpkg) file from NHD (source)
   c(name='nhd',
     file=file.path(src.subdir, 'uyrw_nhd.gpkg'), 
     type='geopackage', 
     description='source geometries from NHDPlus'),
   
   # coordinate reference system details
   c(name='crs',
     file='data/uyrw_crs.rds', 
     type='R list object', 
     description='details of projection/extent for the project'),
   
   # derived watershed boundary
   c(name='boundary',
     file=file.path(out.subdir, 'uyrw_nhd_boundary.rds'), 
     type='R sfc object', 
     description='UYRW watershed boundary polygon derived from NHDPlus catchements'),
   
   # sfc object representing a padded (outer buffer) watershed boundary 
   c(name='boundary_padded',
     file=file.path(out.subdir, 'uyrw_boundary_padded.rds'),
     type='R sf object',
     description='padded watershed boundary polygon'),
                     
   # main stem polygon
   c(name='mainstem',
     file=file.path(out.subdir, 'uyrw_nhd_mainstem.rds'), 
     type='R sfc object', 
     description='UYR main stem line geometry derived from NHDPlus flowlines'),
                     
   # catchment polygons
   c(name='catchment',
     file=file.path(out.subdir, 'uyrw_nhd_catchment.rds'), 
     type='R sf object', 
     description='reprojected/repaired NHDPlus catchment polygons'),
   
   # water body polygons
   c(name='waterbody',
     file=file.path(out.subdir, 'uyrw_nhd_waterbody.rds'), 
     type='R sf object', 
     description='reprojected/repaired NHDPlus water body polygons'),
   
   # flowlines line geometries
   c(name='flowline',
     file=file.path(out.subdir, 'uyrw_nhd_flowline.rds'), 
     type='R sf object', 
     description='reprojected/repaired NHDPlus flowline geometries'),
   
   # flowlines line geometries for mill creek
   c(name='millcreek',
     file=file.path(out.subdir, 'millcreek_nhd.rds'), 
     type='R list object', 
     description='flowlines, catchments, and boundary polygon for mill creek'),
   
   # aesthetic parameters for plotting
   c(name='pars_tmap',
     file=file.path(data.dir, 'tmap_get_basins.rds'), 
     type='R list object', 
     description='parameters for writing png plots using tmap and tm_save'),
   
   # a graphic showing flowlines in study area
   c(name='img_flowlines',
     file=file.path(graphics.dir, 'uyrw_flowlines.png'),
     type='png graphic', 
     description='image of flowlines in the UYRW with placenames'),
   
   # a graphic showing drainage subbasins in study area...
   c(name='img_basins',
     file=file.path(graphics.dir, 'uyrw_basins.png'),
     type='png graphic', 
     description='image of some 2000 drainage basins in the UYRW')
   
   )

#' A helper function writes this file metadata information to a CSV file

# write metadata information to disk and print list of files to be written
basins.meta = my_metadata('get_basins', files.towrite, overwrite=TRUE)
print(basins.meta[, c('file', 'type')])

#' The list of files (and descriptions) is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_basins_metadata.csv)
#' in the `/data` directory. Together, the tables 'get_\*_metadata.csv' should describe all files
#' created in this project, and where they came from.
#' 
#' Since github is not meant for hosting large binaries, most of the output files are not
#' shared in this repository (see my 
#' [.gitignore](https://raw.githubusercontent.com/deankoch/UYRW_data/master/.gitignore) file). 
#' However you can reproduce all of them by running the corresponding R scripts.


#'
#' ## starting location
#' 
#' To define the watershed, we need a starting location. In this example, we identify the main outlet of the watershed at Carter's Bridge,
#' a fishing access bridge just south of Livinston, MT. The UYRW is then defined to include all catchments upstream. 
#' 
#' We locate Carter's Bridge in R using the `AOI` package, then use the `nhdplusTools` to explore upstream. Later on, we can load this
#' information from disk instead of querying OSM and USGS and computing things all over again.
#' 

# define a file containing points-of-interest, their comids, and plotting labels
if(!file.exists(here(basins.meta['poi', 'file'])))
{
  # define some points of interest in the watershed
  poi.name = c(cartersbridge = "Carter's Bridge, MT",
               livingston = 'Livingston, MT',
               emigrant = 'Emigrant, MT',
               corwinsprings = 'Corwin Springs, MT',
               canyonvillage = 'Canyon Village, WY',
               yellowstonelake = 'Yellowstone Lake, WY')
  
  # look up their coordinates on OSM
  poi.pt = lapply(poi.name, function(locstring) geocode(location=locstring, pt=TRUE))
    
  # look up their COMIDs
  poi.comid = sapply(poi.pt, discover_nhdplus_id)
  
  # compile into a list and save to disk
  poi.list = list(name = poi.name, pt = poi.pt, comid = poi.comid)
  saveRDS(poi.list, here(basins.meta['poi', 'file']))
  
} else {
  
  # load from disk 
  poi.list = readRDS(here(basins.meta['poi', 'file']))
}

#'
#' ## download the data
#' package `nhdplusTools` uses the NHD's common identifier number (COMID) from Big Timber to delineate the watershed and
#' download the relevant data. These next few lines use the first entry of `poi.list$comid` (Carter's Bridge) as the outlet
#' location from which to find and download relevant watershed geometries.
#' 
if(!file.exists(here(basins.meta['nhd', 'file'])))
{
  # download a line geometry defining flowlines upstream of Big Timber, MT
  uyrw.flowlines = navigate_nldi(list(featureSource='comid', featureID=poi.list$comid[1]), mode='upstreamTributaries', data_source = '')
  
  # notice that we now have a huge number of COMIDs for the watershed upstream of Big Timber
  print(uyrw.flowlines$nhdplus_comid)
  
  # download geometries defining catchements, water bodies, and the full flowline network
  subset_nhdplus(comids=uyrw.flowlines$nhdplus_comid, output_file=here(basins.meta['nhd', 'file']), nhdplus_data='download')
}

#'
#' ## watershed boundary and projection
#' Once the data are downloaded, we load them into R as sfc objects for processing. This code chunk reprojects the watershed
#' boundary polygon to a reference system more appropriate for hydrology modeling, then computes the bounding box extent.
#' 
if(any(!file.exists(here(basins.meta[c('boundary','crs'), 'file']))))
{
  # load the watershed catchments. There are several thousand
  uyrw.catchment = read_sf(here(basins.meta['nhd', 'file']), 'CatchmentSP')
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
  
  # create extended boundary polygon, with 25km buffer, in case we need info on surrounding area
  uyrw.padded.poly = st_buffer(uyrw.poly, dist = 25e3)
  
  # define CRS info list
  crs.list = list(epsg = uyrw.UTM.epsg,
                  epsg.geo = latlong.epsg,
                  dims.geo = list(xlim=uyrw.geo.xlim, ylim=uyrw.geo.ylim),
                  dims = list(xlim=uyrw.xlim, ylim=uyrw.ylim))
  
  # save to disk
  saveRDS(crs.list, here(basins.meta['crs', 'file']))
  saveRDS(uyrw.poly, here(basins.meta['boundary', 'file']))
  saveRDS(uyrw.padded.poly, here(basins.meta['boundary_padded', 'file']))
  
} else {
  
  # load CRS info list and watershed boundary from disk (padded boundary not needed yet)
  crs.list = readRDS(here(basins.meta['crs', 'file']))
  uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
}

#' Note that holes in this watershed boundary polygon can emerge, such as when the catchement boundaries don't perfectly align - see
#' this by plotting `st_union(uyrw.catchment)`. These are filled using the *fill_holes* function in the `smoothr`package.


#'
#' ## data prep
#' 
#' With the watershed boundaries and projection so defined, we can now transform the rest of the data. Files fetched by `nhdplusTools`
#' may include some invalid geometries (self-intersections) and features lying outside this watershed, so we clean up the sfc objects
#' before continuing.
if(any(!file.exists(here(basins.meta[c('catchment', 'waterbody', 'flowline', 'mainstem'), 'file']))))
{
  # load and reproject all geometries to from latitude/longitude to UTM
  uyrw.catchment = st_transform(read_sf(here(basins.meta['nhd', 'file']), 'CatchmentSP'), crs.list$epsg)
  uyrw.flowline = st_transform(read_sf(here(basins.meta['nhd', 'file']), 'NHDFlowline_Network'), crs.list$epsg)
  uyrw.waterbody = st_transform(read_sf(here(basins.meta['nhd', 'file']), 'NHDWaterbody'), crs.list$epsg)
  
  # fix invalid geometries and mask with watershed boundary
  uyrw.waterbody = st_intersection(st_make_valid(uyrw.waterbody), uyrw.poly)
  uyrw.flowline = st_intersection(st_make_valid(uyrw.flowline), uyrw.poly)
  uyrw.catchment = st_intersection(st_make_valid(uyrw.catchment), uyrw.poly)
  
  # find and join all line segments labeled as the 'Yellowstone River', then fix self-intersection issues
  uyrw.mainstem = uyrw.flowline[uyrw.flowline$gnis_name == 'Yellowstone River',]
  uyrw.mainstem = st_make_valid(st_union(uyrw.mainstem, by_feature=FALSE))
  
  # save to disk
  saveRDS(uyrw.catchment, here(basins.meta['catchment', 'file']))
  saveRDS(uyrw.flowline, here(basins.meta['flowline', 'file']))
  saveRDS(uyrw.waterbody, here(basins.meta['waterbody', 'file']))
  saveRDS(uyrw.mainstem, here(basins.meta['mainstem', 'file']))
  
} else {
  
  # load from disk
  uyrw.catchment = readRDS(here(basins.meta['catchment', 'file']))
  uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))
  uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
  uyrw.mainstem = readRDS(here(basins.meta['mainstem', 'file']))
  
}


#' In a small pilot study leading up to this project, SWAT+ was fitted to the Mill Creek, MT watershed.
#' Define this subset of the UYRW and save those geometries to disk:
if(!file.exists(here(basins.meta['millcreek', 'file'])))
{
  # identify "Mill Creek" from pilot study - this query actually returns 5 separate stream reaches
  millcreek.gniscodes = unique(uyrw.flowline$gnis_id[grepl('Mill Creek', uyrw.flowline$gnis_name, fixed=TRUE)])
  
  # identify the "Mill Creek" based on length - it is the longest of the stream reaches
  millcreek.gniscode = names(which.max(sapply(millcreek.gniscodes, function(gniscode) sum(uyrw.flowline[uyrw.flowline$gnis_id == gniscode,]$lengthkm))))
  millcreek.mainstem = uyrw.flowline[uyrw.flowline$gnis_id == millcreek.gniscode,]
  
  # find COMIDs of upstream tributaries, using COMID creek outlet (notice this searches our local copy of the data)
  millcreek.comids = get_UT(uyrw.flowline, millcreek.mainstem$comid[as.logical(millcreek.mainstem$divergence)])
  
  # query all upstream tributaries and their catchments
  millcreek.flowlines = uyrw.flowline[uyrw.flowline$comid %in% millcreek.comids,]
  millcreek.catchments = uyrw.catchment[uyrw.catchment$featureid %in% millcreek.comids,]
  
  # build a boundary polygon for this watershed
  millcreek.poly = fill_holes(st_union(millcreek.catchments, by_feature=FALSE), threshold=1e6)
  
  # pile everything into a list and save to disk
  millcreek.list = list(boundary=millcreek.poly, flowlines=millcreek.flowlines, catchments=millcreek.catchments)
  saveRDS(millcreek.list, here(basins.meta['millcreek', 'file']))
  
} else {
  
  # load from disk
  millcreek.list = readRDS(here(basins.meta['millcreek', 'file']))
}




#'
#' ## visualization
#' We make a few plots using the `tmap` package to show some of the watershed features now loaded into R. 
#' First define and save some graphical parameters for consistency among plots and tidier code
#' 
if(!file.exists(here(basins.meta['pars_tmap', 'file'])))
{
  # parameter values go into a list
  tmap.pars = list(
    
    # parameters for the PNG write device
    png = c(w=1200, h=2000, pt=12),
    
    # aesthetics for lat/long gridlines, scalebar, title position
    layout = tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
      tm_scale_bar(breaks=c(0, 20, 40), position=c('left', 'bottom'), text.size=0.7) +
      tm_layout(title.position=c('center', 'top'),
                main.title.size=1,
                main.title.position='center',
                frame=FALSE, 
                inner.margins=c(0,0,0.05,0)),
    
    # parameters for labels
    label.txt.size = 0.7
    
  )
  
  # save to disk
  saveRDS(tmap.pars, here(basins.meta['pars_tmap', 'file']))
  
} else {
  
  # load from disk
  tmap.pars = readRDS(here(basins.meta['pars_tmap', 'file']))
}



#' Start with the watershed flowlines. Line widths are scaled according to the Strahler method,
#' using the variable 'streamorde' from NHDPlus (they do not represent physical width of the stream).
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_flowlines.png)
if(!file.exists(here(basins.meta['img_flowline', 'file'])))
{
  # make the plot grob
  tmap.watercourses = 
    tm_shape(uyrw.poly) + 
      tm_polygons(col='greenyellow', border.col='yellowgreen') +
    tm_shape(uyrw.flowline) +
      tm_lines(col='dodgerblue3', lwd='streamorde', scale=5, legend.lwd.show=FALSE) +
    tm_shape(uyrw.mainstem) +
      tm_lines(col='dodgerblue4', lwd=2) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='dodgerblue4', border.col='deepskyblue4') +
    tm_shape(millcreek.list$boundary) +
      tm_polygons(col='blue', alpha=0.2, border.alpha=0) +
    tm_shape(poi.list$pt[['cartersbridge']]) +   
      tm_dots(size=0.2, col='red') +
    tm_text('request', size=tmap.pars$label.txt.size, ymod=0.5) +
      tm_shape(st_sf(st_centroid(millcreek.list$boundary), data.frame(request='Mill Creek pilot study'))) +   
      tm_text('request', size=tmap.pars$label.txt.size, xmod=5) +
    tmap.pars$layout +
    tm_layout(main.title='major watercourses in the UYRW') 
  
  # render the plot
  tmap_save(tm=tmap.watercourses, 
            here(basins.meta['img_flowline', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}

#' Now plot the watershed drainage basins and water bodies:
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/uyrw_basins.png)
if(!file.exists(here(basins.meta['img_basins', 'file'])))
{
  tmap.basins = 
    tm_shape(uyrw.poly) +
      tm_polygons(col=NA, border.col='black') +
    tm_shape(uyrw.catchment) + 
      tm_polygons(col='MAP_COLORS', border.col=adjustcolor('white', alpha=0)) +
    tm_shape(uyrw.flowline) +
      tm_lines(col='dodgerblue4', lwd=0.8, alpha=0.5) +
    tm_shape(uyrw.mainstem) +
      tm_lines(col='dodgerblue4', lwd=2) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
    tmap.pars$layout +
    tm_layout(main.title=paste(nrow(uyrw.catchment), 'drainage basins identified by USGS NHDPlus'))

  # render the plot
  tmap_save(tm=tmap.basins, 
            here(basins.meta['img_basins', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}


#+ include=FALSE
# Development code

# there is another layer here called NHDArea
#st_layers(here(basins.meta['nhd', 'file']))


#+ include=FALSE
####
# testing to make sure we get the same number of catchments if we download everything in pieces (to avoid warning):
# uyrw.flowlines = navigate_nldi(list(featureSource='comid', featureID=poi.list$comid$bigtimber), mode='upstreamTributaries', data_source = '')
# testfiles = sapply(1:10, function(x) here(paste0('data/source/nhd_test', x, '.gpkg')))
# idx.storage = rep(1:10, each=length(uyrw.flowlines$nhdplus_comid)/10)
# for(idx in 1:10)
# {
#   testcomids = uyrw.flowlines$nhdplus_comid[idx.storage==idx]
#   subset_nhdplus(comids=testcomids, output_file=testfiles[idx], nhdplus_data='download')
# }
# testcatch = vector(mode='list', length=10)
# for(idx in 1:10)
# {
#   testcatch[[idx]] = read_sf(testfiles[idx], 'CatchmentSP')
# }
# xx = do.call(rbind, testcatch)
# nrow(xx)
# 4162, same as before. A plot of the flowlines also matches with earlier. I think we are good.
####

#+ include=FALSE
#my_markdown('get_basins')