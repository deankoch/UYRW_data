#' ---
#' title: "snotelr.R"
#' author: "Dean Koch"
#' date: "August 13, 2020"
#' output: github_document
#' ---
#'
#' **MITACS UYRW project**
#' 
#' Fetch SNOTEL data
#' 
#' The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package can be used to fetch
#' [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA. We use it to build a map of climatic
#' sensor stations in the UYRW, and to query historical data for model training.
#' 
#' [NHDPlus.R](https://github.com/deankoch/URYW_data/blob/master/NHDPlus.knit.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
library(snotelr)
library(sf)
library(tmap)
library(here)

#' Data on geographical landmarks and highways are available from OSM using the overpass API via `osmdata`
library(osmdata)

#'
#' ## project data

#+ results='hide'
# project directory names
graphics.dir = 'graphics'
src.subdir = 'data/source'
out.subdir = 'data/prepared'

# load metadata csv, CRS info list and watershed boundary from disk
uyrw.metadata.df = read.csv(here('data/uyrw_metadata.csv'), header=TRUE, row.names=1)
crs.list = readRDS(here(uyrw.metadata.df['crs', 'file']))
uyrw.poly = readRDS(here(uyrw.metadata.df['boundary', 'file']))


# this CSV file will serve as a guide for all files written to the project folder
snotel.metadata.file = 'data/snotel_metadata.csv'
if(!file.exists(here(snotel.metadata.file)))
{
  # filename for metadata table downloaded from SNOTEL website
  snotel.csv.file = c(name='csv',
                      file=file.path(src.subdir, 'snotel_sites.csv'), 
                      type='CSV table', 
                      description='metadata list for sites on SNOTEL website')
  
  # filename for metadata table as an sfc object
  snotel.sfc.file = c(name='sfc',
                      file=file.path(out.subdir, 'snotel_sites.rds'), 
                      type='R sfc object', 
                      description='sfc object with SNOTEL sensor locations in UYRW')
  
  # filename for graphic showing flowlines in study area
  snotel.sites.png.file = c(name='img_snotel',
                            file=file.path(graphics.dir, 'snotel_sites.png'),
                            type='png graphic', 
                            description='image of SNOTEL site locations in the UYRW')

  
  # bind all the individual filename info vectors into a data frame
  snotel.metadata.df = data.frame(rbind(snotel.csv.file,
                                        snotel.sfc.file,
                                        snotel.sites.png.file), row.names='name')
  
  # save the data frame
  write.csv(snotel.metadata.df, here(snotel.metadata.file))
  
} else {
  
  # load the data frame
  snotel.metadata.df = read.csv(here(snotel.metadata.file), header=TRUE, row.names=1)
  
}
#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/URYW_data/blob/master/data/snotel_metadata.csv)
#' in the `/data` directory.

#'
#' ## downloading the data
#' the `snotel_info` function in `snotelr` downloads a CSV containing site IDs and coordinates
if(!file.exists(here(snotel.metadata.df['csv', 'file'])))
{
  # download the metadata csv to the folder specified in `path`. This writes the file "snotel_metadata.csv"
  snotel_info(path=here(src.subdir))
  
  # rename the csv to avoid confusion with identically-named file in the parent folder (my list of project files)
  file.rename(from=here(src.subdir, 'snotel_metadata.csv'), to=here(snotel.metadata.df['csv', 'file']))
  
}

if(!file.exists(here(snotel.metadata.df['sfc', 'file'])))
{
 
   # load the site info table into a data frame and extract coordinates
  sites.df = read.csv(here(snotel.metadata.df['csv', 'file']), header=TRUE)
  sites.coords.matrix = as.matrix(sites.df[, c('longitude', 'latitude')])
  
  # extract the coordinates and convert to sfc object, adding attribute columns to get sf object
  sites.sfc = st_sfc(lapply(1:nrow(sites.df), function(xx) st_point(sites.coords.matrix[xx,])), crs=crs.list$epsg.geo)
  sites.sf = st_sf(cbind(sites.df, sites.sfc))
  
  # transform to UTM and clip to URYW area (13 stations identified)
  sites.sf = st_transform(sites.sf, crs=crs.list$epsg)
  sites.sf = st_intersection(sites.sf, uyrw.poly)
  
  # save to disk
  saveRDS(sites.sf, here(snotel.metadata.df['sfc', 'file']))

  
} else {
  
  # load from disk 
  sites.sf = readRDS(here(snotel.metadata.df['sfc', 'file']))
  
}


# poi.list = readRDS(here(uyrw.metadata.df['poi', 'file']))
# uyrw.catchment = readRDS(here(uyrw.metadata.df['catchment', 'file']))
# uyrw.flowline = readRDS(here(uyrw.metadata.df['flowline', 'file']))
# uyrw.waterbody = readRDS(here(uyrw.metadata.df['waterbody', 'file']))
# uyrw.mainstem = readRDS(here(uyrw.metadata.df['mainstem', 'file']))

# define a new attribute: the number of years in the record
year.start = sapply(strsplit(sites.sf$start, '-'), function(xx) as.numeric(xx[[1]]))
year.end = sapply(strsplit(sites.sf$end, '-'), function(xx) as.numeric(xx[[1]]))
sites.sf$duration = year.end - year.start 


# split the points into upper and lower, for plotting text labels that don't overlap
idx.lower = sites.sf$site_name %in% c('s fork shields ', 'sacajawea ', 'monument peak ')



#'
#' ## visualization
#' 

# define a padded bounding box for plotting
cex.xlim = 1.8
cex.ylim = 1.1
uyrw.xlim.larger = crs.list$dims$xlim + (cex.xlim-1)*c(-1,1)*diff(crs.list$dims$xlim)/2
uyrw.ylim.larger = crs.list$dims$ylim + (cex.ylim-1)*c(0,1)*diff(crs.list$dims$ylim)/2

# determine some reasonable dimensions (in pixels) for output
flowlines.png.res = round(c(diff(uyrw.xlim.larger), diff(uyrw.ylim.larger))/100)

# plot the SNOTEL stations as a png file
if(!file.exists(here(snotel.metadata.df['img_snotel', 'file'])))
{
  # render/write the plot
  png(here(snotel.metadata.df['img_snotel', 'file']), width=flowlines.png.res[1], height=flowlines.png.res[2], pointsize=56)

    print(tm_shape(uyrw.poly, xlim=uyrw.xlim.larger, ylim=uyrw.ylim.larger) + 
            tm_polygons(col='greenyellow', border.col='yellowgreen') +
          tm_shape(uyrw.flowline) +
            tm_lines(col='dodgerblue3') +
          tm_shape(uyrw.mainstem) +
            tm_lines(col='dodgerblue4', lwd=2) +
          tm_shape(uyrw.waterbody) + 
            tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
          tm_shape(sites.sf[idx.lower,]) +
            tm_dots(size=0.5, col='red') +
            tm_text('site_name', just='bottom', ymod=-0.5, size=0.8) +
          tm_shape(sites.sf[!idx.lower,]) +
            tm_dots(size=0.5, col='red') +
            tm_text('site_name', just='top', ymod=0.5, size=0.8) +
          tm_grid(n.x=4, n.y=5, projection=crs.list$epsg.geo, alpha=0.5) +
          tm_scale_bar(breaks=c(0, 20, 40), position=c('center', 'bottom'), text.size=0.5) +
          tm_layout(title='SNOTEL stations in the UYRW', title.position=c('center', 'TOP'), frame=FALSE))
    
  dev.off()
}

#' ![SNOTEL stations in the UYRW](https://raw.githubusercontent.com/deankoch/URYW_data/master/graphics/snotel_sites.png)


#+ include=FALSE
# Development code


#+ include=FALSE
# Convert to markdown by running the following line (uncommented)
# rmarkdown::render(here('snotelr.R'), run_pandoc=FALSE, clean=TRUE)
