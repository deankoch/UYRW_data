#' ---
#' title: "get_snow.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_snow**: finds climatic sensor stations located in the UYRW and downloads their time series
#' 
#' Retrieve snow related datasets from various sources: SNOTEL time series and other snow data from the USDA
#' NRCS National Water and Climate Centre website; SNODAS grids from the National Snow and Ice Data Center
#' FTP Archives

#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' which creates some required directories and project config files, should be run before this script.

#'
#' ## libraries
#' The [`snotelr`](https://github.com/bluegreen-labs/snotelr) package fetches
#' [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA. See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(snotelr)

#'
#' ## project data
#+ echo=FALSE
{
files.towrite = list(
  
  # directory to save NWCC downloads  
  c(name='nwcc_src',
    file=file.path(src.subdir, 'nwcc'),
    type='directory',
    description='directory for storage of NWCC source files (CSV) from NOAA'),
  
  # metadata table downloaded from NWCC website
  c(name='nwcc_sites',
    file=file.path(src.subdir, 'nwcc', 'nwcc_sites.csv'), 
    type='CSV', 
    description='metadata list for site records from NWCC and partner networks'), 
  
  # NWCC metadata table as an sf object
  c(name='nwcc_sf',
    file=file.path(out.subdir, 'nwcc_sites.rds'), 
    type='R sf object', 
    description='sf object containing `nwcc_sites` data frame and point geometries for UYRW area'),
  
  # snotel processed data tables 
  c(name='nwcc_data',
    file=file.path(out.subdir, 'nwcc_data.rds'),
    type='R list object',
    description='list of data frames containing all available NWCC data at the sites in `nwcc_sf`')
  
)
}

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
snow.meta = my_metadata('get_snow', files.towrite, overwrite=TRUE)
print(snow.meta[, c('file', 'type')])

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_snow_metadata.csv)
#' in the `/data` directory.
#' 
#' Load some of the data prepared earlier 
# load metadata csv, CRS info list and watershed geometries from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))
uyrw.poly.padded = readRDS(here(basins.meta['boundary_padded', 'file']))



#'
#' ## List relevant NWCC sites
#' Find list of all NWCC sites, saving as a CSV file and recasting as `sfc`. Mask to URYW area  

# make the destination directory
snow.dir = here(snow.meta['nwcc_src', 'file'])
my_dir(snow.dir)

# download the sites table if we haven't already
snow.csv = here(snow.meta['nwcc_sites', 'file'])
if(!file.exists(snow.csv))
{
  # a helper function downloads and parses the table
  nwcc.sites = my_nwcc_list(snow.csv)

} else {
  
  nwcc.sites = read.csv(snow.csv)
  
}


# extract site coordinates, convert to sf object, transform to UTM
coords.mat = as.matrix(nwcc.sites[, c('longitude', 'latitude')])
nwcc.sfc = st_sfc(lapply(1:nrow(coords.mat), function(site) st_point(coords.mat[site,])), crs=crs.list$epsg.geo)
nwcc.sfc = st_transform(nwcc.sfc, crs=crs.list$epsg)

# find subset of sites within UYRW area
nwcc.uyrw.idx = st_intersects(nwcc.sfc, uyrw.poly.padded, sparse=FALSE)
uyrw.sites = nwcc.sites[nwcc.uyrw.idx,]
head(uyrw.sites)


#' 
#' ## download and import NWCC data
#' This chunk selects all available variables at daily to monthly level at the sites identified
#' above, and executes NWCC requests to download the data as CSV then import it into R, using helper
#' functions based on `snotelr`, saving copies to disk as rds
#'  

# print the first few variable descriptions
nwcc.descriptions = my_nwcc_get()
nwcc.varnames = names(nwcc.descriptions)
head(nwcc.descriptions)

# define the RDS files to be written here
uyrw.files.path = here(snow.meta['nwcc_data', 'file'])
uyrw.sf.path = here(snow.meta['nwcc_sf', 'file'])

# proceed only if they don't exist
if(any(!file.exists(c(uyrw.files.path, uyrw.sf.path))))
{
  # run the helper to download data files to `snow.dir` - two per site (daily and semimonthly)
  uyrw.files = my_nwcc_get(nwcc.varnames, uyrw.sites, snow.dir, reuse=TRUE, retry=2)
  head(uyrw.files %>% select(site_id, state, ntwk, freq, path))
  
  # open everything as list of dataframes and omit empty datasets (sites to discard)
  uyrw.data = my_nwcc_import(uyrw.files, nwcc.varnames)
  idx.empty = sapply(uyrw.data, is.null)
  uyrw.data = uyrw.data[!idx.empty]
  
  # construct an `sf` dataframe of site locations with geometries computed earlier
  uyrw.sfc = nwcc.sfc[nwcc.uyrw.idx][!idx.empty]
  uyrw.sf = st_sf(uyrw.sites[!idx.empty,], geometry = uyrw.sfc)
  
  # replace start/end dates with these verified values, add row counts and a primary key:
  uyrw.sf = uyrw.sf %>% 
    mutate(table_id = 1:nrow(uyrw.sf)) %>% 
    mutate(n_daily = sapply(uyrw.data, function(x) sum(x$period=='daily'))) %>% 
    mutate(n_semimonthly = sapply(uyrw.data, function(x) sum(x$period=='semimonthly'))) %>% 
    mutate(start = do.call(c, lapply(uyrw.data, function(x) min(x$date)))) %>%
    mutate(end = do.call(c, lapply(uyrw.data, function(x) max(x$date)))) %>%
    select(-c(start_yr, start_mo, end_yr, end_mo))
  
  # add site_id field to each data frame for convenience later
  uyrw.data = mapply(function(x, y) cbind(x, data.frame(site_id=y)), uyrw.data, uyrw.sf$site_id)

  # save to disk
  saveRDS(uyrw.data, uyrw.files.path)
  saveRDS(uyrw.sf, uyrw.sf.path)
  
} else {
  
  # load from disk
  uyrw.data = readRDS(uyrw.files.path)
  uyrw.sf = readRDS(uyrw.sf.path)
} 

# report the variables found in these records of the UYRW area
uyrw.varnames = unique(unlist(sapply(uyrw.data, function(x) names(x))))
print(nwcc.descriptions[names(nwcc.descriptions) %in% uyrw.varnames])



#' ## SNODAS
#' 
#' Based on code from (see 'snodas.R', at https://github.com/NCAR/rwrfhydro/)
#' 
#' 




#' 
#' my_nwcc_get()[names(my_nwcc_get())=='SNDN']
#' idx=39
#' names(uyrw.data[[idx]] %>% filter(period=='semimonthly'))
#' 
#' ggplot(uyrw.data[[idx]] %>% filter(period=='semimonthly') %>% filter(complete.cases(.)), aes(x=date), na.rm=TRUE) +
#'   #geom_line(aes(y=as.vector(TMIN)), col='green') +
#'   #geom_line(aes(y=as.vector(TMAX)), col='red') +
#'   #geom_line(aes(y=as.vector(PREC)), col='blue') +
#'   geom_line(aes(y=as.vector(SNDN)), col='violet') +
#'   geom_line(aes(y=as.vector(SNWD)), col='darkblue') +
#'   geom_line(aes(y=as.vector(WTEQ)), col='orange') +
#'   #geom_line(aes(y=as.vector(PRCP)), col='purple') +
#'   #geom_line(aes(y=as.vector(PRCPSA)), col='turquoise') +
#'   #scale_x_date(limits = as.Date(c('1943-1-1', '1946-1-1'))) +
#'   ggtitle(uyrw.sf$site_nm[idx])
#' 
#' range(uyrw.data[[78]]$date)
#' 
#' #' Some of these time series appear to be cross-listed on GHCND, but in many cases we have quite
#' #' different measurements despite matching locations and timestamps (perhaps due to different instruments,
#' #' or different data-cleaning methodologies)
#' 
#' # TODO: demo this 

# # find index of observations lying within elk count units
# elk.sf = st_transform(st_union(st_read('H:/elk/geometry/count_units.shp')), crs=crs.list$epsg)
# elk.idx = st_intersects(st_geometry(uyrw.sf), elk.sf, sparse=F)
# 
# 
# 
# st_write(uyrw.poly, 'UYRW_boundary.shp')
# st_read()


#' # plot of relative locations
#' plot(uyrw.poly.padded, col='grey90', main='SNOTEL time series sites')
#' plot(uyrw.poly, add=TRUE, col='grey50')
#' plot(elk.sf, add=TRUE, col='grey30')
#' plot(st_geometry(uyrw.sf), add=TRUE, pch=16, cex=0.5, col='blue')
#' 
#' # outer join to concatenate everything into single massive dataframe
#' xx = mapply(function(x, y) cbind(x, data.frame(site_id=y)), uyrw.data, uyrw.sf$site_id)
#' yy = Reduce(function(...) merge(..., all=T), xx)
#' 
#' # find the number of stations reporting snow density or depth in the UYRW area
#' zz = yy %>% 
#'   filter(period=='semimonthly') %>%
#'   filter(!is.na(SNDN) | !is.na(SNWD)) %>%
#'   select(date, site_id, SNDN, SNWD) %>% 
#'   mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
#'   group_by(year, month) %>% 
#'   summarise(n_stations = length(unique(site_id)), date)
#' 
#' ggplot(zz, aes(x=date, y=n_stations)) + 
#'   geom_point(cex=0.5) + 
#'   ggtitle('number of stations reporting snow depth or density in URYW area (semimonthly)')
#' 
#' zz = yy %>% 
#'   filter(period=='daily') %>%
#'   filter(!is.na(SNDN) | !is.na(SNWD)) %>%
#'   select(date, site_id, SNDN, SNWD) %>% 
#'   mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
#'   group_by(year, month) %>% 
#'   summarise(n_stations = length(unique(site_id)), date)
#' 
#' ggplot(zz, aes(x=date, y=n_stations)) + 
#'   geom_point(cex=0.5) + 
#'   ggtitle('number of stations reporting snow depth or density in URYW area (daily)')
#' 
#' uyrw.sf[elk.idx,]
#' 
#' # repeat for within elk range (neither is daily)
#' zz = yy[yy$site_id %in% uyrw.sf$site_id[elk.idx],] %>% 
#'   filter(period=='semimonthly') %>%
#'   filter(!is.na(SNDN) | !is.na(SNWD)) %>%
#'   select(date, site_id, SNDN, SNWD) %>% 
#'   mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
#'   group_by(year, month) %>% 
#'   summarise(n_stations = length(unique(site_id)), date)
#' 
#' ggplot(zz, aes(x=date, y=n_stations)) + 
#'   geom_point(cex=0.5) + 
#'   ggtitle('number of stations reporting snow depth or density in elk survey area (semimonthly)')
#' 
#' 
#' 
#' 
#' 
#' #my_markdown('get_snow')