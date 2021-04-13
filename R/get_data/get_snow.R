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
#' Start by sourcing two helper scripts
#' ([helper_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md) and
#' [helper_get_data.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_get_data.md))
#' which set up required libraries and directories and define some utility functions.
library(here)
source(here('R/helper_main.R'))
source(here('R/get_data/helper_get_data.R'))

#' Based on the [`snotelr`](https://github.com/bluegreen-labs/snotelr) package, which fetches
#' [SNOTEL network data](https://www.wcc.nrcs.usda.gov/snow/) from the USDA.


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


### 
###
# develop

if(0)
{
  # print available variables and identify snow-related ones
  xx.nm = lapply(uyrw.data, function(x) names(x))
  xx = unique(do.call(c, xx.nm))
  nwcc.descriptions[names(nwcc.descriptions) %in% xx]
  snow.nm = c('SNWD', 'WTEQ', 'SNDN')
  
  # find stations with snow data
  xx.idx = sapply(xx.nm, function(x) any(x %in% snow.nm))
  yy = uyrw.data[xx.idx]
  zz.sf = uyrw.sf[xx.idx,]
  
  # prune the all-NA rows
  zz = lapply(yy, function(y) y[ apply(y[,snow.nm], 1, function(x) !all(is.na(x))), c('date', snow.nm)])
  
  # make a histogram of data availability by year
  zz.df = do.call(rbind, zz)
  zz.dates = zz.df$date
  
  # R histograms with Dates appears to be broken? No axis labels
  hist.breaks = seq(as.Date('1918-01-01'), as.Date('2022-01-01'), by='year')
  hist(zz.dates, breaks=hist.breaks)
  
  # ggplot approach
  freqs = aggregate(zz.dates, by=list(zz.dates), FUN=length)
  freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
  ggplot(freqs, aes(x=names, y=x)) + geom_bar(stat="identity") +
    scale_x_date(breaks="1 year", labels=date_format("%Y"),
                 limits=c(as.Date('1987-01-01'), as.Date('2021-01-01'))) +
    ylab("Frequency") + xlab("Year") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45))
  
  plot(uyrw.poly)
  plot(zz.sf, add=TRUE)
}

### 
###


#+ include=FALSE
#my_markdown('get_snow', 'R/get_data')