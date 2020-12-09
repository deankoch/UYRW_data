#' ---
#' title: "get_meteo.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_meteo**: download and import various long-term meteorological reconstructions
#' 
#' ## background
#' 
#' SWAT uses weather inputs at the subbasin level to drive the model's day-to-day water
#' budget. Ideally these inputs would be copied directly from quality-controlled station
#' observations of daily weather data, such as total precipitation. In practice, two things
#' prevent that from happening:
#' 
#' * to get sensible subbasin-level inputs, we have to aggregate weather states over the
#' full subbasin extent.
#' 
#' * real spatio-temporal records of weather states are inherently sparse. Space-time is
#' continuous, point data are not.
#' 
#' SWAT can desparsify the record by simulating daily point data using MCMC-based weather
#' generator models. These are parametrized by the local long-term climatology, so they
#' are usually located at points where long-term historical gage data are available. SWAT
#' ships with a large set of parametrized weather generators. 
#' 
#' The aggregation method in SWAT is very simple: Each subbasin centroid is mapped to the
#' nearest point weather record (generated or observed). Orographic adjustments of
#' precipitation (using elevation bands) can introduce additional detail downstream, but
#' ultimately all of the weather drivers enter the model through a nearest-neighbour
#' interpolation of the available/simulated point data.
#' 
#' ## gridded meteorological reconstructions
#' 
#' Depending on the detail level of the subbasin delineation, this nearest-neighbour method
#' may be too simplistic. The introduced bias can be particularly problematic in mountaineous
#' terrain like the URYW (see [this discussion](https://doi.org/10.1016/j.jhydrol.2017.03.008),
#' for example). One alternative is to replace the SWAT weather generators with a high-
#' resolution gridded reconstruction of local weather.
#' 
#' [This recent paper](https://doi.org/10.1111/1752-1688.12819) discusses the use of an ensemble
#' of grids from different sources. We will be exploring this idea in our project, so we download
#' three long-term gridded climatic datasets in the script below:
#' 
#' * [Daymet (1980-2017)](https://daymet.ornl.gov/) ('tmax', 'tmin', 'prcp', 'srad', 'swe')
#' 
#' * [PNWNAmet (1945-2012)](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets) ('tmax', 'tmin', 'prec', 'wind')
#' 
#' * [Livneh (1950-2013)](https://ciresgroups.colorado.edu/livneh/data) ('tmax', 'tmin', 'prec', 'wind')
#' 
#' The Daymet, PNWNAmet, and Livneh source files will require 1GB, 82GB, and 58GB of space, respectively,
#' after compression. Some of the files must be downloaded uncompressed. Expect these downloads to take
#' several days in total to complete. This script downloads the full source datasets, then extracts/imports
#' the relevant subsets containing data on the URYW, writing them into much smaller R data (rds) files.
#' These data may be converted directly to SWAT weather input files later on, so wherever possible I have
#' used the SWAT variable names 'pcp', 'tmin' and 'tmax', 'slr', 'wnd', 'hmd' in prepared outputs.
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
#' should be run before this script.
#' 
#'
#' ## libraries
#' [`daymetr`] automates the retrieval of Daymet rasters and [`RColorBrewer`](https://colorbrewer2.org/)
#' loads some nice colour palettes See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
#library(R.utils)
library(daymetr)
library(stringr)
library(RColorBrewer)

#'
#' ## project data
#+ echo=FALSE
files.towrite = list(
  
  # storage directory for (large) unprocessed Livneh dataset
  c(name='livneh_source',
    file=file.path(src.subdir, 'livneh'), 
    type='directory',
    description='source bz2 archives containing Livneh NetCDF data'),
  
  # Livneh dataset clipped to UYRW study area and reshaped as R objects
  c(name='livneh_uyrw',
    file=file.path(out.subdir, 'livneh_uyrw.rds'), 
    type='R list object',
    description='Livneh meteorological data (1950-2013) clipped to UYRW'),
  
  # storage directory for Livneh dataset clipped to UYRW as raster
  c(name='livneh_rasters',
    file=file.path(out.subdir, 'livneh_uyrw_rasters'), 
    type='directory',
    description='storage for clipped Livneh data as multiband rasters'),
  
  # storage directory for unprocessed Daymet dataset
  c(name='daymet_source',
    file=file.path(src.subdir, 'daymet'), 
    type='directory',
    description='source NetCDF files from Daymet Web Services'),
  
  # Daymet dataset clipped to UYRW study area and reshaped as R objects
  c(name='daymet_uyrw',
    file=file.path(out.subdir, 'daymet_uyrw.rds'), 
    type='R list object',
    description='storage for Daymet data clipped to UYRW'),
  
  # storage directory for Daymet dataset clipped to UYRW as raster
  c(name='daymet_rasters',
    file=file.path(out.subdir, 'daymet_uyrw_rasters'), 
    type='directory',
    description='storage for clipped Daymet data as multiband rasters'),
  
  # storage directory for (large) unprocessed PNWNAmet dataset
  c(name='pnwnamet_source',
    file=file.path(src.subdir, 'pnwnamet'), 
    type='directory',
    description='source NetCDF files from PCIC PNWNAmet'),
  
  # PNWAmet dataset clipped to UYRW study area and reshaped as R objects
  c(name='pnwnamet_uyrw',
    file=file.path(out.subdir, 'pnwnamet_uyrw.rds'), 
    type='R list object',
    description='storage for PNWNAmet data clipped to UYRW'),
  
  # storage directory for PNWAmet dataset clipped to UYRW as raster
  c(name='pnwnamet_rasters',
    file=file.path(out.subdir, 'pnwnamet_uyrw_rasters'), 
    type='directory',
    description='storage for clipped PNWNAmet data as multiband rasters'),
  
  # graphic showing example gridded weather data (tmax) for the UYRW
  c(name='img_meteo',
    file=file.path(graphics.dir, 'meteo_gridded.png'),
    type='png graphic',
    description='image comparing three gridded meteorological datasets for the UYRW')

)

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
meteo.meta = my_metadata('get_meteo', files.towrite, overwrite=TRUE)
print(meteo.meta[, c('file', 'type')])
 
#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_meteo.csv)
#' in the `/data` directory.
#' 
#' All of the pertinent meteorological data for our SWAT model of the UYRW will be stored in
#' the rds files listed as `livneh_uyrw`, `daymet_uyrw`, and `pnwnamet_source` in the metadata
#' table. This script also saves a copy of these data as multiband rasters, with one band per
#' day in the time series, and one file per variable for each of the data sources.
#' 
#' These GeoTIFF files preserve the original gridded projection of the data, allowing easy
#' visualization of the weather grids (using `plot(raster(...))`). Note that different sources have
#' different projections. The tabular data, however are all accompanied by geographical (lat/long)
#' coordinates, as needed in SWAT; whereas the points shapefile is projected to our UYRW project
#' reference system (UTM).
#' 
#' An xml file is generated when we save the rasters (using `writeRaster`). This contains
#' precomputed summary stats (mean, sd, etc) for the large rasters, which may speed up future
#' tasks. To disable the xml files, do `rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")`.
#' 
#' To clip the gridded datasets to size, we need a watershed boundary polygon. This boundary
#' will be padded by a buffer zone (the value of `uyrw.buff` in metres) in order to capture
#' adjacent grid points that may be closer to a subbasin centroid than any of the interior ones.
#' 
#' Load some of the data prepared earlier 

# load CRS info list and watershed polygon from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))

# prepare a mask in geographical coordinates
uyrw.poly.geo = st_transform(uyrw.poly, crs=crs.list$epsg.geo)

# define 3km buffer around the perimeter of the study area
uyrw.buff = 3e3 # meters

#'
#' ## PNWNAmet data
#' 
#' The [PNWNAmet](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets)
#' meteorological dataset covers the period 1945-2012 at 1/16 degree resolution (~6km). See
#' [this Nature paper](https://doi.org/10.1038/sdata.2018.299) for information on methodology.
#' 
#' The chunk below downloads four large NetCDF files (~206GB total) containing the entire time
#' series of the four variables ('tmax', 'tmin', 'prec', 'wind'), clips to the UYRW region,
#' and reshapes the data into a list of variable-specific tables for easier handling in R. 
#' 

# set variable names and filenames from PCIC, and file paths for the downloads
pcic.vn = setNames(nm=c('tmin', 'tmax', 'pcp', 'wnd')) # SWAT names
pcic.vars = setNames(c('tasmin', 'tasmax', 'pr', 'wind'), nm=pcic.vn) # source names
pcic.fn = paste0('PNWNAmet_', pcic.vars, '.nc.nc')
pcic.paths = setNames(here(meteo.meta['pnwnamet_source', 'file'], pcic.fn), nm=pcic.vn)

# PCIC download URL. The suffix ensures we get the full time series
pcic.domain = 'http://data.pacificclimate.org/data/gridded_observations'
pcic.suffix = '?pr[0:24836][][]=&='
pcic.url = setNames(file.path(pcic.domain, pcic.fn, pcic.suffix), nm=pcic.vn)

# define the paths to the zipped pnwnamet source files
pcic.paths.zip = gsub(pattern = '\\.nc\\.nc$', '.zip', pcic.paths)

# define processed output files
pcic.out.path = here(meteo.meta['pnwnamet_uyrw', 'file'])
pcic.out.raster.dir = here(meteo.meta['pnwnamet_raster', 'file'])
pcic.out.rasters = setNames(file.path(pcic.out.raster.dir, paste0(pcic.vn, '.tif')), nm=pcic.vn)


#' The script may be interrupted and restarted at a later time if you need to pause the
#' download. It will scan for files already downloaded, and skip any that exist already
#' in the `livneh_source` directory.

# check for existing unzipped source data
idx.missfile = !file.exists(pcic.paths)

# download a file only if we have neither the zip or uncompressed source data on disk
if(any(idx.missfile & !file.exists(pcic.paths.zip)))
{
  for(idx.file in which(idx.missfile))
    download.file(pcic.url[idx.file], pcic.paths[idx.file], mode='wb')
}

#' After the data have finished downloading, we load them into R and extract the relevant
#' subsets, saving results to set of smaller files on disk that will be faster to load in future

# load the data into R and write output rasters and R object files if they don't already exist
if(any(!file.exists(c(pcic.out.path, pcic.out.rasters))))
{
  # create the raster output directory if it doesn't exist 
  my_dir(pcic.out.raster.dir)
  
  # open one of the nc files as raster to determine grid geometry
  nc.r = raster(pcic.paths[1], band=1)
  
  # mask/crop to `perim` after applying `buff` metres of padding and reprojecting
  perim.t = as(st_transform(st_buffer(uyrw.poly, uyrw.buff), crs=st_crs(nc.r)), 'Spatial')
  nc.r.clip = mask(crop(nc.r, perim.t), perim.t)
  idx.na = values(is.na(nc.r.clip))
  
  # build the lat/long coordinates table
  coords.tab = data.frame(coordinates(nc.r.clip)) %>% filter(!idx.na)
  n.spatpts = nrow(coords.tab)
  colnames(coords.tab) = c('long', 'lat')
  rownames(coords.tab) = paste0('grid', 1:n.spatpts)
  
  # build the output `pts` sf object (projected)
  geopts.sf = st_as_sf(coords.tab, coords=colnames(coords.tab), crs=st_crs(nc.r))
  pts.sf = st_transform(cbind(geopts.sf, data.frame(name=rownames(coords.tab))), crs.list$epsg)
  
  # initialize the other output tables
  n.days = nbands(nc.r)
  template.df = data.frame(matrix(NA, n.days, n.spatpts))
  colnames(template.df) = rownames(coords.tab)
  climdata.list = lapply(pcic.vn, function(x) template.df)
  
  # loop by variable to fill these tables and write rasterstacks
  for(idx.var in 1:length(pcic.vn))
  {
    # load entire time series
    varname = pcic.vn[idx.var]
    print(paste0('loading ', varname, ' (variable ', idx.var, '/', length(pcic.vn), ')...'))
    nc.in = stack(pcic.paths[varname])
    
    # clip to UYRW region and save as multiband raster
    nc.out = mask(crop(nc.in, perim.t), perim.t)
    writeRaster(nc.out, pcic.out.rasters[varname], overwrite=TRUE)
    
    # copy the data into tables list
    climdata.list[[varname]][] = t(values(nc.out)[!idx.na, ])
  }
  
  # create date vector to match rows and bands
  origin.date = as.Date(getZ(nc.r)) - 1
  pcic.dates = as.Date(1:n.days, origin=origin.date)

  # assemble everything into a list
  pcic.list = list(dates=pcic.dates,
                   tables=climdata.list,
                   coords=coords.tab,
                   coords_sf=pts.sf)

  # write the tabular and shapefile data to disk
  saveRDS(pcic.list, file=pcic.out.path)
  
} 

#' Finally, we zip the source files to save storage space. This step reduces storage requirements
#' by about one half (to 111GB)

# identify uncompressed source files that need to be zipped
idx.unzipped = file.exists(pcic.paths) & !file.exists(pcic.paths.zip)

# begin loop to zip each file
if(any(idx.unzipped))
{
  # loop to zip all files
  print(paste('zipping', sum(idx.unzipped), 'pcic source files'))
  pb = txtProgressBar(min=1, max=sum(idx.unzipped), style=3)
  for(idx.tozip in 1:sum(idx.unzipped))
  {
    # assign filenames and report progress
    idx.file = which(idx.unzipped)[idx.tozip]
    path.tozip = pcic.paths[idx.file]
    path.zipped = pcic.paths.zip[idx.file]
    setTxtProgressBar(pb, idx.tozip)
    
    # zip the file then delete the original, argument -j is to junk paths
    zip(path.zipped, files=path.tozip, extras='-j')
    unlink(path.tozip)
    
  }
  close(pb)
  
}


#'
#' ## Ben Livneh's meteorological dataset
#' 
#' The [Livneh](https://ciresgroups.colorado.edu/livneh/data) meteorological dataset is a daily
#' gridded reconstruction of weather for the period 1915-2013, covering the continental USA
#' (CONUS) and part of Canada at 1/16 degree resolution. It accompanies a large-scale
#' hydrological [VIC](https://github.com/UW-Hydro/VIC) model for the CONUS. See
#' [this J Clim paper](https://doi.org/10.1175/JCLI-D-12-00508.1) from
#' [Ben Livneh and colleagues](https://ciresgroups.colorado.edu/livneh2019/web/) describing
#' their methodology, and discussing applications. 
#' 
#' This chunk downloads the NetCDF files for the period 1950-2013 (for which wind data are
#' available), and processes them in the same way as the PCIC data, above. We downloade from the
#' archive linked in
#' [this Geophys Res Lett paper](https://doi.org/10.1002/2016GL069690).
#' 
#' Each of the source NetCDF files contains a month of spatially gridded (1/16 degree) daily
#' estimates of precip, tmax, tmin, and wind. Most are bzipped, but for the 1970-2005 period
#' only uncompressed files are published at this archive. These are compressed by this script
#' (to bz2) after downloading. 
#' 

# set up the directory for storage (make sure you have at least 60GB free space)
livneh.storage = here(meteo.meta['livneh_source', 'file'])
my_dir(livneh.storage)

# archive URL and filenames structure, variable names
livneh.domain = 'ftp://192.12.137.7/pub/dcp/archive/OBS/livneh2014.1_16deg/netcdf/daily'
livneh.fn.prefix = 'livneh_NAmerExt_15Oct2014'
livneh.fn.suffix = 'nc.bz2'
yrs.tofetch = 1950:2013
months.tofetch = formatC(1:12, width=2, flag='0')
livneh.vn = c('pcp', 'tmax', 'tmin', 'wnd') # SWAT names, in order produced by my_livneh_reader

# construct list of files to download and their URLs
yrmonth.tofetch = sapply(yrs.tofetch, function(yr) paste0(yr, months.tofetch))
livneh.fns = paste(livneh.fn.prefix, yrmonth.tofetch, livneh.fn.suffix, sep='.')
livneh.urls = file.path(livneh.domain, livneh.fns)

# paths on disk to write source files
livneh.paths = file.path(livneh.storage, livneh.fns)

# this R data file will store a gridpoint shapefile and tables containing the URYW subset
livneh.out.path = here(meteo.meta['livneh_uyrw', 'file'])

# this directory stores multiband rasters (band=day) for each variable
livneh.out.raster.dir = here(meteo.meta['livneh_raster', 'file'])
my_dir(livneh.out.raster.dir)
livneh.out.rasters = setNames(file.path(livneh.out.raster.dir, paste0(livneh.vn, '.tif')), nm=livneh.vn)

# download a source file only if it is missing on disk
idx.missfile = !file.exists(livneh.paths)
if(any(idx.missfile))
{
  # loop to download each file
  print(paste('downloading', sum(idx.missfile), 'files from', livneh.domain))
  pb = txtProgressBar(min=1, max=sum(idx.missfile), style=3)
  for(idx in 1:sum(idx.missfile))
  {
    # feeback on progress
    idx.file = which(idx.missfile)[idx]
    setTxtProgressBar(pb, idx)
    
    # attempt to download the compressed netcdf file...
    src.url = livneh.urls[idx.file]
    dest.path = livneh.paths[idx.file]
    tryCatch(
      {
        download.file(src.url, dest.path, mode='wb')
        # ... and if it fails with any error ...

      }, error = function(cond) {
      
        # ... download the uncompressed version 
        src.url = strsplit(src.url, '.bz2')[[1]]
        nc.tempfile = tempfile(tmpdir=livneh.storage, fileext='.nc')
        download.file(src.url, nc.tempfile, mode='wb')
        
        # bzip the file and delete uncompressed copy 
        cat('compressing netcdf file...')
        nc.bz2 = memCompress(readBin(nc.tempfile, raw(), file.info(nc.tempfile)$size), 'bzip2')
        writeBin(nc.bz2, dest.path)
        unlink(nc.tempfile)
      
    })

  }
}

#' Variables in the Livneh dataset include near-surface estimates of min/max temperature and
#' precipitation, derived on the grid from a combination of station records and model-based
#' interpolation. Wind speed data are also included for the period 1948-2011, from the
#' [NCEP reanalysis](https://www.cpc.ncep.noaa.gov/products/wesley/reanalysis.html).
#' 
#' This next chunk imports these variables into R, by bunzipping and loading each of the
#' netcdf files into R, clipping to the URYW region, then deleting the uncompressed file.
#' The import will be slow (expect it to take a few hours), but it requires far less storage
#' space, and only needs to be run once. The output (rds) files store the clipped data as
#' tables and multiband rasters, which are much more convenient for analysis in R. A
#' [helper function](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' `my_livneh_reader`, handles the extraction of these files. 

# import relevant subset of Livneh dataset and write output if it doesn't exist already
if(any(!file.exists(c(livneh.out.path, livneh.out.rasters))))
{
  # open all Livneh files and store in memory as object `livneh.in` (~2 hours to process)
  print(paste('decompressing and opening', length(livneh.paths), 'files in directory', livneh.storage))
  livneh.in = lapply(livneh.paths, function(fn) my_livneh_reader(fn, uyrw.poly, buff=3e3))
  
  # extract some reference data on the grid (identical for all `my_livneh_reader` calls)
  livneh.sf = livneh.in[[1]]$spat$pts
  livneh.coords = livneh.in[[1]]$tab$coords
    
  # combine the raster stacks to build complete times series as multiband raster
  livneh.stack = lapply(livneh.vn, function(vn) {
    stack(lapply(livneh.in, function(yrmo) yrmo$spat[[vn]]))
    })
  
  # write the rasters to disk
  lapply(livneh.vn, function(varname) {
    writeRaster(livneh.stack[[varname]], livneh.out.rasters[varname])
    })
  
  # combine tabular data to build complete times series as data tables
  livneh.tab = lapply(livneh.vn, function(vn) {
    do.call(rbind, lapply(livneh.in, function(yrmo) yrmo$tab[[vn]]))
    })
  
  # create dates vector to match order of rows in these tables (and of the raster's bands)
  livneh.dates.char = substr(names(livneh.stack[[1]]), 2, 11)
  livneh.dates.test = as.Date(livneh.dates.char, format='%Y.%m.%d')
  
  # the above should work if we trust the date-like band names in the rasters, however...
  idx.min = which.min(livneh.dates.test)
  print(livneh.dates.test[idx.min + (-2:2)])
  
  # ... we have a random date in the 1980s labeled as 1900-01-01! Safer to use an epoch...
  livneh.dates = as.Date(1:length(livneh.dates.char), origin=livneh.dates.test[1]-1)
  
  # assemble everything into a list
  livneh.list = list(dates=livneh.dates,
                     tables=livneh.tab,
                     coords=livneh.coords,
                     coords_sf=livneh.sf)
  
  # write the tabular and shapefile data to disk
  saveRDS(livneh.list, file=livneh.out.path)
  
}


#'
#' ## Daymet data
#' 
#' The [Daymet](https://daymet.ornl.gov/) dataset is a popular, high-resolution (~1km)
#' time series of daily data covering the period 1980-2017. The `daymetr` package makes
#' it very easy to fetch the data from R.
#' 
#' Like the code above, this chunk saves the source files to storage, loads them
#' into R, clips to the UYRW area, and writes the data as rds for convenience in R.
#' 

# set up the directory for storage (need ~7GB for download, ~1GB after compression)
daymet.storage = here(meteo.meta['daymet_source', 'file'])
my_dir(daymet.storage)

# select the spatial subset to download
daymet.bbox = st_bbox(uyrw.poly.geo)[c('ymax', 'xmin', 'ymin', 'xmax')]

# select the subset of years/variables to download, and paths to save them
daymet.vars = setNames(nm=c('tmin', 'tmax', 'srad', 'prcp', 'swe')) # daymet names
swat.vars = c('tmin', 'tmax', 'slr', 'pcp', 'swe') # SWAT names
daymet.years = 1980:2019
daymet.fns.prefix = lapply(daymet.vars, function(varname) paste0(varname, '_daily_', daymet.years))
daymet.fns = lapply(daymet.vars, function(varname) paste0(daymet.fns.prefix[[varname]], '_ncss.nc'))
daymet.paths = lapply(daymet.vars, function(varname) file.path(daymet.storage, daymet.fns[[varname]]))

#' After the UYRW subset of the Daymet data is extracted and saved, we compress the source NetCDF
#' files to reduce their size. We define the paths of these zipped copies now, so that we
#' can check for them in the next step, and avoid downloading things twice. 

# define the paths to the zipped daymet source files
daymet.paths.orig = unlist(daymet.paths)
daymet.paths.zip = gsub(pattern = '\\.nc$', '.zip', daymet.paths.orig)

#' Daymet files are in NetCDF format, split by year and variable. Similar to the above, we use
#' a [helper function](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' `my_daymet_reader`, to import these files into a large list of R objects containing a continuous
#' daily times series clipped to the UYRW region. This chunk loads all of the (clipped) netcdf data
#' into memory, requiring around 7GB. If your machine is low on RAM, you may need to modify the
#' script to build the output files in chunks. 

# download a file only if we have neither the zip or uncompressed source data on disk
idx.missfile = lapply(daymet.vars, function(varname) !file.exists(daymet.paths[[varname]]))
if(any(unlist(idx.missfile) & !file.exists(daymet.paths.zip)))
{
  # loop to download each file
  print(paste('downloading', sum(unlist(idx.missfile)), 'files from Daymet Web Services'))
  pb = txtProgressBar(min=1, max=sum(unlist(idx.missfile)), style=3)
  for(idx.var in 1:length(daymet.vars))
  {
    daymet.varname = daymet.vars[idx.var]
    if(any(idx.missfile[[idx.var]]))
    {
      # downloading multiple years at once seems to go faster
      if(all(idx.missfile[[idx.var]]))
      {
        download_daymet_ncss(location=daymet.bbox,
                             start=daymet.years[1],
                             end=daymet.years[length(daymet.years)],
                             param=daymet.varname,
                             frequency = 'daily',
                             path = daymet.storage)
        
        # compute progress index and print progress bar to console
        idx = sum(unlist(idx.missfile[1:idx.var]))
        setTxtProgressBar(pb, idx)
        
      } else {
        
        # download years individually
        for(idx.yr in 1:sum(idx.missfile[[idx.var]]))
        {
          # compute progress index and print progress bar to console
          idx = sum(unlist(idx.missfile[1:idx.var])) - sum(idx.missfile[[idx.var]]) + idx.yr
          setTxtProgressBar(pb, idx)
          
          # download the file
          daymet.year = daymet.years[which(idx.missfile[[idx.var]])[idx.yr]]
          download_daymet_ncss(location=daymet.bbox,
                               start=daymet.year,
                               end=daymet.year,
                               param=daymet.varname,
                               frequency = 'daily',
                               path = daymet.storage)
        }
      }
    }
  }
}

#' 
#' Daymet has an odd way of dealing with leap years: the 366th calendar day of these years (Dec 31st) 
#' is [omitted from the time series](https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html).
#' Since we need multi-year records spanning some of these gaps, we have to impute the missing data
#' to avoid temporal drift. We do this by linear interpolation (averaging) of the previous and next
#' days' data.
#' 
#' Note that the current CRAN version of `daymetr` (v1.4) mislabels the projection units of the
#' downloaded data: [see here](https://github.com/bluegreen-labs/daymetr/issues/40). This CRS bug is
#' corrected manually by our helper function, but as a result users may see a large number of warnings
#' from GDAL during the import process.
#' 
#' 

# define output file for tabular and shapefile data
daymet.out.path = here(meteo.meta['daymet_uyrw', 'file'])

# this directory stores multiband rasters (band=day) for each variable
daymet.out.raster.dir = here(meteo.meta['daymet_raster', 'file'])

# import relevant subset of daymet dataset and write output
if(any(!file.exists(daymet.out.path, daymet.out.raster.dir)))
{
  # reshape the list of files (one list entry per year) so we can lapply the import function
  nc.list = lapply(1:length(daymet.years), function(idx.yr) {
    sapply(setNames(nm=daymet.vars), function(varname) daymet.paths[[varname]][idx.yr])
    })

  # call the data import function on all years (ignore warnings about ill-defined CRS) 
  print('loading daymet netcdf data into memory...')
  daymet.in = lapply(nc.list, function(fn) my_daymet_reader(fn, uyrw.poly, buff=3e3))
  
  # extract some reference data on the grid (identical for all `my_daymet_reader` calls)
  daymet.sf = daymet.in[[1]]$spat$pts
  daymet.coords = daymet.in[[1]]$tab$coords
  
  # extract variable names 
  daymet.vn = setNames(nm=names(daymet.in[[1]]$tab)[-1])
  
  # identify leap years in the daymet time series 
  daymet.isleap = (daymet.years %% 4 == 0 & daymet.years %% 100 != 0) | (daymet.years %% 400 == 0)
  
  # loop over leap years to impute missing data
  print(paste('imputing December 31st data for', sum(daymet.isleap), 'leap years'))
  for(idx.leap in which(daymet.isleap))
  {
    # loop over variables to interpolate each one separately
    for(idx.var in 1:length(daymet.vn))
    {
      # load data from 365th day (December 30th) of current leap year
      varname = daymet.vn[idx.var]
      daymet.tab.dec30 = daymet.in[[idx.leap]][['tab']][[varname]][365,]
      daymet.tif.dec30 = daymet.in[[idx.leap]][['spat']][[varname]][[365]]
      
      # handle possibility of leap year occurring in latest year of available data
      if(idx.leap < length(daymet.years))
      {
        # first day of next-year data (Jan 1st) is loaded if it is available...
        daymet.tab.jan01 = daymet.in[[idx.leap+1]][['tab']][[varname]][1,]
        daymet.tif.jan01 = daymet.in[[idx.leap+1]][['spat']][[varname]][[1]]
        
      } else {
        
        # ...otherwise we just make a copy of the Dec 30th data of current leap year
        daymet.tab.jan01 = daymet.tab.dec30
        daymet.tif.jan01 = daymet.tif.dec30
      }
      
      # linear interpolation (averaging) of the two time points
      daymet.tab.dec31 = colMeans(rbind(daymet.tab.dec30, daymet.tab.jan01))
      daymet.tif.dec31 = mean(daymet.tif.dec30, daymet.tif.jan01)
      
      # append the interpolated data to the big `daymet.in` storage list
      daymet.in[[idx.leap]][['tab']][[varname]][366,] = daymet.tab.dec31
      daymet.in[[idx.leap]][['spat']][[varname]][[366]] = daymet.tif.dec31
    }
  }
  
  # combine the raster stacks to build complete times series as multiband raster
  daymet.stack = lapply(daymet.vn, function(vn) {
    stack(lapply(daymet.in, function(yr) yr$spat[[vn]]))
  })
  
  # create the raster storage directory and write the rasters to disk
  my_dir(daymet.out.raster.dir)
  daymet.out.rasters = setNames(file.path(daymet.out.raster.dir, paste0(swat.vars, '.tif')), nm=daymet.vn)
  lapply(daymet.vn, function(varname) {
    writeRaster(daymet.stack[[varname]], daymet.out.rasters[varname])
  })
  
  # combine tabular data to build complete times series as data tables
  daymet.tab = lapply(daymet.vn, function(vn) {
    do.call(rbind, lapply(daymet.in, function(yrmo) yrmo$tab[[vn]]))
  })
  
  # rename variables
  names(daymet.tab) = swat.vars
  
  # create date vector to match rows and bands
  origin.date = as.Date(paste(daymet.years[1], '01', '01', sep='/')) - 1
  daymet.dates = as.Date(1:nlayers(daymet.stack[[varname]]), origin=origin.date)
  
  # assemble everything into a list
  daymet.list = list(dates=daymet.dates,
                     tables=daymet.tab,
                     coords=daymet.coords,
                     coords_sf=daymet.sf)
  
  # write the tabular and shapefile data to disk
  saveRDS(daymet.list, file=daymet.out.path)
  
  
} 

#' Now that the UYRW subset of the Daymet data has been extracted and copied, we can zip the
#' source NetCDF files to reduce their size. We get a very good compression ratio (~5-25%),
#' so this step reduces storage requirements from ~6.6GB to under 1GB

# identify uncompressed source files that need to be zipped
idx.unzipped = file.exists(daymet.paths.orig) & !file.exists(daymet.paths.zip)

# begin loop to zip each file
if(any(idx.unzipped))
{
  # loop to zip all files
  print(paste('zipping', sum(idx.unzipped), 'Daymet source files'))
  pb = txtProgressBar(min=1, max=sum(idx.unzipped), style=3)
  for(idx.tozip in 1:sum(idx.unzipped))
  {
    # assign filenames and report progress
    idx.file = which(idx.unzipped)[idx.tozip]
    path.tozip = daymet.paths.orig[idx.file]
    path.zipped = daymet.paths.zip[idx.file]
    setTxtProgressBar(pb, idx.tozip)
    
    # zip the file then delete the original, -j flag drops the paths in the zip
    zip(path.zipped, files=path.tozip, extras='-j')
    unlink(path.tozip)
  
  }
  close(pb)
  
}

#' visualization: make a plot of the three datasets side by side for tmax on a common date


#' create the multi-panel raster image
#' ![gridded reconstructions](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/meteo_gridded.png)
if(!file.exists(here(meteo.meta['img_meteo', 'file'])))
{
  # pull the dates vectors for all datasets and print their ranges
  wdat.names = c('pnwnamet', 'livneh', 'daymet')
  wdat.paths = setNames(c(pcic.out.path, livneh.out.path, daymet.out.path), wdat.names)
  dates.meteo = lapply(wdat.paths, function(path) readRDS(path)$dates)
  dates.range = sapply(dates.meteo, function(dates) paste(format(range(dates), '%Y'), collapse=' to '))
  
  # define variable to extract and identify the raster files to open
  vn.eg = 'tmax'
  fn.tif = paste0(vn.eg, '.tif')
  tif.paths = sapply(wdat.names, function(nm) file.path(here(meteo.meta[paste0(nm, '_raster'), 'file']), fn.tif))
  
  # set a date to use as example and open this band of the rasterstack for each model 
  date.eg = as.Date('1987-07-01')
  vn.tif = lapply(wdat.names, function(nm) raster(tif.paths[nm], band=which(dates.meteo[[nm]]==date.eg)))
  
  # create a masking polygon with buffer around perimeter of watershed
  uyrw.poly.buff = st_buffer(uyrw.poly, uyrw.buff)
  uyrw.poly.buff.sp = as(uyrw.poly.buff, 'Spatial')
  uyrw.poly.sp = as(uyrw.poly, 'Spatial')
  
  # load hillshade from disk and clip to perimeter, leaving buffer to draw over with tmax data
  dem.meta = my_metadata('get_dem')
  hs.tif = raster(here(dem.meta['hillshade', 'file']))
  hsclip.tif = crop(mask(hs.tif, uyrw.poly.sp), uyrw.poly.buff.sp)
  
  # upsample the temperature rasters to same grid as hillshade (for plotting purposes)
  vn.warped.tif = lapply(vn.tif, function(tif) projectRaster(tif, hsclip.tif, method='ngb'))
  # Note: gdalwarp can do this much faster, but with only 3 small rasters, `projectRaster` is easier;
  
  # load the plotting parameters used in get_basins.R
  tmap.pars = readRDS(here(my_metadata('get_basins')['pars_tmap', 'file']))
  
  # set up two palettes: one for hillshading and one for temperature
  nc = 1e3
  hs.palette = gray(0:nc/nc)
  tmax.palette = rev(brewer.pal('Spectral', n=11))
  legend.string = 'tmax (C)'
  tmax.range = apply(sapply(vn.warped.tif, function(tif) cellStats(tif, range)), 1, range)[c(1,4)]
  tmax.breaks = pretty(tmax.range)
  
  # loop to build the three panels
  tmap.tmax = vector(mode='list', length=length(vn.warped.tif))
  for(idx.src in 1:length(vn.warped.tif))
  {
    title.string = paste0(names(dates.range)[idx.src],' (', dates.range[idx.src], ' daily)\n', date.eg)

    # build the tmap object
    tmap.tmax[[idx.src]] = tm_shape(hsclip.tif, raster.downsample=T, bbox=st_bbox(uyrw.poly)) +
        tm_raster(palette=hs.palette, legend.show=FALSE) +
      tm_shape(vn.warped.tif[[idx.src]], raster.downsample=T) +
        tm_raster(palette=tmax.palette, title=legend.string, style='cont', alpha=0.9, breaks=tmax.breaks) +
      tm_shape(uyrw.poly) +
        tm_borders(col='black') +
      tmap.pars$layout +
      tm_layout(main.title=title.string,
                legend.position = c('right', 'top')) 

  }

  # render the plot
  tmap_save(tm=tmap_arrange(tmap.tmax), 
            here(meteo.meta['img_meteo', 'file']), 
            width=3*tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}


#+ include=FALSE
# Development code
#my_markdown('get_meteo')
