get\_meteo.R
================
Dean Koch
2020-10-28

**Mitacs UYRW project**

**get\_meteo**: download and import various long-term meteorological
reconstructions

## background

SWAT+ uses weather inputs at the subbasin level to drive the model’s
day-to-day water budget. Ideally these inputs would be copied directly
from quality-controlled station observations of daily weather data, such
as total precipitation. In practice, two things prevent that from
happening:

  - to get sensible subbasin-level inputs, we have to aggregate weather
    states over the full subbasin extent.

  - real spatio-temporal records of weather states are inherently
    sparse. Space-time is continuous, point data are not.

SWAT+ can desparsify the record by simulating daily point data using
MCMC-based weather generator models. These are parametrized by the local
long-term climatology, so they are usually located at points where
long-term historical gage data are available. SWAT+ ships with a large
set of parametrized weather generators.

The aggregation method in SWAT+ is very simple: Each subbasin centroid
is mapped to the nearest point weather record (generated or observed).
Orographic adjustments of precipitation (using elevation bands) can
introduce additional detail downstream, but ultimately all of the
weather drivers enter the model through a nearest-neighbour
interpolation of the available/simulated point data.

## gridded meteorological reconstructions

Depending on the detail level of the subbasin delineation, this
nearest-neighbour method may be too simplistic. The introduced bias can
be particularly problematic in mountaineous terrain like the URYW (see
[this discussion](https://doi.org/10.1016/j.jhydrol.2017.03.008), for
example). One alternative is to replace the SWAT+ weather generators
with a high- resolution gridded reconstruction of local weather.

[This recent paper](https://doi.org/10.1111/1752-1688.12819) discusses
the use of an ensemble of grids from different sources. We will be
exploring this idea in our project, so we download three long-term
gridded climatic datasets in the script below:

  - [Daymet (1980-2017)](https://daymet.ornl.gov/)

  - [PNWNAmet
    (1945-2012)](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets)

  - [Livneh (1950-2013)](https://ciresgroups.colorado.edu/livneh/data)

Users of this script should be aware of the filesizes involved. The
Daymet, PNWNAmet, and Livneh source files will consume 7GB, 12GB, and
58GB of space, respectively. Expect these downloads to take several days
to complete.

This script downloads the full source datasets, then extracts/imports
the relevant subsets containing data on the URYW, writing them into much
smaller R data (rds) files.

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
should be run before this script.

## libraries

See the [get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
for other required libraries

``` r
library(here)
source(here('R/get_helperfun.R'))
#library(R.utils)
library(daymetr)
library(stringr)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
meteo.meta = my_metadata('get_meteo', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/get_meteo_metadata.csv"

``` r
print(meteo.meta[, c('file', 'type')])
```

    ##                                                 file          type
    ## livneh_source                     data/source/livneh     directory
    ## livneh_uyrw            data/prepared/livneh_uyrw.rds R list object
    ## livneh_rasters     data/prepared/livneh_uyrw_rasters     directory
    ## daymet_source                     data/source/daymet     directory
    ## daymet_uyrw            data/prepared/daymet_uyrw.rds R list object
    ## daymet_rasters     data/prepared/daymet_uyrw_rasters     directory
    ## pnwnamet_source                 data/source/pnwnamet     directory
    ## pnwnamet_uyrw        data/prepared/pnwnamet_uyrw.rds R list object
    ## pnwnamet_rasters data/prepared/pnwnamet_uyrw_rasters     directory
    ## metadata                 data/get_meteo_metadata.csv           CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_livneh_metadata.csv)
in the `/data` directory.

All of the pertinent meteorological data for our SWAT+ model of the UYRW
will be stored in the rds files listed as `livneh_uyrw`, `daymet_uyrw`,
and `pnwnamet_source` in the metadata table. This script also saves a
copy of these data as multiband rasters, with one band per day in the
time series, and one file per variable for each of the data sources.

These GeoTIFF files preserve the original gridded projection of the
data, allowing easy visualization of the weather grids in future
analysis (using `plot(raster(...))`). Note that different sources have
different projections. The tabular data, however are all accompanied by
geographical (lat/long) coordinates, as needed in SWAT+; whereas the
points shapefile is projected to our UYRW project reference system
(UTM).

An xml file is generated when we save the rasters (using `writeRaster`).
This contains precomputed summary stats (mean, sd, etc) for the large
rasters, which may speed up future tasks. To disable the xml files, do
`rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")`.

To clip the gridded datasets to size, we need a watershed boundary
polygon. This boundary will be padded by a buffer zone (the value of
`uyrw.buff` in metres) in order to capture adjacent grid points that may
be closer to a subbasin centroid than any of the interior ones. Load
some of the data prepared earlier

``` r
# load CRS info list and watershed polygon from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))

# prepare a mask in geographical coordinates
uyrw.poly.geo = st_transform(uyrw.poly, crs=crs.list$epsg.geo)

# define 3km buffer around the perimeter of the study area
uyrw.buff = 3e3 # meters
```

## PNWNAmet data

The
[PNWNAmet](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets)
meteorological dataset covers the period 1945-2012 at 1/16 degree
resolution (\~6km). See [this Nature
paper](https://doi.org/10.1038/sdata.2018.299) for information on
methodology.

The chunk below downloads four large NetCDF files (\~128GB total)
containing the entire time series of the four variables (‘tmax’, ‘tmin’,
‘prec’, ‘wind’), clips to the UYRW region, and reshapes the data into a
list of variable-specific tables for easier handling in R.

``` r
# PCIC download URL, variable names, filenames, and write paths
pcic.domain = 'https://data.pacificclimate.org/data/gridded_observations'
pcic.vn = setNames(nm=c('tmin', 'tmax', 'prcp', 'wind')) # my names
pcic.vars = setNames(c('tasmin', 'tasmax', 'pr', 'wind'), nm=pcic.vn) # source names
pcic.fn = paste0('PNWNAmet_', pcic.vars, '.nc.nc')
pcic.url = setNames(file.path(pcic.domain, pcic.fn), nm=pcic.vn)
pcic.paths = setNames(here(meteo.meta['pnwnamet_source', 'file'], pcic.fn), nm=pcic.vn)

# define the paths to the zipped pnwnamet source files
pcic.paths.zip = gsub(pattern = '\\.nc\\.nc$', '.zip', pcic.paths)

# define processed output files
pcic.out.path = here(meteo.meta['pnwnamet_uyrw', 'file'])
pcic.out.raster.dir = here(meteo.meta['pnwnamet_raster', 'file'])
my_dir(pcic.out.raster.dir)
pcic.out.rasters = setNames(file.path(pcic.out.raster.dir, paste0(pcic.vn, '.tif')), nm=pcic.vn)
```

The script may be interrupted and restarted at a later time if you need
to pause the download. It will scan for files already downloaded, and
skip any that exist already in the `livneh_source` directory.

``` r
# check for existing unzipped source data
idx.missfile = !file.exists(pcic.paths)

# download a file only if we have neither the zip or uncompressed source data on disk
if(any(idx.missfile & !file.exists(pcic.paths.zip)))
{
  for(idx.file in which(idx.missfile))
    download.file(pcic.url[idx.file], pcic.paths[idx.file], mode='wb')
}
```

After the data have finished downloading, we load them into R and
extract the relevant subsets, saving results to set of smaller files on
disk that will be faster to load in future

``` r
# load the data into R and write output rasters and R object files if they don't already exist
if(any(!file.exists(c(pcic.out.path, pcic.out.rasters))))
{
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
```

Finally, we zip the source files to save storage space. This step
reduces storage requirements by about one half (to \~82GB)

``` r
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
    
    # zip the file then delete the original
    zip(path.zipped, files=path.tozip)
    unlink(path.tozip)
    
  }
  close(pb)
  
}
```

## Ben Livneh’s meteorological dataset

The [Livneh](https://ciresgroups.colorado.edu/livneh/data)
meteorological dataset is a daily gridded reconstruction of weather for
the period 1915-2013, covering the continental USA (CONUS) and part of
Canada at 1/16 degree resolution. It accompanies a large-scale
hydrological [VIC](https://github.com/UW-Hydro/VIC) model for the CONUS.
See [this J Clim paper](https://doi.org/10.1175/JCLI-D-12-00508.1) from
[Ben Livneh and
colleagues](https://ciresgroups.colorado.edu/livneh2019/web/) describing
their methodology, and discussing applications.

This chunk downloads the NetCDF files for the period 1950-2013 (for
which wind data are available), and processes them in the same way as
the PCIC data, above. We downloade from the archive linked in [this
Geophys Res Lett paper](https://doi.org/10.1002/2016GL069690).

Each of the source NetCDF files contains a month of spatially gridded
(1/16 degree) daily estimates of precip, tmax, tmin, and wind. Most are
bzipped, but for the 1970-2005 period only uncompressed files are
published at this archive. These are compressed by this script (to bz2)
after downloading.

``` r
# set up the directory for storage (make sure you have at least 60GB free space)
livneh.storage = here(meteo.meta['livneh_source', 'file'])
my_dir(livneh.storage)

# archive URL and filenames structure, variable names
livneh.domain = 'ftp://192.12.137.7/pub/dcp/archive/OBS/livneh2014.1_16deg/netcdf/daily'
livneh.fn.prefix = 'livneh_NAmerExt_15Oct2014'
livneh.fn.suffix = 'nc.bz2'
yrs.tofetch = 1950:2013
months.tofetch = formatC(1:12, width=2, flag='0')
livneh.vn = c('prec', 'tmax', 'tmin', 'wind') # my names

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
        # ... and if it fails with any error ...
        download.file(src.url, dest.path, mode='wb')

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
```

Variables in the Livneh dataset include near-surface estimates of
min/max temperature and precipitation, derived on the grid from a
combination of station records and model-based interpolation. Wind speed
data are also included for the period 1948-2011, from the [NCEP
reanalysis](https://www.cpc.ncep.noaa.gov/products/wesley/reanalysis.html).

This next chunk imports these variables into R, by bunzipping and
loading each of the netcdf files into R, clipping to the URYW region,
then deleting the uncompressed file. The import will be slow (expect it
to take a few hours), but it requires far less storage space, and only
needs to be run once. The output (rds) files store the clipped data as
tables and multiband rasters, which are much more convenient for
analysis in R.

A [helper
function](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
`my_livneh_reader`, handles the opening of these files. This will take a
long time because each file must be decompressed to a temporary file
before it can be loaded and subsetted.

``` r
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
```

## Daymet data

The [Daymet](https://daymet.ornl.gov/) dataset is a popular,
high-resolution (\~1km) time series of daily data covering the period
1980-2017. The `daymetr` package makes it very easy to fetch the data
from R.

Like the code above, this chunk saves the source files to storage, loads
them into R, clips to the UYRW area, and writes the data as rds for
convenience in R.

``` r
# set up the directory for storage (need ~7GB for download, ~1GB after compression)
daymet.storage = here(meteo.meta['daymet_source', 'file'])
my_dir(daymet.storage)

# select the spatial subset to download
daymet.bbox = st_bbox(uyrw.poly.geo)[c('ymax', 'xmin', 'ymin', 'xmax')]

# select the subset of years/variables to download, and paths to save them
daymet.vars = setNames(nm=c('tmin', 'tmax', 'srad', 'prcp', 'swe'))
daymet.years = 1980:2019
daymet.fns.prefix = lapply(daymet.vars, function(varname) paste0(varname, '_daily_', daymet.years))
daymet.fns = lapply(daymet.vars, function(varname) paste0(daymet.fns.prefix[[varname]], '_ncss.nc'))
daymet.paths = lapply(daymet.vars, function(varname) file.path(daymet.storage, daymet.fns[[varname]]))
```

After the UYRW subset of the Daymet data is extracted and saved, we will
compress the source NetCDF files to reduce their size. We define the
paths of these zipped copies now, so that we can check for them in the
next step, and avoid downloading things twice.

``` r
# define the paths to the zipped daymet source files
daymet.paths.orig = unlist(daymet.paths)
daymet.paths.zip = gsub(pattern = '\\.nc$', '.zip', daymet.paths.orig)
```

Daymet files are in NetCDF format, split by year and variable. Similar
to the above, we use a [helper
function](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
`my_daymet_reader`, to import these files into a large list of R objects
containing a continuous daily times series clipped to the UYRW region.
This chunk loads all of the (clipped) netcdf data into memory, requiring
around 7GB. If your machine is low on RAM, you may need to modify the
script to build the output files in chunks.

``` r
# download a file only if we have neither the zip or uncompressed source data on disk
idx.missfile = lapply(daymet.vars, function(varname) !file.exists(daymet.paths[[varname]]))
if(any(unlist(idx.missfile) & !file.exists(daymet.paths.zip)) )
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
```

Daymet has an odd way of dealing with leap years: the 366th calendar day
of these years (Dec 31st) is [omitted from the time
series](https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html).
Since we need multi-year records spanning some of these gaps, we have to
impute the missing data to avoid temporal drift. We do this by linear
interpolation (averaging) of the previous and next days’ data.

Note that the current CRAN version of `daymetr` (v1.4) mislabels the
projection units of the downloaded data: [see
here](https://github.com/bluegreen-labs/daymetr/issues/40). This CRS bug
is corrected manually by our helper function, but as a result users will
see many warnings from GDAL during the import process.

``` r
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
  daymet.out.rasters = setNames(file.path(daymet.out.raster.dir, paste0(daymet.vn, '.tif')), nm=daymet.vn)
  lapply(daymet.vn, function(varname) {
    writeRaster(daymet.stack[[varname]], daymet.out.rasters[varname])
  })
  
  # combine tabular data to build complete times series as data tables
  daymet.tab = lapply(daymet.vn, function(vn) {
    do.call(rbind, lapply(daymet.in, function(yrmo) yrmo$tab[[vn]]))
  })
  
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
```

Now that the UYRW subset of the Daymet data has been extracted and
copied, we can zip the source NetCDF files to reduce their size. We get
a very good compression ratio (\~5-25%), so this step reduces storage
requirements from \~6.6GB to under 1GB

``` r
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
    
    # zip the file then delete the original
    zip(path.zipped, files=path.tozip)
    unlink(path.tozip)
  
  }
  close(pb)
  
}
```
