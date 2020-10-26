get\_meteo.R
================
Dean Koch
2020-10-26

**Mitacs UYRW project**

**get\_meteo**: download and import various long-term meteorological
reconstructions

## background

SWAT+ uses weather inputs at the subbasin level to drive the model’s
day-to-day water budget. Ideally these inputs would be copied directly
from (quality-controlled) station observations of daily weather data,
such as total precipitation. In practice, two things prevent that from
happening:

  - to get sensible subbasin-level inputs, we have to aggregate weather
    states over the subbasin extent.

  - real spatio-temporal records of weather states are inherently sparse
    (space-time is continuous, point data are not).

SWAT+ can desparsify the record by simulating daily point data using
MCMC-based weather generator models. These are parametrized by the local
long-term climatology, so they are usually located at points where
long-term historical gage data are available. SWAT+ ships with a large
set of parametrized weather generators.

The aggregation method in SWAT+ is very simple: Each subbasin centroid
is mapped to the nearest point weather record, generated or observed.
Orographic adjustments of precipitation (using elevation bands) can
introduce additional detail downstream, but all weather drivers enter
the model through a nearest-neighbour interpolation of the
available/simulated point data.

## gridded meteorological reconstructions

Particularly when weather generators are sparse, this interpolation
method can be too simplistic. Systematic bias becomes a problem
particularly in mountaineous terrain like the URYW (see
[here](https://doi.org/10.1016/j.jhydrol.2017.03.008), for example). An
alternative is to replace the SWAT+ weather generators with a
high-resolution gridded reconstruction of local weather.

[This recent paper](https://doi.org/10.1111/1752-1688.12819) discusses
the approach of using an ensemble of grids from different sources. To
test this idea, we download three long-term gridded climatic datasets
below.

  - The [Daymet](https://daymet.ornl.gov/) dataset is a popular,
    high-resolution (\~1km) time series of daily data covering the
    period 1980-2017.

  - The
    [PNWNAmet](https://www.pacificclimate.org/data/daily-gridded-meteorological-datasets)
    meteorological dataset covers the period 1945-2012 at 1/16 degree
    resolution (\~6km). It has a slight bias towards overpredicting wet
    days, but has been favourably compared against other gridded
    datasets in hydrology models of mountainous areas in Northwestern
    North America. See [this Nature
    paper](https://doi.org/10.1038/sdata.2018.299) for information on
    methodology.

  - The [Livneh](https://ciresgroups.colorado.edu/livneh/data)
    meteorological dataset is a daily gridded reconstruction of weather
    for the period 1915-2013, covering the continental USA (CONUS) and
    part of Canada at 1/16 degree resolution. It accompanies a
    large-scale hydrological [VIC](https://github.com/UW-Hydro/VIC)
    model for the CONUS. See [this J Clim
    paper](https://doi.org/10.1175/JCLI-D-12-00508.1) from [Ben Livneh
    and colleagues](https://ciresgroups.colorado.edu/livneh2019/web/)
    describing their methodology, and discussing applications.
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

    ##                                          file          type
    ## livneh_uyrw     data/prepared/livneh_uyrw.rds R list object
    ## daymet_source              data/source/daymet     directory
    ## pnwnamet_source       data/source/pnwnamet.nc        NetCDF
    ## livneh_source              data/source/livneh     directory
    ## metadata          data/get_meteo_metadata.csv           CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_livneh_metadata.csv)
in the `/data` directory.

Load some of the data prepared earlier

``` r
# load CRS info list and watershed polygon from disk
basins.meta = my_metadata('get_basins')
crs.list = readRDS(here(basins.meta['crs', 'file']))
uyrw.poly = readRDS(here(basins.meta['boundary', 'file']))

# prepare a mask in geographical coordinates, to pull data from UYRW region only
uyrw.poly.geo = st_transform(uyrw.poly, crs=crs.list$epsg.geo)
```

## Download Daymet data

The `daymetr` package makes this very easy

``` r
# set up the directory for storage (source files are ~21GB total)
daymet.storage = here(meteo.meta['daymet_source', 'file'])
my_dir(daymet.storage)

# select the spatial subset to download
daymet.bbox = st_bbox(uyrw.poly.geo)[c('ymax', 'xmin', 'ymin', 'xmax')]

# select the subset of years/variables to download
daymet.vars = setNames(nm=c('tmin', 'tmax', 'srad', 'prcp', 'swe'))
daymet.years = 1980:2019
daymet.fns.prefix = lapply(daymet.vars, function(varname) paste0(varname, '_daily_', daymet.years))
daymet.fns = lapply(daymet.vars, function(varname) paste0(daymet.fns.prefix[[varname]], '_ncss.nc'))
daymet.paths = lapply(daymet.vars, function(varname) file.path(daymet.storage, daymet.fns[[varname]]))

# download a file only if it is missing on disk
idx.missfile = lapply(daymet.vars, function(varname) !file.exists(daymet.paths[[varname]]))
if(any(unlist(idx.missfile)))
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
  

# open the daymet files

# # change daymet proj4 string manually to fix a bug (units should be "km" not "m")
# daymet.proj4 = '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +a=6378137 +b=6356752.314706705 +units=km +no_defs'
# zz = st_transform(uyrw.poly, crs=daymet.proj4)
# xx = stack(here('data/source/daymet', 'tmin_daily_1982_ncss.nc'))
# projection(xx) = daymet.proj4 
# yy = mask(xx, as(zz, 'Spatial'))
# plot(yy[[9]])
# 
# millcreek.list = readRDS(here(basins.meta['millcreek', 'file']))
# 
# # define its drainage boundary
# subbasin.poly = as(st_transform(millcreek.list$boundary, crs=daymet.proj4), 'Spatial')
# y = mask(crop(yy[[9]], subbasin.poly), subbasin.poly)
# plot(y)
# plot(subbasin.poly, add=T)
# sum(!is.na(as.matrix(y)))
```

## Download Livneh data

This chunk downloads the NetCDF files for the period 1950-2013, and
reshapes them into a list of dataframes for easier handling in R. The
data are downloaded from the archive linked in [this Geophys Res Lett
paper](https://doi.org/10.1002/2016GL069690).

Variables include near-surface estimates of min/max temperature,
precipitation, relative humidity, and solar radiation, which are derived
on the grid from a combination of station records and model-based
interpolation. Wind speed data are also included for the period
1948-2011, from the [NCEP
reanalysis](https://www.cpc.ncep.noaa.gov/products/wesley/reanalysis.html).

This is a large dataset, with a total size of \~59GB compressed. Each of
the source NetCDF files contains a month of spatially gridded (1/16
degree) daily estimates of precip, tmax, tmin, and wind. Most are
bzipped, but for the 1970-2005 period only uncompressed files are
published at this archive. These are compressed by this script (to bz2,
like the others) after downloading.

The script may be interrupted and restarted at a later time if you need
to pause the download. It will scan for files already downloaded, and
skip any that exist already in the `livneh_source` directory.

``` r
# set up the directory for storage (make sure you have at least 60GB free space)
livneh.storage = here(meteo.meta['livneh_source', 'file'])
my_dir(livneh.storage)

# archive URL and filenames structure
livneh.domain = 'ftp://192.12.137.7/pub/dcp/archive/OBS/livneh2014.1_16deg/netcdf/daily'
livneh.fn.prefix = 'livneh_NAmerExt_15Oct2014'
livneh.fn.suffix = 'nc.bz2'
yrs.tofetch = 1950:2013
months.tofetch = formatC(1:12, width=2, flag='0')

# construct list of files to download and their URLs
yrmonth.tofetch = sapply(yrs.tofetch, function(yr) paste0(yr, months.tofetch))
livneh.fns = paste(livneh.fn.prefix, yrmonth.tofetch, livneh.fn.suffix, sep='.')
livneh.urls = file.path(livneh.domain, livneh.fns)

# paths on disk to write
livneh.paths = file.path(livneh.storage, livneh.fns)

# download a file only if it is missing on disk
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


# IN DEVELOPMENT: import relevant subset of Livneh dataset
if(0)
{
  livneh.toopen = livneh.paths[1:((length(yrs.tofetch)-1)*12)]
  livneh.out = lapply(livneh.toopen, function(fn) my_livneh_reader(fn, uyrw.poly, buff=3e3))
  
  # these objects should be identical for all `my_livneh_reader` calls
  livneh.sf = livneh.out[[1]]$spat$pts
  livneh.coords = livneh.out[[1]]$tab$coords
  livneh.vn = setNames(nm=names(livneh.out[[1]]$tab)[-1])
    
  # combine the raster stacks to build complete times series as multiband raster
  livneh.stack = lapply(livneh.vn, function(vn) stack(lapply(livneh.out, function(yrmo) yrmo$spat[[vn]])))
  
  # combine tabular data to build complete times series as data tables
  livneh.tab = lapply(livneh.vn, function(vn) do.call(rbind, lapply(livneh.out, function(yrmo) yrmo$tab[[vn]])))
  
  # add a column indicating dates
  livneh.date.char = names(livneh.stack[[1]])
  str_replace_all(substr(names(livneh.stack[[1]]), 2, 11), '.', '-')
  
  
  livneh.date = substr(livneh.date.char, 2, nchar(livneh.date.char))
  
  x = livneh.date[1]
  
  
  plot(1:9862, livneh.tab[['tmax']][,1], pch=NA)
  lines(1:9862, livneh.tab[['tmax']][,1])
}
```

## Download PNWNAmet data

This chunk downloads a large (\~50GB) NetCDF file containing the entire
time series (1945-2012), crops to the UYRW region, and reshapes the data
into a list of variable- specific tables for easier handling in R.

``` r
# PCIC download URL, filename, and write path
pcic.domain = 'https://data.pacificclimate.org/data/gridded_observations'
pcic.fn = 'PNWNAmet_pr.nc.nc' # change pr -> tmax etc
pcic.url = file.path(pcic.domain, pcic.fn)
pcic.path = here(meteo.meta['pnwnamet_source', 'file'])

# download the file only if it is missing on disk
if(!file.exists(pcic.path))
{
  download.file(pcic.url, pcic.path, mode='wb')
}
```
