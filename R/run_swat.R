#' ---
#' title: "run_swat.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **run_swat.R**: opens Mill Creek QSWAT3 model, runs SWAT2012 simulation, displays results (WORK IN PROGRESS)
#' 
#' After running `make_qswat`, users should be able to complete the 'Delineate Watershed' and 'Create HRUs'
#' steps in QSWAT3. The next step in the worflow is to define weather inputs and set the model parameters.
#' This should initially be done be via QSWAT (which opens SWATEditor) using default parameters and the
#' weather inputs files written by`make_qswat`.
#' 
#' Eventually we plan to set up parameters and weather inputs directly by writing the inputs (.pcp, .tmp,
#' .cio, etc) to the SWAT2012 project directory (TxtInOut)

#' The following scripts should be run first to fetch and process data inputs:
#' The following scripts should be run first to fetch and process data inputs:
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_weatherstations](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_weatherstations.md),
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_soils](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_soils.md),
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_landuse.md), and
#' [make_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_qswat.md).

#' ## libraries
#' See the
#' [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
#' for other required libraries

library(here)
source(here('R/get_helperfun.R'))

#'
#' ## project data

# load meteorological data and qswat setup parameters
meteo.meta = my_metadata('get_meteo')
makeqswat.meta = my_metadata('make_qswat')

# path to QSWAT source and I/O files
swat.dir = here(makeqswat.meta['swat_source', 'file'])
swat.projdir = makeqswat.meta['swat_proj', 'file']
swat.name = basename(swat.projdir)

#+ echo=FALSE
# define the files to write
{
  files.towrite = list(
    
    # directory to write SWAT wdat input text files
    c(name='swat_weatherstn',
      file=file.path(swat.projdir, 'UYRW_weather_data'),
      type='directory',
      description='directory for writing SWAT weather input text files')
  )
}

# store as metadata table csv
runqswat.meta = my_metadata('run_qswat', files.towrite, overwrite=TRUE)
print(runqswat.meta[, c('file', 'type')])

#'
#' ## write weather station data and position files
#' 

# For now we use Ben Livneh's climatic reconstruction 
wdat.in = readRDS(here(meteo.meta['livneh_uyrw', 'file']))

# map variable names from input dataset to SWAT names
vn.in = list(temp=c('tmax', 'tmin'), prec='prec', wind='wind')

# define and create the weather station data directory
wstn.dir = runqswat.meta['swat_weatherstn', 'file']
my_dir(wstn.dir)

# Load study area polygon and add 3km buffer for intersection with weather grid points
bd.poly = readRDS(makeqswat.meta['swat_boundary', 'file'])
bd.poly.buff = st_buffer(bd.poly, dist=units::set_units(3, km))

# load DEM and call the weather station data export function
dem.tif = dem.in = raster(dem.meta['dem', 'file'])
my_grid2wstn(wdat.in, vn.in, bd.poly.in=bd.poly.buff, dem.in=dem.tif, exdir=wstn.dir)
















#' At last it works! 
#' 
#' ISSUES TO WORK OUT: 
#' 
#' - adding custom weather data
#' - elevation band options
#' - lakes as reservoirs
#' - test adding full usersoil database
#' 


#my_markdown('run_swat')
