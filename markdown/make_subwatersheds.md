make\_subwatersheds.R
================
Dean Koch
2021-03-29

**Mitacs UYRW project**

**make\_subwatersheds.R**: divide the UYRW into subwatersheds to be
fitted with separate SWAT+ models

This script runs the TauDEM workflow on the DEM for the UYRW to
determine the channel network and drainage layout for the entire
watershed. It then partitions this area into subwatersheds based on the
locations of long-term (2+yr daily records) USGS streamflow gages, and a
minimum area threshold.

Our algorithm delineates subwatersheds along catchment boundaries such
that each one has gets an outlet at the site of a unique long-term USGS
gage record. In some cases a subwatershed located at the headwaters will
represent the full catchment for the gage (ie having no other long-term
gage records upstream); whereas the rest have inlets connecting them via
shared channels to other subwatersheds upstream.

In the no-inlet cases, SWAT+ models can be fitted independently using
the gage data at the outlet. To fit the other cases, we will provide
either simulation output from upstream models, or the observed flow data
from the gage that lies on the connecting channel. This
divide-and-conquer approach should allow us to:

1)  increase the model complexity (number of subbasins) on
    well-instrumented catchments, and vice versa
2)  take a more targeted approach to parameter-fitting, requiring fewer
    simulations
3)  fit SWAT+ to a large watershed sequentially, in managable stages

## libraries

Start by sourcing two helper scripts
([helper\_main.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_main.md)
and
[helper\_analysis.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_analysis.md))
which set up required libraries and directories and define some utility
functions.

``` r
library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))

# the rswat library reads/writes SWAT+ files and builds/runs the model
source(here('R/rswat.R'))

# source of the stateline polygons for the plots
library(maps)

# set the minimum number of daily gage records for each subwatershed outlet
obsmin = 2*365

# set the minimum subwatershed area in km^2
areamin = 5
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
subwatersheds.meta = my_metadata('get_subwatersheds', files.towrite, overwrite=TRUE)
```

    ## [1] "> writing metadata to: data/get_subwatersheds_metadata.csv"

``` r
print(subwatersheds.meta[, c('file', 'type')])
```

    ##                                                        file          type
    ## img_taudem                    graphics/my_upslope_areas.png   png graphic
    ## img_my_catchments_full      graphics/my_catchments_full.png   png graphic
    ## usgs_allcatchments     data/prepared/usgs_allcatchments.rds R list object
    ## img_my_catchments                graphics/my_catchments.png   png graphic
    ## taudem                            data/prepared/taudem_uyrw     directory
    ## usgs_catchments           data/prepared/usgs_catchments.rds R list object
    ## metadata                data/get_subwatersheds_metadata.csv           CSV

``` r
# load the DEM and basins info
dem.meta = my_metadata('get_dem')
basins.meta = my_metadata('get_basins')
streamgages.meta = my_metadata('get_streamgages')

# load some watershed geometry data
uyrw.dem = raster(here(dem.meta['dem', 'file']))
uyrw.flowline = readRDS(here(basins.meta['flowline', 'file']))
uyrw.waterbody = readRDS(here(basins.meta['waterbody', 'file']))
crs.list = readRDS(here(basins.meta['crs', 'file']))

# load the point representing the main outlet of the UYRW (Carter's Bridge)
uyrw.outlet.geo = readRDS(here(basins.meta['poi', 'file']))$pt$cartersbridge
uyrw.outlet = st_transform(uyrw.outlet.geo, crs=crs.list$epsg)
```

Start by running TauDEM on the full DEM, with only one outlet point (on
main stem). This will delineate the watershed upstream of that outlet,
automatically identifying the lowest (ie most detailed) feasible channel
delineation threshold by drop analysis.

Read about drop analysis
[here](https://hydrology.usu.edu/taudem/taudem5/help53/StreamDropAnalysis.html)

``` r
# run TauDEM on full watershed at max detail, or load files if this is already done
taudem.odir = subwatersheds.meta['taudem', 'file']
if(file.exists(taudem.odir))
{
  # load the list of file paths and descriptions
  taudem_uyrw = my_metadata('taudem', data.dir=subwatersheds.meta['taudem', 'file'])
  
} else {
  
  # run the TauDEM workflow (~10 mins)
  taudem_uyrw = my_taudem(uyrw.dem, taudem.odir, outlet.sf=uyrw.outlet, bsf=uyrw.flowline)
  
}
```

load some of the output

``` r
w = raster(here(taudem_uyrw['w', 'file']))
demnet = st_read(here(taudem_uyrw['demnet', 'file']))
```

    ## Reading layer `taudem_demnet' from data source `H:\UYRW_data\data\prepared\taudem_uyrw\taudem_demnet.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 19477 features and 17 fields
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 491361.9 ymin: 4866855 xmax: 601472.6 ymax: 5049907
    ## projected CRS:  WGS 84 / UTM zone 12N

``` r
subb = st_read(here(taudem_uyrw['subb', 'file']))
```

    ## Reading layer `subbasins' from data source `H:\UYRW_data\data\prepared\taudem_uyrw\subbasins.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 19377 features and 1 field
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 490354.8 ymin: 4866412 xmax: 602291.8 ymax: 5050297
    ## projected CRS:  WGS 84 / UTM zone 12N

The resulting channel and catchment network is too detailed to use in a
single SWAT+ model, but it will be useful right now for finding
catchments whose main outlets have a stream flow record from USGS. SWAT+
models can later be fitted to some of these catchments separately, then
combined together at the end.

The next chunk partitions the UYRW area by delineating gaged
subwatersheds.

``` r
# load USGS stream gage station points and time series
usgs.dat = readRDS(here(streamgages.meta['USGS_data', 'file']))$dat

# these are arranged in a list, where each entry corresponds to a distinct site (n=105)
usgs.pts = do.call(rbind, lapply(usgs.dat, function(site) site$sf[1,]))

# TODO: these totals weren't correct (counting only first dataframe?) - go back and fix the bug in get_streamgages.R
usgs.pts$count_nu = sapply(usgs.dat, function(x) sum(sapply(x$dat, nrow)))

# the number of observations varies a lot - pick out the sites with > 1000 records
usgs.nobs = sapply(usgs.dat, function(site) sum(sapply(site$dat, function(record) nrow(record))))
usgs.pts.long = usgs.pts[usgs.nobs > obsmin,]

# delineate subwatersheds based on these outlet locations, or load results list if this is already done
usgs.catchments.path = here(subwatersheds.meta['usgs_catchments', 'file'])
if(!file.exists(usgs.catchments.path))
{
  # run the catchments workflow (~1-5 mins, depending on thresholds)
  usgs.catchments = my_find_catchments(usgs.pts.long, demnet, subb, areamin)
  saveRDS(usgs.catchments, usgs.catchments.path)
  
} else {
  
  # load the list of file paths and descriptions
  usgs.catchments = readRDS(usgs.catchments.path)
  
}
```

Repeat for all sites (\>0 records total)

``` r
# delineate subwatersheds based on these outlet locations, or load results list if this is already done
usgs.allcatchments.path = here(subwatersheds.meta['usgs_allcatchments', 'file'])
if(!file.exists(usgs.allcatchments.path))
{
  # run the catchments workflow (~1-5 mins, depending on thresholds)
  usgs.allcatchments = my_find_catchments(usgs.pts, demnet, subb, areamin)
  saveRDS(usgs.allcatchments, usgs.allcatchments.path)
  
} else {
  
  # load the list of file paths and descriptions
  usgs.allcatchments = readRDS(usgs.allcatchments.path)
  
}
```

## plotting

Add a graphic to show the subwatershed delineation results
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_catchments.png)

``` r
# output paths defined above
img.taudem.path = here(subwatersheds.meta['img_taudem', 'file'])
img.longterm.path = here(subwatersheds.meta['img_my_catchments', 'file'])
img.all.path = here(subwatersheds.meta['img_my_catchments_full', 'file'])

# create boundary polygon and grab outlets points
bou = st_union(st_geometry(usgs.allcatchments$boundary))
pts = usgs.allcatchments$pts

# define breaks for the scale bar
km.breaks = c(0, pretty(1e-3 * diff(st_bbox(bou)[c('xmin', 'xmax')]) / 4, 2))

# get parks boundaries polygons
uyrw.nps = readRDS(here(basins.meta['ynp_boundary', 'file']))

# get polygons for state lines from `maps` package
states.sf = my_maps('state', outcrs=st_crs(bou)) %>% 
  filter(name %in% c('montana', 'wyoming', 'idaho'))

# make a text label for the park boundary
line.idaho = states.sf %>% filter(name == 'idaho') %>% st_geometry %>% st_cast('LINESTRING')
line.nps = uyrw.nps %>% st_geometry %>% st_cast('POLYGON') %>% st_cast('LINESTRING')
pt.parkbou = st_cast(st_intersection(line.nps, line.idaho), 'POINT')
pt.parkbou = st_sf(data.frame(name='Yellowstone Park boundary'), geometry=pt.parkbou[2]) 

# print some counts of the outlets points
idx.pts.multiyear = pts$count_nu > 2 * 365

# make labels for point legend
msg.usgs = paste(nrow(usgs.allcatchments$pts), 'streamflow record sites')
msg.multi = paste(sum(idx.pts.multiyear), 'sites have 2+ year records')

# make a copy of the catchments with multiyear records
mainout.idx = usgs.catchments$boundary$catchment_name == 'main_nr_livingston'
bou.cat = usgs.catchments$boundary

# define layout options for the plot
layout.out = tm_layout(main.title = 'USGS daily streamflow record sites in the UYRW and their catchments',
                       main.title.fontface ='bold',
                       main.title.size = 1,
                       main.title.position = 'center',
                       legend.outside = TRUE,
                       legend.outside.position = c('right', 'top'),
                       legend.outside.size = 0.3,
                       legend.text.size = 0.8,
                       inner.margins = rep(1e-2, 4),
                       frame = FALSE)

if(!file.exists(img.all.path))
{
  # initialize tmap object with elevation raster
  tmap.dem = tm_shape(bou) + 
    tm_polygons(alpha=0, col='black') + 
    tm_add_legend('symbol', 
                  labels=c(msg.usgs, msg.multi), 
                  col=c('black', 'red'), 
                  title='USGS sites') +
    tm_shape(uyrw.dem) +
    tm_raster(legend.reverse=TRUE, 
              palette=hcl.colors(1e3, 'Dark 3'), 
              style='cont', 
              title='elevation (m)')
  
  # add shapes and annotations
  tmap.out = tmap.dem +
    tm_shape(states.sf) + tm_borders(alpha=0.5, lty='dotted') +
    tm_shape(usgs.allcatchments$boundary) + tm_borders(alpha=0.8, col='black') +
    tm_shape(uyrw.nps) + tm_borders(col='white', lty='dashed') +
    tm_shape(pt.parkbou) + tm_text('name', col='white', size=0.6, just='left', ymod=0.5) +
    tm_shape(uyrw.flowline) + tm_lines(alpha=0.2, col='white', lwd=1) +
    tm_shape(uyrw.waterbody) + tm_polygons(col='blue', border.col=NULL) +
    tm_shape(pts) + tm_dots(alpha=0.7, size=0.2) + 
    tm_shape(pts[idx.pts.multiyear,]) + tm_dots(col='red', size=0.05) +
    tm_scale_bar(breaks=km.breaks, position=c('left', 'bottom'), text.size=0.8) + 
    tm_grid(n.x=2, n.y=2, projection=crs.list$epsg.geo, alpha=0.5) +
    layout.out
  
  # save a copy to disk
  tmap_save(tm=tmap.out, filename=img.all.path, height=3000, width=3000, pointsize=14)
}
```

Now make a version of only the multi-year records, and headwaters
catchments shaded
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_catchments_full.png)

``` r
if(!file.exists(img.longterm.path))
{
  # grab the (smaller subset of) outlets and identify subwatersheds without inlets
  pts2 = usgs.catchments$pts
  idx.hw = usgs.catchments$boundary$n_inlet == 0
  
  # modify the legend to include only the red points
  tmap.dem2 = tm_shape(bou) + 
    tm_polygons(alpha=0, col='black') + 
    tm_add_legend('symbol', 
                  labels=c(msg.usgs, msg.multi), 
                  col=c('black', 'red'), 
                  title='USGS sites') +
    tm_add_legend('title', title='(catchments without upstream gages shaded)') +
    tm_shape(uyrw.dem) +
    tm_raster(legend.reverse=TRUE, 
              palette=hcl.colors(1e3, 'Dark 3'), 
              style='cont', 
              title='elevation (m)')
  
  # add shapes and annotations
  tmap.out2 = tmap.dem2 +
    tm_shape(states.sf) + tm_borders(alpha=0.5, lty='dotted') +
    tm_shape(usgs.catchments$boundary) + tm_borders(alpha=0.8, col='black') +
    tm_shape(uyrw.nps) + tm_borders(col='white', lty='dashed') +
    tm_shape(pt.parkbou) + tm_text('name', col='white', size=0.6, just='left', ymod=0.5) +
    tm_shape(usgs.catchments$boundary[idx.hw,]) + tm_polygons(alpha=0.2, col='black', border.col=NULL) +
    tm_shape(uyrw.flowline) + tm_lines(alpha=0.2, col='white', lwd=1) +
    tm_shape(uyrw.waterbody) + tm_polygons(col='blue', border.col=NULL) +
    tm_shape(pts) + tm_dots(alpha=0.7, size=0.2) + 
    tm_shape(pts2) + tm_dots(col='red', size=0.05) +
    tm_scale_bar(breaks=km.breaks, position=c('left', 'bottom'), text.size=0.8) + 
    tm_grid(n.x=2, n.y=2, projection=crs.list$epsg.geo, alpha=0.5) +
    layout.out + 
    tm_layout(main.title = 'headwaters catchments with long-term USGS streamflow records')
  
  # save a copy to disk
  tmap_save(tm=tmap.out2, filename=img.longterm.path, height=3000, width=3000, pointsize=14)

}
```

Plot one of the TauDEM raster outputs - upstream contributing area
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/my_taudem.png)

``` r
if(!file.exists(img.longterm.path))
{
  # open the raster, convert to m^2, crop to UYRW
  acell = prod(res(uyrw.dem))
  ad8 = ( acell * raster(taudem_uyrw['ad8', 'file'])  ) 
  ad8 = crop(ad8, as(bou, 'Spatial'))
  
  # split at drop analysis optimum
  drop.min = acell * as.integer(gsub('stream threshold:', '', taudem_uyrw['nstream', 'description']))
  ad8.low = mask(ad8, ad8 > drop.min, maskvalue=1)
  ad8.high = mask(ad8, ad8 < drop.min, maskvalue=1)

  # log transformation to show detail on lower end of channels range
  logbase = 1.2
  ad8.loghigh = log(ad8.high, logbase)
  
  # make breakpoints on normal scale for low end
  n.low = 3
  ad8.lowbreak = seq(cellStats(ad8, min), drop.min, length=n.low)
  
  # make breakpoints on log scale for high end
  ad8.highquant = pretty(cellStats(ad8.loghigh, range), n.low + 1)
  ad8.highbreak = ad8.highquant[-1]
  
  # make labels
  ad8.label = format(c(ad8.lowbreak, logbase^ad8.highbreak)/1e6,
                     scientific=FALSE,
                     trim=TRUE,
                     digits=1)
  
  # pad them all the same width
  padlen = max(sapply(ad8.label, nchar))
  for(i in 1:length(ad8.label)) 
  {
    # there has to be an easier way but this works for now...
    addpad = paste( rep(' ', padlen - nchar(ad8.label[i]) ), collapse='')
    ad8.label[i] = paste0(addpad, ad8.label[i] )
  }

  # split by legend
  ad8.lowlabel = ad8.label[1:n.low]
  ad8.highlabel = ad8.label[(n.low + 1):length(ad8.label)]
  
  # indicate special values in labels
  ad8.lowlabel[1] = paste( ad8.lowlabel[1], ' << resolution limit')
  ad8.lowlabel[length(ad8.lowlabel)] = paste( ad8.lowlabel[length(ad8.lowlabel)], ' << drop analysis optimum')
  ad8.highlabel[length(ad8.highlabel)] = paste( ad8.highlabel[length(ad8.highlabel)], ' << main outlet')
  
  # make the palettes
  fullpal = hcl.colors(1e3, 'Sunset')
  bgcol = 'grey70'
  
  # offset the two ranges to distinguish them better
  idx.low = 1:200
  idx.high = 250:1e3
  lowpal = fullpal[idx.low]
  highpal =  fullpal[idx.high]
  

  # initialize the plot object with low values
  tmap.ad8.low = tm_shape(ad8.low, raster.downsample=FALSE, max.raster=1e9) +
    tm_raster(palette = lowpal, 
              style = 'cont',
              breaks = ad8.lowbreak,
              labels = ad8.lowlabel,
              title = 'upslope contributing area (km2)',
              colorNA = bgcol,
              showNA = FALSE)
  
  # add the high values
  tmap.ad8 = tmap.ad8.low +
    tm_shape(ad8.loghigh, raster.downsample=FALSE, max.raster=1e9) +
    tm_raster(palette = highpal, 
              style = 'cont',
              breaks = ad8.highbreak,
              labels = ad8.highlabel,
              title = '',
              colorNA = NULL,
              showNA = FALSE)

  # add shapes and annotations
  tmap.out3 = tmap.ad8 +
    tm_shape(uyrw.waterbody) + tm_polygons(col=bgcol, border.col=NULL) +
    tm_scale_bar(breaks=km.breaks, position=c('left', 'bottom'), text.size=0.8) + 
    tm_grid(n.x=2, n.y=2, projection=crs.list$epsg.geo, alpha=0.5) +
    tm_layout(main.title = 'Channel delineation with D8 flow model in TauDEM (file ad8.tif)',
              main.title.fontface ='bold',
              main.title.size = 1,
              main.title.position = 'center',
              legend.position = c('right', 'top'),
              legend.text.size = 0.8,
              legend.title.fontface ='bold',
              inner.margins = rep(1e-2, 4),
              legend.format = list(text.align = 'left'),
              frame = FALSE)
  
  # save a copy to disk
  tmap_save(tm=tmap.out3, filename=img.taudem.path, height=8000, width=5000, pointsize=28)
  
}
```

``` r
#my_markdown('make_subwatersheds', 'R/analysis')
```
