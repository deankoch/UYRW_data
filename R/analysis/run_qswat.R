#' ---
#' title: "run_qswat.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **run_qswat.R**: builds SWAT+ models for headwaters catchments in the UYRW
#' 
#' This uses `rswat` to run [QSWAT+](https://swatplus.gitbook.io/docs/user/qswat+), on all
#' headwaters catchments identified in
#' [make_subwatersheds](https://github.com/deankoch/UYRW_data/blob/master/markdown/make_subwatersheds.md).
#' This will eventually be expanded to include all catchments in the UYRW. The data required for this
#' script should be assembled by running the "get_*.R" functions (namely,
#' [get_basins](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_dem](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_landuse](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md),
#' [get_streamgages](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_streamgages.md),
#' [get_meteo](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_meteo.md)).
#' 
#' See the demonstration script
#' [demo_qswat](https://github.com/deankoch/UYRW_data/blob/master/markdown/demo_qswat.md) for
#' step-by-step explanations of the workflow below.
#' 
#' For now this builds SWAT+ models for the catchments of the following USGS stations:
#' 
#'  * Yellowstone Lake Outlet
#'  * Soda Butte Creek
#'  * Blacktail Deer Creek
#'  * Big Creek
#'  * Mill Creek
#'  
#'  I am calling these "headwaters" catchments because they have no upstream USGS gages. There
#'  are four other such catchments in the UYRW, but they are ignored for now because their station
#'  time series end before our current weather input data (PNWNAMet) begins. 
#'  
#'  The script runs `qswat_setup` and `qswat_run` (from
#'  [`rswat_qgis`](https://github.com/deankoch/UYRW_data/blob/master/markdown/rswat_qgis.md))
#' 

#'
#' ## libraries and helper functions

library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))

# low-level R graphics control for making overview plot
library(grid)

#'
#' ## project data
#' 

# TODO: turn this stuff into functions and make a plot of all catchments with names
# TODO: add support for lakes
# TODO: add support for catchments with inlets

# file metadata for some earlier scripts
basins.meta = my_metadata('get_basins')
meteo.meta = my_metadata('get_meteo')
streamgages.meta = my_metadata('get_streamgages')

# load geographical features, weather inputs and streamgage data
meteo = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
usgs.ts = readRDS(here(streamgages.meta['USGS_data', 'file']))
uyrw = readRDS(here(basins.meta['boundary', 'file']))
lakes = readRDS(here(basins.meta['waterbody', 'file']))

# load USGS station time series and their catchment data, prepared in "make_subwatersheds.R"  
subwatersheds.meta = my_metadata('make_subwatersheds')
usgs.w = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))

# identify headwaters (no inlets) stations and the end dates of their records
id.candidate = usgs.w$boundary$catchment_id[usgs.w$boundary$n_inlet == 0]
end.candidate = as.Date(usgs.w$pts$end_date[match(id.candidate, usgs.w$pts$catchment_id)])

# include only catchments where we have overlap with the weather inputs
id.head = id.candidate[ end.candidate > min(meteo$dates) ]
nm.head = usgs.w$boundary$catchment_name[match(id.head, usgs.w$boundary$catchment_id)]

# # omit yellowstone lake catchment for now
# idx.yello = which(nm.head == 'main_at_yellowstone_lk_outlet')
# nm.head = nm.head[-idx.yello]
# id.head = id.head[-idx.yello]

# scan for contiguous time series
pts.head = usgs.w$pts[match(id.head, usgs.w$pts$catchment_id),]
usgs.head = usgs.ts$dat[pts.head$site_no]
dates.head = lapply(usgs.head, function(x) my_split_series(x$dat[[1]]$date, meteo$dates) )
ranges.head = lapply(dates.head, function(x) sapply(x, length) )

# identify the largest series under 10,000 days at each station and extract its dates
idx.ts = sapply(ranges.head, function(x) which(x < 1e4)[ which.max(x[which(x < 1e4)]) ] )
seq.head = lapply(seq_along(idx.ts), function(x) dates.head[[x]][[ idx.ts[x] ]])

# import geometry data for the catchments
pts.head = lapply(id.head, function(id) usgs.w$pts[usgs.w$pts$catchment_id==id,] %>% na.omit)
boundary.head = lapply(id.head, function(id) usgs.w$boundary[usgs.w$boundary$catchment_id==id,] %>% na.omit)
demnet.head = lapply(id.head, function(id) usgs.w$demnet[usgs.w$demnet$catchment_id==id,] %>% na.omit)

# define relative output folders
sw.parent = file.path(sci.subdir, 'subwatersheds')
sw.dir = file.path(sw.parent, nm.head)
graphics.subdir = file.path(graphics.dir, 'subwatersheds')

# Make a list of the files created by this script
files.towrite = lapply(seq_along(nm.head), function(idx) {
  c(name=nm.head[idx], 
    file=file.path(sw.dir[idx], 'qswat'), 
    type='directory', 
    description=paste('QSWAT+ folder for', nm.head[idx]))
})

# write to disk as CSV
qswat.metadata = my_metadata('run_qswat', files.towrite, overwrite=TRUE)

#' ## loop over subwatersheds
for(idx.sw in seq_along(nm.head))
{
  # progress message
  nm = nm.head[idx.sw]
  dest.dir = qswat.metadata[nm, 'file']
  cat(paste('processing', dest.dir, '...\n'))
  
  # create directories as needed
  my_dir( here(graphics.subdir) )
  my_dir( here(dest.dir) )
  
  # Make a list of the files created for this catchment
  {
    files.towrite = list(
      
      # directory for QSWAT+ files
      c(name='dir_qswat',
        file=dest.dir,
        type='directory',
        description='directory for QSWAT+ files'),
      
      # gage data for the catchment outlet
      c(name='gage',
        file=file.path(dest.dir, 'gage.rds'), 
        type='R dataframe',
        description='USGS gage data for the catchment outlet'),
      
      # overview map of the study catchment
      c(name='img_catchment',
        file=file.path(graphics.subdir, paste0(nm, '_catchment.png')),
        type='png graphic', 
        description='image of catchment location, channels, and a USGS hydrograph'),
      
      # overview map of the SWAT+ model
      c(name='img_hrus',
        file=file.path(graphics.subdir, paste0(nm, '_hrus.png')),
        type='png graphic', 
        description='image of SWAT+ model HRUs in the catchment'),
      
      # copy of the TxtInOut directory of the SWAT+ model
      c(name='txtinout',
        file=file.path(dirname(dest.dir), paste0(nm, '_stock.zip')),
        type='zip archive', 
        description='copy of the TxtInOut directory of the stock SWAT+ model'),
      
      # where to find metadata on QSWAT+ files
      c(name='metadata_qswat',
        file=file.path(dest.dir, paste0(nm, '_metadata.csv')),
        type='CSV', 
        description='metadata for files created by qswat_run')
    )
  }
  
  # write metadata from last chunk to disk and load into dataframe
  sw.metadata = my_metadata(nm, files.towrite, overwrite=TRUE, data.dir=dirname(dest.dir))
  
  # import gage data
  dates = seq.head[[idx.sw]]
  gage = usgs.head[[pts.head[[idx.sw]]$site_no]]$dat[[1]] %>% filter(date %in% dates)
  
  # skip if the file exists already
  catchment.png = here(sw.metadata['img_catchment', 'file'])

  # plot grob for the selected gage data
  ggp.usgs = my_tsplot(setNames(gage, c('Date', 'USGS'))) +
    theme(legend.position = 'none', 
          text = element_text(size=26), 
          axis.title = element_text(face='bold'),
          plot.margin = unit(c(1,1,1,1), 'cm'),
          panel.background = element_rect(fill='white', colour=NA)) 
  
  # grab the colour of the line chart to match inset frame
  insetcol = ggp.usgs$scales$scales[[1]]$palette(1)
  
  # quick plot of the subwatershed
  tmap.w = tm_shape(boundary.head[[idx.sw]]) + tm_polygons('grey90', border.col=NULL) + 
    tm_shape(demnet.head[[idx.sw]]) + tm_lines('grey60') +
    tm_shape(pts.head[[idx.sw]]) + tm_dots(col='white', size=1.2) +
    tm_shape(pts.head[[idx.sw]]) + tm_dots(col=insetcol, size=1.0) +
    tm_scale_bar(text.size=1) + tm_layout(main.title=nm, main.title.fontface='bold')
  
  # smaller plot showing the location in the greater URYW region
  tmap.uyrw = tm_shape(uyrw) + tm_polygons('grey90', border.col=NULL) +
    tm_shape(boundary.head[[idx.sw]]) + tm_polygons(insetcol, alpha=0.5 , border.col=NULL) + 
    tm_shape(usgs.w$demnet %>% filter(Order > 2)) + tm_lines('grey80') +
    tm_shape(lakes) + tm_polygons('grey60', border.col=NULL) +
    tm_layout(frame=NA)
  
  # create plot device for overview graphic
  png(catchment.png, width=1200, height=1200, pointsize=26)
  
    # initialize plot device and left/right pane layout 
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(2,2)))
    
    # draw a background for the left pane
    grid.draw(rectGrob(gp=gpar(fill=insetcol, alpha=0.1), vp=viewport(layout.pos.col=1)))
    
    # draw the hydrograph and subwatershed plot on the left pane
    print(tmap.w, vp=viewport(layout.pos.col=1, layout.pos.row=1))
    print(ggp.usgs, vp=viewport(layout.pos.col=1, layout.pos.row=2))
    
    # add the location plot on the right
    print(tmap.uyrw, vp=viewport(layout.pos.col=2))
    
    # draw a box around the left pane
    grid.draw(rectGrob(gp=gpar(fill=NA, col=insetcol, lwd=3), vp=viewport(layout.pos.col=1)))
  
  # close the plot device
  dev.off()

  
  # set QSWAT+ project directory root for `qswat_setup` to write project files
  dir.qswat = here(sw.metadata['dir_qswat', 'file'])
  
  # scale drop levels to get roughly the same number of HRUs as the Big Creek model
  drop.stream = max(1e3, drop_units( round( 4e3*boundary.head[[idx.sw]]$area/set_units(173922457, m^2) ) ))
  config = list(skip_editor = FALSE, drop_stream = drop.stream, drop_channel = drop.stream - 1)
  
  # write QSWAT+ input files using a helper function (NOTE: `wipe=TRUE` erases directory `dir.qswat`!)
  qswat.meta = qswat_setup(id.head[idx.sw], usgs.w, projdir=dir.qswat, config=config, wipe=TRUE)
  
  # pass these inputs to QSWAT+ for processing in PyQGIS, then SWAT+ Editor
  qswat_run(qswat.meta)
  
  # save a copy of gage data for easy loading later
  gage.path = here(sw.metadata['gage', 'file'])
  my_dir(dirname(gage.path))
  saveRDS(gage, gage.path)
  
  # load the QSWAT+ shapefiles into R by passing the dataframe returned by `qswat_setup`  
  wsh = qswat_read(qswat.meta)

  # compute distances between outlet point geometry and SWAT+ channels, identifying nearest channel
  outlet.distances = st_distance(wsh$cha, pts.head[[idx.sw]])
  idx.min = which.min(outlet.distances)
  id.outlet = wsh$cha$Channel[idx.min]
  
  # define output graphic of HRUs
  wsh.png = here(sw.metadata['img_hrus', 'file'])

  # modify the title but use default settings for everything else
  wsh.titles = list(main=paste('hydrologic response units for', nm))
  wsh.tmap = qswat_plot(wsh, titles=wsh.titles)
  
  # write the file
  tmap_save(tm=wsh.tmap, filename=wsh.png, height=1400, width=2000, pointsize=12)
 
  # set up filenames and directories and zip all files in TxtInOut
  dir.txtinout = wsh$sta$paths[['txtio']]
  fname.txtinout = list.files(dir.txtinout)
  zip.path = here(sw.metadata['txtinout', 'file'])
  zip(zip.path, files=file.path(wsh$sta$paths[['txtio']], fname.txtinout), flags='-j9X')
}

#+ include=FALSE
# Development code
#my_markdown('run_qswat', 'R/demo')


