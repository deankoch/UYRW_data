#' ---
#' title: "calibrate_initial.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **calibrate_initial.R**: Initial model fitting for headwaters catchments 
#' 
#' This script takes the output of
#' [run_qswat.R'](https://github.com/deankoch/UYRW_data/blob/master/markdown/run_qswat.md), enables the
#' Hargreaves method for PET estimation, and does a short (100 generation) run of a parallelized differential
#' evolution (parDE) optimizer on a selection of around 20 SWAT+ parameters that strongly influence streamflow
#' simulations. 
#' 
#' The output is a copy of the SWAT+ project folder with optimized parameters, along with an RDS file saving
#' the final generation of the parDE population - parameters, simulations, and scores. It also generates two
#' graphics showing the USGS hydrograph against SWAT+ model prediction bands, for temperature and discharge
#' 
#' Expect model fitting to take in the range of 30 minutes - 6 hours per model
#'
#' TODO: burn-in periods and/or initialize soil water, groundwater, flow, and snow
#' TODO: test out-of-sample error for estimating forecast skill
#'
#' ## libraries and helper functions
#' 

library(here)
source(here('R/helper_main.R'))
source(here('R/analysis/helper_analysis.R'))
source(here('R/rswat.R'))

# nonlinear optimization
library(DEOptimR)

# a weighted quantile implementation from Andrey Akinshin
if(1)
{
  # TODO: go through this more carefully
  # two function definitions below copied from: https://aakinshin.net/posts/weighted-quantiles/
  
  # Weighted generic quantile estimator
  wquantile.generic <- function(x, probs, cdf.gen, weights = NA) {
    n <- length(x)
    if (any(is.na(weights)))
      weights <- rep(1 / n, n)
    nw <- sum(weights) / max(weights)
    
    indexes <- order(x)
    x <- x[indexes]
    weights <- weights[indexes]
    
    weights <- weights / sum(weights)
    cdf.probs <- cumsum(c(0, weights))
    
    sapply(probs, function(p) {
      cdf <- cdf.gen(nw, p)
      q <- cdf(cdf.probs)
      w <- tail(q, -1) - head(q, -1)
      sum(w * x)
    })
  }
  
  # Weighted Harrell-Davis quantile estimator
  whdquantile <- function(x, probs, weights = NA) {
    cdf.gen <- function(n, p) return(function(cdf.probs) {
      pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
    })
    wquantile.generic(x, probs, cdf.gen, weights)
  }
}


#'
#' ## project data
#' 

# open metadata from "run_qswat.R" and extract USGS station names (each one is a SWAT+ model)
qswat.meta = my_metadata('run_qswat') %>% filter(type=='directory')
qswat.nm = rownames(qswat.meta)

# QSWAT+ directory and its parent, where we will write the new project folder copy
qswat.dir = qswat.meta[qswat.nm, 'file']
sw.parent = dirname(qswat.dir)

# define new project folder locations
init.dir = file.path(sw.parent, 'initial')
graphics.subdir = file.path(graphics.dir, 'subwatersheds')

# Make a list of the directories created by this script (see below in FOR-loop for file outputs metadata)
files.towrite = lapply(seq_along(qswat.nm), function(idx) {
  c(name=qswat.nm[idx], 
    file=init.dir[idx], 
    type='directory', 
    description=paste('SWAT+ folder for initial calibration', qswat.nm[idx]))
})

# write to disk as CSV and define a graphics output directory
init.metadata = my_metadata('calibrate_initial', files.towrite, overwrite=TRUE)



#'
#' ## calibration loop
#' 
#' Using parallelized adaptive differential evolution from the DEoptimR package

# optimizer settings: size of population and number of generations to run
NP = 8 * 21 # multiples of 8 probably most efficient (8 nodes, roughly equal job times)
maxiter = 100

# loop over QSWAT+ project directories
#idx.torun = seq_along(qswat.dir)
idx.torun = c(3)
for( idx.sw in idx.torun )
{
  # project name and paths
  nm = qswat.nm[idx.sw]
  dest.dir = init.metadata[nm, 'file']
  sw.parent = dirname(dest.dir)
  sw.meta = my_metadata(nm, data.dir=sw.parent)
  src.zip = sw.meta['txtinout', 'file']

  # unzip the stock model
  cat(paste('unzipping stock model to', dest.dir, '...\n'))
  textio = here(dest.dir)
  unzip(here(src.zip), exdir=textio, overwrite=TRUE)
  
  # Make a list of the files created for this catchment
  {
    files.towrite = list(
      
      # directory for SWAT+ files
      c(name='dir_swat',
        file=dest.dir,
        type='directory',
        description='SWAT+ TextInOut directory for initial calibration'),
      
      # list of inputs to `rswat_pardevol` and resulting outputs
      c(name='pardevol',
        file=file.path(sw.parent, 'initial.rds'), 
        type='R list',
        description='I/O objects to/from rswat_pardevol: cal, gage, bf, NP, maxiter, par, score, sim'),
      
      # hydrograph of streamflow showing SWAT+ predictions after initial model fit
      c(name='img_ribbon',
        file=file.path(graphics.subdir, paste0(nm, '_initial_streamflow.png')),
        type='png graphic', 
        description='image of initial SWAT+ model prediction intervals for streamflow'),
      
      # detail of img_ribbon showing the final year
      c(name='img_ribbon_detail',
        file=file.path(graphics.subdir, paste0(nm, '_initial_streamflow_detail.png')),
        type='png graphic', 
        description='detail of img_ribbon showing final year only')
      
    )
  }

  # write metadata from last chunk to disk and load into dataframe
  init.meta = my_metadata('initial', files.towrite, overwrite=TRUE, data.dir=sw.parent)
  
  # load the gage data for this catchment, copy its units, 
  gage = readRDS(here(sw.meta['gage', 'file']))
  gage.units = as.character( units(gage$flow) )
  
  # detect baseflow periods intercept and set intercept to their median seasonal minima
  idx.bf = my_baseflow(gage, rmode='index')
  dates.bf = my_split_series(gage$date[idx.bf])
  min.bf = sapply(dates.bf, function(x) min( gage$flow[gage$date %in% x] ) )
  bf.intercept = set_units(median(min.bf), gage.units, mode='standard')
  
  # pre-load all output files to determine units (except decision tables)
  # TODO: find bug preventing us from skipping the loadall
  cio = rswat_cio(textio, reload=TRUE, ignore='decision_table', quiet=TRUE)
  odf = rswat_output(loadall=TRUE)
  
  # load the TxtInOut files and configure for Hargreaves PET method
  rswat_open('codes.bsn', quiet=TRUE) %>% mutate(pet=2) %>% rswat_write(preview=FALSE)
  
  # define calibration settings
  # TODO: make this a function that writes a JSON file
  if(1)
  {
    # reload all files so search will work
    # TODO: track down 'newfuzzy' not found bug when (almost) nothing is loaded
    cio = rswat_cio(reload=TRUE, ignore='decision_table', quiet=TRUE)
    
    # find depth of soil profile and depth of aquifer (note units conversion)
    aqu.dep = rswat_open('aquifer.aqu') %>% pull(dep_bot) %>% min %>% set_units(m)
    prof.dep = rswat_open('soils.sol') %>% pull(dp_tot) %>% min(na.rm=TRUE) %>% set_units(mm) %>% set_units(m)
    
    # load documentation and calibration settings that ship with SWAT+
    #rswat_pdf_open(reload=T, desc.maxlen=75)
    calparms = rswat_open('cal_parms.cal')
    
    # snow parameters and their bounds
    #rswat_docs('snow.sno')
    parms.snow = rswat_find(include='snow.sno') %>% slice(1:7)
    bds.snow = calparms %>% slice(26:30) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias, min, max)
    bds.snow = rbind(bds.snow, data.frame(alias=c('snow_h2o', 'cov50'), min=c(0,0), max=c(1e3,1)))
    cal.snow = cbind(parms.snow, bds.snow)
    
    # aquifer parameters and their bounds
    #rswat_docs('aquifer.aqu')
    parms.aqu = rswat_find(include='aquifer.aqu') %>% slice( c(11, 16, 12, 17, 13) )
    bds.aqu = calparms %>% slice(147, 149:151) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias, min, max)
    bds.aqu = rbind(bds.aqu, data.frame(alias=c('rchg_dp'), min=c(0), max=c(0.5)))
    cal.aqu = cbind(parms.aqu, bds.aqu)
    
    # make an adjustment to some of the bounds
    cal.aqu$min[ cal.aqu$name == 'alpha_bf' ] = 1/365 # arbitrary max of one year
    #cal.aqu$max[ cal.aqu$name == 'alpha_bf' ] = 1 # make sure we get baseflow through the winter?
    cal.aqu$max[ cal.aqu$name == 'flo_min' ] = aqu.dep # set to dep_bot
    cal.aqu$max[ cal.aqu$name == 'revap_min' ] = aqu.dep # set to dep_bot
    cal.aqu$min[ cal.aqu$name == 'flo_min' ] = prof.dep # set to lowest soil profile layer
    cal.aqu$min[ cal.aqu$name == 'revap_min' ] = prof.dep # set to lowest soil profile layer
    cal.aqu$min[ cal.aqu$name == 'revap' ] = 0 # allow lower than default to promote baseflow
    cal.aqu$i = -51 # specify fitting to shallow aquifer only
    
    # general hydrology parameters and their bounds (TODO: look into perco)
    #rswat_docs('hydrology.hyd')
    parms.hyd = rswat_find(include='hydrology.hyd') %>% slice( c(3:5, 13) )
    bds.hyd = calparms %>% slice(c(12:14,20)) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias,min,max)
    cal.hyd = cbind(parms.hyd, bds.hyd)
    
    # allow a lower hargreaves coefficient
    cal.hyd$min[ cal.hyd$name=='harg_pet' ] = 1e-5
    
    # basin-wide hydrology parameters and bounds
    #rswat_docs('parameters.bsn')
    parms.bsn = rswat_find(include='parameters.bsn') %>% slice(3)
    bds.bsn = calparms %>% slice(55) %>% mutate(alias=name, min=abs_min, max=abs_max) %>% select(alias,min,max)
    cal.bsn = cbind(parms.bsn, bds.bsn)
    
    # set some initial values
    parms.ini = c(fall_tmp = 5,
                  melt_tmp = 2,
                  melt_max = 9,
                  melt_min = 0,
                  tmp_lag = 0.25,
                  snow_h2o = 500,
                  cov50 = 0.5,
                  alpha_bf = 1/10,
                  flo_min = 0.9 * aqu.dep,
                  revap = 0.1,
                  revap_min = 0.9 * aqu.dep,
                  rchg_dp = 1e-3,
                  can_max = 44,
                  esco = 0.5,
                  epco = 0.5,
                  harg_pet = 0.0023,
                  surq_lag = 11)
    
    # combine everything into a single dataframe and add initial values
    cal = rbind(cal.snow, cal.aqu, cal.hyd, cal.bsn) %>% mutate( ini = parms.ini )
  }
  
  # set a 1-year burn in period
  burn.days = 365
  idx.burn = c(1:nrow(gage)) %in% seq(burn.days)
  
  # define an error function that omits burn-in period
  errfn = function(x, y) { 1 - my_nse(x[!idx.burn], y[!idx.burn], L=1, normalized=TRUE) }
  
  # initialize cluster
  rswat_cluster()
  
  # run optimizer with baseflow intercept
  pdevol.out = rswat_pardevol(cal, gage, 
                              errfn = errfn, 
                              bf = bf.intercept, 
                              quiet = FALSE, 
                              NP = NP, 
                              maxiter = maxiter)
  
  # # pick out the best 50% of the population
  # n.best = round(NP)
  # idx.best = order(1-pdevol.out$score, decreasing=FALSE)[1:n.best]
  # par.best = pdevol.out$par[,idx.best]
  # 
  # # resume with same population for a shorter run
  # df.best = data.frame(par.best) %>% setNames(paste0('ini_', 1:n.best))
  # cal.resume = cal %>% select(-ini) %>% cbind(df.best)
  # pdevol.out = rswat_pardevol(cal.resume, gage, 
  #                             errfn = errfn, 
  #                             bf = bf.intercept, 
  #                             quiet = FALSE, 
  #                             NP = n.best, 
  #                             maxiter = maxiter)
    
  # shut down cluster
  rswat_cluster(wipe=TRUE)
  
  # create graphics directory as needed
  my_dir( here(graphics.subdir) )
  
  # simulate all parameter sets in final generation
  cal.amod = rswat_amod(cal)
  psim.out = rswat_pexec(pdevol.out$par, cal.amod)
  
  # convert units to match gage and add baseflow adjustment
  pdevol.list = lapply(1:NP, function(x) {
    set_units(bf.intercept + psim.out[[x+1]], gage.units, mode='standard')
  })
  
  # ribbon plot settings
  idx.plot = which(!idx.burn)
  
  # reshape as dataframe and merge with USGS time series
  swat.nm = paste0('swat_', 1:NP)
  gage.pdevol = gage %>% 
    rename(USGS = flow) %>% 
    cbind( setNames(data.frame(pdevol.list), swat.nm) )
  
  # reshape as matrix (no units) for computing statistics 
  sim.mat = do.call(cbind, pdevol.list)
  
  ## weighted 99th quantiles (using NSE scores as weights)
  gage.pdevol$swat_wq99low = apply(sim.mat, 1, function(x) whdquantile(x, 1-0.99, weights=pdevol.out$score))
  gage.pdevol$swat_wq99high = apply(sim.mat, 1, function(x) whdquantile(x, 0.99, weights=pdevol.out$score))
  
  # smoother versions (fits a cubic smoothing spline with default settings)
  gage.pdevol$swat_wsq99low = pmax(smooth.spline( gage.pdevol$swat_wq99low )$y, drop_units(bf.intercept) )
  gage.pdevol$swat_wsq99high = pmax(smooth.spline( gage.pdevol$swat_wq99high )$y, drop_units(bf.intercept) )
  
  # repeat for 95th quantile 
  gage.pdevol$swat_wq95low = apply(sim.mat, 1, function(x) whdquantile(x, 1-0.95, weights=pdevol.out$score))
  gage.pdevol$swat_wq95high = apply(sim.mat, 1, function(x) whdquantile(x, 0.95, weights=pdevol.out$score))
  gage.pdevol$swat_wsq95low = pmax(smooth.spline( gage.pdevol$swat_wq95low )$y, drop_units(bf.intercept) ) 
  gage.pdevol$swat_wsq95high = pmax(smooth.spline( gage.pdevol$swat_wq95high )$y, drop_units(bf.intercept) ) 
  
  # repeat for 90th quantile
  gage.pdevol$swat_wq90low = apply(sim.mat, 1, function(x) whdquantile(x, 1-0.90, weights=pdevol.out$score))
  gage.pdevol$swat_wq90high = apply(sim.mat, 1, function(x) whdquantile(x, 0.90, weights=pdevol.out$score))
  gage.pdevol$swat_wsq90low = pmax(smooth.spline( gage.pdevol$swat_wq90low )$y, drop_units(bf.intercept) )
  gage.pdevol$swat_wsq90high = pmax(smooth.spline( gage.pdevol$swat_wq90high )$y, drop_units(bf.intercept) )
  
  ######
  
  # make a more readable title
  nm.split = strsplit(nm, '_')[[1]]
  nm.read = paste(paste0(toupper( substring(nm.split, 1, 1) ) , substring(nm.split, 2)), collapse=' ')

  # make a ribbon plot of the 90th and 99th (smoothed) quantiles against station data
  gage.ribbon = gage.pdevol %>% slice(idx.plot) %>% select(date, USGS)
  ggp.ribbon = my_tsplot(gage.ribbon, legnm='streamflow', alph=1, colors='blue4') +
    geom_ribbon(aes_(ymin = gage.pdevol$swat_wsq99low[idx.plot], ymax = gage.pdevol$swat_wsq99high[idx.plot]), fill='cyan3', alpha=0.2) +
    geom_ribbon(aes_(ymin = gage.pdevol$swat_wsq90low[idx.plot], ymax = gage.pdevol$swat_wsq90high[idx.plot], fill='rib'), alpha=0.2) +
    scale_fill_manual(name=NULL, values=c(rib='cyan3'), labels='SWAT+ predictions') +
    ggtitle(nm.read) +
    theme(legend.title = element_text(face='bold'), legend.spacing = unit(-0.5, 'cm')) +
    guides(colour=guide_legend(order=1),  fill=guide_legend(order=2))
 
  # write to disk as PNG
  ribbon.png = here(init.meta['img_ribbon', 'file'])
  ggsave(ribbon.png, ggp.ribbon, width=8, height=3, units='in', dpi=320)
  
  # detail plot: soom in on the final year only
  idx.detail = which( !( seq(nrow(gage)) %in%  seq(nrow(gage)-365) ) )
  gage.detail = gage.pdevol %>% slice(idx.detail) %>% select(date, USGS)
  ggp.detail = my_tsplot(gage.detail, legnm='streamflow', alph=1, colors='blue4') +
    geom_ribbon(aes_(ymin = gage.pdevol$swat_wsq99low[idx.detail], ymax = gage.pdevol$swat_wsq99high[idx.detail]), fill='cyan3', alpha=0.2) +
    geom_ribbon(aes_(ymin = gage.pdevol$swat_wsq90low[idx.detail], ymax = gage.pdevol$swat_wsq90high[idx.detail], fill='rib'), alpha=0.2) +
    scale_fill_manual(name=NULL, values=c(rib='cyan3'), labels='SWAT+ predictions') +
    ggtitle(nm.read) +
    theme(legend.title = element_text(face='bold'), legend.spacing = unit(-0.5, 'cm')) +
    guides(colour=guide_legend(order=1),  fill=guide_legend(order=2))
  
  # write to disk as PNG
  detail.png = here(init.meta['img_ribbon_detail', 'file'])
  ggsave(detail.png, ggp.detail, width=8, height=3, units='in', dpi=320)
  
  #########
  
  # append mean, median
  gage.pdevol$swat_mean = rowMeans(sim.mat) %>% set_units(gage.units, mode='s')
  gage.pdevol$swat_median = apply(sim.mat, 1, median) %>% set_units(gage.units, mode='s')
  
  # # make a ribbon plot of smoothed quantiles
  # legnm = 'SWAT+ simulations'
  # gage.ribbon = gage.pdevol %>% slice(idx.plot) %>% select(date, USGS)
  # ggp.ribbon = my_tsplot(gage.ribbon, alph=1, colors='blue4') +
  #   geom_ribbon(aes_(ymin = drop_units(gage.pdevol$swat_sqlow[idx.plot]), 
  #                    ymax = drop_units(gage.pdevol$swat_sqhigh[idx.plot]), 
  #                    fill='rib'), 
  #               alpha=0.4) +
  #   scale_fill_manual(name=NULL, values=c(rib='cyan3'), labels=legnm)
  # 
  # # write to disk as PNG
  # ribbon.png = here(init.meta['img_ribbon', 'file'])
  # ggsave(ribbon.png, ggp.ribbon, width=8, height=3, units='in')
  
  # append best parameter sets from the model fit to calibration dataframe
  cal = cal %>% mutate(best = pdevol.out$par[,which.max(pdevol.out$score)] )

  # overwrites current parameter set on disk with best values
  cal.amod(cal$best)

  # make a copy of important I/O objects to store in an RDS file
  olist = list(cal = cal, 
               bf = bf.intercept, 
               NP = NP, 
               maxiter = maxiter, 
               par = pdevol.out$par, 
               score = pdevol.out$score, 
               odf = gage.pdevol)
  
  # write to disk
  io.rds = here(init.meta['pardevol', 'file'])
  saveRDS(olist, io.rds)
}


rswat_cio(wipe=T)
rswat_cio(here(init.dir[4]), reload=T, ignore='decision_table')
# TODO: fix bugs with the excluded files listed below
xx = rswat_compare(compare=here(init.dir[5]), ignore=c('decision_table', 
                                                  'climate', 
                                                  'aquifer.con',
                                                  'soils.sol'))

xx %>% filter(file %in% cal$file) %>% filter(i == 1)


#+ include=FALSE
# Development code
#my_markdown('calibrate_initial', 'R/demo')


