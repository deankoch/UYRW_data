# development: benchmark model construction and simulation times

## libraries

library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))


## data from earlier scripts

# drop analysis and subwatershed delineation based on gage locations
subwatersheds.meta = my_metadata('make_subwatersheds')
taudem.meta = my_metadata('taudem', data.dir=subwatersheds.meta['taudem', 'file'])

# data on streamflow and weather
streamgages.meta = my_metadata('get_streamgages')
meteo.meta = my_metadata('get_meteo')


## make a directory for output

bench.dir = here(file.path(out.subdir, 'benchmarks'))
my_dir(bench.dir)


## load the data

# USGS gage data and output of "make_subwatersheds.R"
usgs = readRDS(here(streamgages.meta['USGS_data', 'file']))
usgs.sw = readRDS(here(subwatersheds.meta['usgs_catchments', 'file']))


## pick out a subset

# filter catchments with no inlets, omit the big lake, order by area
boundary.noinlet = usgs.sw$boundary %>% 
  filter( n_inlet == 0 ) %>%
  filter( !grepl('yellowstone_lk', catchment_name) ) %>%
  arrange( area ) %>% 
  as.data.frame()

# copy the corresponding USGS point data
pts.noinlet = usgs.sw$pts %>% 
  filter(catchment_id %in% boundary.noinlet$catchment_id) %>%
  arrange( match(catchment_id, boundary.noinlet$catchment_id) ) 

# pick out a catchment to demo
eg.name = 'tower_c_at_tower_falls'
eg.id = boundary.noinlet$catchment_id[ boundary.noinlet$catchment_name == eg.name ]


## set up thresholds and dates to test

# the minimum identified in the TauDEM stream drop analysis
drop.min = as.integer(gsub('stream threshold:', '', taudem.meta['nstream', 'description']))

# sequence up to 10X and 25X this minimum
n.each = 5
drop.channel.seq = c(round(drop.min/2), round(seq(drop.min, 10*drop.min, length=n.each)))
drop.stream.seq = c(round(drop.min/2), round(seq(drop.min, 25*drop.min, length=n.each)))

# subbasin threshold must be >= than channel threshold
drop.stream.list = lapply(1:(n.each + 1), function(x) drop.stream.seq[drop.stream.seq > drop.channel.seq[x]])
n.streamdrop = sapply(drop.stream.list, length)

# store as data.frame 
n.total = sum(n.streamdrop)
drop.df = data.frame(sdrop = unlist(drop.stream.list),
                     cdrop = unlist(mapply(function(x, y) rep(x, length(y)), 
                                           x=drop.channel.seq, 
                                           y=drop.stream.list)))




# for now we use the PNWNAmet analysis for weather - load this data and pull start date
meteo.eg = readRDS(here(meteo.meta['pnwnamet_uyrw', 'file']))
date.start = range(meteo.eg$dates)[1]

# build a sequence of end dates
yr.seq = c(1, 2, 4, 8, 16)
date.end.seq = date.start + ( 365 * yr.seq ) 

# append storage for benchmark times
execnames = c('texec1', 'texec2', 'texec4', 'texec8', 'texec16')
drop.df = drop.df %>% 
  mutate(nsub = rep(NA, n.total)) %>%
  mutate(nhru = nsub) %>%
  mutate(ncha = nsub) %>%
  mutate(name = paste(eg.name, sdrop, cdrop, sep='__')) %>%
  mutate(tbuild = nsub) %>%
  mutate(texec1 = nsub) %>%
  mutate(texec2 = nsub) %>%
  mutate(texec4 = nsub) %>%
  mutate(texec8 = nsub) %>%
  mutate(texec16 = nsub)

## loop over thresholds

# do this only once then reload the results in future
drop.csv = file.path(bench.dir, paste0(eg.name, '.csv'))
if( !file.exists(drop.csv) )
{
  # make some dummy response data
  gage = data.frame(date=seq(date.start, date.end.seq[length(date.end.seq)], by='day'))
  gage$flow = set_units(rep(0, nrow(gage)), m^3/s)
  
  # loop over drop thresholds
  for(idx in 1:n.total)
  {
    cat(paste('running job', idx, 'of', n.total))
    
    # set the drop thresholds
    drop_stream = drop.df$sdrop[idx]
    drop_channel = drop.df$cdrop[idx]
    
    # set the project name and directory
    projnm = drop.df$name[idx]
    projdir = file.path(bench.dir, projnm)
    
    # set QSWAT+ config parameters and build the model (timing it and writing result to df)
    config = list(skip_editor=FALSE, drop_stream=drop_stream, drop_channel=drop_channel)
    qswat = qswat_setup(eg.id, usgs.sw, projdir, wipe=TRUE, config=config, quiet=T)
    drop.df$tbuild[idx] = system.time({ 
      qswat_run(qswat, quiet=T) 
    })['elapsed']
    
    # load in the data and make a plot  
    qswat.dat = qswat_read(qswat)
    titles = list(main = pts.noinlet$station_nm[pts.noinlet$catchment_id == eg.id])
    tmap.out = qswat_plot(qswat.dat, titles=titles)
    tmap_save(tm=tmap.out, 
              filename = file.path(projdir, paste0(projnm, '.png')), 
              width = 3000, 
              pointsize = 12)
    
    # copy the watershed element counts to storage
    drop.df[idx, c('nsub', 'ncha', 'nhru')] = qswat.dat$sta$counts
    
    # load the config files
    textio = qswat.dat$sta$paths['txtio']
    cio.path = file.path(textio, 'file.cio')
    cio = rswat_cio(cio.path, ignore='decision_table', reload=TRUE)
    
    # turn off burn-in period to avoid truncated output and missing weather issues
    print.prt = rswat_open('print.prt', quiet=T)
    print.prt[[1]]$nyskip = 0
    rswat_write(print.prt[[1]], preview=F, quiet=T)
    
    # loop over time periods, running the model and timing its execution
    pb = txtProgressBar(max=length(date.end.seq), style=3)
    for(idx.len in 1:length(date.end.seq))
    {
      setTxtProgressBar(pb, idx.len)
      
      # filter dummy gage data to time period of interest 
      execnm = execnames[idx.len]
      date.end = date.end.seq[idx.len]
      gage.temp = gage %>% filter(date < date.end)
      
      # run and time execution of simulation
      drop.df[idx, execnm] = system.time({ 
        my_gage_objective(gage.temp, qswat.dat$sta$paths['txtio'], 1, quiet=T)
      })['elapsed']
    }
    close(pb)
    
    print(drop.df)
  }
  
  # save the results to disk
  write.csv(drop.df, file=drop.csv)
  
} else {
  
  # load from disk
  drop.df = read.csv(drop.csv)
  
}

## make some plots

# reshape to include nyear column
drop.list = as.list(drop.df[, ! names(drop.df) %in% execnames])
drop.df.long = data.frame(lapply(drop.list, function(x) rep(x, length(execnames)))) %>%
  mutate(texec = unlist(drop.df[, execnames])) %>%
  mutate(n_year = rep(yr.seq, each=nrow(drop.df))) %>%
  mutate(n_channel = ncha) %>%
  mutate(n_subbasin = as.factor(nsub))

# compute means by subbasin
texec.mean = drop.df.long %>% group_by(n_channel, n_year) %>% 
  summarize(texec=mean(texec), .groups='drop_last') %>%
  mutate(n_subbasin=11)

# fit a linear model to the data by n_year
x.pred = seq(25, 1000, length=100)
lm.pred.list = vector(mode='list', length=length(yr.seq))
for(idx in 1:length(yr.seq))
{
  # data.exp = setNames(drop.df %>% select(ncha, as.name(execnames[idx])), c('ncha', 'texec'))
  # lm.loglog = lm(log(texec) ~ log(ncha), data=data.exp)
  # lm.poly = lm( texec ~ ncha + I(ncha^lm.loglog$coefficients[2]), data=data.exp)
  # texec.pred = predict(lm.poly, data.frame(ncha=x.pred))
  # lm.pred.list[[idx]] = data.frame(ncha=x.pred, texec=texec.pred, n_subbasin=11, n_year=yr.seq[idx])
  # 
  
  data.yr = setNames(drop.df %>% select(ncha, as.name(execnames[idx])), c('ncha', 'texec'))
  lm.yr = lm( texec ~ ncha, data=data.yr)
  texec.pred = predict(lm.yr, data.frame(ncha=x.pred))
  lm.pred.list[[idx]] = data.frame(ncha=x.pred, texec=texec.pred, n_subbasin=11, n_year=yr.seq[idx])
}

# plot the results

ggp.yr = ggplot(drop.df.long, aes(n_year, texec/60, group=n_subbasin)) +
  geom_point(aes(col=as.factor(n_channel), size=n_subbasin), alpha=0.2) +
  geom_line(aes(n_year, texec/60, group=n_channel, col=as.factor(n_channel)), data=texec.mean) +
  xlab('number of years simulated') +
  ylab('simulation runtime (minutes)') +
  ggtitle('execution time for SWAT+ simulations by years') +
  guides(col=guide_legend(order=1)) +
  labs(col = 'number of channels', size = 'number of subbasins') +
  theme_minimal() +
  theme(legend.position = c(1e-2, 1-1e-2),
        legend.justification = c(0, 1),
        legend.box = 'horizontal',
        axis.line = element_line())

ggsave(file.path(bench.dir, 'runtime_projection_byyear.png'), plot=ggp.yr)

ggp.ncha = ggplot(drop.df.long, aes(n_channel, texec/60, group=n_subbasin)) +
  geom_point(aes(col=as.factor(n_year), size=n_subbasin), alpha=0.2) +
  geom_line(aes(ncha, texec/60, col=as.factor(n_year)), data=lm.pred.list[[1]]) + 
  geom_line(aes(ncha, texec/60, col=as.factor(n_year)), data=lm.pred.list[[2]]) + 
  geom_line(aes(ncha, texec/60, col=as.factor(n_year)), data=lm.pred.list[[3]]) + 
  geom_line(aes(ncha, texec/60, col=as.factor(n_year)), data=lm.pred.list[[4]]) + 
  geom_line(aes(ncha, texec/60, col=as.factor(n_year)), data=lm.pred.list[[5]]) + 
  xlab('number of channels') +
  ylab('simulation runtime (minutes)') +
  ggtitle('execution time for SWAT+ simulations by number of channels') +
  guides(col=guide_legend(order=1)) +
  labs(col = 'years simulated', size = 'number of subbasins') +
  theme_minimal() +
  theme(legend.position = c(1e-2, 1-1e-2),
        legend.justification = c(0, 1),
        legend.box = 'horizontal',
        axis.line = element_line())
    
ggsave(file.path(bench.dir, 'runtime_projection_bychannel.png'), plot=ggp.ncha)

