#' ---
#' title: "get_soils.R"
#' author: "Dean Koch"
#' date: "August 28, 2020"
#' output: github_document
#' ---
#'
#' **MITACS UYRW project**
#' 
#' **get_soils**: download NRCS SSURGO data and warp to our reference coordinate system 
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md) and
#' [get_dem.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
#' should be run first.

#'
#' ## libraries
#' [`FedData`](https://cran.r-project.org/web/packages/FedData/index.html) is used to fetch the
#' [NRCS SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053627) soils data.
#' See the [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('get_helperfun.R'))
library(FedData)
library(raster)
library(gdalUtils)
library(dplyr)

#'
#' ## project data

# This list describes all of the files created by this script:
files.towrite = list(
  
  # Soil survey area codes for SSURGO datasets in the UYRW area
  c(name='soils_acodes',
    file=file.path(out.subdir, 'nrcs_acodes.rds'), 
    type='R sf object',
    description='survey area polygons in the UYRW area corresponding to NRCS Soil Data Mart products'),
  
  # SSURGO datasets downloaded from the NRCS Soil Data Mart
  c(name='soils_sdm',
    file=file.path(src.subdir, 'nrcs_sdm'), 
    type='directory',
    description='NRCS Soil Data Mart products (subfolders correspond to FedData calls using soils_acodes)'), 
  
  # SSURGO map unit polygons with some associated data, projected and clipped to UYRW boundary
  c(name='soils_sfc',
    file=file.path(out.subdir, 'nrcs_sf.rds'), 
    type='R sf object',
    description='SSURGO soils mapping units for UYRW, derived from soils_sdm'), 
  
  # SSURGO tabular data, after merging and removing duplicate rows
  c(name='soils_tab',
    file=file.path(out.subdir, 'nrcs_tab.rds'), 
    type='R list object',
    description='SSURGO tabular data for the UYRW area (a list of data frames), derived from soils_sdm'), 
  
  # # aesthetic parameters for plotting
  # c(name='pars_tmap',
  #   file=file.path(data.dir, 'tmap_get_soils.rds'), 
  #   type='R list object', 
  #   description='parameters for writing png plots using tmap and tm_save'),
  
  # graphic showing soils for the UYRW
  c(name='img_soils',
    file=file.path(graphics.dir, 'soils.png'),
    type='png graphic',
    description='image of soils for the UYRW')
  
)

#' Note that `soils_sdm` points to a subdirectory, "data/source/nrsc_sdm", containing a large number of files
#' (too many to list individually). The most important of these are the "ssa_chunk_\*.gml" files, which
#' delineate Soil Survey Areas (SSA) in our region of interest, and the "wss_SSA_\*.zip" archives, which
#' contain the raw data for each SSA. 
#' 
#' We use the SSA data to find "area code" strings to query on the Soil Data Mart. The `get_ssurgo` function
#' downloads the data zip corresponding to each code, then restructures its contents into a more usable
#' format (the contents of the subdirectories of "nrsc_sdm")  

# write this information to disk
my_metadata('get_soils', files.towrite, overwrite=TRUE)

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_soils_metadata.csv)
#' in the `/data` directory.

#' Load some of the data prepared earlier 
# load metadata csv, CRS info list and watershed polygons from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
uyrw.waterbody = readRDS(here(my_metadata('get_basins')['waterbody', 'file']))
uyrw.mainstem = readRDS(here(my_metadata('get_basins')['mainstem', 'file']))

#'
#' ## Download the SSURGO Soil Survey Area (SSA) polygons
#' There seem to be some issues with the `FedData` functions for fetching data based on a bounding
#' box (with v2.5.7, both `get_ssurgo` and `get_ssurgo_inventory` failed when argument `template` was
#' set to the UYRW boundary). The chunk below is a workaround. It fetches a collection of polygons which
#' identify SSA codes for those spatial data overlapping with the study area. In the next chunk,
#' these SSA codes are used to construct the `template` argument for `get_ssurgo`
#' 
if(any(!file.exists(here(my_metadata('get_soils')[c('soils_acodes', 'soils_sdm'), 'file']))))
{
  # divide bounding box of UYRW boundary into 4 chunks (to stay below max area limit)
  sdm.bbox.split = st_transform(st_make_grid(uyrw.poly, n=c(2,2)), crs.list$epsg.geo)
  
  # set up the NRCS Soil Data Mart Data Access URL, and the request text
  sdm.domain = 'https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs'
  request.prefix = '?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=SurveyAreaPoly&BBOX='
  request.urls = sapply(lapply(sdm.bbox.split, st_bbox), function(bb) paste0(sdm.domain, request.prefix, paste(bb, collapse=',')))
  
  # create storage folder, define destination files containing soil survey area info on each chunk
  request.dest = here(my_metadata('get_soils')['soils_sdm', 'file'])
  request.files = file.path(request.dest, paste0('ssa_chunk_', 1:length(request.urls), '.gml'))
  my_dir(request.dest)
  
  # loop over the four requests, loading polygons into R via tempfile
  sdm.acodes.list = vector(mode='list', length=length(request.urls))
  for(idx.request in 1:length(request.urls))
  {
    # download the shapefile from the data mart
    download.file(request.urls[idx.request], request.files[idx.request])
    
    # load into R via `readOGR` (workaround for `st_read`, which warns about a GDAL error)
    sdm.acodes.list[[idx.request]] = st_as_sf(rgdal::readOGR(request.files[idx.request]))
    
    # `readOGR` seems to have trouble parsing the `srsName` fields (specifying the NAD83 datum). Fix that
    st_crs(sdm.acodes.list[[idx.request]]) = 'epsg:4269'
  }
  
  # combine the four multipolygon objects into one (with 23 elements) and transform to our projection
  sdm.acodes.split = st_transform(do.call(rbind, sdm.acodes.list), crs=crs.list$epsg)
  nrow(sdm.acodes.split)

  # merge all polygons with like `areasymbol` fields (14 unique elements) 
  uyrw.acodes = unique(sdm.acodes.split$areasymbol)
  sdm.acodes = sdm.acodes.split[match(uyrw.acodes, sdm.acodes.split$areasymbol),]
  nrow(sdm.acodes)
  
  # clip to UYRW boundary (7 unique elements)
  sdm.acodes = st_intersection(sdm.acodes, uyrw.poly)
  
  # save to disk
  saveRDS(sdm.acodes, here(my_metadata('get_soils')['soils_acodes', 'file']))
  
} else {
  
  # load the polygons from disk
  sdm.acodes = readRDS(here(my_metadata('get_soils')['soils_acodes', 'file']))
}

#'
#' ## Download the SSURGO polygons/dataframes
#' 
#' Now that we have the SSA codes, we can request the SSURGO data from the Soil Data Mart. This is
#' delivered in a zip archive containing ESRI shapefiles (delineating the mapping units), and a huge collection of 
#' tabular data as pipe-delimited (txt) tables, defining attributes in a relational database.
#' 
#' These tabular data are meant to be opened using an MS Access template, so there are unfortunately no column
#' headers in any of the txt data. The `get_ssurgo` function from `FedData` adds the column headers, and converts
#' the tabular data to properly labeled CSV files. It also handles the download/extraction of the zip files.
#' 
#' Note: a large number of files not listed explicitly in "get_soils_metadata.csv" are written to the subdirectory
#' "data/source/nrcs_sdm" by this chunk. Their data are simplified and consolidated into two output files, listed as
#' `soils_sfc` and `soils_tab`
#'  
if(any(!file.exists(here(my_metadata('get_soils')[c('soils_sfc', 'soils_tab'), 'file']))))
{
  # identify the survey area codes and create a list of destination subdirectories
  uyrw.acodes = sdm.acodes$areasymbol
  acodes.dest = here(file.path(my_metadata('get_soils')['soils_sdm', 'file'], uyrw.acodes))
  
  # create the storage subdirectories (as needed)
  sapply(acodes.dest, my_dir)
  
  # loop over the (7) survey area codes, loading each dataset into a list
  sdm.data.list = vector(mode='list', length=length(uyrw.acodes))
  pb = txtProgressBar(min=0, max=length(uyrw.acodes), style=3)
  for(idx.acode in 1:length(uyrw.acodes))
  {
    setTxtProgressBar(pb, idx.acode)
    sdm.data.list[[idx.acode]] = get_ssurgo(template=uyrw.acodes[idx.acode], 
                                            label='UYRW', 
                                            raw.dir=here(my_metadata('get_soils')['soils_sdm', 'file']),
                                            extraction.dir=acodes.dest[[idx.acode]])
  }
  close(pb)
  
  # merge spatial data from all survey areas, transform to our projection
  sdm.sf = do.call(rbind, lapply(sdm.data.list, function(acode) st_transform(st_as_sf(acode$spatial), crs.list$epsg)))
  
  # fix broken geometries and clip to UYRW boundary
  sdm.sf = st_intersection(st_make_valid(sdm.sf), uyrw.poly)
  
  # save the polygons file to disk, then start processing tabular data
  saveRDS(sdm.sf, here(my_metadata('get_soils')['soils_sfc', 'file']))

  # identify all (61) different unique tabular data names and note that SSAs vary in the number of associated tables
  names(sdm.data.list) = uyrw.acodes
  db.tablenames = sapply(sdm.data.list, function(xx) names(xx[['tabular']]))
  unique.tablenames = unique(unlist(db.tablenames))
  
  # build an index of which tablename is in which SSA 
  idx.tablenames = sapply(unique.tablenames, function(tablename) sapply(db.tablenames, function(acode) tablename %in% acode))
  
  # loop to build a list and fill with merged tables, where duplicate entries and empty columns are omitted
  sdm.tab = vector(mode='list', length=length(unique.tablenames))
  names(sdm.tab) = unique.tablenames
  pb = txtProgressBar(min=0, max=length(unique.tablenames), style=3)
  for(idx.table in 1:length(unique.tablenames))
  {
    # print some console output
    tablename = unique.tablenames[idx.table]
    print(paste('adding table', tablename, '...'))
    setTxtProgressBar(pb, idx.table)
    
    # build a sublist of dataframes for this merge
    sdm.data.sublist = sdm.data.list[uyrw.acodes[idx.tablenames[, tablename]]]
    
    # pull the tabular data for each SSA, merge, eliminate duplicate entries, and add to the list
    sdm.tab[[tablename]] = distinct(do.call(rbind, lapply(sdm.data.sublist, function(xx) xx[['tabular']][[tablename]])))
    
    # omit any empty columns (where all values are NA)
    sdm.tab[[tablename]] = sdm.tab[[tablename]][,!apply(sdm.tab[[tablename]], 2, function(colvals) all(is.na(colvals)))]
  }
  close(pb)

  # save tabular data to disk
  saveRDS(sdm.tab, here(my_metadata('get_soils')['soils_tab', 'file']))
  
} else {
  
  # load the sf object from disk
  sdm.sf = readRDS(here(my_metadata('get_soils')['soils_sfc', 'file']))
  sdm.tab = readRDS(here(my_metadata('get_soils')['soils_tab', 'file']))
}

#'
#' ## Download STATSGO and gSSURGO data
#' Looking at the distribution of
#' [map unit keys](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631)
#' (mukeys) across the landscape, we find two areas of incomplete coverage; Absaroka-Beartooth Wilderness Area, 
#' and Teton National Forest. These areas are shaded below in the map of SSURGO mukey polygons:
#' 

# identify area codes for areas of incomplete coverage and create a polygon
acodes.partial = sdm.tab$legend$areasymbol[!(sdm.tab$legend$legenddesc %in% 'Detailed Soil Map Legend')]
anames.partial = sdm.acodes$areaname[sdm.acodes$areasymbol %in% acodes.partial]
poly.partial = st_make_valid(st_union(st_geometry(sdm.acodes[sdm.acodes$areasymbol %in% acodes.partial,])))

# plot available survey coverage from SSURGO
if(!file.exists(here(my_metadata('get_soils')['img_soils', 'file'])))
{
  # load DEM plotting parameters from disk
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  
  # define a title and subtitle
  tmap.tstr = paste0('SSURGO map units in the UYRW (n=', nrow(sdm.sf), ')')
  tmap.tstr.sub ='(dark colours indicate partial/incomplete coverage)'
  
  # prepare the plot grob
  tmap.soils = tm_shape(sdm.sf) +
      tm_polygons(col='MAP_COLORS', border.alpha=0) +
    tm_shape(uyrw.poly) +
      tm_borders(col='black') +
    tm_shape(uyrw.mainstem) +
      tm_lines(col='dodgerblue4', lwd=2) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
    tm_shape(poly.partial) +
      tm_polygons(col='black', border.alpha=0, alpha=0.3) +
    tmap.pars$layout +
    tm_layout(main.title=paste(tmap.tstr, tmap.tstr.sub, sep='\n'))
  
  # render the plot
  tmap_save(tm=tmap.soils, 
            here(my_metadata('get_soils')['img_soils', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}

#' ![SSURGO coverage map of the UYRW](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png)
#' 
#' To do:

# 
# 

# 
# plot(uyrw.poly, border=NA, lwd=2, bg='darkgrey')
# plot(wstor.sf[,'wstor'], border=NA, add=TRUE)
# plot(st_geometry(uyrw.waterbody), add=TRUE, border=NA, col='black')
# plot(poly.partial, add=TRUE, border='white', lwd=3)
# 
# xx = st_union(st_geometry(sdm.acodes[sdm.acodes$areasymbol %in% acodes.undetailed,]))
# plot(xx)
# 
# ##
# # make a sample plot of water storage estimate
# # I follow the guide at https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?revision=705&root=aqp
# # but run queries with base R instead of using SQL 
# 
# # pull some data on water profile
# cp.vars = c('mukey', 'compname')
# hz.vars = c('cokey', 'comppct.r', 'hzdept.r', 'hzdepb.r', 'hzname', 'awc.r')
# hz.df = left_join(sdm.tab[['component']], sdm.tab[['chorizon']], by='cokey')[, c(cp.vars, hz.vars)]
# 
# # define a total water storage (aggregate) calculator and a general mukey-level aggregator
# wstor.fun = function(hz) { data.frame(pct=hz$comppct.r[1], size=sum(hz$awc.r * (hz$hzdepb.r - hz$hzdept.r), na.rm=TRUE)) }
# wstor.agg = function(hz) { data.frame(wstor = weighted.mean(hz$size, hz$pct))}
# 
# # use dplyr to perform the split-apply-merge then reorder to match the mukey order in polygons sf
# wstor.uyrw = hz.df %>% group_by(mukey, cokey) %>% do(wstor.fun(.)) %>% group_by(mukey) %>% do(wstor.agg(.))
# wstor.uyrw = wstor.uyrw[match(as.integer(sdm.sf$MUKEY), wstor.uyrw$mukey),]
# 
# # copy the polygons sf and append the water storage totals
# wstor.sf = cbind(sdm.sf, wstor.uyrw)
# 



#'
#' ## visualization
#' 

# load the plotting parameters used in get_dem.R




#+ include=FALSE
# Development code

  # # set up a mySQL database (see guide at datacarpentry.org/R-ecology-lesson/05-r-and-databases.html)
  # 
  # library(dplyr)
  # library(dbplyr)
  # library(RSQLite)
  # library(DBI)
  # 
  # test.db.file = here('test_db.sqlite')
  # 
  # # identify all csv files created by `get_ssurgo` and make a list of table names
  # csv.pre = 'UYRW_SSURGO_'
  # csv.ext = '.csv'
  # acodes = unique(sdm.sf$AREASYMBOL)
  # acodes.csv = sapply(here(my_metadata('get_soils')['soils_sdm', 'file'], acodes), function(xx) list.files(xx)[endsWith(list.files(xx), '.csv')])
  # acodes.names = sapply(acodes.csv, function(xx) substr(xx, nchar(csv.pre)+1, nchar(xx)-nchar(csv.ext)))
  # 
  # # identify all (61) different unique table names. Note survey areas vary in the number of associated tables
  # db.tablenames = unique(unlist(acodes.names))
  # length(db.tablenames)
  # sapply(acodes.names, length)
  # 
  # # loop over table names, adding each one to the database
  # pb = txtProgressBar(min=0, max=length(db.tablenames), style=3)
  # for(idx.table in 1:length(db.tablenames))
  # {
  #   # print some console output
  #   tablename = db.tablenames[idx.table]
  #   print(paste('adding table', tablename, '...'))
  #   setTxtProgressBar(pb, idx.table)
  #   
  #   # identify survey areas containing this table, make a vector of their paths on disk
  #   idx.acode = sapply(acodes.names, function(varname) varname %in% tablename) 
  #   csv.toread = file.path(names(acodes.names[sapply(idx.acode, any)]), paste0(csv.pre, tablename, csv.ext))
  #   
  #   # read in (and merge if there's more than one), then add to database
  #   test.db = dbConnect(RSQLite::SQLite(), test.db.file)
  #   dbWriteTable(test.db, tablename, do.call(rbind, lapply(csv.toread, function(path) read.csv(path, header=TRUE))))
  #   dbDisconnect(test.db)
  # }
  # close(pb)
  # 
  # # reconnect to the database
  # test.db = dbConnect(RSQLite::SQLite(), test.db.file)
  # db.names = dbListTables(test.db)
  # 
  # # aggregate spatial polygons into 
  # 
  # 
  # 
  # dbListFields(test.db, 'legend')
  # xx = tbl(test.db, 'legend') %>% collect()
  # xx$legenddesc
  # print(xx)
  # 
  # # aggregate by survey area, assign a legend key
  # sdm.sf$AREASYMBOL
  # dbGetQuery(test.db, 'SELECT legenddesc FROM legend WHERE areasymbol IS "MT623"')
  # 
  # 
  # 
  # mukey.tablenames = db.names[sapply(db.names, function(tablename) 'mukey' %in% dbListFields(test.db, tablename))]
  # 
  # dbListFields(test.db, 'legend')
  # xx = dbGetQuery(test.db, 'SELECT runoff FROM component')
  # unique(xx)
  # 
  # 
  # component.db = tbl(test.db, 'component')
  # xx = component.db %>% collect()
  # 
  # xx$areatypename
  # 
  # allfiles.acode
  # 
  # ####
  # ###
  # # test some SQL code following nceas.github.io/oss-lessons/sql-database-access-in-r/sql-database-access-in-r.html

####


# zz1 = st_snap(zz1, zz2, tolerance=0)
# 
# xx.inter = st_intersection(zz1, zz2)
# xx.diff12 = st_difference(zz1, zz2)
# xx.diff21 = st_difference(zz2, zz1)
# xx.symdiff = st_sym_difference(zz1, zz2)
# 
# dev.off()
# par(mfrow=c(2,2))
# {
#   
#   plot(zz, lwd=10, main='intersection (1=red, 2=blue)')
#   plot(uyrw.poly, add=T, border='grey', lwd=3)
#   plot(zz1, add=T, border='red', col=adjustcolor('red', alpha.f=0.5), lwd=3)
#   plot(zz2, add=T, border='blue', col=adjustcolor('blue', alpha.f=0.5), lwd=3)
#   plot(xx.inter, add=T, border='purple', col='purple', lwd=3)
#   
#   plot(zz, lwd=10, main='symmetric difference')
#   plot(uyrw.poly, add=T, border='grey', lwd=3)
#   plot(xx.symdiff, add=T, border='purple', col=adjustcolor('purple', alpha.f=0.5), lwd=2)
#   
#   plot(zz, lwd=10, main='1 difference 2')
#   plot(uyrw.poly, add=T, border='grey', lwd=3)
#   plot(xx.diff12, add=T, border='red', col=adjustcolor('red', alpha.f=0.5), lwd=2)
#   
#   plot(zz, lwd=10, main='2 difference 1')
#   plot(uyrw.poly, add=T, border='grey', lwd=3)
#   plot(xx.diff21, add=T, border='blue', col=adjustcolor('blue', alpha.f=0.5), lwd=2)
# }
# 
# 
# 
# 
# zzz1 = st_make_valid(st_snap(zz1, xx.symdiff, tolerance=300))
# plot(xx.symdiff)
# plot(zzz1, add=TRUE, col='blue')
# xx = st_sym_difference(xx.symdiff, zzz1)
# 
# plot(xx)
# ?st_area



# library(smoothr)
# st_crs(sdm.acodes)
# 
# zz = st_geometry(sdm.acodes[sdm.acodes$areasymbol %in% acodes.detailed,])
# zz1 = zz[1,]
# zz2 = zz[2,]
# zz3 = zz[3,]
# zz4 = zz[4,]
# 
# zz1 = drop_crumbs(zz1, threshold=units::set_units((1e3)^2, 'm^2'))
# zz2 = drop_crumbs(zz2, threshold=units::set_units((1e3)^2, 'm^2'))
# zz3 = drop_crumbs(zz3, threshold=units::set_units((1e3)^2, 'm^2'))
# zz4 = drop_crumbs(zz4, threshold=units::set_units((1e3)^2, 'm^2'))
# 
# dev.off()
# par(mfrow=c(2,2))
# {
#   plot(uyrw.poly, border='blue', lwd=3, main='poly 1')
#   plot(zz1, add=T, border='black', col=adjustcolor('black', alpha.f=0.2), lwd=3)
#   
#   plot(uyrw.poly, border='blue', lwd=3, main='poly 2')
#   plot(zz2, add=T, border='black', col=adjustcolor('black', alpha.f=0.2), lwd=3)
#   
#   plot(uyrw.poly, border='blue', lwd=3, main='poly 3')
#   plot(zz3, add=T, border='black', col=adjustcolor('black', alpha.f=0.2), lwd=3)
#   
#   plot(uyrw.poly, border='blue', lwd=3, main='poly 4')
#   plot(zz4, add=T, border='black', col=adjustcolor('black', alpha.f=0.2), lwd=3)
# }
# 
# xx = fill_holes(st_union(c(zz1, zz2, zz3, zz4)), threshold=units::set_units((1e3)^2, 'm^2'))
# plot(xx)
# 
# 
# xx = sdm.sf[sdm.sf$AREASYMBOL == acodes.detailed[2],]
# yy = st_difference(xx, zz2)
# plot(yy[,'MUKEY'])
# 
# plot(zz2, add=TRUE, col='blue')



#+ include=FALSE
# render as markdown by uncommenting the following line (note: run_pandoc=FALSE causes output_dir to be ignored)
#rmarkdown::render(here(paste0('get_soils', '.R')), clean=TRUE, output_file=here(file.path(markdown.dir, paste0('get_soils', '.md'))))

