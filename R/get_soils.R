#' ---
#' title: "get_soils.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project**
#' 
#' **get_soils**: download, process NRCS SSURGO and STATSGO2 soils data for use with SWAT+,
#' and generate SWAT+ AW input files
#' 
#' [get_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md) and
#' [get_dem.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
#' should be run first.

#'
#' ## libraries
#' [`gdalUtilities`](https://cran.r-project.org/web/packages/gdalUtilities/index.html) provides a wrapper
#' for GDAL calls to rasterize the mapunit polygons,
#' [`FedData`](https://cran.r-project.org/web/packages/FedData/index.html) is used to fetch the
#' [NRCS SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053627) soils data,
#' and [`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf) is used to parse the NRCS website for links to STATSGO2
#' data archives. See the [get_helperfun.R script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
#' for other required libraries
library(here)
source(here('R/get_helperfun.R'))
library(gdalUtilities)
library(FedData)
library(rvest)

#'
#' ## project data
#+ echo=FALSE
files.towrite = list(
  
  # Soil survey area codes for SSURGO datasets in the UYRW area
  c(name='soils_acodes',
    file=file.path(out.subdir, 'nrcs_acodes.rds'), 
    type='R sf object',
    description='survey area polygons in the UYRW area corresponding to NRCS Soil Data Mart products'),
  
  # SSURGO datasets downloaded from the NRCS Soil Data Mart
  c(name='soils_sdm',
    file=file.path(src.subdir, 'nrcs'), 
    type='directory',
    description='NRCS Soil Data Mart products (subfolders correspond to FedData calls using soils_acodes)'), 
  
  # SSURGO map unit polygons with some associated data, projected and clipped to UYRW boundary
  c(name='ssurgo_sf',
    file=file.path(out.subdir, 'ssurgo_sf.rds'), 
    type='R sf object',
    description='SSURGO soils mapping units for UYRW, derived from soils_sdm'), 
  
  # SSURGO tabular data, after merging and removing duplicate rows
  c(name='ssurgo_tab',
    file=file.path(out.subdir, 'ssurgo_tab.rds'), 
    type='R list object',
    description='SSURGO tabular data for the UYRW area (a list of data frames), derived from soils_sdm'),
  
  # STATSGO2 map unit polygons with some associated data, projected and clipped to UYRW boundary
  c(name='statsgo_sf',
    file=file.path(out.subdir, 'statsgo_sf.rds'), 
    type='R sf object',
    description='STATSGO2 soils mapping units for UYRW, derived from soils_sdm'), 
  
  # STATSGO2 tabular data, after merging and removing duplicate rows
  c(name='statsgo_tab',
    file=file.path(out.subdir, 'statsgo_tab.rds'), 
    type='R list object',
    description='STATSGO2 tabular data for the UYRW area (a list of data frames), derived from soils_sdm'),
  
  # multipolygon representing areas in URYW with incomplete coverage in SSURGO
  c(name='ssurgo_incomplete_sfc',
    file=file.path(out.subdir, 'ssurgo_incomplete_sfc.rds'), 
    type='R sfc object',
    description='areas in URYW with incomplete SSURGO coverage, derived from ssurgo_sf and ssurgo_tab'), 
  
  # combined set of SSURGO and STATSGO2 map unit polygons, covering the UYRW area
  c(name='soils_merged_sf',
    file=file.path(out.subdir, 'soils_merged_sf.rds'), 
    type='R sf object',
    description='merged STATSGO2/SSURGO soil mapping units for UYRW'), 
  
  # combined set of SSURGO and STATSGO2 tabular data
  c(name='soils_merged_tab',
    file=file.path(out.subdir, 'soils_merged_tab.rds'), 
    type='R list object',
    description='merged STATSGO2/SSURGO tabular data for the UYRW area (a list of data frames)'), 
  
  # input data table for SWAT+ based on dominant soil component for each SSURGO/STATSGO2 map unit 
  c(name='swat_usersoil',
    file=file.path(out.subdir, 'swat_usersoil.csv'), 
    type='CSV',
    description='soil data input CSV for SWAT+, derived from soils_merged_tab'), 
  
  # lookup table for integer codes in the SWAT+ soils raster
  c(name='swat_lookup',
    file=file.path(out.subdir, 'swat_soil_lookup.csv'), 
    type='CSV',
    description='integer code to mukey (MUID) table for SWAT+, connects swat_tif to swat_usersoil'), 
  
  # geotiff mapping rows of soil data table for SWAT+ to locations in the watershed
  c(name='swat_tif',
    file=file.path(out.subdir, 'swat_soil.tif'), 
    type='GeoTIFF',
    description='map unit (mukey) raster for SWAT+, connected to swat_usersoil by swat_lookup'), 
  
  # graphic showing soils for the UYRW
  c(name='img_soils',
    file=file.path(graphics.dir, 'soils.png'),
    type='png graphic',
    description='image of map units with SSURGO/STATSGO2 data in the UYRW'),
  
  # graphic showing soils for the UYRW
  c(name='img_soils_wstor',
    file=file.path(graphics.dir, 'soils_wstor.png'),
    type='png graphic',
    description='image of soil water storage for the UYRW')
)

#' A list object definition here (`files.towrite`) has been hidden from the markdown output for
#' brevity. The list itemizes all files written by the script along with a short description.
#' We use a helper function to write this information to disk:
soils.meta = my_metadata('get_soils', files.towrite, overwrite=TRUE)
print(soils.meta[, c('file', 'type')])

#' This list of files and descriptions is now stored as a
#' [.csv file](https://github.com/deankoch/UYRW_data/blob/master/data/get_soils_metadata.csv)
#' in the `/data` directory.
#' 
#' `soils_sdm` points to a subdirectory ("data/source/nrsc") containing a large number of files
#' (too many to list individually). The most important of these are the "ssa_chunk_\*.gml" files, which
#' delineate Soil Survey Areas (SSA) in our region of interest, and the "wss_SSA_\*.zip" archives, which
#' contain the raw data for each SSA. 
#' 
#' We use the SSA data to find "area code" strings to query on the Soil Data Mart. The `get_ssurgo` function
#' downloads the data zip corresponding to each code, then restructures its contents into a more usable
#' format (the contents of the subdirectories of "nrsc_sdm")  
#' 
#' Load some of the data prepared earlier 
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
uyrw.waterbody = readRDS(here(my_metadata('get_basins')['waterbody', 'file']))
uyrw.mainstem = readRDS(here(my_metadata('get_basins')['mainstem', 'file']))
dem.tif = raster(here(my_metadata('get_dem')['dem', 'file']))

#'
#' ## Download the SSURGO Soil Survey Area (SSA) polygons
#' There seem to be some issues with the current version (v2.5.7) of `FedData`: both `get_ssurgo` and 
#' `get_ssurgo_inventory` failed when argument `template` was set to the UYRW boundary. The chunk below
#' is a workaround, which fetches a collection of polygons identifying SSA codes for those spatial data
#' overlapping with the study area. In the next chunk, these SSA codes are used to construct the `template`
#' argument for `get_ssurgo`
#' 
if(any(!file.exists(here(soils.meta[c('soils_acodes', 'soils_sdm'), 'file']))))
{
  # divide bounding box of UYRW boundary into 4 chunks (to stay below max area limit)
  sdm.bbox.split = st_transform(st_make_grid(uyrw.poly, n=c(2,2)), crs.list$epsg.geo)
  
  # set up the NRCS Soil Data Mart Data Access URL, and the request text
  sdm.domain = 'https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs'
  request.prefix = '?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=SurveyAreaPoly&BBOX='
  request.urls = sapply(lapply(sdm.bbox.split, st_bbox), function(bb) paste0(sdm.domain, request.prefix, paste(bb, collapse=',')))
  
  # create storage folder, define destination files containing soil survey area info on each chunk
  request.dest = here(soils.meta['soils_sdm', 'file'])
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
  
  # omit survey area codes for which the data are not available on Soil Data Mart (SDM)
  sdm.acodes = sdm.acodes[sdm.acodes$sapubstatusname != 'Unpublished',]
  
  # save to disk
  saveRDS(sdm.acodes, here(soils.meta['soils_acodes', 'file']))
  
} else {
  
  # load the polygons from disk
  sdm.acodes = readRDS(here(soils.meta['soils_acodes', 'file']))
}

#'
#' ## Download the SSURGO polygons/dataframes
#' 
#' Now that we have the SSA codes, we can request SSURGO data from the Soil Data Mart. This is delivered
#' in a zip archive containing ESRI shapefiles (delineating the mapping units), and a huge collection of
#' tabular data as pipe-delimited (txt) tables, defining attributes in a relational database.
#' 
#' These tabular data are meant to be opened using an MS Access template, so there are unfortunately no column
#' headers in any of the txt data. The `get_ssurgo` function from `FedData` adds the column headers, and converts
#' the tabular data to properly labeled CSV files. It also handles the download/extraction of the zip files.
#' 
#' Note: a large number of files not listed explicitly in "get_soils_metadata.csv" are written to the subdirectory
#' "data/source/nrcs" by this chunk. Their data are simplified and consolidated into two output files, listed as
#' `ssurgo_sf` and `ssurgo_tab`
#'  
if(any(!file.exists(here(soils.meta[c('ssurgo_sf', 'ssurgo_tab'), 'file']))))
{
  # identify the soil survey area codes and create a list of destination subdirectories
  uyrw.acodes = sdm.acodes$areasymbol
  acodes.dest = here(file.path(soils.meta['soils_sdm', 'file'], uyrw.acodes))
  
  # create the storage subdirectories (as needed)
  sapply(acodes.dest, my_dir)
  
  # loop over the (5) survey area codes, loading each dataset into a list
  sdm.data.list = vector(mode='list', length=length(uyrw.acodes))
  pb = txtProgressBar(min=0, max=length(uyrw.acodes), style=3)
  for(idx.acode in 1:length(uyrw.acodes))
  {
    # note that force.redo=FALSE leads to parsing errors if the data have already been extracted
    setTxtProgressBar(pb, idx.acode)
    sdm.data.list[[idx.acode]] = get_ssurgo(template=uyrw.acodes[idx.acode], 
                                            label='UYRW', 
                                            raw.dir=here(soils.meta['soils_sdm', 'file']),
                                            extraction.dir=acodes.dest[[idx.acode]],
                                            force.redo=TRUE)
  }
  close(pb)
  
  ## start processing tabular data...

  # identify all (61) different unique tabular data names and note that SSAs vary in the number of associated tables
  names(sdm.data.list) = uyrw.acodes
  db.tablenames = sapply(sdm.data.list, function(xx) names(xx[['tabular']]))
  unique.tablenames = unique(unlist(db.tablenames))
  
  # build an index of which tablename is in which SSA 
  idx.tablenames = sapply(unique.tablenames, function(tablename) sapply(db.tablenames, function(acode) tablename %in% acode))
  
  # loop to build a list and fill with merged tables, where duplicate entries and empty columns are omitted
  ssurgo.tab = vector(mode='list', length=length(unique.tablenames))
  names(ssurgo.tab) = unique.tablenames
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
    ssurgo.tab[[tablename]] = distinct(do.call(rbind, lapply(sdm.data.sublist, function(xx) xx[['tabular']][[tablename]])))
    
    # omit any empty columns (where all values are NA)
    ssurgo.tab[[tablename]] = ssurgo.tab[[tablename]][,!apply(ssurgo.tab[[tablename]], 2, function(colvals) all(is.na(colvals)))]
  }
  close(pb)
  
  # save the list of tables to disk, omitting any empty ones
  ssurgo.tab = ssurgo.tab[sapply(ssurgo.tab, nrow) > 0]
  saveRDS(ssurgo.tab, here(soils.meta['ssurgo_tab', 'file']))
  
  # merge spatial data from all survey areas, transform to our projection
  ssurgo.sf = do.call(rbind, lapply(sdm.data.list, function(acode) st_transform(st_as_sf(acode$spatial), crs.list$epsg)))
  
  # fix broken geometries and clip to UYRW boundary
  ssurgo.sf = st_intersection(st_make_valid(ssurgo.sf), uyrw.poly)
  
  # save the polygons file to disk
  saveRDS(ssurgo.sf, here(soils.meta['ssurgo_sf', 'file']))
  
} else {
  
  # load the sf object from disk
  ssurgo.sf = readRDS(here(soils.meta['ssurgo_sf', 'file']))
  ssurgo.tab = readRDS(here(soils.meta['ssurgo_tab', 'file']))
}

#'
#' ## Download STATSGO2 data
#' Looking at the distribution of
#' [map unit keys](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631)
#' (mukeys) across the landscape, we find a large area of incomplete coverage around the Absaroka-Beartooth
#' Wilderness Area, and many smaller ones scattered throughout the watershed (see
#' [this graphic](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png),
#' generated at the end of this script)
#' 
#' Incomplete areas can be filled in with STATSGO2 data, available from the
#' [USDA/NCRS Geospatial Data Gateway](https://gdg.sc.egov.usda.gov), 
#' which hosts direct downloads in the shared folders
#' [at this link](https://nrcs.app.box.com/v/soils).
#' 
#' This chunk fetches the STATSGO data for Wyoming and Montana, creating two new output files:
#' `statsgo_sf` and `statsgo_tab`. Note: a large number of files not listed explicitly in
#' "get_soils_metadata.csv" are written to the subdirectory "data/source/nrcs_sdm" by this chunk. 
#'  
if(any(!file.exists(here(soils.meta[c('statsgo_sf', 'statsgo_tab'), 'file']))))
{
  # define the website url and read in the javascript as text
  nrcs.domain = 'https://nrcs.app.box.com/v/soils/folder/18247487156'
  nrcs.html.nodes = html_nodes(html_session(nrcs.domain), xpath='/html/body/script')
  nrcs.script.text = html_text(nrcs.html.nodes[[length(nrcs.html.nodes)]])
  
  # parse individual file listings and extract filenames and ID strings for download URLs
  zip.metadata = head(unlist(strsplit(nrcs.script.text, '\"typedID\":\"'))[-1], -1)
  zip.ids = unname(sapply(zip.metadata, function(meta.text) unlist(strsplit(meta.text, '\"'))[[1]]))
  zip.filenames = unname(sapply(zip.metadata, function(meta.text) unlist(strsplit(unlist(strsplit(meta.text, '\"name\":\"'))[[2]], '\"'))[1]))
  
  # select the files for Montana, Wyoming and their download URLs
  idx.todownload = sapply(c('MT', 'WY'), function(stateabb) which(startsWith(zip.filenames, paste0('wss_gsmsoil_', stateabb))))
  n.todownload = length(idx.todownload)
  nrcs.download.url.prefix = 'https://nrcs.app.box.com/index.php?rm=box_download_shared_file&vanity_name=soils&file_id='
  nrcs.download.url = paste0(nrcs.download.url.prefix, zip.ids)
  
  # loop to download and extract them
  statsgo.data.list = vector(mode='list', length=n.todownload)
  pb = txtProgressBar(min=0, max=n.todownload, style=3)
  for(idx.loop in 1:n.todownload)
  {
    # feedback for user
    setTxtProgressBar(pb, idx.loop)
    idx.file = idx.todownload[idx.loop]
    print(paste('fetching and processing', zip.filenames[idx.file]))
    
    # download the zip file to the same folder as the SSURGO data
    out.dir = here(soils.meta['soils_sdm', 'file'])
    if(!file.exists(file.path(out.dir, zip.filenames[idx.file])))
    {
      download.file(nrcs.download.url[idx.file], file.path(out.dir, zip.filenames[idx.file]), mode='wb')
    }
    
    # extract the data to a temporary folder (and maintain directory structure in the zip)
    temp.dir = file.path(out.dir, zip.ids[idx.file])
    ex.paths = unzip(file.path(out.dir, zip.filenames[idx.file]), exdir=temp.dir)
    temp.subdir = dirname(ex.paths)[which.min(nchar(dirname(ex.paths)))]
    
    # process the data (saving CSVs, etc, to a new subfolder), import into R, delete tempfiles
    perm.subdir = paste0(names(idx.file), '_STATSGO')
    statsgo.data.list[[idx.loop]] = my_get_statsgo(temp.subdir, names(idx.file), file.path(out.dir, perm.subdir))
    unlink(temp.dir, recursive=TRUE)
  }
  
  # merge spatial data from all requested states, transform to our projection
  statsgo.sf = do.call(rbind, lapply(statsgo.data.list, function(statsgo) st_transform(st_as_sf(statsgo$spatial), crs.list$epsg)))
  
  # fix broken geometries and clip to UYRW boundary
  statsgo.sf = st_intersection(st_make_valid(statsgo.sf), uyrw.poly)
  
  # save the polygons file to disk (30 features), then start processing tabular data
  saveRDS(statsgo.sf, here(soils.meta['statsgo_sf', 'file']))
  
  # identify all (54) different unique tabular data names
  names(statsgo.data.list) = names(idx.todownload)
  db.tablenames = sapply(statsgo.data.list, function(xx) names(xx[['tabular']]))
  unique.tablenames = unique(unlist(db.tablenames))
  
  # build an index of which tablename is in which SSA 
  idx.tablenames = sapply(unique.tablenames, function(tablename) sapply(db.tablenames, function(acode) tablename %in% acode))
  
  # loop to build a list and fill with merged tables, where duplicate entries and empty columns are omitted
  statsgo.tab = vector(mode='list', length=length(unique.tablenames))
  names(statsgo.tab) = unique.tablenames
  pb = txtProgressBar(min=0, max=length(unique.tablenames), style=3)
  for(idx.table in 1:length(unique.tablenames))
  {
    # print some console output
    tablename = unique.tablenames[idx.table]
    print(paste('adding table', tablename, '...'))
    setTxtProgressBar(pb, idx.table)
    
    # build a sublist of dataframes for this merge
    statsgo.data.sublist = statsgo.data.list[names(idx.tablenames[, tablename])]
    
    # pull the tabular data for each state, merge, eliminate duplicate entries, and add to the list
    statsgo.tab[[tablename]] = distinct(do.call(rbind, lapply(statsgo.data.sublist, function(xx) xx[['tabular']][[tablename]])))
    
    # omit any empty columns (where all values are NA)
    statsgo.tab[[tablename]] = statsgo.tab[[tablename]][,!apply(statsgo.tab[[tablename]], 2, function(colvals) all(is.na(colvals)))]
  }
  close(pb)
  
  # omit any empty tables 
  statsgo.tab = statsgo.tab[sapply(statsgo.tab, nrow) > 0]
  
  # save tabular data to disk
  saveRDS(statsgo.tab, here(soils.meta['statsgo_tab', 'file']))
  
} else {
  
  # load the sf object from disk
  statsgo.sf = readRDS(here(soils.meta['statsgo_sf', 'file']))
  statsgo.tab = readRDS(here(soils.meta['statsgo_tab', 'file']))

}
  
#'
#' ## SSURGO vs STATSGO coverage
#' Looking at the mapunits only, the SSURGO database appears to cover most of the UYRW apart from a large
#' wilderness area in the northeast. In many other small areas, however, we are missing some information
#' needed by SWAT+.
#' 
#' This chunk looks for information on the dominant soil components in the SSURGO database, and identifies
#' map units for which these data are incomplete relative to the STATSGO2 database. It then replaces the 
#' corresponding mapunit keys with those from STATSGO2, and merges the two datasets.
#' 
if(any(!file.exists(here(soils.meta[c('ssurgo_incomplete_sfc', 'soils_merged_sf', 'soils_merged_tab'), 'file']))))
{
  # identify 330 unique SSURGO map units in UYRW
  ssurgo.mukeys = sort(unique(as.integer(ssurgo.sf$MUKEY)))
  length(ssurgo.mukeys)
  
  # compile the usersoil tables for these mukeys
  ssurgo.us = my_usersoil(ssurgo.tab, ssurgo.mukeys)
  
  # 26 of the SSURGO map units are missing data in >70% of the SWAT+ fields. These correspond to 663 polygons
  mukeys.lowq = ssurgo.us$MUID[ssurgo.us$pmiss > 0.7]
  idx.lowq = which(ssurgo.sf$MUKEY %in% mukeys.lowq)
  length(idx.lowq)
  
  # make a copy of the SSURGO polygons dataset
  ssurgo.trimmed.sf = ssurgo.sf
  
  # for each of these incomplete polygons, swap in the STATSGO2 mukey corresponding to the largest area of overlap
  pb = txtProgressBar(min=0, max=length(idx.lowq), style=3)
  print('replacing missing data in SSURGO with STATSGO2 entries')
  for(idx.loop in 1:length(idx.lowq))
  {
    # pull the polygon and compute its total area
    idx.poly = idx.lowq[idx.loop]
    poly.sf = ssurgo.trimmed.sf[idx.poly,]
    poly.mukey = as.integer(poly.sf$MUKEY)
    poly.area = st_area(poly.sf)
    
    # identify the STATSGO2 mapunit(s) with maximal overlap area with this polygon
    poly.sf.statsgo = st_intersection(statsgo.sf, st_geometry(poly.sf))
    poly.area.statsgo = st_area(poly.sf.statsgo)/poly.area
    mukey.statsgo = as.integer(poly.sf.statsgo$MUKEY[which.max(poly.area.statsgo)])
    
    # replace the mapunit key in the SSURGO sfc object and indicate the swap in the SPATIALVER field
    ssurgo.trimmed.sf$MUKEY[idx.poly] = mukey.statsgo
    ssurgo.trimmed.sf$SPATIALVER[idx.poly] = 'from_STATSGO2'
    
    # output to console
    setTxtProgressBar(pb, idx.loop)
  }
  
  # dissolve all remaining SSURGO mapunit polygons into a single multipolygon
  idx.remaining = ssurgo.trimmed.sf$SPATIALVER != 'from_STATSGO2'
  ssurgo.coverage.poly = st_make_valid(st_union(st_geometry(ssurgo.trimmed.sf[idx.remaining,])))
  
  # take difference with watershed boundary polygon to get map of incomplete SSURGO coverage, save to disk
  ssurgo.incomplete.poly = st_make_valid(st_difference(uyrw.poly, ssurgo.coverage.poly))
  saveRDS(ssurgo.incomplete.poly, here(soils.meta['ssurgo_incomplete_sfc', 'file']))
  
  # trim STATSGO2 mapunits to keep only those areas needed to complete the SSURGO dataset
  statsgo.trimmed.sf = st_make_valid(st_intersection(statsgo.sf, ssurgo.incomplete.poly))
  
  # merge the SSURGO and STATSGO2 mapunit shapefiles, save to disk
  soils.merged.sf = st_make_valid(rbind(ssurgo.trimmed.sf[idx.remaining,], statsgo.trimmed.sf))
  saveRDS(soils.merged.sf, here(soils.meta['soils_merged_sf', 'file']))
  
  # change data type for one of the STATSGO2 tables to match SSURGO 
  statsgo.tab$component$constreeshrubgrp = as.character(statsgo.tab$component$constreeshrubgrp)
  
  # before merging, add a column to each table to denote data source
  statsgo.tab = lapply(statsgo.tab, function(tab) mutate(tab, source_db='STATSGO'))
  ssurgo.tab = lapply(ssurgo.tab, function(tab) mutate(tab, source_db='SSURGO'))
  
  # join the STATSGO2 data to the SSURGO tables list and save to disk
  statsgo.table.names = setNames(nm=names(statsgo.tab)[names(statsgo.tab) %in% names(ssurgo.tab)])
  soils.merged.tab = lapply(statsgo.table.names, function(tname) bind_rows(ssurgo.tab[[tname]], statsgo.tab[[tname]]) )
  soils.merged.tab = c(soils.merged.tab, ssurgo.tab[!(names(ssurgo.tab) %in% statsgo.table.names)])
  saveRDS(soils.merged.tab, here(soils.meta['soils_merged_tab', 'file']))

} else {
  
  # load the sf object from disk
  ssurgo.incomplete.poly = readRDS(here(soils.meta['ssurgo_incomplete_sfc', 'file']))
  soils.merged.sf = readRDS(here(soils.meta['soils_merged_sf', 'file']))
  soils.merged.tab = readRDS(here(soils.meta['soils_merged_tab', 'file']))
}

#'
#' ## build data inputs for SWAT+
#' 
#' Three data structures are needed by SWAT+ in the automated workflow: "usersoil.csv", a table of parameters
#' derived from the SSURGO/STATSGO2 databases; "soil.tif", a raster indicating which entry of the former to use
#' for each gridpoint in the watershed; and "soil.csv" a lookup table matching integer codes in "soil.tif"
#' to the string-valued keys in the `SNAM` field of "usersoil.csv".
#' 
#' This chunk builds these three objects and writes them to disk
#' 
if(any(!file.exists(here(soils.meta[c('swat_usersoil', 'swat_lookup', 'swat_tif'), 'file']))))
{
  # pull mukey values and build the usersoil table using a helper function
  soils.mukeys = sort(unique(as.integer(soils.merged.sf$MUKEY)))
  usersoil = my_usersoil(soils.merged.tab, soils.mukeys)
  
  # rasterize the MUKEY data and write geotiff to disk
  soils.tif.path = here(soils.meta['swat_tif', 'file'])
  soils.tif.prelim = gRasterize(as(soils.merged.sf, 'Spatial'), dem.tif, field='MUKEY', soils.tif.path)
  
  # crop the geotiff to URYW boundary
  soils.tif = crop(soils.tif.prelim, as(uyrw.poly, 'Spatial'))
  writeRaster(soils.tif, soils.tif.path, overwrite=TRUE)
  
  # store the integer code order for this raster, and add unique "soil name" 
  soils.rat = levels(soils.tif)[[1]] %>% mutate(NAME = paste0('UYRW_', MUKEY))
  #soils.rat = levels(soils.tif)[[1]] %>% mutate(NAME = MUKEY)
  
  # tidy up usersoil table, add soil names in order given, and write to disk 
  idx.usersoil = match(soils.rat$MUKEY, usersoil$MUID)
  usersoil.out = usersoil %>% 
    slice(idx.usersoil) %>%
    select(-pmiss) %>%
    mutate(OBJECTID=1:nrow(usersoil)) %>%
    mutate(SNAM=soils.rat$NAME)
  usersoil.path = here(soils.meta['swat_usersoil', 'file'])
  write.csv(usersoil.out, usersoil.path, row.names=FALSE)
  
  # tidy up the lookup table and write to disk
  soil.rat.out = setNames(soils.rat[, c('ID', 'NAME')], c('VALUE', 'NAME'))
  soil.rat.path = here(soils.meta['swat_lookup', 'file'])
  write.csv(soil.rat.out, soil.rat.path, row.names=FALSE)
}


#'
#' ## visualization
#' 
#' Create two plots: The first shows the complete set of SSURGO map units by tiling the UYRW area in
#' different colours, with darkened areas indicating map units with data filled in by STATSGO2 surveys data.
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png)
if(!file.exists(here(soils.meta['img_soils', 'file'])))
{
  # load DEM plotting parameters from disk
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  
  # define a title and subtitle
  tmap.tstr = paste0('soil survey map units in the UYRW (n=', nrow(soils.merged.sf), ')')
  tmap.tstr.sub ='(SSURGO=light, STATSGO2=dark)'
  
  # prepare the plot grob
  tmap.soils = tm_shape(soils.merged.sf) +
      tm_polygons(col='MAP_COLORS', border.alpha=0) +
    tm_shape(uyrw.poly) +
      tm_borders(col='black') +
    tm_shape(uyrw.mainstem) +
      tm_lines(col='black', lwd=1) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='black', border.alpha=0) +
    tm_shape(ssurgo.incomplete.poly) +
      tm_polygons(col='black', border.alpha=0, alpha=0.7) +
    tmap.pars$layout +
    tm_layout(main.title=paste(tmap.tstr, tmap.tstr.sub, sep='\n'))
  
  # render the plot
  tmap_save(tm=tmap.soils, 
            here(soils.meta['img_soils', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}


#' The second plot illustrates the type of data contained in the soils database. It shows estimates of
#' the soil water content across the UYRW. The imprecision of the STATSGO2 data (which has much larger
#' soil survey area polygons) is evident in the northeast part of the UYRW, where the SSURGO datasets are
#' incomplete.
#' ![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils_wstor.png)
if(!file.exists(here(soils.meta['img_soils_wstor', 'file'])))
{
  # load DEM plotting parameters from disk and modify for this plot
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  tmap.tstr = 'soil water storage available to plants'
  tmap.pars$layout = tmap.pars$layout + 
    tm_layout(legend.text.color='black',
              legend.title.color='black')
  
  # compute a total water storage estimate, combining all horizons and aggregating over map units
  awc.df = soils.merged.tab$chorizon %>% 
    mutate(lyr_awc = awc.r*(hzdepb.r - hzdept.r)) %>%
    group_by(cokey) %>%
    summarize(component_awc = sum(lyr_awc, na.rm=TRUE)) %>%
    left_join(soils.merged.tab$component, by='cokey') %>%
    group_by(mukey) %>%
    summarize(total_awc = weighted.mean(component_awc, comppct.r)) %>%
    as.data.frame

  # copy the polygons sf and append the water storage totals
  awc = awc.df$total_awc[match(as.integer(soils.merged.sf$MUKEY), awc.df$mukey)]
  wstor.sf = cbind(soils.merged.sf, awc=awc)
  
  # prepare the plot grob
  tmap.wstor = tm_shape(wstor.sf) +
    tm_polygons(col='awc', palette='YlGnBu', border.alpha=0, style='cont', title='cm', colorNA='red') +
      tm_shape(uyrw.poly) +
    tm_borders(col='black') +
      tm_shape(uyrw.mainstem) +
    tm_lines(col='black', lwd=2) +
      tm_shape(uyrw.waterbody) + 
    tm_polygons(col='black', border.alpha=0) +
      tmap.pars$layout +
    tm_layout(main.title=paste(tmap.tstr, sep='\n'))
  
  # render the plot
  tmap_save(tm=tmap.wstor, 
            here(soils.meta['img_soils_wstor', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}

#+ include=FALSE


# check if all the mukeys exist in the standard SWAT+ database
as.integer(soils.merged.sf$MUKEY)

# library(Hmisc)
# library(RODBC)
# library(ImportExport)
# install.packages('ImportExport')
# 
# 
# access_import('H:/UYRW_QGIS3_project/test/QSWATRef2012.mdb', 'usersoil')
# con <- odbcConnectAccess('H:/UYRW_QGIS3_project/test/QSWATRef2012.mdb')
# sqlFetch(con, 'your_table')
# 
# odbcClose(con)





# # convert any GEOMETRYCOLLECTION feature types to POLYGON/MULTIPOLYGON
# idx.geomcol = st_is(statsgo.trimmed.sf, 'GEOMETRYCOLLECTION')
# statsgo.addback.sf = st_cast(st_collection_extract(statsgo.trimmed.sf[idx.geomcol,], 'POLYGON'), 'GEOMETRY')
# statsgo.trimmed.sf = rbind(statsgo.trimmed.sf[!idx.geomcol,], statsgo.addback.sf)

#my_markdown('get_soils')