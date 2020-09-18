get\_soils.R
================
Dean Koch
August 28, 2020

**MITACS UYRW project**

**get\_soils**: download, process NRCS SSURGO (and gSSURGO) soils data
(work in progress)

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
and
[get\_dem.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
should be run first.

## libraries

[`FedData`](https://cran.r-project.org/web/packages/FedData/index.html)
is used to fetch the [NRCS
SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053627)
soils data,
[`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html) is
used for omitting duplicate rows from the tabular data, and
[`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf) is
used to parse the NRCS website for links to STATSGO2 data archives. See
the [get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md),
for other required libraries

``` r
library(here)
source(here('R/get_helperfun.R'))
library(FedData)
library(raster)
#library(gdalUtils)
library(dplyr)
library(rvest)
```

## project data

``` r
# This list describes all of the files created by this script:
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
  c(name='soils_sf',
    file=file.path(out.subdir, 'nrcs_sf.rds'), 
    type='R sf object',
    description='SSURGO soils mapping units for UYRW, derived from soils_sdm'), 
  
  # STATSGO2 tabular data, after merging and removing duplicate rows
  c(name='soils_tab',
    file=file.path(out.subdir, 'nrcs_tab.rds'), 
    type='R list object',
    description='SSURGO tabular data for the UYRW area (a list of data frames), derived from soils_sdm'),
  
  # STATSGO2 map unit polygons with some associated data, projected and clipped to UYRW boundary
  c(name='soils_statsgo_sf',
    file=file.path(out.subdir, 'nrcs_statsgo_sf.rds'), 
    type='R sf object',
    description='STATSGO2 soils mapping units for UYRW, derived from soils_sdm'), 
  
  # STATSGO2 tabular data, after merging and removing duplicate rows
  c(name='soils_statsgo_tab',
    file=file.path(out.subdir, 'nrcs_statsgo_tab.rds'), 
    type='R list object',
    description='STATSGO2 tabular data for the UYRW area (a list of data frames), derived from soils_sdm'),
  
  # multipolygon representing areas in URYW with incomplete coverage in SSURGO
  c(name='ssurgo_incomplete_sfc',
    file=file.path(out.subdir, 'nrcs_incomplete_sfc.rds'), 
    type='R sfc object',
    description='areas in URYW with incomplete SSURGO coverage, derived from soils_sf and soils_tab'), 
  
  # combined set of SSURGO and STATSGO2 map unit polygons, covering the UYRW area
  c(name='soils_merged_sf',
    file=file.path(out.subdir, 'nrcs_merged_sf.rds'), 
    type='R sf object',
    description='merged STATSGO2/SSURGO soil mapping units for UYRW'), 
  
  # # aesthetic parameters for plotting
  # c(name='pars_tmap',
  #   file=file.path(data.dir, 'tmap_get_soils.rds'), 
  #   type='R list object', 
  #   description='parameters for writing png plots using tmap and tm_save'),
  
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
```

Note that `soils_sdm` points to a subdirectory, “data/source/nrsc\_sdm”,
containing a large number of files (too many to list individually). The
most important of these are the “ssa\_chunk\_\*.gml” files, which
delineate Soil Survey Areas (SSA) in our region of interest, and the
“wss\_SSA\_\*.zip” archives, which contain the raw data for each SSA.

We use the SSA data to find “area code” strings to query on the Soil
Data Mart. The `get_ssurgo` function downloads the data zip
corresponding to each code, then restructures its contents into a more
usable format (the contents of the subdirectories of “nrsc\_sdm”)

``` r
# write this information to disk
my_metadata('get_soils', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/get_soils_metadata.csv"

    ##                                                        file          type
    ## soils_acodes                  data/prepared/nrcs_acodes.rds   R sf object
    ## soils_sdm                                  data/source/nrcs     directory
    ## soils_sf                          data/prepared/nrcs_sf.rds   R sf object
    ## soils_tab                        data/prepared/nrcs_tab.rds R list object
    ## soils_statsgo_sf          data/prepared/nrcs_statsgo_sf.rds   R sf object
    ## soils_statsgo_tab        data/prepared/nrcs_statsgo_tab.rds R list object
    ## ssurgo_incomplete_sfc data/prepared/nrcs_incomplete_sfc.rds  R sfc object
    ## soils_merged_sf            data/prepared/nrcs_merged_sf.rds   R sf object
    ## img_soils                                graphics/soils.png   png graphic
    ## img_soils_wstor                    graphics/soils_wstor.png   png graphic
    ## metadata                        data/get_soils_metadata.csv           CSV
    ##                                                                                                    description
    ## soils_acodes               survey area polygons in the UYRW area corresponding to NRCS Soil Data Mart products
    ## soils_sdm             NRCS Soil Data Mart products (subfolders correspond to FedData calls using soils_acodes)
    ## soils_sf                                           SSURGO soils mapping units for UYRW, derived from soils_sdm
    ## soils_tab                SSURGO tabular data for the UYRW area (a list of data frames), derived from soils_sdm
    ## soils_statsgo_sf                                 STATSGO2 soils mapping units for UYRW, derived from soils_sdm
    ## soils_statsgo_tab      STATSGO2 tabular data for the UYRW area (a list of data frames), derived from soils_sdm
    ## ssurgo_incomplete_sfc       areas in URYW with incomplete SSURGO coverage, derived from soils_sf and soils_tab
    ## soils_merged_sf                                             merged STATSGO2/SSURGO soil mapping units for UYRW
    ## img_soils                                             image of map units with SSURGO/STATSGO2 data in the UYRW
    ## img_soils_wstor                                                       image of soil water storage for the UYRW
    ## metadata                                                            list files of files written by get_soils.R

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_soils_metadata.csv)
in the `/data` directory. Load some of the data prepared earlier

``` r
# load metadata csv, CRS info list and watershed polygons from disk
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
uyrw.waterbody = readRDS(here(my_metadata('get_basins')['waterbody', 'file']))
uyrw.mainstem = readRDS(here(my_metadata('get_basins')['mainstem', 'file']))
```

## Download the SSURGO Soil Survey Area (SSA) polygons

There seem to be some issues with the current version of `FedData` when
fetching data based on a bounding box (with v2.5.7, both `get_ssurgo`
and `get_ssurgo_inventory` failed when argument `template` was set to
the UYRW boundary). The chunk below is a workaround. It fetches a
collection of polygons which identify SSA codes for those spatial data
overlapping with the study area. In the next chunk, these SSA codes are
used to construct the `template` argument for `get_ssurgo`

``` r
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
```

## Download the SSURGO polygons/dataframes

Now that we have the SSA codes, we can request SSURGO data from the Soil
Data Mart. This is delivered in a zip archive containing ESRI shapefiles
(delineating the mapping units), and a huge collection of tabular data
as pipe-delimited (txt) tables, defining attributes in a relational
database.

These tabular data are meant to be opened using an MS Access template,
so there are unfortunately no column headers in any of the txt data. The
`get_ssurgo` function from `FedData` adds the column headers, and
converts the tabular data to properly labeled CSV files. It also handles
the download/extraction of the zip files.

Note: a large number of files not listed explicitly in
“get\_soils\_metadata.csv” are written to the subdirectory
“data/source/nrcs\_sdm” by this chunk. Their data are simplified and
consolidated into two output files, listed as `soils_sf` and `soils_tab`

``` r
if(any(!file.exists(here(my_metadata('get_soils')[c('soils_sf', 'soils_tab'), 'file']))))
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
    # note that force.redo=FALSE leads to parsing errors if the data have already been extracted
    setTxtProgressBar(pb, idx.acode)
    sdm.data.list[[idx.acode]] = get_ssurgo(template=uyrw.acodes[idx.acode], 
                                            label='UYRW', 
                                            raw.dir=here(my_metadata('get_soils')['soils_sdm', 'file']),
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
  
  # save the list of tables to disk, omitting any empty ones
  sdm.tab = sdm.tab[sapply(sdm.tab, nrow) > 0]
  saveRDS(sdm.tab, here(my_metadata('get_soils')['soils_tab', 'file']))
  
  # merge spatial data from all survey areas, transform to our projection
  sdm.sf = do.call(rbind, lapply(sdm.data.list, function(acode) st_transform(st_as_sf(acode$spatial), crs.list$epsg)))
  
  # fix broken geometries and clip to UYRW boundary
  sdm.sf = st_intersection(st_make_valid(sdm.sf), uyrw.poly)
  
  # save the polygons file to disk
  saveRDS(sdm.sf, here(my_metadata('get_soils')['soils_sf', 'file']))
  
} else {
  
  # load the sf object from disk
  sdm.sf = readRDS(here(my_metadata('get_soils')['soils_sf', 'file']))
  sdm.tab = readRDS(here(my_metadata('get_soils')['soils_tab', 'file']))
}
```

## Download STATSGO2 data

Looking at the distribution of [map unit
keys](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631)
(mukeys) across the landscape, we find a large area of incomplete
coverage around the Absaroka-Beartooth Wilderness Area, and many smaller
ones scattered throughout the watershed

![SSURGO coverage map of the
UYRW](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png)

Incomplete areas can be filled in with STATSGO2 data, available from the
[USDA/NCRS Geospatial Data Gateway](https://gdg.sc.egov.usda.gov), which
hosts direct downloads in the shared folders [at this
link](https://nrcs.app.box.com/v/soils).

We will save the STATSGO data for Wyoming and Montana, creating two new
output files: `soils_statsgo_sf` and `soils_statsgo_tab`. Note: a large
number of files not listed explicitly in “get\_soils\_metadata.csv” are
written to the subdirectory “data/source/nrcs\_sdm” by this chunk.

``` r
if(any(!file.exists(here(my_metadata('get_soils')[c('soils_statsgo_sf', 'soils_statsgo_tab'), 'file']))))
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
    out.dir = here(my_metadata('get_soils')['soils_sdm', 'file'])
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
  saveRDS(statsgo.sf, here(my_metadata('get_soils')['soils_statsgo_sf', 'file']))
  
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
  
  # omit data not pertinent to the selected mapunits, and delete any empty tables 
  statsgo.tab = extract_ssurgo_data(statsgo.tab, statsgo.sf$MUKEY)
  statsgo.tab = statsgo.tab[sapply(statsgo.tab, nrow) > 0]
  
  # save tabular data to disk
  saveRDS(statsgo.tab, here(my_metadata('get_soils')['soils_statsgo_tab', 'file']))
  
} else {
  
  # load the sf object from disk
  statsgo.sf = readRDS(here(my_metadata('get_soils')['soils_statsgo_sf', 'file']))
  statsgo.tab = readRDS(here(my_metadata('get_soils')['soils_statsgo_tab', 'file']))

}
```

## SSURGO vs STATSGO coverage

Looking at the mapunits only, the SSURGO database appears to cover most
of the UYRW apart from a large in the northeast. However, in many other
small areas, we are missing information on soil horizons needed by
SWAT+. The dataset is completed in these areas using STATSGO2

``` r
if(any(!file.exists(here(my_metadata('get_soils')[c('ssurgo_incomplete_sfc', 'soils_merged_sf'), 'file']))))
{
  # identify mapunits in SSURGO for which we have layers data (chorizon) on the dominant soil component...
  cokey.dominant = sdm.tab$component %>% group_by(mukey) %>% slice(which.max(comppct.r)) %>% select(mukey, cokey)
  mukey.tokeep = cokey.dominant$mukey[cokey.dominant$cokey %in% sdm.tab$chorizon$cokey]
  
  # ...and keep only those SSURGO mapunit polygons, discarding the incomplete ones
  sdm.sf = sdm.sf[as.integer(sdm.sf$MUKEY) %in% mukey.tokeep, ]
  
  # dissolve all remaining SSURGO mapunit polygons into a single multipolygon
  ssurgo.coverage.poly = st_make_valid(st_union(st_geometry(sdm.sf)))
  
  # take difference with watershed boundary polygon to get map of incomplete SSURGO areas, save to disk
  ssurgo.incomplete.poly = st_make_valid(st_difference(uyrw.poly, ssurgo.coverage.poly))
  saveRDS(ssurgo.incomplete.poly, here(my_metadata('get_soils')['ssurgo_incomplete_sfc', 'file']))
  
  # clip all STATSGO2 mapunits to keep only those areas needed to complete the SSURGO dataset
  statsgo.tokeep.sf = st_make_valid(st_intersection(statsgo.sf, ssurgo.incomplete.poly))
  
  # merge the SSURGO and STATSGO2 mapunit shapefiles, with preference for SSURGO, save to disk
  soils.merged.sf = st_make_valid(rbind(sdm.sf, statsgo.tokeep.sf))
  saveRDS(soils.merged.sf, here(my_metadata('get_soils')['soils_merged_sf', 'file']))

} else {
  
  # load the sf object from disk
  ssurgo.incomplete.poly = readRDS(here(my_metadata('get_soils')['ssurgo_incomplete_sfc', 'file']))
  soils.merged.sf = readRDS(here(my_metadata('get_soils')['soils_merged_sf', 'file']))
  
}
```

## visualization

``` r
# plot available survey coverage from SSURGO
if(!file.exists(here(my_metadata('get_soils')['img_soils', 'file'])))
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
      tm_lines(col='dodgerblue4', lwd=2) +
    tm_shape(uyrw.waterbody) + 
      tm_polygons(col='deepskyblue3', border.col='deepskyblue4') +
    tm_shape(ssurgo.incomplete.poly) +
      tm_polygons(col='black', border.alpha=0, alpha=0.7) +
    tmap.pars$layout +
    tm_layout(main.title=paste(tmap.tstr, tmap.tstr.sub, sep='\n'))
  
  # render the plot
  tmap_save(tm=tmap.soils, 
            here(my_metadata('get_soils')['img_soils', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}


# 
# ##
# make a sample plot of water storage estimate
# I follow the guide at https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?revision=705&root=aqp
# but run queries with base R instead of using SQL

# plot water storage
if(!file.exists(here(my_metadata('get_soils')['img_soils_wstor', 'file'])))
{
  # load DEM plotting parameters from disk
  tmap.pars = readRDS(here(my_metadata('get_dem')['pars_tmap', 'file']))
  
  # merge the required tables from SSURGO and STATSGO2, coercing one of the STATSGO2 columns to character type
  relevant.tables = setNames(nm=c('component', 'chorizon'))
  statsgo.tab$component$constreeshrubgrp = as.character(statsgo.tab$component$constreeshrubgrp)
  soils.tab = lapply(relevant.tables, function(tname) bind_rows(sdm.tab[[tname]], statsgo.tab[[tname]]) )
  
  # compute a total water storage estimate, combining all horizons and aggregating over map units
  awc.df = soils.tab$chorizon %>% 
    mutate(lyr_awc = awc.r*(hzdepb.r - hzdept.r)) %>%
    group_by(cokey) %>%
    summarize(component_awc = sum(lyr_awc, na.rm=TRUE)) %>%
    filter(!is.na(component_awc)) %>%
    left_join(soils.tab$component, by='cokey') %>%
    group_by(mukey) %>%
    summarize(total_awc = weighted.mean(component_awc, comppct.r)) %>%
    as.data.frame
  
  # pull the subset corresponding to the URYW study area
  awc.uyrw = awc.df$total_awc[match(as.integer(soils.merged.sf$MUKEY), awc.df$mukey)]

  # copy the polygons sf and append the water storage totals, omitting NA values
  wstor.sf = cbind(soils.merged.sf, awc.uyrw)
  
  tmap.pars$layout = tmap.pars$layout + 
    tm_layout(legend.text.color='black',
              legend.title.color='black')
  
  tmap.tstr = 'soil water storage available to plants'
  
  # prepare the plot grob
  tmap.wstor = tm_shape(wstor.sf) +
    tm_polygons(col='awc.uyrw', palette='YlGnBu', border.alpha=0, style='cont', title='cm', colorNA='red') +
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
            here(my_metadata('get_soils')['img_soils_wstor', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}
# 
```

![SSURGO soil water storage
estimates](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils_wstor.png)
