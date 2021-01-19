get\_soils.R
================
Dean Koch
2021-01-19

**Mitacs UYRW project**

**get\_soils**: download and process NRCS SSURGO and STATSGO2 soils data
for use with SWAT/SWAT+

[get\_basins.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_basins.md)
and
[get\_dem.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_dem.md)
should be run first.

This script downloads and rasterizes the polygon data that SWAT+ uses to
assign soil properties to different watershed areas. Soil properties are
defined for the model in the `usersoil` parameter table, each row of
which is mapped to a map unit key (mukey) in the soil raster.

An older version of this script built a custom `usersoil` table to
import into a SWAT+ project. This copied data for each mukey in the
URYW, directly from the `component` and `chorizon` tables in the latest
SSURGO and STATSGO2 attribute databases, with a preference for SSURGO.
However, that led to errors in SWAT/SWAT+ simulations indicating that we
need to go back and do some data cleaning (eg verify that horizon depths
are in descending order), or check for mistakes in the usersoil table
syntax (eg verify no-data flag is correct), or both.

For now, we use the default soil database that ships with SWAT+. This
contains all STATSGO2 mukeys for the UYRW but is missing many SSURGO
mukeys (particularly in Idaho and Wyoming), so it produces a far less
detailed soil map. Development on the custom `usersoil` table is
ongoing.

## libraries

[`FedData`](https://cran.r-project.org/web/packages/FedData/index.html)
is used to fetch the [NRCS
SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053627)
soils data,
[`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf) is
used to parse the NRCS website for links to STATSGO2 data archives, and
[‘RSQLite’](https://cran.r-project.org/web/packages/RSQLite/index.html)
is used to open the soil parameters database (usersoil) that ships with
SWAT+. [‘RODBC’](https://db.rstudio.com/odbc/) connects to MS Access
databases using ODBC drivers. See the [get\_helperfun.R
script](https://github.com/deankoch/UYRW_data/blob/master/markdown/get_helperfun.md)
for other required libraries

``` r
library(here)
source(here('R/get_helperfun.R'))
library(FedData)
library(rvest)
```

    ## Loading required package: xml2

``` r
library(RSQLite)
library(RODBC)
```

## project data

A list object definition here (`files.towrite`) has been hidden from the
markdown output for brevity. The list itemizes all files written by the
script along with a short description. We use a helper function to write
this information to disk:

``` r
soils.meta = my_metadata('get_soils', files.towrite, overwrite=TRUE)
```

    ## [1] "writing to data/get_soils_metadata.csv"

``` r
print(soils.meta[, c('file', 'type')])
```

    ##                                                          file            type
    ## soils_sqlite                data/source/swatplus_soils.sqlite SQLite database
    ## soils_acodes                    data/prepared/nrcs_acodes.rds     R sf object
    ## soils_sdm                                    data/source/nrcs       directory
    ## ssurgo_sf                         data/prepared/ssurgo_sf.rds     R sf object
    ## ssurgo_tab                       data/prepared/ssurgo_tab.rds   R list object
    ## statsgo_sf                       data/prepared/statsgo_sf.rds     R sf object
    ## statsgo_tab                     data/prepared/statsgo_tab.rds   R list object
    ## ssurgo_incomplete_sfc data/prepared/ssurgo_incomplete_sfc.rds    R sfc object
    ## soils_merged_sf             data/prepared/soils_merged_sf.rds     R sf object
    ## soils_merged_tab           data/prepared/soils_merged_tab.rds   R list object
    ## swat_soils_tif                    data/prepared/swat_soil.tif         GeoTIFF
    ## swat_soils_lookup         data/prepared/swat_soils_lookup.csv             CSV
    ## img_soils                                  graphics/soils.png     png graphic
    ## metadata                          data/get_soils_metadata.csv             CSV

This list of files and descriptions is now stored as a [.csv
file](https://github.com/deankoch/UYRW_data/blob/master/data/get_soils_metadata.csv)
in the `/data` directory. Load some of the data prepared earlier

``` r
crs.list = readRDS(here(my_metadata('get_basins')['crs', 'file']))
uyrw.poly = readRDS(here(my_metadata('get_basins')['boundary', 'file']))
uyrw.waterbody = readRDS(here(my_metadata('get_basins')['waterbody', 'file']))
uyrw.mainstem = readRDS(here(my_metadata('get_basins')['mainstem', 'file']))
dem.tif = raster(here(my_metadata('get_dem')['dem', 'file']))
```

## download the SWAT+ SQLite soils databases

Before writing the soil mukeys raster, we need to find out which keys
are listed in the default `usersoil` parameter table. A soil mukey that
appears in the raster but not in the parameter table will produce errors
in QSWAT/QSWAT+ watershed delineation. To avoid that, we cross-reference
mukeys discovered via NRCS with those in the default `usersoil`, and
avoid writing mukeys not listed in that table.

This chunk downloads the .sqlite soils database that ships with SWAT+,
then imports the `usersoil` table, which we’ll use later on in the
script. Note that although the mdb table for SWAT appears to be smaller
than the SQL table for SWAT+ (at this time), it contains the same SSURGO
mukeys, at least as far as the URYW is concerned. Assuming that errors
in parameter values are more likely to have been corrected in the SWAT+
database, we will use that version going forward

``` r
# download the sqlite file if it is not already on the destination path
sqlite.dest.path = here(soils.meta['soils_sqlite', 'file'])
if(!file.exists(sqlite.dest.path))
{
  # URL pulled from the [SWAT+ docs](https://swatplus.gitbook.io/docs/installation)
  soils.sql.url = 'https://bitbucket.org/swatplus/swatplus.editor/downloads/swatplus_soils.sqlite'
  download.file(soils.sql.url, sqlite.dest.path, mode='wb')
 
} 

# load the relevant tables from the SWAT+ database
soils.sql = dbConnect(RSQLite::SQLite(), here(soils.meta['soils_sqlite', 'file']))
ssurgo.sql.ref = dbReadTable(soils.sql, 'ssurgo')
statsgo.sql.ref = dbReadTable(soils.sql, 'statsgo')
dbDisconnect(soils.sql)
```

## download the SSURGO Soil Survey Area (SSA) polygons

There seem to be some issues with the current version (v2.5.7) of
`FedData`: both `get_ssurgo` and `get_ssurgo_inventory` fail when
argument `template` is set to the UYRW boundary. The chunk below is a
workaround.

It fetches a collection of polygons identifying SSA codes (“area code”
strings) for spatial data overlapping with the study area. In the next
step, these SSA codes will be used to construct the `template` argument
for `get_ssurgo` so that we can query the Soil Data Mart. The
`get_ssurgo` function downloads the data zip corresponding to each code,
then restructures its contents into a more usable format, with all files
saved to the subdirectory listed in metadata entry `soils_sdm`.

The most important of these are the “ssa\_chunk\_\*.gml” files, which
delineate Soil Survey Areas (SSA) in our region of interest, and the
“wss\_SSA\_\*.zip” archives, which contain the raw data for each SSA.

``` r
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
```

## download the SSURGO polygons/dataframes

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

Note: a large number of files not listed explicitly in the (`get_soils`)
metadata file are written (to the `soils_sdm` subdirectory) by this
chunk. Their data are simplified and consolidated into two output files,
`ssurgo_sf` and `ssurgo_tab`.

``` r
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
```

## download STATSGO2 data

Looking at the distribution of SSURGO [map unit
keys](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631)
(mukeys) across the landscape, we find a large area of incomplete
coverage around the Absaroka-Beartooth Wilderness Area, and many smaller
ones scattered throughout the watershed (see [this
graphic](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png),
generated at the end of this script)

Incomplete areas can be filled in with STATSGO2 data, available from the
[USDA/NCRS Geospatial Data Gateway](https://gdg.sc.egov.usda.gov), which
hosts direct downloads in the shared folders [at this
link](https://nrcs.app.box.com/v/soils).

This chunk fetches the STATSGO data for Wyoming and Montana, creating
two new output files: `statsgo_sf` and `statsgo_tab`.

``` r
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
```

## usersoil.csv (work in progress)

This chunk copies the SSURGO\_Soils (‘usersoil’) table from the MS
Access soil reference database that is used with SWAT2012
(‘SWAT\_US\_SSURGO\_Soils.mdb’), exporting it as CSV. This includes
data for STATSGO2 mukeys. Since the dataset is very large, only the
mukeys identified above as part of the URYW region are copied.

QSWAT+ stores the same data in its SQLite soils reference database
(‘swatplus\_soils.sqlite’), but it is indexed differently (split over
several tables). QSWAT+ has an option to import this data automatically,
or it can use a usersoil table like the one copied here. SWAT+AW,
however, requires the usersoil table, which is why we save a copy here.

``` r
# Save a copy of the usersoil table from the mdb database
usersoil.path = here(out.subdir, 'usersoil.csv')
swatmdb.path = 'H:/UYRW_installers/SWAT_US_SSURGO_Soils.mdb' 
if(0)
{
  # open the database to extract the (very large) usersoil table
  mdb.string = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)};'
  swatmdb.con = odbcDriverConnect(paste0(mdb.string, 'DBQ=', swatmdb.path))
  usersoil.ref = sqlFetch(swatmdb.con, 'SSURGO_Soils')
  odbcClose(swatmdb.con)
  
  # prune to remove entries whose mukeys aren't found in the study area
  ssurgo.mukeys = unique(as.integer(ssurgo.sf$MUKEY))
  statsgo.mukeys = unique(as.integer(statsgo.sf$MUKEY))
  idx.keep = usersoil.ref$MUID %in% c(ssurgo.mukeys, statsgo.mukeys)
  
  # write to disk as CSV
  write.csv(usersoil.ref[idx.keep,], file=usersoil.path, row.names=F)
  
 
}
```

## SSURGO vs STATSGO coverage

The soils database for SWAT/SWAT+ is currently (October, 2020) missing a
large number of SSURGO mukeys in the UYRW area. However the STATSGO2
coverage is complete. This chunk identifies all missing SSURGO mukeys
(there are 74) and replaces them with the appropriate STATSGO2 mukey.

``` r
if(any(!file.exists(here(soils.meta[c('ssurgo_incomplete_sfc', 'soils_merged_sf', 'soils_merged_tab'), 'file']))))
{
  # identify 330 unique SSURGO map units in UYRW
  ssurgo.mukeys = sort(unique(as.integer(ssurgo.sf$MUKEY)))
  length(ssurgo.mukeys)

  # some of these are missing from the SWAT+ database
  ssurgo.mukeys.ref = as.integer(ssurgo.sql.ref$muid)
  mukey.miss = ssurgo.mukeys[!(ssurgo.mukeys %in% ssurgo.mukeys.ref)]
  length(mukey.miss) # missing 74 (22%) ...
  
  # this corresponds to around 3300 polygons (~50%), mostly in Wyoming, Idaho
  sum(ssurgo.sf$MUKEY %in% mukey.miss)
  
  # make a copy of the SSURGO polygons dataset and index the missing mukeys
  ssurgo.trimmed.sf = ssurgo.sf
  idx.miss = ssurgo.sf$MUKEY %in% mukey.miss
  
  # for each of these incomplete polygons, swap in the STATSGO2 mukey corresponding to the largest area of overlap
  pb = txtProgressBar(min=0, max=sum(idx.miss), style=3)
  print('replacing missing data in SSURGO with STATSGO2 entries')
  for(idx.loop in 1:sum(idx.miss))
  {
    # pull the polygon and compute its total area
    idx.poly = which(idx.miss)[idx.loop]
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
```

## build soil raster for SWAT+

SWAT+ should now be able to understand all of the mukeys listed in
`soils.merged.sf`, as we have verified that each one appears in the
default soils parameter table.

This chunk rasterizes that polygon data so that it can be loaded by
QSWAT+

``` r
if(!file.exists(here(soils.meta['swat_soils_tif', 'file'])))
{
  # pull mukey values and build the usersoil table using a helper function
  soils.mukeys = sort(unique(as.integer(soils.merged.sf$MUKEY)))
  usersoil = my_usersoil(soils.merged.tab, soils.mukeys)
  
  # rasterize the MUKEY data and write geotiff to disk
  soils.tif.path = here(soils.meta['swat_soils_tif', 'file'])
  soils.tif.prelim = gRasterize(as(soils.merged.sf, 'Spatial'), dem.tif, field='MUKEY', soils.tif.path)
  
  # the MUKEY attribute was a character string, so we have to reclassify to integer
  rcl.df = levels(soils.tif.prelim)[[1]]
  rcl.mat = apply(rcl.df, 2, as.integer)
  soils.tif.prelim = reclassify(soils.tif.prelim, rcl.mat)
  
  # crop the geotiff to URYW boundary and write to disk
  soils.tif = crop(soils.tif.prelim, as(uyrw.poly, 'Spatial'))
  writeRaster(soils.tif, soils.tif.path, overwrite=TRUE, NAflag=tif.na.val)
}
```

## visualization

Create a plot showing the complete set of SSURGO map units by tiling the
UYRW area in different colours, with darkened areas indicating map units
with data filled in by STATSGO2 surveys data.
![](https://raw.githubusercontent.com/deankoch/UYRW_data/master/graphics/soils.png)

``` r
if(!file.exists(here(soils.meta['img_soils', 'file'])))
{
  # load the plotting parameters used in get_basins.R
  tmap.pars = readRDS(here(my_metadata('get_basins')['pars_tmap', 'file']))
  
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
      tm_polygons(col='black', border.alpha=0, alpha=0.5) +
    tmap.pars$layout +
    tm_layout(main.title=paste(tmap.tstr, tmap.tstr.sub, sep='\n'))
  
  # render the plot
  tmap_save(tm=tmap.soils, 
            here(soils.meta['img_soils', 'file']), 
            width=tmap.pars$png['w'], 
            height=tmap.pars$png['h'], 
            pointsize=tmap.pars$png['pt'])
}
```
