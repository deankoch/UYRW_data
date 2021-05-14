# TODO: try parsing the I/O pdf to get snippets of text to add as description field

#install.packages('pdftools')
library(pdftools)

library(here)
source(here('R/helper_main.R'))
source(here('R/rswat.R'))

# load the doc into memory and display a list of files
rswat_docs() %>% str

rswat_docs('aquifer.aqu')
rswat_docs('hru-data.hru')

rswat_docs('hargreaves', indesc=TRUE)
rswat_docs('harg')

rswat_docs(pattern='snow')
rswat_docs(86)

rswat_docs(pattern='timp')

rswat_docs(pattern='fallow', indesc=TRUE)

rswat_docs('soils.sol')

rswat_docs(fname='soils.sol')

rswat_docs(pattern='snowmelt base', indesc=TRUE)
# 
rswat_docs(222)

rswat_docs(pattern='yr start')
rswat_docs(pattern='yr start', fname='time.sim', fuzzy=1)



rswat_docs(120)




# testing
#pageout %>%  select(page, line, file, name, description) 






cat(pdf.rawtext[122:123])


xx = pageout %>% filter(file=='plants.plt') %>%  filter(name=='bmp_bac') %>% pull(description)
grepl('\\\\n', xx, perl=TRUE)




pdf.delim

x = pdf.delim[[221]]
regex.fname = '([A-Z][A-Z0-9_]*)'
fname.suffix = c(', cont\\.', '\\(layer \\#\\)', '\\(top layer\\)')
regex.suffix =  paste0('((', paste(fname.suffix, collapse=')|('), '))')
grepl(paste0(regex.fname, regex.suffix), x)


# allow the following suffixes to appear after a variable declaration (eg. for soils.sol)

regex.fname = paste0( regex.fname,  )





pageout %>% filter(file=='codes.bsn') %>%  filter(name=='NUM_SALTS')
pageout %>% filter(file=='hru.con') %>%  filter(name=='GIS_ID')


# TODO: deal with soils.sol (", cont", "(layer #)", etc in variable names)


pageout %>% filter(file=='recall_day.rec')%>%  select(page, line, file, name, description)


xx[6]



vname = 'hi_ovr'
fname = 'harv.ops'
pageout %>% filter(name == vname) %>% filter(file == fname) %>% pull(description) %>% cat



pageout.trim

xx %>% slice((1:100) + 100)

xx %>% filter(section=='CODES.BSN')


#page.idx = sample(1:pdf.npage, 1)
pnum = 0
pnum = pnum + 1


pnum = 0
pnum = pnum + 1
pdftext = pdf.delim[[pnum]]
cat(pdf.rawtext[[pnum]])
rswat_pdf_parse(pdftext, ragged=TRUE)

rswat_pdf_parse(pdftext)



yy
  
  
  # look for 
  
  # TODO: identify continuations of tables by leading whitespace
  # TODO: turn the above into a function that outputs: named dataframes, raw extra text
  # TODO: iterate over pages
  
    # test 135 (easy)
    # test 42 (edge case, last line)
    # test 225 (table continues next page)

