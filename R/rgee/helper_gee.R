#' ---
#' title: "helper_GEE.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **helper_gee**: helper functions for google earth engine downloads
#' 
#' This uses the `rgee` package, which depends on `reticulate`, an R wrapper for python calls.
#' We use the native python API for GEE, so a working python installation is required.

# relative paths for working directory
library(here)

# `googledrive` and `future` are required by `rgee` to download raster collections via gdrive
library(googledrive)
library(future)

# define directories for dependencies
gee.dir = here('rgee')
conda.dir = file.path(gee.dir, 'conda')
conda.env = file.path(conda.dir, 'envs/r-reticulate')

# set an environmental variable to ensure reticulate finds the right environment
Sys.setenv(RETICULATE_PYTHON = conda.env)
library('reticulate')
library('rgee')


#+ include=FALSE
#library(here)
#source(here('R/helper_main.R'))
#my_markdown('helper_gee', 'R/rgee')