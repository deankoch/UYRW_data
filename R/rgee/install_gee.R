#' ---
#' title: "install_gee.R"
#' author: "Dean Koch"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' **Mitacs UYRW project** 
#' 
#' **install_rgee**: installs `rgee` in Windows and sets up user account info 
#' 
#' Based on [rgee](https://github.com/r-spatial/rgee) and this
#' [vignette](https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html).
#' 
#' This script requires a registered google earth engine account, a google drive account,
#' and Python v3.5+. It will create a python environment in the subfolder "gee" of your R project directory
#' (deleting anything in that path!).
#' 
#' The installation routines in `rgee` are interactive so this script isn't meant to be run
#' automatically. Users should step through the code with an R IDE, uncommenting/changing lines as needed.
#' There are some Y/N prompts, and a web-browser based google services login step. There is also
#' at least one point at which the R session must be restarted.
#' 
#' ## Dependencies
#' 
#' see also [helper_gee.R](https://github.com/deankoch/UYRW_data/blob/master/markdown/helper_gee.md)

# `here` is for relative paths to project directory 
library(here)

# this sets (and prints) a python path environmental variable for reticulate within the miniconda folder
source(here('R/rgee/helper_gee.R'))
print(conda.dir)

# pick a unique name for your project on GEE
gee.nm = 'dk_uyrw_rgee'

#' 
#' ## Installation

#' First, request an earth engine account (if you haven't got one already) as this can take some time,
#' and make sure it is linked to a google drive account (used for transferring files to your local machine).
#' Then ensure that Python (3.5+) is installed on your system before continuing
#' 
#' This code has only been tested on a Windows 10 machine. I haven't tested on linux or macOS but I
#' imagine the steps are similar if not identical. Miniconda is required, at least for Windows users,
#' by `reticulate` - see [this issue](https://github.com/r-spatial/rgee/issues/99). So the first step is
#' to install miniconda, which can be done from within R using `reticulate`
#' 
#' **step 1**
#' 
#' The chunk below checks for the dependencies directory `gee.dir` and if it doesn't exist, miniconda
#' is installed there. Note that the installation will use about 2GB of disk space.

# only proceed if `gee.dir` doesn't exist
if( !dir.exists(gee.dir) )
{
  # make the directory
  dir.create(gee.dir, recursive=TRUE)
  
  # install miniconda
  install_miniconda(conda.dir, update=FALSE, force=TRUE)
}

#' Restart the R session before continuing. If the previous chunk was successful, it will
#' be skipped in future sessions. 
#' 

#' 
#' **step 2**
#' 
#' Next we install the earth engine API using miniconda. We specify the version that
#' was tested with the most recent stable `rgee` release. If this is already installed,
#' the `py_install` call will skip installation

reticulate::py_install('earthengine-api==0.1.262')

#' print python info and verify things are set up correctly

py_config()
print(conda.dir)

#' This should indicate that `reticulate` has loaded a python environment located in a subfolder
#' of `conda.dir`. This path is set in 'helper_gee.R', which is sourced at the beginning of this
#' script. The contents of this folder - the miniconda installation and a Python environment for
#' `rgee` - are created in step 1. `py_config()` should now report a version of "numpy" and "ee"
#' installed in this environment. 
#' 
#' You may have to start a new R session at this point. Previous steps in the script will be
#' skipped if they completed successfully.

#' 
#' **step 3**
#' 
#' Lastly, we set the email address associated with the GEE account. Change the line below to
#' your email and log in to your GEE user account via your web-browser ('rgee' should send you
#' to this page automatically)
#' 

# load email (string) which I've stored in a private file - replace it with your own 
my.email = scan(file.path(dirname(conda.dir), 'email.txt'), what='character')
ee_Initialize(email=my.email, drive=TRUE)

#' `rgee` should prompt you for credentials at this point if you haven't entered them already.
#' After a successful log-in, you'll be asked to set up a root folder on GEE for your
#' account (if you haven't already). In my case I set this to the string `gee.nm` ("dk_uyrw_rgee"). 
#' 
#' Verify one last time that everything is working:

# check Python version, numpy, EE API
ee_check()

# check that credentials are found for EE, google drive, google cloud storage
ee_check_credentials()


#' If all the checks are successful, then we are finished. GEE can be accessed from R in future
#' sessions by sourcing `helper_gee.R` and running `ee_Initialize()`.

#+ include=FALSE
# Development code
#library(here)
#source(here('R/helper_main.R'))
#my_markdown('install_gee', 'R/rgee')