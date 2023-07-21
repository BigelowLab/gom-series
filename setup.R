# Run this script before working with this project.  It handles package installation and loading
# and sources local functions.
#
# A. check for CRAN available packages - install missing
# B. check for GITHUB available packages - install missing
# C. install any others
# D. load packages from library
# E. set the working directory
# F. source each file in subdir 'functions'

installed = rownames(installed.packages())

cran_packages = c("remotes", "rlang", "here", "httr", "R6", "xml2", "rerddap", "sf", "stars",
                  "rnaturalearth", "rnaturalearthdata", "dataRetrieval", "ncdf4", "ggplot2",
                  "scales", "tidyr", "readr", "dplyr", "stringr", "purrr", "magrittr", "corrr")
ix = (cran_packages %in% installed)
for (package in cran_packages[!ix]) {
  install.packages(package)
}


github_packages = c("cofbb" = "BigelowLab", 
                    "stsaav" = "BigelowLab", 
                    "wdspaleo" = "BigelowLab", 
                    "ghcnd" =  "BigelowLab")
ix = names(github_packages) %in% installed
for (package in names(github_packages[!ix])) {
  remotes::install_github(sprintf("%s/%s", github_packages[package], package))
}

if (!("rnaturalearthhires" %in% installed)) {
  install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
}


suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
  for (package in names(github_packages)) library(package, character.only = TRUE)
  library(rnaturalearthhires)
})

here::i_am("setup.R")

for (f in list.files(here("functions"), pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
