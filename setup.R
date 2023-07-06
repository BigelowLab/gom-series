# add a check for Bigelow-github packages here
# cofbb, stsaav, wdspaleo, ghcnd
# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")



suppressPackageStartupMessages({
  here::i_am("setup.R")
  library(rlang)
  library(httr)
  library(here)
  library(R6)
  library(xml2)
  library(rerddap)
  library(sf)
  library(stars)
  library(rnaturalearth)
  library(rnaturalearthdata)
  #library(copernicus)
  library(stsaav)
  library(wdspaleo)
  library(ghcnd)
  library(ncdf4)
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(readr)
  library(dplyr)
  library(dataRetrieval)
  library(stringr)
  library(purrr)
})

PATH = here::here()
# source functionality
for (f in list.files(here("functions"), pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
