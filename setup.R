suppressPackageStartupMessages({
  here::i_am("setup.R")
  library(rlang)
  library(here)
  library(R6)
  library(xml2)
  library(rerddap)
  library(sf)
  library(stars)
  library(rnaturalearth)
  library(copernicus)
  library(stsaav)
  library(ghcnd)
  library(ncdf4)
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(dplyr)
})

PATH = here::here()
# source functionality
for (f in list.files(here("functions"), pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
