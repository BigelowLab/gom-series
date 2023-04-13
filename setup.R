suppressPackageStartupMessages({
  here::i_am("setup.R")
  library(here)
  library(xml2)
  library(rerddap)
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(dplyr)
})

PATH = here::here()
# source functionality
for (f in list.files(here("scripts"), pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

