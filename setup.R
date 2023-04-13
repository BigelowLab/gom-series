suppressPackageStartupMessages({
  here::i_am("setup.R")
  library(here)
  library(rerddap)
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(dplyr)
})

# source functionality
for (f in list.files(here("scripts"), pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

