## Script for generating Figure 3 (Network Plot) for manuscript

source("setup.R")

x = read_export(by = 'year', 
                selection = read_target_vars(treatment = c("median")),
                replace_names = FALSE, 
                standardize = FALSE) |>
  dplyr::filter(date >= as.Date("1900-01-01"))

vars <- read_target_vars(treatment = "median")

p <- network(x = x, 
             include=vars,
             min_cor=0.3) +
  theme(legend.position = "left")

p
