## Script to generate network plot figure for manuscript 

source("./setup.R")

x <- read_export(by = "year", selection = "all")

p <- network(x, include = read_target_vars(treatment = "median")) +
  theme(text = element_text(size=18),
        legend.position = "left",
        plot.margin = margin(0,0,0,0.3,"cm"))

p
