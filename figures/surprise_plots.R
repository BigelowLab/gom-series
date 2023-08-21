## Script for generating surprise plots to be used in manuscript

source("./setup.R")

## Q25

q25 = read_export(by = 'year', 
                  standardize = FALSE, 
                  selection = read_target_vars(treatment = "q25"),
                  replace_names = TRUE) |>
  dplyr::filter(date >= as.Date("1970-01-01")) 

q25_plot = plot_surprise(surprise(q25, win = 20), surprise = 2, title = "Q25 Surprise by Year")

q25_plot

## Median

med_sup = read_export(by = 'year', 
                      standardize = FALSE, 
                      selection = read_target_vars(treatment = "median"),
                      replace_names=TRUE) |>
  dplyr::filter(date >= as.Date("1970-01-01"))

med_plot = plot_surprise(surprise(med_sup, win = 20), surprise = 2, title = "Median Surprise by Year")

med_plot

## Q75

q75 = read_export(by = 'year', 
                  standardize = FALSE, 
                  selection = read_target_vars(treatment = "q75"),
                  replace_names = TRUE) |>
  dplyr::filter(date >= as.Date("1970-01-01")) 

q75_plot = plot_surprise(surprise(q75, win = 20), surprise = 2, title = "Q75 Surprise by Year")

q75_plot
