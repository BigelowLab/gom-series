## Script for generating Figure 4 (Surprise Plot) for manuscript

source("setup.R")

surprise_window = 20
surprise_threshold = 2

x = read_export(by = 'year', 
                selection = read_target_vars(treatment = c("median")),
                replace_names = TRUE, 
                standardize = FALSE) |>
  dplyr::filter(date >= as.Date("1900-01-01"))

p <- plot_departure_surprise(x, surprise_window = surprise_window)

p
