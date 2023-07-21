#' Make a network plot with multiple sources of data
#' 
#' @param x tibble of data from `read_export()`
#' @param include character vector of keywords to select data sources
#' @return a network plot
#' 
network <- function(x, 
                    include = c("nao", "amo", "gsi")) {
  
  x |>
    dplyr::select(dplyr::contains(include)) |>
    corrr::correlate() |>
    corrr::network_plot()
  
}