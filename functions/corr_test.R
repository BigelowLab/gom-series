#' Make a network plot with multiple sources of data
#' 
#' @param x tibble of data from `read_export()`
#' @param include character vector of variable names
#' @param start_year integer year to filter data going into network plot
#' @return a network plot
#' 
network <- function(x = read_export(), 
                    include = analysis_vars(),
                    start_year = 1970) {
  
  start <- sprintf("%s-01-01", start_year)
  
  x |>
    dplyr::filter(date >= as.Date(start)) |>
    dplyr::select(dplyr::all_of(include)) |>
    dplyr::select(dplyr::where(~abs(sum(., na.rm=TRUE)) > 0)) |>
    corrr::correlate() |>
    corrr::network_plot(colours = c("skyblue1", "white", "indianred2"))
  
}