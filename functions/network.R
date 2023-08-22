#' Make a network plot with multiple sources of data
#' 
#' @param x tibble of data from `read_export()`
#' @param include character vector of variable names
#' @param start_year integer year to filter data going into network plot
#' @param min_cor Number from 0 to 1 indicating the minimum value of correlations (in absolute terms) to plot passed to `network_plot`
#' @return a network plot
#' 
network <- function(x = read_export(), 
                    include,
                    start_year = 1970,
                    min_cor = 0.3) {
  
  start <- sprintf("%s-01-01", start_year)
  
  x |>
    dplyr::filter(date >= as.Date(start)) |>
    dplyr::select(dplyr::all_of(include)) |>
    dplyr::select(dplyr::where(~abs(sum(., na.rm=TRUE)) > 0)) |>
    replace_var_names() |>
    corrr::correlate() |>
    corrr::network_plot(min_cor = min_cor,
                        colours = c(get_color("blue"), get_color("white"), get_color("red")))
  
}