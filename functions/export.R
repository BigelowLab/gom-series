#' Plot an export data frame
#'
#' @param x data frame or tibble of exported data
#' @return ggplot object
plot_export = function(x = read_export(by = 'year', standardize = TRUE) |>
                         dplyr::select(date, dplyr::contains("mean")) |>
                         dplyr::filter(date >= as.Date("1950-01-01")),
                       title = 'Standardized Values'){
  
  period = if(diff(x$date[1:2]) > 32){
      'Year'
    } else {
      'Month'
    }
  
  long = long_export(x)
  ggplot2::ggplot(long, ggplot2::aes(name, date)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "#f7f7f7") +
    ggplot2::scale_fill_distiller(palette = "Spectral", na.value = 'grey90') +
    ggplot2::labs(x = "", y = "Date", 
                  title = paste("Standard Score by", period)) +
    ggplot2::scale_x_discrete(name = "Parameter", 
                     breaks = ggplot2::waiver(), 
                     labels = as.character(seq(to = ncol(x)-1)),
                     guide = guide_axis(angle = 90)) + 
    ggplot2::theme_bw()
}


#' Pivot an export table to long format
#' 
#' @param x tibble of export data
#' @return tibble in long format
long_export = function(x = read_export(by = 'year', standardize = TRUE)){
  tidyr::pivot_longer(x,
                     !date)
}


#' Read in one of the export files
#' 
#' @param by char, one of 'month' or 'year'
#' @param path, char, the path to the export files
#' @param scale_it if TRUE then apply the \code{\link[base]{scale} argument to each column
#' @param standardize logical, if TRUE the pass the variables through the \code{scale}
#'   function so each variable has a mean of 0 and standard deviation of 1.
#' @param ... other arguments passed to \code{scale}
#' @return very wide tibble
read_export = function(by = c("year", "month")[1],
                       path = here::here("data", "export"),
                       standardize = FALSE, 
                       ...){
  filename = file.path(path, sprintf("export_%s.csv.gz", by))
  if (!file.exists(filename)) stop("export file not found:", filename)
  x = readr::read_csv(filename, show_col_types = FALSE)
  if (standardize){
    x = x |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ as.vector(scale(.x, ...)))
)  }
  x
}


#' Export a suite of datasets to a single wide format.
#' 
#' @param what char, vector of datasets to include 
#' @param by char the interval to aggregate over (one of 'year' or 'month')
#' @param ofile char or NULL, if not NULL then write to this file. If NULL write nothing
#' @return a very very very wide aggregate table
export = function(what = c("all","sst", "chlor", "usgs", "ghcn", "indices", "buoys")[1],
                  by = c("year", "month")[1], 
                  ofile = here::here("data", "export", sprintf("export_%s.csv.gz", by))){
 
  if ("all" %in% what) what = c("sst", "chlor", "usgs", "ghcn", "indices", "buoys")
  
  xx = sapply(what,
              function(w){
                switch(w,
                       "sst" = export_oisst(by = by),
                       "chlor" = export_chlor_cmems(by=by),
                       "usgs" = export_usgs(by = by),
                       "ghcn" = export_ghcn(by = by),
                       "buoys" = export_buoy(by = by),
                       "indices" = export_climate_indices(by = by) 
                ) # switch
              }, simplify = FALSE)
  
  # reorder based upon number of rows (most rows first)
  n = sapply(xx, nrow)
  ix = order(n, decreasing = TRUE)
  xx = xx[ix]
  
  
  x = purrr::reduce(xx, dplyr::left_join, by = 'date')  |>
    dplyr::arrange(date)
  
  if (!is.null(file)) readr::write_csv(x, ofile)
  
  x
}