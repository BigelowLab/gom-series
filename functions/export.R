plot_export = function(...) plot_wide(...)

#' Plot a wide data frame
#'
#' @param x data frame or tibble of wide data
#' @param purge_empty, logical if TRUE drop empty columns (populated by NAs only)
#' @return ggplot object
plot_wide = function(x = read_export(by = 'year') |>
                         dplyr::filter(date >= as.Date("1950-01-01")) |>
                         standardize_export(),
                       purge_empty = TRUE,
                       title = NULL){
  
  period = if(diff(x$date[1:2]) > 32){
      'Year'
    } else {
      'Month'
    }
  
  if (is.null(title)) title = paste("Values by", period)
  
  
  if (purge_empty) x = purge_export(x)
  
  cnames = dplyr::select(x, -date) |> colnames()
  long = long_export(x) |>
    dplyr::mutate(name = factor(name, levels = cnames))
  
  ggplot2::ggplot(long, ggplot2::aes(date, name)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "#f7f7f7") +
    ggplot2::scale_fill_gradient2(low="blue", high="red", na.value="grey80", name="") + 
    ggplot2::labs(x = "Date", 
                  y = "", 
                  title = title) +
    ggplot2::scale_y_discrete(name = "Parameter", 
                     #breaks = ggplot2::waiver(), 
                     #labels = levels(cnames),
                     guide = guide_axis(angle = 0)) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = element_text(size=10))
}


#' Drop columns that are exclusively filled with NA
#' 
#' @param x tibble of exported data
#' @return the input tibble but with 'empty' columns dropped
purge_export = function(x){
  empty = sapply(x, function(column) all(is.na(column)))
  x[,!empty]
} 


#' Pivot an export table to long format
#' 
#' @param x tibble of export data
#' @return tibble in long format
long_export = function(x = read_export(by = 'year', standardize = TRUE)){
  tidyr::pivot_longer(x,
                     !date)
}



#' Standardize export data such that each variable has a mean of 0 and a standard deviation of 1
#' 
#' @param x wide export data (tibble with leading date column)
#' @param ... other arguments passed to \code{scale}
#' @param input table with numeric variables standardized
standardize_export = function(x, ...){
  x |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ as.vector(scale(.x, ...))))
}


#' Read in one of the export files
#' 
#' @param by char, one of 'month' or 'year'
#' @param path, char, the path to the export files
#' @param selection 
#' @param standardize logical, if TRUE the pass the variables through the \code{scale}
#'   function so each variable has a mean of 0 and standard deviation of 1.
#' @param ... other arguments passed to \code{scale}
#' @return very wide tibble
read_export = function(by = c("year", "month")[1],
                       path = here::here("data", "export"),
                       selection = list("all", read_target_vars())[[2]],
                       standardize = FALSE, 
                       ...){
  filename = file.path(path, sprintf("export_%s.csv.gz", by))
  if (!file.exists(filename)) stop("export file not found:", filename)
  x = readr::read_csv(filename, col_types = readr::cols(date = col_date(),
                                                        .default = col_double()))
  
  if (!("all" %in% selection)) x = dplyr::select(x,dplyr::all_of(c("date", selection)))
  
  if (standardize){
    x = standardize_export(x, ...)
  }
  x
}


#' Export a suite of datasets to a single wide format.
#' 
#' @param what char, vector of datasets to include 
#' @param by char the interval to aggregate over (one of 'year' or 'month')
#' @param ofile char or NULL, if not NULL then write to this file. If NULL write nothing
#' @return a very very very wide aggregate table
export = function(what = c("all","sst", "chlor", "bswm", "usgs", "ghcn", "climate", "hab", "buoys", "pci", "calanus")[1],
                  by = c("year", "month")[1], 
                  ofile = here::here("data", "export", sprintf("export_%s.csv.gz", by))){
 
  if ("all" %in% what) what = c("sst", "chlor", "bswm", "usgs", "ghcn", "climate", "hab", "buoys","pci", "calanus")
  
  xx = sapply(what,
              function(w){
                switch(w,
                       "sst" = export_oisst(by = by),
                       "chlor" = export_chlor_cmems(by=by),
                       "usgs" = export_usgs(by = by),
                       "ghcn" = export_ghcn(by = by),
                       "buoys" = export_buoy(by = by),
                       "climate" = export_climate_indices(by = by),
                       "hab" = export_hab_index(by = by),
                       "pci" = export_pci(by = by),
                       "bswm" = export_bswm(by=by),
                       "calanus" = export_calanus(by=by)
                ) # switch
              }, simplify = FALSE)
  
  # reorder based upon number of rows (most rows first)
  isnull = sapply(xx, is.null)
  xx = xx[!isnull]
  n = sapply(xx, nrow)
  ix = order(n, decreasing = TRUE)
  xx = xx[ix]
  
  
  x = purrr::reduce(xx, dplyr::left_join, by = 'date')  |>
    dplyr::arrange(date)
  
  if (!is.null(file)) readr::write_csv(x, ofile)
  
  x
}