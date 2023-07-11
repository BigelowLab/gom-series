#' Read in one of the export files
#' 
#' @param by char, one of 'month' or 'year'
#' @param path, char, the path to the export files
#' @param scale_it if TRUE then apply the \code{\link[base]{scale} argument to each column
read_export = function(by = c("year", "month")[1],
                       path = here::here("data", "export"),
                       scale_it = FALSE){
  filename = file.path(path, sprintf("export_%s.csv.gz", by))
  if (!file.exists(filename)) stop("export file not found:", filename)
  x = readr::read_csv(filename, show_col_types = FALSE)
  if (scale_it){
    nc = ncol(x)
   x[-1] = scale(x[-1], ...)
  }
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