#' Aggregate ERSST by month or year
#' 
#' @param x ERSST data
#' @param by char one of "month" or "year"
#' @return table of date and mean ERSST
aggregate_ersst = function(x = read_ersst(),
                           by = c("month", "year")[2]){
  
  if (tolower(by[1]) == 'month'){
    # message("oisst comes as monthly aggregation - returning input")
    return(x)
  }
  
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  x |>
    complete_intervals_ersst(by = by) |>
    dplyr::mutate(interval_ = format(.data$date, fmt) |> as.Date(), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "year", "month", "week", "season"))) |>
    dplyr::group_by(interval_) |>
    dplyr::group_map(
      function(tbl, key, parameters = c("min", "q25", "median", "mean", "q75", "max")){
        vals = dplyr::pull(tbl, "ersst") |> 
          sixnum() |>
          as.list() |>
          dplyr::as_tibble()
        dplyr::select(tbl, dplyr::any_of(c("interval_"))) |>
          dplyr::slice(1) |>
          dplyr::bind_cols(vals)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::rename(date = "interval_")
}

#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  We count unique days in the interval, even though data may
#'  ne recorded much more frequently
#'
#' @param x tibble of ERSST data
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364 or 365, but adjust to 28 or 29 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_ersst = function(x = read_ersst(), 
                                    by = "year", 
                                    min_count = c("year" = 11)[[by]]){
  if (by != "year"){
    message("complete intervals available only by year - returning input")
    return(x)
  }
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$date, fmt)) |>
    dplyr::group_by(interval_) |>
    dplyr::group_map(
      function(tbl, key){
        n_u = length(unique(tbl$date))
        if (n_u <= min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
}


#' Read ERSST data
#' 
#' @param filename char the name of the file
#' @return tibble of date and ersst
read_ersst = function(filename = here::here("data", "ersst","ersst.csv.gz")){
  readr::read_csv(filename, col_types = "Dn")
}



#' Extract ERSST data for the GoM region
#' 
#' @param bb 4 element bounding box or "bbox" class, we use the 
#'   bounding box of the union of regions.
#' @return a CSV file of the aggregate mean for the given bounding box
fetch_ersst = function(bb = c(xmin = 289, ymin = 40, xmax = 294, ymax = 45)){
  
  path = ersst::ersst_path("v5")
  db = ersst::read_database(path) |>
    dplyr::filter(!anomaly)
  files = ersst::compose_filename(db, path)
  ix = file.exists(files)
  S = stars::read_stars(files[ix],
                        along = list(time = db$date[ix]))
  
  bb = sf::st_bbox(bb, crs = sf::st_crs(S)) |>
    sf::st_as_sfc()
  suppressMessages({
    orig = sf::sf_use_s2(FALSE)
    v = stars::st_extract(S, bb) |>
      dplyr::as_tibble() |>
      dplyr::select(-dplyr::all_of("geometry")) |>
      rlang::set_names(c("date", "ersst")) |>
      readr::write_csv(here::here("data", "ersst","ersst.csv.gz"))
    sf::sf_use_s2(orig)
  })
  v
}


#' Export the annual (or monthly) data in a wide format
#' @param by character, one of 'year' or 'month'
#' @param x tibble or NULL, aggregated dataset.  If NULL we read it internally
#' @return wide tibble of aggregated data
export_ersst = function(by = c("year", "month")[1],
                        x = NULL){
  
  if (is.null(x)) x = aggregate_ersst(by = by)

  nm = names(x)
  ix = seq(from = 2, to = length(nm))
  nm[ix] = paste0("ersst.", nm[ix])
  names(x) = nm
  
  dplyr::arrange(x, date)
}
