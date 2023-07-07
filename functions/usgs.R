#' Aggregate USGS daily data by month or year
#'
#' @param x table of USGS daily data for one or more sites (identified by site_no)
#' @param by string, the interbval over which to aggregate - one of month or year
#' @return tibble with aggregate stats 
aggregate_usgs = function(x = fetch_usgs(),
                          by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  x |>
    complete_intervals_usgs(by=by) |>
    dplyr::mutate(interval_ = format(.data$date, fmt) |> as.Date(), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "week", "season"))) |>
    dplyr::group_by(site_no, interval_) |>
    dplyr::group_map(
      function(tbl, key){
        v = sixnum(tbl |> dplyr::pull()) |>
          as.list() |>
          dplyr::as_tibble()
        dplyr::select(tbl, dplyr::all_of(c("interval_", "site_no"))) |>
          dplyr::slice(1) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::rename(date = "interval_")
}


#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).
#'
#' @param x tibble of usgs data
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364 or 365, but adjust to 28 or 29 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_usgs = function(x, 
                                   by = c("year", "month")[1], 
                                   min_count = c("year" = 12*28, "month" = 28)[[by]]){
  
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$date, fmt)) |>
    dplyr::group_by(site_no, interval_) |>
    dplyr::group_map(
      function(tbl, key){
        n_u = length(unique(as.Date(tbl$date)))
        if (n_u <= min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
}




#' Retrieve a slightly modified parameter look-up-table
#' 
#' @param x data frame of the dataRetrieval::parameterCdFile object
#' @return the input as a tibble with a guess at a name column
usgs_parameter_lut = function(x = dataRetrieval::parameterCdFile){
   x |> 
    dplyr::as_tibble() |>
    dplyr::mutate(name = sapply(strsplit(.data$parameter_nm, ",", fixed = TRUE), '[', 1) |>
                    tolower(),
                  .before = 1)
}

#' Guess an appropriate parmeter name 
#'
#' @param paramter string, one or more parameter codes
#' @param lut data frame the parameter lut
#' @return one or more parameter names
usgs_guess_parameter_name = function(parameter = "00060", lut = usgs_parameter_lut()){
  lut |>
    dplyr::filter(.data$parameter_cd %in% parameter) |>
    dplyr::pull(name)
}

#' Returns a listing of USGS stream gauges that empty into the Gulf of Maine
#' 
#' @return tibble
usgs_lut <- function(filename = here::here("data","usgs", "gom_stations.csv")) {
  readr::read_csv(filename, show_col_types = FALSE)
}


#' Fetches USGS daily values
#' @param table of USGS station data
#' @param parameter str, the parameter code - defaults to discgarge rate in ft^3/s
#' @return tibble of discharge data
fetch_usgs <- function(stations = usgs_lut(), 
                       parameter = "00060") {
  
  name = usgs_guess_parameter_name(parameter)
  
  dataRetrieval::readNWISdv(stations$site_no, parameter = parameter) |>
    dplyr::as_tibble() |>
    dplyr::select(-dplyr::matches("^X_.*_cd$")) |>
    dplyr::rename(date = Date,
                  !!name := dplyr::matches(glob2rx(paste0("X_", parameter, "*"))))
  
}




## siteNumbers <- c("01021480","01022500", "01022840","01037380", "01059000", "01069500",  "01069700","01021000")
## 
## readNWISsite(siteNumbers) |>
##   tibble() |>
##   select(site_no, station_nm, dec_lat_va, dec_long_va) |>
##   rename(name = station_nm,
##          lat = dec_lat_va,
##          lon = dec_long_va) |>
##   readr::write_csv("gom_stations.csv")

#' Export the annual (or monthly) data in a wide format
#' @param by character, one of 'year' or 'month'
#' @param x tibble or NULL, aggregated dataset.  If NULL we read it internally
#' @return wide tibble of aggregated data
export_usgs = function(by = c("year", "month")[1],
                       x = NULL){
  
  if (is.null(x)) x = aggregate_usgs(by = by)
  
  x|>
    tidyr::pivot_wider(names_from = "site_no", 
                       id_cols = "date",
                       names_glue = "USGS{site_no}.discharge.{.value}",
                       values_from = where(is.numeric))  |>
    dplyr::arrange(date)
}

