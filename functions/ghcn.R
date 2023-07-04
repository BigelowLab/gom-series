#' Read a stations look up table
#' 
#' @param filename the name of the file to read
#' @return sf tibble inheriting "ghcnd_station_list" class 
ghcn_lut = function(filename = here::here("data","ghcn", "station_list.gpkg")){
  
  #station_ids = c(PWM = "USW00014764",
  #                BHC = "USC00190736",
  #                #LIA = "USW00014739",
  #                DNH = "USC00272174",
  #                ANP = "USC00170100",
  #                COR = "USC00171628")
  #stations = fetch_station_list() |>
  #  dplyr::filter(id %in% station_ids) >
  #  sf::write_sf(here::here("data","ghcn", "station_list.gpkg"))
  
  
  x = sf::read_sf(filename)
  class(x) <- c("ghcnd_station_list", class(x))
  x
}


#' Retrieve inventories for one or more stations
#'
#' @param stations sf tibble of stations identifiers
#' @return sf tibble inheriting "ghcnd_inventory" class
fetch_ghcn_inventory = function(stations = ghcn_lut()){
  

  
  ghcnd::fetch_inventory(stations = stations)
}


#' Retrieve data for one or more stations
#'
#' @param stations sf tibble of stations identifiers
#' @return sf tibble
fetch_ghcn = function(stations = ghcn_lut()){
  
  ghcnd::fetch_station(stations)
}


#' A wrapper around  \code{fetch_ghcn}
#'
#' @param ... arguments passed to \code{fetch_ghcn}
#' @return sf tibble
read_ghcn = function(...){
  fetch_ghcn(...)
}


#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  We count unique days in the interval, even though data may
#'  ne recorded much more frequently
#'
#' @param x tibble of buoy data
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364 or 365, but adjust to 28 or 29 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_ghcn = function(x = fetch_ghcn(), 
                                   by = c("month", "year")[1],
                                   min_count = c("year" = 12*28, "month" = 28)[[by]]){
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$DATE, fmt)) |>
    dplyr::group_by(STATION, interval_) |>
    dplyr::group_map(
      function(tbl, key){
        n_u = length(unique(tbl$DATE))
        if (n_u <= min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
  
}

#' Aggregate GHCN daily data by month or year
#'
#' @param x table of GHCN daily data for one or more sites (identified by STATION)
#' @param by string, the interval over which to aggregate - one of 'month' or 'year'
#' @param parameters char, the names of the columns to aggregate
#' @return tibble with aggregate stats 
aggregate_ghcn = function(x = fetch_ghcn(), 
                          by = c("month", "year")[1],
                          parameters = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")){
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  if (inherits(x, c("sf", "sfc"))) x <- sf::st_drop_geometry(x)
  x |>
    complete_intervals_ghcn(by=by) |>
    dplyr::mutate(interval_ = format(.data$DATE, fmt) |> as.Date(), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "DATE", "year", "week", "season"))) |>
    dplyr::group_by(STATION, interval_) |>
    dplyr::group_map(
      function(tbl, key, params = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")){
        v = lapply(params,
                   function(p){
                     v = sixnum(tbl |> dplyr::pull(dplyr::all_of(p))) |>
                       as.list() |>
                       dplyr::as_tibble()
                     names(v) <- paste(p, names(v), sep = ".")
                     v
                   }) |>
          dplyr::bind_cols()
        dplyr::select(tbl, dplyr::all_of(c("interval_", "STATION", "NAME"))) |>
          dplyr::slice(1) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::rename(date = "interval_")
}

export_ghcn = function(x, variable = "something", form = c("long", "wide")){
  
}
