# Accessing NERACOOS buoy data

#' Returns a table of buoy metadata
#' 
#' @return tibble
buoy_lut = function(filename = here::here("data", "buoy_listing.csv")){
  readr::read_csv(filename, show_col_types = FALSE)
}

#' Modify a timezone attribute on a POSIX object
#' 
#' @param x POSIX time (default current time)
#' @param input with timezone attribute modified
time_to_utc <- function(x = Sys.time()){
  attr(x, "tzone") <- "UTC"
  x
}

#
met_query_template = function(buoyid = "$BUOYID"){
  
  "http://www.neracoos.org/erddap/tabledap/B01_met_all.csv?station%2Ctime%2Cair_temperature%2Cbarometric_pressure%2Cwind_gust%2Cwind_speed%2Cwind_direction%2Cvisibility&time%3E=1980-01-01T00%3A00%3A00Z&time%3C=2023-04-13T13%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_met_all.csv", buoyid)
  query = c("station", "time", "air_temperature","barometric_pressure","wind_gust", 
            "wind_speed", "wind_direction", "visibility")
  constraints = c("time>=1980-01-01T00:00:00Z",
                    format(time_to_utc(x = Sys.time()), "time<=%Y-%m-%dT%H:%M:%SZ"))
  
  file.path(base_uri,
            name, 
            paste0("?", paste(query, collapse = ","), "&", paste(constraints, collapse = "&"))) |>
    xml2::url_escape()
}


fetch_buoy_met = function(buoy = "B01")
  
  