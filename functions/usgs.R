
#' Returns a listing of USGS stream gauges that empty into the Gulf of Maine
#' 
#' @return tibble
usgs_lut <- function(filename = here::here("data","usgs", "gom_stations.csv")) {
  readr::read_csv(filename, show_col_types = FALSE)
}


#' Reads USGS daily values using 
fetch_usgs_discharge <- function(stations) {
  
  discharge <- dataRetrieval::readNWISdv(stations$site_no, parameter = "00060") |>
    dplyr::as_tibble() |>
    dplyr::select(-"X_00060_00003_cd") |>
    dplyr::rename(date = Date,
                  discharge = X_00060_00003)
  
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
