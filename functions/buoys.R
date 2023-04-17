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
met_query_uri = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_met_all.csv?station%2Ctime%2Cair_temperature%2Cbarometric_pressure%2Cwind_gust%2Cwind_speed%2Cwind_direction%2Cvisibility&time%3E=1980-01-01T00%3A00%3A00Z&time%3C=2023-04-13T13%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_met_all.csv", buoyid)
  query = c("station", "time", "air_temperature","barometric_pressure","wind_gust", 
            "wind_speed", "wind_direction", "visibility")
  constraints = c("time>=1980-01-01T00:00:00Z",
                    format(time_to_utc(x = Sys.time()), "time<=%Y-%m-%dT%H:%M:%SZ"))
  if (escape){
    uri =  paste0(file.path(base_uri,  name), 
                  "?", 
                  xml2::url_escape(paste(query, collapse = ",")), 
                  "&", 
                  paste(xml2::url_escape(constraints), collapse = "&"))
  } else {
    uri =  paste0(file.path(base_uri,  name), "?", 
                  paste(query, collapse = ","), "&", 
                  paste(constraints, collapse = "&"))
  }
  
  uri
}


#' Decompose a direction to u,v vectors
#'
#' @param x tibble of data
#' @param varname char the name of the varoable to decompose
#' @return tibble with add varname_u and varname_v columns added
decompose_direction = function(x, varname = 'wind_direction'){
  
  d = pi*x[[varname]]/180
  u = sin(d)
  v = cos(d)
  newnames = paste(varname, c("u", "v"), sep = "_")
  dplyr::mutate(x,
                !!newnames[1] := u,
                !!newnames[2] := v)
}



#' Aggregate columns by month
#' 
#' Add month date (first of each month)
#' Group by month
#' Sumamrise
#' 
#' @param x tibble of data
met_aggregate_monthly = function(x){
  dplyr::mutate(x, date = format(time, "%Y-%m-01")) |>
    dplyr::group_by(date) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)))
}

#' 
#' Read raw met data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
met_read_raw <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    mutate(across(where(is.numeric), ~na_if(., NaN))) |>
    decompose_direction() 
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data
fetch_buoy_met = function(buoy = "B01", 
                         path = here::here("data")){
  
  ofile = file.path(path, paste0(buoy, "_met_monthly.csv"))
  tmpfile = tempfile()
  uri = met_query_uri(buoy)
  ok <- download.file(uri, tmpfile)
  met_read_raw(tmpfile) |>
    met_aggregate_monthly() |>
    dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(ofile)
}


#' Read monthly met data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_met <- function(buoy = buoy_lut()$id, 
                          path = here::here("data")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_met_monthly.csv"))
           readr::read_csv(filename, show_col_types = FALSE)
         }) |>
    dplyr::bind_rows()
  
}

  
  