# Accessing NERACOOS buoy data

#' Returns a table of buoy metadata
#' 
#' @return tibble
buoy_lut = function(filename = here::here("data","buoy", "buoy_listing.csv")){
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

#' Given a raw data table, for each 'name_qc' mask with a relacement 
#' wherever qc fails threshold
#'  
#' @param x tibble of raw data
#' @param threshold numeric records at or below this value are retained
#' @param suffix character the pattern to identify columns to be dropped
#' @param replacement numeric or NA, the value to substitute in where qc is bad
#' @return the input table masked
mask_qc <- function(x, threshold = 0, suffix = "_qc", replacement = NA_real_){
  nm = colnames(x)
  ix = grepl("_qc", nm, fixed = TRUE)
  if (!any(ix)) return(x)
  nameqc = nm[ix]
  name = gsub(suffix[1], "", nameqc, fixed = TRUE) 
  if (length(nameqc) > 0){
    for (i in seq_along(nameqc)){
      bad = x[[ nameqc[i] ]] > threshold
      x[[ name[i] ]][bad] <- replacement
    }
  }
  x
}

#' Fetch ERRDAP CSV file
#' 
#' @param x char a url for download
#' @param filename char, the destination for the file
#' @param ... other argument for \code{download.file}
#' @return 0 for success, non-zero for problems
fetch_errdap_csv <- function(x, filename = tempfile(), ...){
  ok <- try(download.file(x, filename, ...))
  if (inherits(ok, 'try-error')){
    print(ok)
    ok = 1
  }
  ok
}

#' Given a raw data table, drop 'name_qc' columns
#' 
#' @param x tibble of raw data
#' @param suffix character the pattern to identify columns to be dropped
#' @return the input table with selected columns dropped
drop_suffix <- function(x,  suffix = "_qc"){
  dplyr::select(x,-dplyr::any_of(dplyr::ends_with(suffix[1])))
}


### general purpose stuff above

### MET

#
met_query_uri = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_met_all.csv?station%2Ctime%2Cair_temperature%2Cbarometric_pressure%2Cwind_gust%2Cwind_speed%2Cwind_direction%2Cvisibility&time%3E=1980-01-01T00%3A00%3A00Z&time%3C=2023-04-13T13%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_met_all.csv", buoyid)
  query = c("station", "time", 
            "air_temperature",
            "air_temperature_qc",
            "barometric_pressure",
            "barometric_pressure_qc",
            "wind_gust", 
            "wind_gust_qc",
            "wind_speed", 
            "wind_speed_qc",
            "wind_direction", 
            "wind_direction_qc", 
            "visibility",
            "visibility_qc")
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
                         path = here::here("data","buoy")){
  
  ofile = file.path(path, paste0(buoy, "_met_monthly.csv.gz"))
  tmpfile = tempfile()
  uri = met_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  met_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix() |>
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
                          path = here::here("data","buoy")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_met_monthly.csv.gz"))
           readr::read_csv(filename, show_col_types = FALSE)
         }) |>
    dplyr::bind_rows()
  
}



#### CTD


#
ctd_query_uri = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_sbe37_all.csv?station%2Ctime%2Ctemperature%2Csalinity%2Csigma_t%2Cdepth&time%3E=2023-04-10T00%3A00%3A00Z&time%3C=2023-04-17T14%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_sbe37_all.csv", buoyid)
  query = c("station", "time", 
            "temperature",
            "temperature_qc",
            "salinity",
            "salinity_qc",
            "sigma_t", 
            "sigma_t_qc",
            "depth")
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

#' Aggregate columns by month
#' 
#' Add month date (first of each month)
#' Group by month and depth
#' Sumamrize with mean
#' 
#' @param x tibble of data
ctd_aggregate_monthly = function(x){
  dplyr::mutate(x, date = format(time, "%Y-%m-01")) |>
    dplyr::group_by(date, depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)))
}


#' Read raw ctd data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
ctd_read_raw <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    mutate(across(where(is.numeric), ~na_if(., NaN)))
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data
fetch_buoy_ctd = function(buoy = "B01", 
                          path = here::here("data","buoy")){
  
  ofile = file.path(path, paste0(buoy, "_ctd_monthly.csv.gz"))
  tmpfile = tempfile()
  uri = ctd_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  ctd_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix() |>
    ctd_aggregate_monthly() |>
    dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(ofile)
}
  

#' Read monthly ctd data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_ctd <- function(buoy = buoy_lut()$id, 
                          path = here::here("data","buoy")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_ctd_monthly.csv.gz"))
           readr::read_csv(filename, show_col_types = FALSE)
         }) |>
    dplyr::bind_rows()
  
}

### Optics

# http://www.neracoos.org/erddap/tabledap/B01_optics_hist.csv?station%2Ctime%2Cwater_depth%2Csolar_zenith_angle%2CEd_PAR%2Cchlorophyll%2Clongitude%2Clatitude%2Cdepth&time%3E=2007-09-16T00%3A00%3A00Z&time%3C=2007-09-23T19%3A00%3A00Z
# http://www.neracoos.org/erddap/tabledap/B01_optics_all.csv?station%2Ctime%2Cwater_depth%2Csolar_zenith_angle%2CEd_PAR%2Cchlorophyll&time%3E%3D1980-01-01T00%3A00%3A00Z&time%3C%3D2023-04-17T17%3A52%3A48Z

#
query_uri_optics = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_optics_all.csv?station%2Ctime%2Cwater_depth%2Csolar_zenith_angle%2CEd_PAR%2Cchlorophyll&time%3E=2023-04-10T00%3A00%3A00Z&time%3C=2023-04-17T14%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_optics_hist.csv", buoyid)
  query = c("station", "time", "water_depth",
            "solar_zenith_angle",
            "solar_zenith_angle_qc",
            "Ed_PAR", 
            "Ed_PAR_qc", 
            "chlorophyll",
            "chlorophyll_qc")
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

#' Aggregate columns by month
#' 
#' Add month date (first of each month)
#' Group by month and depth
#' Sumamrize with mean
#' 
#' @param x tibble of data
aggregate_monthly_optics = function(x){
  dplyr::mutate(x, date = format(time, "%Y-%m-01")) |>
    dplyr::group_by(date, water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)))
}


#' Read raw optics data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_optics <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    mutate(across(where(is.numeric), ~na_if(., NaN)))
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data
fetch_buoy_optics= function(buoy = "B01", 
                          path = here::here("data","buoy")){
  
  ofile = file.path(path, paste0(buoy, "_optics_monthly.csv.gz"))
  tmpfile = tempfile()
  uri = query_uri_optics(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  r = read_raw_optics(tmpfile) |>
    mask_qc() |>
    drop_suffix() |>
    aggregate_monthly_optics() |>
    dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(ofile)

  r
}


#' Read monthly optics data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_optics <- function(buoy = buoy_lut()$id, 
                          path = here::here("data","buoy")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_optics_monthly.csv.gz"))
           x <- if (file.exists(filename)){
            readr::read_csv(filename, show_col_types = FALSE)
           } else {
             NULL
           }
         }) |>
    dplyr::bind_rows()
  
}


# RTSC
# 

#
query_uri_rtsc = function(buoyid = "$BUOYID", escape = TRUE){
  
  # http://www.neracoos.org/erddap/tabledap/B01_aanderaa_all.csv?station%2Ctime%2Ccurrent_speed%2Ccurrent_direction%2Ctemperature%2Cdepth&time%3E=2023-04-11T00%3A00%3A00Z&time%3C=2023-04-18T11%3A00%3A00Z
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_aanderaa_all.csv", buoyid)
  query = c("station", "time", 
            "current_speed",
            "current_speed_qc",
            "current_direction",
            "current_direction_qc",
            "temperature", 
            "temperature_qc",
            "depth")
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

#' Aggregate columns by month
#' 
#' Add month date (first of each month)
#' Group by month and depth
#' Sumamrize with mean
#' 
#' @param x tibble of data
aggregate_monthly_rtsc = function(x){
  dplyr::mutate(x, date = format(time, "%Y-%m-01")) |>
    dplyr::group_by(date, depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)))
}


#' Read raw rtsc data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_rtsc <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    mutate(across(where(is.numeric), ~na_if(., NaN))) |>
    decompose_direction("current_direction") 
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data
fetch_buoy_rtsc= function(buoy = "B01", 
                            path = here::here("data","buoy")){
  
  ofile = file.path(path, paste0(buoy, "_rtsc_monthly.csv.gz"))
  tmpfile = tempfile()
  uri = query_uri_rtsc(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  r = read_raw_rtsc(tmpfile) |>
    mask_qc() |>
    drop_suffix() |>
    aggregate_monthly_rtsc() |>
    dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(ofile)

  r
}


#' Read monthly rtsc data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_rtsc <- function(buoy = buoy_lut()$id, 
                             path = here::here("data","buoy")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_rtsc_monthly.csv.gz"))
           x <- if (file.exists(filename)){
             readr::read_csv(filename, show_col_types = FALSE)
           } else {
             NULL
           }
         }) |>
    dplyr::bind_rows()
  
}

# adcp
# 

#
query_uri_adcp = function(buoyid = "$BUOYID", escape = TRUE){
  
  # http://www.neracoos.org/erddap/tabledap/B01_doppler_rt.csv?station%2Cwater_depth%2Ctime%2Cdepth%2Coffset_time%2Ccurrent_u%2Ccurrent_u_qc%2Ccurrent_v%2Ccurrent_v_qc&time%3E=2023-04-11T00%3A00%3A00Z&time%3C=2023-04-18T11%3A00%3A00Z
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_doppler_rt.csv", buoyid)
  query = c("station", "water_depth", "time", "depth",
            "current_u",
            "current_u_qc",
            "current_v",
            "current_v_qc")
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

#' Aggregate columns by month
#' 
#' Add month date (first of each month)
#' Group by month and depth
#' Sumamrize with mean
#' 
#' @param x tibble of data
aggregate_monthly_adcp = function(x){
  dplyr::mutate(x, date = format(time, "%Y-%m-01")) |>
    dplyr::group_by(date, water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)))
}




#' Read raw adcp data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_adcp <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    mutate(across(where(is.numeric), ~na_if(., NaN)))
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data
fetch_buoy_adcp= function(buoy = "B01", 
                          path = here::here("data","buoy")){
  
  ofile = file.path(path, paste0(buoy, "_adcp_monthly.csv.gz"))
  tmpfile = tempfile()
  uri = query_uri_adcp(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  r = read_raw_adcp(tmpfile) |>
    mask_qc() |>
    drop_suffix() |>
    aggregate_monthly_adcp() |>
    dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(ofile)

  r
}


#' Read monthly adcp data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_adcp <- function(buoy = buoy_lut()$id, 
                           path = here::here("data","buoy")){
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, "_adcp_monthly.csv.gz"))
           x <- if (file.exists(filename)){
             readr::read_csv(filename, show_col_types = FALSE)
           } else {
             NULL
           }
         }) |>
    dplyr::bind_rows()
  
}


#' Fetch buoy data for one or more datasets
#' 
#' @param buoy char, one or more buoy id (B01, etc)
#' @param what char, one or more datasets names
#' @return NULL
fetch_buoys <- function(buoy = buoy_lut()$id,
                         what = c("met", "ctd", "rtsc", "optics", "adcp")){
  cat("updating buoys:", paste(buoy, collapse = ", "), "\n") 
  for (wh in what){
    cat("updating", wh, "\n")
    switch(wh,
           "met" = lapply(buoy, fetch_buoy_met),
           'ctd' = lapply(buoy, fetch_buoy_ctd),
           "rtsc" = lapply(buoy, fetch_buoy_rtsc),
           "optics" = lapply(buoy, fetch_buoy_optics),
           "adcp" = lapply(buoy, fetch_buoy_adcp),
           stop("dataset not known:", wh) )
  }
  invisible(NULL)
}