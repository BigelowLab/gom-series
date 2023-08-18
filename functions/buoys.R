# Accessing NERACOOS buoy data

#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  We count unique days in the interval, even though data may
#'  ne recorded much more frequently
#'
#' @param x tibble of buoy data
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364 or 365, but adjust to 28 or 29 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_buoys = function(x, 
  by = c("year", "month")[1], 
  min_count = c("year" = 364, "month" = 28)[[by]]){
  
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$time, fmt)) |>
    dplyr::group_by(station, interval_) |>
    dplyr::group_map(
      function(tbl, key){
        n_u = length(unique(as.Date(tbl$time)))
        if (n_u <= min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
}


#' Returns a table of buoy metadata
#' 
#' @param filename the name of the file to read
#' @param form char, one of 'table' or 'sf'
#' @return tibble
buoy_lut = function(filename = here::here("data","buoy", "buoy_listing.csv"),
                    form = c("table", "sf")[1]){
  x = readr::read_csv(filename, show_col_types = FALSE)
  if (tolower(form[1]) == 'sf'){
    x = sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  x
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
  
  r = httr::HEAD(x)
  if (httr::http_error(r)){
    warning("http error for ", x)
    return(1)
  }
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

#' Aggregate columns by interval
#' 
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @param where_fun function, the function used to select which columns are computed
#' @return summary tibble 
aggregate_buoy = function(x, by = c("month", "year")[1]) {
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  # first two columns should be station and time
  cnames = colnames(x)
  has_depth = "depth" %in% cnames
  ix <- sapply(x, is.numeric) & !(cnames %in% c("depth", "water_depth"))
  groupies = c("station", "date", "depth", "water_depth")
  
  PARAMS <- colnames(x)[ix]
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(groupies))) |>
    dplyr::group_map(
      function(tbl, key, params = NULL){
        v = lapply(params,
                   function(p){
                     v = sixnum(tbl |> dplyr::pull(dplyr::all_of(p))) |>
                       as.list() |>
                       dplyr::as_tibble()
                     names(v) <- paste(p, names(v), sep = ".")
                     v
                   }) #|>
        dplyr::bind_cols()
        # now add date and station
        tbl |> 
          dplyr::slice(1) |>
          dplyr::select(dplyr::any_of(groupies)) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE, params = PARAMS) |>
    dplyr::bind_rows()
}



######## MET

#' Aggregate columns by interval
#' 
#' @param x tibble of data
#' @param by char the interval overwich to aggregate
#' @return summary tibble
met_aggregate = function(x, by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  # first two columns should be station and time
  ix <- sapply(x, is.numeric)
  PARAMS <- colnames(x)[ix]
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(station, date) |>
    dplyr::group_map(
      function(tbl, key, params = NULL){
        v = lapply(params,
                   function(p){
                     v = sixnum(tbl |> dplyr::pull(dplyr::all_of(p))) |>
                       as.list() |>
                       dplyr::as_tibble()
                     names(v) <- paste(p, names(v), sep = ".")
                     v
                   }) #|>
          dplyr::bind_cols()
        # now add date and station
        tbl |> 
          dplyr::slice(1) |>
          dplyr::select(station, date) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE, params = PARAMS) |>
    dplyr::bind_rows()
    
}

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
  
  
  tmpfile = tempfile()
  uri = met_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  x = met_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix()
  
  yfile = file.path(path, paste0(buoy, "_met_yearly.csv.gz"))
  y = complete_intervals_buoys(x, by = "year") |>
    aggregate_buoy(by = "year") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(yfile)
  
  mfile = file.path(path, paste0(buoy, "_met_monthly.csv.gz"))
  complete_intervals_buoys(x, by = "month") |>
    aggregate_buoy(by = "month") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(mfile)
}

#' Read met data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_met <- function(buoy = buoy_lut()$id, 
                          interval = c("month", "year")[1],
                          path = here::here("data","buoy")){
  
  suffix = switch(tolower(interval[1]),
                  "month" = "_met_monthly.csv.gz",
                  "year" = "_met_yearly.csv.gz")
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, suffix))
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

#' Aggregate columns by interval
#' 
#' @param x tibble of data
#' @param by char the interval overwich to aggregate
#' @return summary tibble
ctd_aggregate = function(x, by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  where_compute = function(x){
    is.numeric(x) 
  }
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(date) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
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

#' Fetch met data for a given buoy - saves monthly and yearly aggregates
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data (monthly aggregate)
fetch_buoy_ctd = function(buoy = "B01", 
                          path = here::here("data","buoy")){
  
  mfile = file.path(path, paste0(buoy, "_ctd_monthly.csv.gz"))
  yfile = file.path(path, paste0(buoy, "_ctd_yearly.csv.gz"))
  
  tmpfile = tempfile()
  uri = ctd_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  r = ctd_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix()
  
  y = complete_intervals_buoys(r, by = "year")  |>
    aggregate_buoy(by = "year") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(yfile)
  
  complete_intervals_buoys(r, by = "month")|>
    aggregate_buoy(by = "month") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(mfile)
}
  

#' Read monthly ctd data
#' 
#' @param buoy char, one or more buoy id codes
#' @param interval char the interval aggregate to read
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_ctd <- function(buoy = buoy_lut()$id, 
                          interval = c("month", "year")[1],
                          path = here::here("data","buoy")){
  
  suffix = switch(tolower(interval[1]),
                  "month" = "_ctd_monthly.csv.gz",
                  "year" = "_ctd_yearly.csv.gz")
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, suffix))
           if (file.exists(filename)){
             r = readr::read_csv(filename, show_col_types = FALSE)
           } else {
             r = NULL
           }
           r
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

#' Aggregate optics by the user specifed interval
#' @param x tibble of data
#' @param by cahr the interval over which to aggregate
#' @return aggregated pixel
aggregate_optics = function(x, by = c("month", "year")[1]){
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(date, water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
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

#' Fetch met data for a given buoy (aggregates by year and month)
#' 
#' @param buoy char, the buoy id
#' @param ofile char, the path to save data to
#' @return tibble of data (monthly)
fetch_buoy_optics= function(buoy = "B01", 
                            path = here::here("data","buoy")){

  mfile = file.path(path, paste0(buoy, "_optics_monthly.csv.gz"))
  yfile = file.path(path, paste0(buoy, "_optics_yearly.csv.gz"))
  tmpfile = tempfile()
  
  uri = query_uri_optics(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  r = read_raw_optics(tmpfile) |>
    mask_qc() |>
    drop_suffix()
  
  y = complete_intervals_buoys(r, by = "year") |>
    aggregate_buoy(by = "year") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(yfile)

  complete_intervals_buoys(r, by = "month") |>
    aggregate_buoy(by = "month") |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(mfile)
}


#' Read monthly optics data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_optics <- function(buoy = buoy_lut()$id, 
                             interval = c("month", "year")[1],
                          path = here::here("data","buoy")){
  
  suffix = switch(tolower(interval[1]),
                  "month" = "_optics_monthly.csv.gz",
                  "year" = "_optics_yearly.csv.gz")
  
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, suffix))
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

#' Aggregate columns by by the interval specified by user
#'  
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @return aggregated tibble
aggregate_rtsc = function(x, by = c('year', 'month')[2]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(date, depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
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
fetch_buoy_rtsc = function(buoy = "B01", 
                            path = here::here("data","buoy")){
  
  mfile = file.path(path, paste0(buoy, "_rtsc_monthly.csv.gz"))
  yfile = file.path(path, paste0(buoy, "_rtsc_yearly.csv.gz"))
  
  tmpfile = tempfile()
  uri = query_uri_rtsc(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  x = read_raw_rtsc(tmpfile) |>
    mask_qc() |>
    drop_suffix() 

  y = complete_intervals_buoys(x, by = "year") |>  
    aggregate_buoy(by = 'year') |>
    #aggregate_rtsc(by = 'year') |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(yfile)

  complete_intervals_buoys(x, by = "month") |>  
    aggregate_buoy(by = 'month') |>
    #aggregate_rtsc(by = 'month') |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(mfile)
}


#' Read monthly rtsc data
#' 
#' @param buoy char, one or more buoy id codes
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_rtsc <- function(buoy = buoy_lut()$id, 
                           interval = c("month", "year")[1],
                           path = here::here("data","buoy")){
  
  suffix = switch(tolower(interval[1]),
                  "month" = "_rtsc_monthly.csv.gz",
                  "year" = "_rtsc_yearly.csv.gz")
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, suffix))
           x <- if (file.exists(filename)){
             readr::read_csv(filename, show_col_types = FALSE)
           } else {
             NULL
           }
         }) |>
    dplyr::bind_rows()
  
}

#########
# adcp
#########

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

#' Aggregate columns by by the interval specified by user
#'  
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @return aggregated tibble
aggregate_adcp = function(x, by = c("month", "year")[1]){

  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")  
  
  dplyr::mutate(x, date = format(time, fmt)) |>
    dplyr::group_by(date, water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
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
  
  mfile = file.path(path, paste0(buoy, "_adcp_monthly.csv.gz"))
  yfile = file.path(path, paste0(buoy, "_adcp_yearly.csv.gz"))
  tmpfile = tempfile()
  
  uri = query_uri_adcp(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok != 0) return(NULL)
  
  x = read_raw_adcp(tmpfile) |>
    mask_qc() |>
    drop_suffix()
  
  y = complete_intervals_buoys(x, by = "year") |>  
    aggregate_buoy(by = 'year') |>
    #aggregate_adcp(by = 'year') |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(yfile)
  
  complete_intervals_buoys(x, by = "month") |>  
    aggregate_buoy(by = 'month') |>
    #aggregate_adcp(by = 'month') |>
    #dplyr::mutate(buoy = buoy, .before = 1) |>
    readr::write_csv(mfile)

}


#' Read monthly adcp data
#' 
#' @param buoy char, one or more buoy id codes
#' @param interval char, one of "month" (default) or "year"
#' @param path char, the path to the data
#' @return tibble of monthly met means  If more than one buoy is requested they
#'   are bound into one tibble
read_buoy_adcp <- function(buoy = buoy_lut()$id, 
                           interval = c("month", "year")[1],
                           path = here::here("data","buoy")){
  
  suffix = switch(tolower(interval[1]),
                  "month" =  "_adcp_monthly.csv.gz",
                  "year" =  "_adcp_yearly.csv.gz")
  lapply(buoy, 
         function(id){
           filename = file.path(path, paste0(id, suffix))
           x <- if (file.exists(filename)){
             readr::read_csv(filename, show_col_types = FALSE)
           } else {
             NULL
           }
         }) |>
    dplyr::bind_rows()
  
}



### Fetching all

#' Fetch buoy data for one or more datasets
#' 
#' @param buoy char, one or more buoy id (B01, etc)
#' @param what char, one or more datasets names
#' @return NULL
fetch_buoys <- function(buoy = buoy_lut()$id,
                         what = c("met", "ctd", "rtsc", "optics", "adcp")){
  
  if ("all" %in% what) what = c("met", "ctd", "rtsc", "optics", "adcp")
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


#' Export the annual (or monthly) data in a wide format
#' @return wide tibble of aggregated data
export_buoy = function(by = c("month", "year")[2]){
  
  src = c("met", "ctd", "optics", "rtsc", "adcp")
  xx = sapply(src, 
              function(s){
                glue = sprintf('BUOY_{station}.%s.{.value}', s)
                r = switch (s,
                  "met" = read_buoy_met(interval = by),
                  "ctd" = read_buoy_ctd(interval = by),
                  "optics" = read_buoy_optics(interval = by),
                  "rtsc" = read_buoy_rtsc(interval = by),
                  "adcp" = read_buoy_adcp(interval = by)
                ) |>
                  tidyr::pivot_wider(names_from = "station", 
                                     id_cols = "date",
                                     names_glue = glue,
                                     values_fn = mean,
                                     values_from = where(is.numeric))
              },
              simplify = FALSE
  )
  
  # reorder based upon number of rows (most rows first)
  n = sapply(xx, nrow)
  ix = order(n, decreasing = TRUE)
  xx = xx[ix]
  purrr::reduce(xx, dplyr::left_join, by = "date") |>
    dplyr::arrange(date)
  
}
