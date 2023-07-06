#' Annualize chlor values (from monthly)
#' 
#' @param x tibble of date, region and params
#' @return annualized tibble
annualize_chlor_cmems = function(x = read_chlor_cmems()){
  
  x |>
    complete_intervals_chlor_cmems() |>
    dplyr::mutate(year = as.numeric(format(date, "%Y")), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "month", "week", "season"))) |>
    dplyr::group_by(region, year) |>
    dplyr::group_map(
      function(tbl, key){
        dplyr::reframe(tbl,
                       min = mean(min),
                       q25 = mean(q25),
                       median = mean(median),
                       mean = mean(mean),
                       q75 = mean(q75),
                       max = mean(max),
                       .by = dplyr::all_of(c("year", "region")))
       
      }, .keep = TRUE) |>
    dplyr::bind_rows()
  
}


#' Aggregate chlor_cmems daily data by month or year
#'
#' @param x table of chlor_cmems monthkly values (raw - not logscaled)
#' @param by string, the interval over which to aggregate - one of month or year although
#'   chlor_cmems comes natively as monthly aggregations.  So this really only handles by 
#'   year.
#' @param logscale logical, if TRUE take the log base 10
#' @return tibble with aggregate stats 
aggregate_chlor_cmems = function(x = read_chlor_cmems(logscale = FALSE),
                           by = c("month", "year")[2],
                           logscale = FALSE){
  if (tolower(by[1]) == 'month'){
    message("cholr_cmems comes as monthly aggregation - returning input")
    return(x)
  }
  
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  x = x |>
    complete_intervals_chlor_cmems(by=by) |>
    dplyr::mutate(interval_ = format(.data$date, fmt) |> as.Date(), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "year", "month", "week", "season"))) |>
    dplyr::group_by(region, interval_) |>
    dplyr::group_map(
      function(tbl, key, parameters = c("min", "median", "mean", "max")){
        v = sapply(parameters,
                   function(p){
                     vals = tbl |> 
                       dplyr::pull(p) |> 
                       sixnum()
                     vals[[p]]
                   }, simplify = FALSE) |>
          dplyr::as_tibble()
        dplyr::select(tbl, dplyr::all_of(c("interval_", "region"))) |>
          dplyr::slice(1) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::rename(date = "interval_")
  if (logscale) x = dplyr::mutate_if(x, is.numeric, log10)
  x
}



#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  
#'
#' @param x tibble of chlor_cmems data
#' @param by string, the interval over which to aggregate - one of month or year although
#'   chlor_cmems comes natively as monthly aggregations.  So this really only handles by 
#'   year.
#' @param min_count numeric defaults to 12 for year and 28 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_chlor_cmems = function(x = read_chlor_cmems(),
                                          by = c("month", 'year')[[2]],
                                          min_count = c(month = 28, year = 12)[[by]]){
  
  if (tolower(by[1]) == 'month'){
    message("chlor_cmems comes as monthly aggregation - returning input")
    return(x)
  }
  
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")

  
  dplyr::mutate(x, interval_ = format(.data$date, "%Y-01-01")) |>
    dplyr::group_by(region, interval_) |>
    dplyr::group_map(
      function(tbl, key){
        if (nrow(tbl) < min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
}


#' Read chlorophyll data from CMEMS
#' 
#' @param filename char the name of the file
#' @param path char the path to the file
#' @param logscale logical, if TRUE take the log base 10
#' @return tibble of date, region and chlor
read_chlor_cmems = function(filename =  "chlor_cmems.csv.gz",
                            path = here::here("data", "chlor"),
                            logscale = FALSE){
  
  x = readr::read_csv(file.path(path[1], filename[1]),
                      col_types = 'Dcnnnnnn')
  if (logscale) x = dplyr::mutate_if(x, is.numeric, log10)
  x
}


#' Extract data from the online CMEMS chlor dataset
#'
#' Requires credentials.
#' 
#' @param x regions to extract
#' @param path the output path
#' @param progress logical, if TRUE then show a progress bar
#' @return tibble for date, region, mean chlorophyll
fetch_chlor_cmems <- function(x = read_regions(),
                              path = here::here("data", "chlor"),
                              progress = TRUE){
  

  # get the bounding area, pad it and rearrange orer  
  bb = sf::st_bbox(x) |>
    as.vector()
  bb = bb[c(1,3,2,4)] + c(-0.1, 0.1, -0.1, 0.1)
  
  xx <- dplyr::rowwise(x) |>
    dplyr::group_map(
      function(x, ...){
        sf::st_coordinates(x)[,1:2]
      }) |>
    rlang::set_names(x$region)
     
  X = CMEMS_CHLOR$new()
  nav = X$get_nav(bb=bb)
  dates = X$get_time(klass = 'Date')
  
  if (progress) pb = txtProgressBar(min = 0, max = length(dates), style = 3)
  
  
  r = lapply(seq_along(dates),
    function(i){
      if (progress) setTxtProgressBar(pb, i)
      
      # pull out the pixels for each region - matrix ops are faster than st_extract()
      # call the summarizing function
      # transpose and cast as tibble
      # add in date/region info
      s = X$get_var(time = i, nav = nav)
      m <- sapply(xx, 
                  function(x){
                    v = stars::st_extract(s, x, na.rm = TRUE)
                    sixnum(v[[1]])
                  }) |>
        t() |>
        dplyr::as_tibble(rownames = "region") |>
        dplyr::mutate(date = dates[i], .before = 1)
      
      #s = stars::st_extract(X$get_var(time = i, nav = nav), x, na.rm = TRUE) |>
      #  sf::st_as_sf() |>
      #  sf::st_drop_geometry() |> 
      #  rlang::set_names("chlor") |> 
      #  dplyr::mutate(date = dates[i], region = x$region, .before = 1)
    }) |>
    dplyr::bind_rows() |>
    readr::write_csv(file.path(path, "chlor_cmems.csv.gz"))
  if (progress) close(pb)
  X$close_nc()
  r
} # fetch_cmems_chlor
  


##### R6 class below ###########################################################
# Used for the purpose of harvesting chlor data from CMEMS/Copernicus as a stand alone
# Provides NCDF navigation and extraction tools
################################################################################


#' R6 class for accessing CMEMS/Copernicus multiyear monthly CHLOR
CMEMS_CHLOR = R6::R6Class("CMEMS_CHLOR",
  public = list(
    product_id = NULL,
    base_uri= NULL,
    credentials_file = NULL,
    NC = NULL,

    initialize = function(product_id = "c3s_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M",
                          base_uri = "https://my.cmems-du.eu/thredds/dodsC",
                          credentials_file = "~/.copernicuscredentials"){
      cat("initializing: ", product_id, "\n")
      self$product_id = product_id[1]
      self$base_uri = base_uri[1]
      self$credentials_file = credentials_file[1]
      self$open_nc()
    },
    
    finalize = function(){
      self$close_nc()
    },
    
    get_credentials = function(){
      if (is.null(self$credentials_file) || !file.exists(self$credentials_file)) {
        stop("credentials file not found")
      }
      readLines(self$credentials_file)
    },
    
    close_nc = function(){
      if (inherits(self$NC, "ncdf4")) try(ncdf4::nc_close(self$NC))
      invisible(self)
    },
    
    open_nc = function(){
      creds = paste0(self$get_credentials(), "@")
      uri = file.path(gsub("https://", paste0("https://", creds), 
                           self$base_uri, fixed = TRUE), self$product_id)
      self$NC = try(ncdf4::nc_open(uri))
      if (inherits(self$NC, "try-error")) stop("error opening NCDF")
      invisible(self)
    },
    
    get_res = function(){
      lon = ncdf4::ncatt_get(self$NC, "longitude")
      if (is.null(lon$step)){
        lon$step = abs(mean(diff(self$NC$dim$longitude$vals)))
      }
      lat = ncdf4::ncatt_get(self$NC, "latitude")
      if (is.null(lat$step)){
        lat$step = abs(mean(diff(self$NC$dim$latitude$vals)))
      }
      c(lon$step, lat$step)
    }, # get_res
    
    get_lon = function(){
      self$NC$dim$longitude$vals
    },
    
    get_lat = function(){
      self$NC$dim$latitude$vals
    },
    
    get_time = function(klass = c("Date", "POSIXct")[2]){
      # pretty weak ...
      # @param x ncdf4 object
      # @return format string
      guess_format = function(x){
        if (grepl("1970-01-01 00:00:00", x$dim$time$units, fixed = TRUE)){
          f = sub("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", x$dim$time$units, fixed = TRUE)
        } else if(grepl("1970-01-01", x$dim$time$units, fixed = TRUE)){
          f = sub("1970-01-01", "%Y-%m-%d", x$dim$time$units, fixed = TRUE)
        }
        f
      }
      
      # scaling for time
      # @param x ncdf4 object
      # @return numeric multiplier
      guess_scale = function(x){
        u = strsplit(x$dim$time$units, " ", fixed = TRUE)[[1]][1]
        switch(tolower(u),
               'seconds' = 1,
               'minutes' = 60,
               'hours' = 3600)
      }
      
      
      origin = as.POSIXct(self$NC$dim$time$units,
                           format = guess_format(self$NC),
                           tz = 'UTC')
      time = origin + (guess_scale(self$NC) * self$NC$dim$time$vals)
      if (tolower(klass[1]) == "date") time = as.Date(time)
      return(time)
    }, # get_time
    

    get_nav = function(bb = c(-180, 180,-90, 90), varid = "CHL"){
      
      stopifnot(varid %in% names(self$NC$var))
      res = self$get_res()
      r2 = res/2
      lon = self$get_lon()
      lat = self$get_lat()
      
      closest_index = function(x, vec){
        which.min(abs(vec-x))
      } 
      
      ix = unname(sapply(bb[1:2] + c(-r2[1], r2[1]), closest_index, lon))
      nx = ix[2] - ix[1] + 1
      xmin = lon[ix[1]] - r2[1]
      xmax = lon[ix[2]] + r2[1]
      
      iy = unname(sapply(bb[3:4] + c(-r2[2], r2[2]), closest_index, lat))
      if (iy[1] >= iy[2]) {
        ny = iy[1] - iy[2] + 1
        ymin = lat[iy[1]] - r2[2]
        ymax = lat[iy[2]] + r2[1]
        iy = rev(iy)
      } else {
        ny = iy[2] - iy[1] + 1
        ymin = lat[iy[1]] - r2[2]
        ymax = lat[iy[2]] + r2[1]
      }

      
      list(
        bb = bb,
        varid = varid,
        bbox = sf::st_bbox(c(xmin = xmin, 
                             ymin = ymin,
                             xmax = xmax,
                             ymax = ymax),
                           crs = 4326),
        start = c(ix[1], iy[1],1),
        count = c(nx, ny, 1) )
    }, # get_nav
    
    get_var = function(time = 1, 
                       bbox = c(-180, 180, -90, 90), 
                       varid = varid,
                       nav = NULL,
                       form = c("stars", "array")[1]){
      
      if (inherits(time, "POSIXt")) time = as.Date(time)
      if (inherits(date, "Date")) time = findInterval(time, self$get_time(klass = "Date"))
      if (time <= 0) stop("time must be at or later than:", 
                          format(self$get_time()[1], "%y-%m-%d"))
      if (is.null(nav)) nav = self$get_nav(bb = bbox, varid = varid)
      
      nav$start = c(nav$start[1:2], time)
      m <- ncdf4::ncvar_get(self$NC, nav$varid,
                            start = nav$start,
                            count = nav$count)
      if (tolower(form[1]) == 'array') return(m)
  
      stars::st_as_stars(nav$bbox,
                         values = m,
                         nx = nav$count[1],
                         ny = nav$count[2])
    }
    
    
  ) # public
  
  
  )# CMEMS_CHLOR


