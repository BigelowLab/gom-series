# https://coastwatch.noaa.gov/cwn/products/noaa-ncei-blended-seawinds-nbs-v2.html

#' Aggregate BSWm daily data by month or year
#'
#' @param x table of USGS daily data for one or more sites (identified by site_no)
#' @param by string, the interbval over which to aggregate - one of month or year although
#'   BSWm comes natively as monthly aggregations.  So this really only handles by 
#'   year.
#' @return tibble with aggregate stats 
aggregate_bswm = function(x = read_bswm(),
                           by = c("month", "year")[2]){
  if (tolower(by[1]) == 'month'){
    # message("BSWm comes as monthly aggregation - returning input")
    return(x)
  }
  
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  x |>
    complete_intervals_bswm(by = by) |>
    dplyr::mutate(interval_ = format(.data$date, fmt) |> as.Date(), .before = 1) |>
    dplyr::select(-dplyr::any_of(c("date", "year", "month", "week", "season"))) |>
    dplyr::group_by(region, var, interval_) |>
    dplyr::group_map(
      function(tbl, key, parameters = c("min", "q25", "median", "mean", "q75", "max")){
        v = sapply(parameters,
                   function(p){
                     vals = tbl |> 
                       dplyr::pull(p) |> 
                       sixnum()
                     vals[[p]]
                   }, simplify = FALSE) |>
          dplyr::as_tibble()
        dplyr::select(tbl, dplyr::all_of(c("interval_", "var", "region"))) |>
          dplyr::slice(1) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::rename(date = "interval_")
}


#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  
#'
#' @param x tibble of BSWm data
#' @param by string, the interval over which to aggregate - one of month or year although
#'   BSWm comes natively as monthly aggregations.  So this really only handles by 
#'   year.
#' @param min_count numeric defaults to 12 for year and 28 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_bswm = function(x = read_bswm(), 
                                    by = c("month", "year")[2],
                                    min_count = c(month = 28, year = 12)[[by]]){
  
  if (tolower(by[1]) == 'month'){
    message("oisst comes as monthly aggregation - returning input")
    return(x)
  }
  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  
  dplyr::mutate(x, interval_ = format(.data$date, fmt)) |>
    dplyr::group_by(region, var, interval_) |>
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


#' Read BSWm data for each region
#' 
#' @param filename char the name of the file
#' @param path char the path to the file
#' @return tibble of date, region and sst params
read_bswm = function(filename =  "bswm.csv.gz",
                      path = here::here("data", "bsw")){
  
  readr::read_csv(file.path(path[1], filename[1]), col_types = 'Dccnnnnnn')
}


#' Extract data from the online OISST dataset
#'
#' 
#' @param x regions to extract
#' @param path the output path
#' @param progress logical, if TRUE then show a progress bar
#' @return tibble for date, region, mean sst
fetch_BSWm<- function(x = read_regions(),
                      path = here::here("data", "bsw"),
                      varnames= c("u_wind", "v_wind", "windspeed"),
                      progress = FALSE){
  if (FALSE){
    x = read_regions()
    path = here::here("data", "bsw")
    varnames= c("u_wind", "v_wind", "windspeed")
    progress = FALSE
  }
  # get the bounding area, pad it and rearrange order  
  bb = sf::st_bbox(x) |>
    as.vector()
  bb = bb[c(1,3,2,4)] + c(-0.1, 0.1, -0.1, 0.1)
  
  xy <- dplyr::rowwise(x) |>
    dplyr::group_map(
      function(x, ...){
        sf::st_coordinates(x)[,1:2]
      }) |>
    rlang::set_names(x$region)
  
  X = BSWm$new()
  nav = X$get_nav(bb=bb)
  dates = X$get_time()
  if (progress) pb = txtProgressBar(min = 0, max = length(dates), style = 3)
  
  
  r = lapply(seq_along(dates),
             function(i){
               if (progress) setTxtProgressBar(pb, i)
               #pull out the stars object for this date
               s = lapply(varnames,
                 function(varname){
                    s = X$get_var(time = i, varid = varname, nav = nav)
                    sapply(xy, 
                                function(xp){
                                  v = stars::st_extract(s, xp)
                                  sixnum(v[[1]], na.rm = TRUE)
                                }) |>
                      t() |>
                      dplyr::as_tibble(rownames = "region") |>
                      dplyr::mutate(date = dates[i], var = varname, .before = 1)
                 }) |>   # varnames-loop
                 dplyr::bind_rows() 
             }) |>   # i-loop
    dplyr::bind_rows() |>
    readr::write_csv(file.path(path, "bswm.csv.gz"))
  if (progress) close(pb)
  X$close_nc()
  r
} # fetch_oisst

##### R6 class below ###########################################################
# Used to access BSW monthky aggregations (either 'uvcomp' or 'stress')
################################################################################


# R6 class for accessing BSW monthly aggregations
BSWm = R6::R6Class("BSWm",
                    public = list(
                      product = c('uvcomp','stress')[1],
                      base_uri= "https://www.star.nesdis.noaa.gov/thredds/dodsC/CoastWatch/NCEI/Blended",
                      NC = NULL,
                      
                      initialize = function(
                        product = c('uvcomp','stress')[1],
                        base_uri = "https://www.star.nesdis.noaa.gov/thredds/dodsC/CoastWatch/NCEI/Blended"){
                        self$product = product[1]
                        self$base_uri = base_uri[1]
                        self$open_nc()
                      },
                      
                      finalize = function(){
                        self$close_nc()
                      },
                      
                      build_uri = function(){
                        file.path(self$base_uri, self$product,  "SCIMonthlyGlobal/WW00/LoM")
                      },
                      
                      close_nc = function(){
                        if (inherits(self$NC, "ncdf4")) try(ncdf4::nc_close(self$NC))
                        invisible(self)
                      },
                      
                      open_nc = function(){
                        uri = self$build_uri()
                        self$NC = try(ncdf4::nc_open(uri))
                        if (inherits(self$NC, "try-error")) stop("error opening NCDF")
                        invisible(self)
                      },
                      
                      get_res = function(){
                        atts = ncdf4::ncatt_get(self$NC, 0)
                        
                        lon = atts[['geospatial_lon_resolution']] |> as.numeric()
                        lat = atts[['geospatial_lat_resolution']] |> as.numeric()
                      
                        c(lon, lat)
                      }, # get_res
                      
                      get_lon = function(){
                        self$NC$dim$lon$vals
                      },
                      
                      get_lat = function(){
                        self$NC$dim$lat$vals
                      },
                      
                      get_time = function(){
                        origin = as.Date(self$NC$dim$time$units,
                                         format = "days since %Y-%m-%d")
                        origin + self$NC$dim$time$vals
                      }, # get_time
                      
                      
                      get_nav = function(bb = c(-180, 180,-90, 90), varid = "u_wind"){
                        stopifnot(varid %in% names(self$NC$var))
                        
                        bb = to360BB(bb)
                        
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
                        
                        # for bbox we still want [-180,180]
                        bbox = c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |>
                          to180BB()
                        bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
                        
                        list(
                          bb = bb,
                          varid = varid,
                          bbox = sf::st_bbox(bbox, crs = 4326),
                          start = c(ix[1], iy[1], 1, 1),
                          count = c(nx, ny, 1, 1) )
                      }, # get_nav
                      
                      get_var = function(time = 1, 
                                         bbox = c(-180, 180, -90, 90), 
                                         varid = 'u_wind',
                                         nav = NULL,
                                         form = c("stars", "array")[1]){
                        
                        if (inherits(time, "POSIXt")) time = as.Date(time)
                        if (inherits(date, "Date")) time = findInterval(time, self$get_time(klass = "Date"))
                        if (time <= 0) stop("time must be at or later than:", 
                                            format(self$get_time()[1], "%y-%m-%d"))
                        if (is.null(nav)) nav = self$get_nav(bb = bbox, varid = varid)
                        
                        nav$start[4] <- time
                        m <- ncdf4::ncvar_get(self$NC, nav$varid,
                                              start = nav$start,
                                              count = nav$count)
                        if (tolower(form[1]) == 'array') return(m)
                        
                        stars::st_as_stars(nav$bbox,
                                           values = m,
                                           nx = nav$count[1],
                                           ny = nav$count[2]) |>
                          rlang::set_names(varid) |>
                          stars::st_flip("y")
                      }
                      
                      
                    ) # public
                    
                    
)# BSWm
