# Accessing NERACOOS buoy data

#' Returns a table of buoy metadata
#' 
#' @return tibble
buoy_lut = function(){
  vars = "wtemp, wsal, curspeed, curdir, atemp, chlor, airspeed, airdir, gusts"
  depths = "any"
  dplyr::tribble(
    ~name, ~longname, ~id, ~lon, ~lat, 
    "wms", "Western Maine Shelf", "B01", NA_real_, NA_real_, 
    "cms", "Central Maine Shelf", "E01" ,NA_real_, NA_real_, 
    "penbay", "Penobscot Bay", "F01", NA_real_, NA_real_, 
    "ems", "Eastern Maine Shelf",  "I01", NA_real_, NA_real_, 
    "jb", "Jordan Basin", "M01", NA_real_, NA_real_, 
    "nec", "Northeast Channel", "N01", NA_real_, NA_real_)
}