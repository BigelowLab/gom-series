#' Eastern Maine Coastal Shelf mask
#'
#' @return sfc object
eastern_coastal_mask = function(){
  sf::st_polygon(list(rbind(c(-69.172732, 44.880641),
                            c(-68.373953, 43.143774),
                            c(-66.028377, 44.279317),
                            c(-66.915528, 45.478131),
                            c(-69.172732, 44.880641))))  |>
    sf::st_sfc(crs = 4326)
}


#' Read the regions database
#' 
#' @param filename char, the name of the file
#' @param path the path to the file
#' @param split_coastal logical, if TRUE, split the coast of Maine
#' @param form char, one of 'raw', 'bbox' or 'chull' 
#' @param keep char, one or more regions to keep, or "all" to keep all
#' @return sf object
read_regions <- function(filename = "gulf_of_maine_regions.gpkg",
                         path = here::here("data"),
                         split_coastal = TRUE,
                         form = c("raw", "bbox", "chull"),
                         keep = c("Wikinson Basin", "Jordan Basin", "Georges Basin",  
                                  "Georges Bank", "Eastern Maine Coastal Shelf", 
                                  "Western Coastal Shelf")){
  #Jordan Basin, Wilkinson Basin, Georges Basin, Georges Bank, EMCC, WMCC
  filename = file.path(path, filename[1])
  if (!file.exists(filename)) stop("file not found:", filename)
  
  x <- sf::read_sf(filename) |>
    dplyr::select(-objectid, -regionnum, -dplyr::starts_with("area"))
  

  
  if (split_coastal){
    suppressWarnings({
      mask = eastern_coastal_mask()
      east = sf::st_intersection(dplyr::filter(x, region == "Northern Coastal Shelf"), mask) |>
        dplyr::mutate(region = "Eastern Maine Coastal Shelf")
      west = sf::st_difference(dplyr::filter(x, region == "Northern Coastal Shelf"), mask) |>
        dplyr::mutate(region = "Western Coastal Shelf")
    })
    x <- dplyr::bind_rows(x, east, west) |>
      dplyr::filter(region != "Northern Coastal Shelf")
  }
  
  x = switch(tolower(form[1]),
    "bbox" = {
      x |>
        dplyr::rowwise() |>
        dplyr::group_map(
          function(x, dummy){
            sf::st_sf(
              region = x$region,
              geom = sf::st_bbox(x) |> sf::st_as_sfc() )
          }) |>
        dplyr::bind_rows()
        },
    "chull" = {
      x |>
        dplyr::rowwise() |>
        dplyr::group_map(
          function(x, dummy){
            sf::st_convex_hull(x) 
          }) |>
        dplyr::bind_rows()
    },
    x)
  
  
  if (!("all" %in% tolower(keep))){
    x <- dplyr::filter(x, region %in% keep)
  }
  x
}