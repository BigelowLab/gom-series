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
                         path = here::here("data", "regions"),
                         split_coastal = TRUE,
                         form = c("raw", "bbox", "chull")[1],
                         keep = c("Wilkinson Basin", "Jordan Basin", "Georges Basin",  
                                  "Georges Bank", "Eastern Maine Coastal Shelf", 
                                  "Western Coastal Shelf")){
  #Jordan Basin, Wilkinson Basin, Georges Basin, Georges Bank, EMCC, WMCC
  filename = file.path(path, filename[1])
  if (!file.exists(filename)) stop("file not found:", filename)
  
  x <- sf::read_sf(filename) |>
    dplyr::select(-dplyr::any_of(c("objectid", "regionnum")), 
                  -dplyr::starts_with("area"))
  

  
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
  
  shortnames = region_shortnames()
  name = rep("", nrow(x))
  x = dplyr::mutate(x, name = shortnames[region], .after = 1)
  x
}


#' A collection of shortnames (no spaces) to crisply identify unique
#' 
#' By default it contains the regions that match the Gulf of Maine regions,
#' but simply append your own to data/regions/shortnames.csv.  If not match is found then
#' the shortname is simply and empty string.
#' 
#' @param filename char, the filename with the shortname definitions
#' @param form char one of 'table' or 'vector' (the default)
#' @return a tibble or a named vector
region_shortnames = function(filename = here::here("data", "regions", "shortnames.csv"),
                             form = c("table", "vector")[2]){
  
  x = readr::read_csv(filename, col_types = 'cc')
  if (tolower(form[1]) == 'vector') {
    x = dplyr::pull(x, dplyr::all_of("shortname")) |>
      rlang::set_names(dplyr::pull(x, dplyr::all_of("name")))
  }
  
}