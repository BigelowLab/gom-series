#' Read the watersheds
#' 
#' @param path char, the path to the watershed data
#' @param return sf table of watershed polygons
read_watersheds = function(path = here::here("data", "watersheds")){
  
 sf::read_sf(file.path(path, 'watersheds.gpkg'))
}