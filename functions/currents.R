#' Read in vectors of hand drawn current vectors
#' 
#' @param filename the bane of the file
#' @param path the path to the file
#' @return sf object
read_currents = function(filename = "currents.gpkg", 
                         path = here::here("data", "currents"),
                         smooth = TRUE){
  
  x = sf::read_sf(file.path(path, filename))
  if (smooth) x = smoothr::smooth(x, method = "chaikin")
  x
}