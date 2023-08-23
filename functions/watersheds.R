#' Read the watersheds
#' 
#' @param path char, the path to the watershed data
#' @param return sf table of watershed polygons
read_watersheds = function(path = here::here("data", "watersheds")){
  
  ff = list.files(path, full.names = TRUE)
  lapply(ff, sf::read_sf) |>
    dplyr::bind_rows()
}