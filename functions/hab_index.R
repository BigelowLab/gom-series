#' Reads HAB index data
#' 
#' @param filename character name of file containing HAB index data
#' @param path character path to HAB index data
#' @return a tibble of HAB index data for Eastern and Western Maine 
#' @export
read_hab_index <- function(filename = "hab_index_1975_2022.csv.gz",
                           path = here::here("data", "hab_index")) {
  
  file <- file.path(path, filename)
  
  r <- suppressMessages(readr::read_csv(file)) 
  
  return(r)
}


#' Calls read_hab_index
#' 
#' @param by char, anything but 'year' will return NULL
#' @param ... other arguments for \code{read_hab_index}
#' @return a tibble of HAB index data for Eastern and Western Maine
#' @export
export_hab_index <- function(by = c("month", "year")[2], ...) {
  
  if (tolower(by[1]) != 'year') return(NULL)
  
  read_hab_index(...)
}