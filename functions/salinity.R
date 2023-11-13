#' Reads salinity data
#' 
#' @param filename character name of file containing salinity data
#' @param path character path to salinity data
#' @return a tibble of salintiy data for the Gulf of Maine 
#' 
#' @export
read_salinity <- function(filename = "Whitney2015salinity.csv",
                          path = here::here("data", "salinity")) {
  
  file <- file.path(path, filename)
  
  r <- suppressMessages(readr::read_csv(file, col_names = c("year", "salinity"))) |>
    dplyr::mutate(date = as.Date(sprintf("%s-01-01", year)), .before=year) |>
    dplyr::select(-dplyr::all_of("year"))
  
}

#' Calls read_salinity
#' 
#' @param by char, anything but 'year' will return NULL
#' @param ... other arguments for \code{read_salinity}
#' @return a tibble of salinity data for the Gulf of Maine
#' 
#' @export
export_salinity <- function(by = c("month", "year")[2], ...) {
  
  if (tolower(by[1]) != 'year') return(NULL)
  
  read_salinity(...)
}