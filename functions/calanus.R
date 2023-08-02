#' Reads calanus index data
#' 
#' @param filename character name of file containing calanus index data
#' @param path character path to calanus index data
#' @return a tibble of calanus index data for fall and spring 
#' @export
read_calanus <- function(filename = "calIdxEcoMon.csv",
                         path = here::here("data", "calanus")) {
  
  file <- file.path(path, filename)
  
  r <- suppressMessages(readr::read_csv(file)) |>
    dplyr::rename(cal_index.fall = fall,
                  cal_index.spring = spring,
                  date = year) |>
    dplyr::mutate(date = as.Date(sprintf("%s-01-01", date)))
  
  return(r)
}


#' Calls read_calanus
#' 
#' @param by char, anything but 'year' will return NULL
#' @param ... other arguments for \code{read_calanus}
#' @return a tibble of calanus index data for fall and spring
#' @export
export_calanus <- function(by = "year", ...) {
  
  if (tolower(by[1]) != 'year') return(NULL)
  
  read_calanus(...)
}