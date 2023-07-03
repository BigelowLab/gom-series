#' Reads the North Atlantic Oscillation Index (NAO) data
#' 
#' @param form character defining form to return data table long or wide
#' @return a tibble of monthly NAO data in the form specified
#' 
#' @export
fetch_nao <- function(form = c("long", "wide")[1]) {
  
  #nao <- readLines("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table")
  nao <- readLines("https://psl.noaa.gov/data/correlation/nao.data")
  
  dims <- nao[1] |>
    stringr::str_trim(side = "both") |>
    stringr::str_squish() |>
    stringr::str_split(pattern = " ") %>%  # Error: function '[[' not supported in RHS call of a pipe
    `[[`(1) |>
    as.numeric()
  
  n <- dims[2]-dims[1] + 1
  
  x <- nao[2:n] |>
    stringr::str_trim(side = "both") |>
    stringr::str_squish() |>
    stringr::str_replace_all(stringr::fixed(" "), stringr::fixed(",")) |>
    paste(collapse = "\n") |>
    readr::read_csv(col_names = c("Year", month.abb),
                    col_types = strrep("d", 13)) |>
    dplyr::filter(.data$Jan > -99.9) |> # 1948-49 read in as -99.9 all months
    dplyr::rename_with(tolower)
  
  if (form == "long") {
    x <- x |>
      tidyr::pivot_longer(dplyr::all_of(tolower(month.abb)), 
                          names_to = "Month",
                          values_to = "nao") |>
      dplyr::mutate(date = as.Date(paste("1", tolower(Month), year, sep=""), format="%d%b%Y"), .before=nao) |>
      dplyr::select(-Month, -year)
  }
  
  return(x)
}


#' Reads the Atlantic Multidecadal Oscillation (AMO) Index data
#' AMO unsmoothed from the Kaplan SST V2                                                                 
#' Calculated at NOAA PSL1                                                                         
#' http://www.psl.noaa.gov/data/timeseries/AMO/                                                                 
#' 
#' @param form character defining form to return data table long or wide
#' @return a tibble with monthly AMO data in the form specified
#' 
#' @export
fetch_amo <- function(form = c("long", "wide")[1]) {
  
  amo <- readLines("https://psl.noaa.gov/data/correlation/amon.us.long.data") 
  
  dims <- amo[1] |>
    stringr::str_trim(side = "both") |>
    stringr::str_squish() |>
    stringr::str_split(pattern = " ") %>%  # Error: function '[[' not supported in RHS call of a pipe
    `[[`(1) |>
    as.numeric()
  
  n <- dims[2]-dims[1] + 1
  
  x <- amo[2:n] |>
    stringr::str_trim(side = "both") |>
    stringr::str_squish() |>
    stringr::str_replace_all(stringr::fixed(" "), stringr::fixed(",")) |>
    paste(collapse = "\n") |>
    readr::read_csv(col_names = c("Year", month.abb),
                    col_types = strrep("d", 13)) |>
    dplyr::rename_with(tolower)
  
  if (form == "long") {
    x <- x |>
      tidyr::pivot_longer(dplyr::all_of(tolower(month.abb)), 
                          names_to = "Month",
                          values_to = "amo") |>
      dplyr::mutate(date = as.Date(paste("1", tolower(Month), year, sep=""), format="%d%b%Y"), .before=amo) |>
      dplyr::select(-Month, -year)
  }
  
  return(x)
  
}