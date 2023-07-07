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

#' A wrapper around \code fetch_nao()
#' 
#' @return a tibble of nao data
read_nao <- function(...) {
  fetch_nao(...)
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

#' A wrapper around \code fetch_amo()
#' 
#' @return a tibble of amo
read_amo <- function(...) {
  fetch_amo(...)
}

#' Reads in the locally stored gulf stream index (GSI)
#' provided by Chen et al
#' 
#' @param filename character filename of which GSI to read
#' @param path character path to the climate data directory
#' @param form character defining form to return data table long or wide
#' @return a tibble of gsi data 
#' 
#' @export
read_gsi <- function(filename = c("Chen_EN4_T200_GSI_1954_2021_monthly.xlsx", "Chen_SSH-based_GSI_monthly_199301-202112.xlsx")[1],
                     path = here::here("data", "climate"),
                     form = c("long", "wide")[1]) {
  
  file <- file.path(path, filename)
  
  x <- readxl::read_xlsx(file) |>
    #dplyr::mutate(date = sprintf("%0.2f.01", .data$Month) |> as.Date(format = "%Y.%m.%d"), .before = .data$GSI) |>
    dplyr::mutate(date = sprintf("%0.2f.01", .data$Month) |> as.Date(format = "%Y.%m.%d"), .before = dplyr::all_of("GSI")) |>
    dplyr::select(-"Month") |>
    dplyr::rename(gsi = dplyr::all_of("GSI"))
  
  if (form == "wide") {
    
    x <- x |>
      dplyr::mutate(year = as.numeric(format(date, format="%Y")),
                    month = tolower(format(date, format="%b")),
                    .before = .data$gsi)
      tidyr::pivot_wider(id_cols = .data$year,
                         names_from = .data$month,
                         values_from = .data$gsi)
  }
  
  return(x)
}


#' Aggregate climate index data by month or year
#' 
#' @param x a tibble of climate index data
#' @param by character the interval over which to aggregate - one of 'month' or 'year'
#' @retun a tibble of climate index data
#' 
#' 
aggregate_climate_index <- function(x, 
                            by = c("month", "year")[2]) {
  
  if (by == "month") {
    return(x)
  } else if (by == "year") {
    x |>
      dplyr::group_by(format(date, format="%Y")) |>
      dplyr::group_map(
        function(tbl, key) {
          v <- sixnum(tbl |> dplyr::pull()) |>
            as.list() |>
            dplyr::as_tibble()
          v |>
            #dplyr::mutate(date = as.Date(format(tbl[1,]$date, format="%Y-01-01")), .before = dplyr::all_of("min")) |>
            dplyr::mutate(date = as.Date(format(tbl[1,]$date, format="%Y-01-01")), .before = dplyr::all_of("min"))
        }
      ) |>
      dplyr::bind_rows()
  } else {
    stop("argument by can only be 'month' or 'year'")
  }
  
}


#' Join aggregated climate index data into one wide table
#' 
#' @param by character. one or 'year' or 'month'
#' @param complete_intervals logical if TRUE only returns years that do not have NA values
#' @return a wide table of aggregated climate index data
#' 
export_climate_indices <- function(by = c("year", "month")[1],
                                   complete_intervals = TRUE) {
  
  amo <- read_amo() |>
    aggregate_climate_index(by = by) |>
    dplyr::mutate(index = "amo")
  
  nao <- read_nao() |>
    aggregate_climate_index(by = by) |>
    dplyr::mutate(index = "nao")
  
  gsi <- read_gsi() |>
    aggregate_climate_index(by = by) |>
    dplyr::mutate(index = "gsi")
  
  r <- bind_rows(amo, nao, gsi) |>
    tidyr::pivot_wider(id_cols = dplyr::all_of("date"),
                       names_from = "index",
                       names_glue = "{index}.{.value}",
                       values_from = where(is.numeric))  |>
    dplyr::arrange(date)
  
  if (complete_intervals) {
    r <- r |>
      tidyr::drop_na()
  }
  
  return(r)
  
}


