# Processed data from the continuous plankton recorder (CPR)
# 
# - PCIsprfallanomlog.csv and PCIsprfallanom.csv
# - PCI = phytoplankton color index
# - Spring = months 3-5  XXXX-03-01
# - Fall = months 9-11   XXXX-09-01
# - Anomalies (or log anom) from full time series mean


#' Fetch Phytoplankton Color Index data
#' 
#' This data is manually curated so it is a wrapper around \code{read_cpr}
#' 
#' @param ... arguments for \code{read_cpr}
#' @return tibble of cpr data
fetch_pci = function(...){
  read_pci(...)
}

#' Read Phytoplankton Color Index data
#' 
#' @param path 
#' @return tibble of cpr data
read_pci = function(path = here::here("data", "CPR")){
  # two files to file - both anomalies - raw and logged
  # read each, rename logged, merge by year
  
  rawfilename = file.path(path, "PCIsprfallanom.csv")
  logfilename = file.path(path, "PCIsprfallanomlog.csv")
  raw = readr::read_csv(rawfilename, show_col_types = FALSE)
  logged = readr::read_csv(logfilename, show_col_types = FALSE) |>
    dplyr::rename(fall.anom.log = fall.anom, spring.anom.log = spring.anom)
  
  raw |>
    dplyr::left_join(logged, by = 'year') |>
    dplyr::mutate(date = sprintf("%0.4i-01-01", year) |> as.Date(),
                  .before = 1) |>
    dplyr::select(-dplyr::any_of("year"))
}

#' Transform PCI data to long format, includes recoding of date to the 
#' beginning date of the period.
#' 
#' @param x tibble of raw PCI data
#' @return long format verison of input with date recoded
long_pci = function(x = read_pci()){
  
  recode_date = function(x){
    r = format(x, "%Y-03-01")
    ix = grep("fall", y, fixed = TRUE)
    r[ix] <- format(x[ix], "%Y-09-01")
    as.Date(r)
  }
  
  tidyr::pivot_longer(x,
                      dplyr::where(is.numeric),
                      names_to = "name",
                      values_to = "pci") |>
    dplyr::mutate(date = recode_date(date, name))
}


#' Export the annual (no monthly) data in a wide format
#' @param by character, one of 'year' or 'month'
#' @param x tibble or NULL, aggregated dataset.  If NULL we read it internally
#' @return wide tibble of aggregated data
export_pci = function(by = c("year", "month")[1],
                      x = NULL){
  
  if (tolower(by[1]) == 'month') return(NULL)
  if (is.null(x)) x = read_pci()
  
  dplyr::rename(x,
                PCI.spring.anom  = spring.anom,
                PCI.fall.anom = fall.anom, 
                PCI.spring.anom.log = spring.anom.log,
                PCI.fall.anom.log = fall.anom.log)
}
