#' Returns a vector of variable names to use in gom-series analysis
#' 
#' @param x tibble returned from calling `read_export()`
#' @param sources character vector of data sources to include in vars
#' @param usgs_stations character vector of usgs stations to select
#' @param ghcn_stations character vector of ghcn stations to select
#' @param ghcn_params character vector of ghcn parameters to select
#' @param treatment string indicating which treatment to select ie median, q25, q75 
#' @return a tibble of selected variables 
#'  
#'  
analysis_vars <- function(x = read_export(),
                          sources = c("sst", "chlor.", "USGS", "GHCN", "amo", "nao", "gsi", "hab", "PCI"),
                          usgs_stations = c("1059000", "1022500"),
                          ghcn_stations = c("00272174", "00190736", "00171628"),
                          ghcn_params = c("TMIN", "TMAX", "PRCP"),
                          treatment = c("median", "q25", "q75")[1]) {
  
  usgs_vars <- x |>
    dplyr::select(dplyr::contains(usgs_stations) & dplyr::contains(treatment)) |>
    colnames()
  
  ghcn_vars <- x |>
    dplyr::select(dplyr::contains(ghcn_stations) & dplyr::contains(ghcn_params) & dplyr::contains(treatment)) |>
    colnames()
  
  other_vars <- x |>
    dplyr::select(!dplyr::contains(c("USGS", "GHCN", "PCI", "hab")) & dplyr::contains(sources) & dplyr::contains(treatment)) |>
    colnames()
  
  pci_hab <- x |>
    dplyr::select(dplyr::contains(c("PCI")) & dplyr::contains("log") | dplyr::contains("hab")) |>
    colnames()
  
  vars <- c(usgs_vars, ghcn_vars, other_vars, pci_hab)
  
  return(vars)
  
}



