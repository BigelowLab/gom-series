#' Reads the target vars file and appends the treatment as required.
#' 
#' @param treatment string indicating which treatment to select ie median, q25, q75 or "none" to skip
#' @param no_treatment string, zero or more variables to exclude form adding the treatment
#' @param filename string, the file path to the 'target_vars.txt' file
#' @return character vector
read_target_vars = function(treatment = c("median", "q25", "q75", "none", "all")[1],
                            no_treatment = c("PCI", "hab", "cal"),
                            filename = here::here("data", "target_vars.txt")){
          
  x <- readLines(filename[1])
  
  
  if ("all" %in% treatment) treatment =c("median", "q25", "q75")
  
  if (!("none" %in% treatment)){
    inot = if (length(no_treatment) > 0) {
        !mgrepl(no_treatment, x, fixed = TRUE)
      } else {
        !rep(FALSE, length(x))
      }
  
    x = sapply(treatment,
      function(trt, x = NULL, inot = NULL){
        x[inot] = paste(x[inot], trt, sep = ".")
        x
      }, x = x, inot = inot) |>
     as.vector()
  }
  x
}



#' Reads a lookup table of human readable variable names and replaces them in a tibble of analysis variables
#' 
#' @param x 
#' @param path 
#' @param file 
#' @return a tibble similar to x with variable names as human readable version
#' 
#' 
replace_var_names <- function(x, 
                              file = here::here("data", "var_names.csv")) {
  
  var_names <- suppressMessages(readr::read_csv(file))
  
  lut <- var_names$analysis_name
  names(lut) <- var_names$human_readable_name
  
  r <- x |>
    dplyr::rename(dplyr::any_of(lut))
  
  return(r)
}


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
analysis_vars <- function(x = read_export(selection = "all"),
                          sources = c("sst", "chlor.", "USGS", "GHCN", "amo", "nao", "gsi", "hab", "PCI", "cal"),
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
  
  pci_hab_cal <- x |>
    dplyr::select(dplyr::contains(c("PCI")) & dplyr::contains("log") | dplyr::contains("hab") | dplyr::contains("cal")) |>
    colnames()
  
  vars <- c(usgs_vars, ghcn_vars, other_vars, pci_hab_cal)
  
  return(vars)
  
}
