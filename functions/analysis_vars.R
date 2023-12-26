#' Reads the target vars file and appends the treatment as required.
#' 
#' @param treatment string indicating which treatment to select ie median, q25, q75 or "none" to skip
#' @param no_treatment string, zero or more variables to exclude form adding the treatment
#' @param filename string, the file path to the 'target_vars.txt' file
#' @return character vector
read_target_vars = function(treatment = c("median", "q25", "q75", "none", "all")[1],
                            no_treatment = c("PCI", "hab", "cal", "salinity"),
                            filename = here::here("data", "variables", "target_vars.txt")){
          
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
                              file = here::here("data", "variables", "var_names.csv")) {
  
  var_names <- read_var_meta(file)
  
  lut <- var_names$analysis_name
  names(lut) <- var_names$human_readable_name
  
  r <- x |>
    dplyr::rename(dplyr::any_of(lut))
  
  return(r)
}

#' Retrieve tge identifiers for staions used in analysis
#' 
#' @param what char, one of 'buoys', 'ghcn' or 'usgs'
#' @return charcater vector or NULL if none selected
analysis_stations = function(what = c("buoys", "usgs", "ghcn")[2]){
  
  switch(tolower(what[1]),
         "usgs" = c("1059000", "1022500"),
         "ghcn" = c("00272174", "00190736", "00171628"),
         "buoys" = NULL)
  
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
                          sources = c("ersst", "sst", "chlor.", "USGS", "GHCN", "amo", "nao", "gsi", "hab", "PCI", "cal"),
                          usgs_stations = analysis_stations("usgs"),
                          ghcn_stations =  analysis_stations("ghcn"),
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

#' Read the table of variable names and metadata
#' 
#' @param filename char, the name of the file
#' @return tibble
read_var_meta = function(filename = here::here("data", "variables", "var_names.csv")){
  readr::read_csv(filename, show_col_types = FALSE)
}
