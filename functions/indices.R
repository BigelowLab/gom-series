#' Reads the North Atlantic Oscillation Index (NAO) data
#' 
#' @param form character defining form to return data table long or wide
#' @return a tibble of monthly NAO data in the form specified
#' 
#' @export
fetch_nao <- function(form = c("long", "wide")[1]) {
  
  #nao <- readLines("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table")
  #nao <- readLines("https://psl.noaa.gov/data/correlation/nao.data")
  nao <- readLines("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nao.long.data")
  
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


#' Filter climate indices for complete years (they are already assumed to be 
#' complete months)
#' 
#' 
#' @param x a wide-form tibble of climate index data
#' @param by character the interval over which to filter - one of 'month' or 'year'
#' @retun a tibble of climate index data
complete_intervals_index = function(x = read_climate_indices(), 
                                    by = c("month", "year")[2]){
  if (by == "month") return(x)

  if (nrow(x) == 0) return(x)
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  
  indices = colnames(x)
  indices = indices[!(indices %in% "date")]
  
  x = dplyr::mutate(x, interval_ = format(date, format="%Y"))
  
  y = lapply(indices,
     function(index){
       dplyr::select(x, dplyr::all_of(c("date", "interval_", index))) |>
         dplyr::group_by(interval_) |>
         dplyr::group_map(
           function(tbl, key){
             ix = !is.na(tbl[[index]])
             if (sum(ix) < 12) tbl[[index]] <- NA
             tbl
           }) |>
         dplyr::bind_rows()
     }) |>
  purrr::reduce(dplyr::left_join, by = 'date')
}


#' Aggregate climate index data by month or year
#' 
#' @param x a tibble of climate index data
#' @param by character the interval over which to aggregate - one of 'month' or 'year'.
#'  Month is ignored and the inout is returned
#' @retun a tibble of climate index data
#' 
#' @export
aggregate_climate_indices <- function(x = read_climate_indices(), 
                            by = c("month", "year")[2],
                            form = c("wide", "long")[1]) {
  
  if (by == "month") return(x)
 
  
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  
  indices = colnames(x)
  indices = indices[!(indices %in% "date")]
  
  x = complete_intervals_index(x, by = 'year') |>
    dplyr::mutate(x, date = as.Date(format(date, format=fmt)))
  
  x = x |>
      dplyr::group_by(date) |>
      dplyr::group_map(
        function(tbl, key, params = c("nao", "amo", "gsi")) {
          v = lapply(params,
                     function(p){
                       v = sixnum(tbl |> dplyr::pull(dplyr::all_of(p))) |>
                         as.list() |>
                         dplyr::as_tibble()
                       names(v) <- paste(p, names(v), sep = ".")
                       v
                     }) |>
            dplyr::bind_cols()
          # now bind to tbl
          dplyr::select(tbl, dplyr::all_of(c("date"))) |>
            dplyr::slice(1) |>
            dplyr::bind_cols(v)
        }, params = indices, .keep = TRUE ) |>
      dplyr::bind_rows()
  
  if (tolower(form[1]) == 'long'){
    
    x = tidyr::pivot_longer(x,
                         cols = dplyr::where(is.numeric),
                         names_to = c('foo'),
                         values_to = 'value') |>
      tidyr::separate_wider_delim(dplyr::any_of("foo"),
                                  delim = ".",
                                  names = c("index", "measure"))
    
    
  }
  return(x)
}


#' Pivot a long climate indices table to a wide one
#'
#' @param x climate indices long table
#' @return long table
wide_climate = function(x = long_climate()){
  tidyr::pivot_wider(x,
                      names_from = 'index',
                      values_from = 'value')
}

#' Pivot a wide climate indices table to a long one
#'
#' @param x climate indices wide table
#' @return long table
long_climate = function(x = read_climate_indices(form = "wide")){
  tidyr::pivot_longer(x,
                      where(is.numeric),
                      names_to = 'index',
                      values_to = 'value')
}
#' Read climate indices into one table
#' 
#' @param what character one or more index names or all to read all
#' @param path character path to the climate data directory
#' @param form character defining form to return data table long or wide
#' @return tibble of climate indices
read_climate_indices = function(what = c("all", "nao", "amo", "gsi")[1],
  form = c("long", "wide")[2]){
  
  if ("all" %in% what) what = c("amo", "nao", "gsi")
  
  x = sapply(tolower(what),
               function(w){
                 switch(w, 
                        "nao" = read_nao(),
                        "amo" = read_amo(),
                        "gsi" = read_gsi())
               }, simplify = FALSE) |>
    purrr::reduce(dplyr::full_join, by = 'date')  |>
    dplyr::arrange(date)
  
  if (tolower(form[1]) == 'long'){
    x = tidyr::pivot_longer(x,
                            where(is.numeric),
                            names_to = 'index',
                            values_to = 'value')
  }
  x
}



#' Join aggregated climate index data into one wide table
#' 
#' @param by character. one or 'year' or 'month'
#' @return a wide table of aggregated climate index data
#' 
#' @export
export_climate_indices <- function(by = c("year", "month")[1]) {
  
  #amo <- read_amo() |>
  #  aggregate_climate_index(by = by) |>
  #  dplyr::mutate(index = "amo") |>
  #  dplyr::rename(value = amo)
  #
  #nao <- read_nao() |>
  #  aggregate_climate_index(by = by) |>
  #  dplyr::mutate(index = "nao")|>
  #  dplyr::rename(value = nao)
  #
  #gsi <- read_gsi() |>
  #  aggregate_climate_index(by = by) |>
  #  dplyr::mutate(index = "gsi") |>
  #  dplyr::rename(value = gsi)
  
  x = aggregate_climate_indices(by = by)
  x
  
  
  #r <- bind_rows(amo, nao, gsi) |>
  #  tidyr::pivot_wider(id_cols = dplyr::all_of("date"),
  #                     names_from = "index",
  #                     names_glue = "{index}.index",
  #                     values_from = where(is.numeric))  |>
  #  dplyr::arrange(date)
  
  # we don't want to drop NAs actually, but we do need to make sure that when we compute
  # an annual index from the monthlies that we have 12 minths each year.  In this case
  # each of amo, nao and gsi start on Jan 1 (of different years) and end on Dec 31
  # (of different years)
  #if (complete_intervals) {
  #  r <- r |>
  #    tidyr::drop_na()
  #}
  
  return(x)
  
}


