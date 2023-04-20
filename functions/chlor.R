#' Read mean chlorophyll data from CMEMS
#' 
#' @param filename char the name of the file
#' @param path char the path to the file
#' @param logscale logical, if TRUE take the log base 10
#' @return tibble of date, region and chlor
read_chlor_cmems = function(filename =  "chlor_cmems.csv.gz",
                            path = here::here("data", "chlor"),
                            logscale = TRUE){
  
  x = readr::read_csv(file.path(path[1], filename[1]),
                      col_types = 'Dcn')
  if (logscale) x = dplyr::mutate(x, chlor = log10(chlor))
  x
}



#' Extract data from the CMEMS chlor dataset
#' 
#' @param x regions to extract
#' @param path the output path
#' @return tibble for date, region, mean chlorophyll
chlor_cmems_extract_regions <- function(x = read_regions(),
                                  path = here::here("data", "chlor")){
  
  PATH = copernicus::copernicus_path("c3s_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M","world")
  DB = copernicus::read_database(PATH)
  ff = file.path(PATH, format(DB$date, "%Y"),
                  sprintf("%s_%s_%s.tif",
                          format(DB$date, "%Y-%m-%d"),
                          DB$var,
                          DB$depth))
  DB <- dplyr::mutate(DB, file = ff)
  
  # for each date
  # make a table of table, date region, and mean chlor
  rowwise(DB) |>
    dplyr::group_map(
      function(db, key){
        stars::st_extract(stars::read_stars(db$file), x, na.rm = TRUE) |>
          sf::st_as_sf() |>
          sf::st_drop_geometry() |> 
          rlang::set_names("chlor") |>
          dplyr::mutate(date = db$date, region = x$region, .before = 1)
      }) |>
    dplyr::bind_rows() |>
    readr::write_csv(file.path(path, "chlor_cmems.csv.gz"))
  
}