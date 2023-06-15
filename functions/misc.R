#' Given a set of values compute the standard summary of 
#' [min, q25, median, mean, q75, max]
#' 
#' @param x numeric vector of values
#' @param na.rm logical, if TRUE remove NA values before computing summaries
#' @return named vector of summary values
sixnum = function(x, na.rm = TRUE){
  m = mean(x, na.rm = TRUE)
  r = fivenum(x, na.rm = TRUE)
  c(r[1:3], m, r[4:5]) |>
    rlang::set_names(c("min", "q25", "median", "mean", "q75", "max"))
}


#' Compute a season from POSIXct or Date values
#'
#' @export
#' @param x POSIXct or Date vector
#' @param lut charcater vector of length 12.  x is converted to month number
#'  and is then used as a index into this look up table
#' @return charcater vector of season names, same length as x
date_to_seasonname <- function(x,
                               lut = c(
                                 'DJF', 'DJF',
                                 'MAM', 'MAM', 'MAM',
                                 'JJA', 'JJA', 'JJA',
                                 'SON', 'SON', 'SON',
                                 'DJF')){
  if (missing(x))  x <- Sys.Date()
  lut[as.integer(format(x, "%m"))]
}

#' Convert to as POSIXct to an 8D week number (1-46) or 7D week number (1-52)
#'
#' @export
#' @param x POSIXct or Date vector
#' @param week_length numeric, the number of days per week
#' @return char 01-46 or 01-52 week number
date_to_week <- function(x, week_length = 7){
  if (missing(x)) x <- Sys.Date()
  J <- as.numeric(format(x, "%j"))
  sprintf("%0.2i", (J-1) %/% week_length + 1)
}