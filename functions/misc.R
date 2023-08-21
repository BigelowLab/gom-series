#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/316441.html
#'
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
}


#' Compute the range of values per column
#' 
#' @param x a table with one or more numeric columns
#' @param na.rm logical, if TRUE compute after removing NAs
#' @param collapse logical, if TRUE compute a single range for all data
#' @return if \code{collapse} is TRUE then a vector of [min, max]
#'   otherwise three column summary of [var, min, max]
table_range = function(x, na.rm = TRUE, collapse = TRUE){
  
  mm = dplyr::select(x, dplyr::where(is.numeric)) |>
    apply(2, range, na.rm = na.rm) |>
    t() |>
    dplyr::as_tibble(.name_repair = "minimal",
                     rownames = "var") |>
    rlang::set_names(c("var", "min", "max"))
  
  if (collapse){
    mm = c(min(mm$min, na.rm = na.rm), max(mm$max, na.rm = na.rm))
  }
  mm 
}

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

#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/316441.html
#'
#' @seealso \code{\link[base]{grepl}}
#' @export
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
#' @examples
#' \dontrun{
#'  x <- c("dog", "canine", "squirrel", "fish", "banana")
#'  pattern <- c("nine", "ana")
#'  mgrepl(pattern, x, fixed = TRUE)
#'  # [1] FALSE  TRUE FALSE FALSE  TRUE
#'}
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
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