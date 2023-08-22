#' Defines red value to use in figures
get_color_red <- function() {
  return("red")
}

#' Defines blue value to use in figures
get_color_blue <- function() {
  return("blue")
}

#' Defines gray value to use in figures
get_color_grey <- function() {
  return("grey60")
}

#' Defines white value to use in figures
get_color_white <- function() {
  return("white")
}


#' Retrieve hex color(s) by name(s)
#' 
#' @param x char, one or more color names
#' @return one or more color specifications
get_color = function(x){
  
  col2rgb(x) |>
    t() |>
    rgb(maxColorValue = 255)
}