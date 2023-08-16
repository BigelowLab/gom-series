plot_departure = function(x = read_export(by = 'year') |>
                            dplyr::filter(date >= as.Date("1970-01-01")) |>
                            departure(win = 20),
                          title = NULL,
                          ...){
  
  period = if(diff(x$date[1:2]) > 32){
    'Year'
  } else {
    'Month'
  }
  
  if (is.null(title)) title = paste("Departure by", period)
  
  plot_surprise(x, title = title, ...)
  
}


#' Plot an export data frame
#'
#' @param x data frame or tibble of exported data
#' @param surprise numeric, the threshold for what defines a surprise
#' @param clip_window logical, if TRUE clip the leading window of missing data.
#'   Note that at this point it guesses what that width is.
#' @param purge_empty, logical if TRUE drop empty columns (populated by NAs only)
#' @param title char or NULL, if NULL no title is added, if 'auto' then an title is 
#'   automatically gueesed at, if any other string then use that
#' @return ggplot object
plot_surprise = function(x = read_export(by = 'year') |>
                           dplyr::filter(date >= as.Date("1970-01-01")) |>
                           surprise(win = 20), 
                         surprise = 2,
                         clip_window = TRUE,
                         purge_empty = TRUE,
                         title = 'auto'){
  
  period = if(diff(x$date[1:2]) > 32){
    'Year'
  } else {
    'Month'
  }
  
  if (is.character(title) && grepl("auto", title, fixed = TRUE)) title = paste("Surprise by", period)
  
  if (purge_empty) x = purge_export(x)
  if (clip_window) {
    empty_rows = apply(dplyr::select(x,-date), 1, function(x) all(is.na(x)))
    x = dplyr::filter(x, !empty_rows)
  }
  
  if (!is.null(surprise)){
    # leaves a wide empty -1 to 1 spot
    isurprise = c(-surprise, surprise)
    recode_surprise = function(x, vals = c(-1, 1)){
      iy = !is.na(x)
      ix = findInterval(x[iy], vals) + 1
      newvals = c("-surprise", "no surprise", "+surprise")[ix]
      r = rep(NA_integer_, length(x))
      r[which(iy)] = newvals
      factor(r, levels = c("+surprise", "no surprise", "-surprise"))
    }
    x = dplyr::ungroup(x) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) recode_surprise(x, vals = isurprise)))
    rng = c(-surprise, surprise)
  } else {
    rng = table_range(dplyr::ungroup(x), na.rm = TRUE, collapse = TRUE)
    if (sign(rng[1]) < 0 && sign(rng[2]) > 0){
      rng = rep(max(rng),2) * c(-1, 1)    
    } 
  }
  
  cnames = dplyr::select(x, -date) |> colnames()
  long = long_export(x) |>
    dplyr::mutate(name = factor(name, levels = cnames))
  
  gg = ggplot2::ggplot(long, ggplot2::aes(date, name)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "grey60") +
    ggplot2::labs(x = "Date", 
                  y = "", 
                  title = title) +
    ggplot2::scale_y_discrete(name = NULL, 
                              guide = guide_axis(angle = 45)) + 
    ggplot2::theme_gray() + 
    ggplot2::theme(axis.text.x = element_text(size=10),
                   legend.title=element_blank())
  
  if (!is.null(surprise)){
    gg = gg + ggplot2::scale_fill_discrete(drop = FALSE,
                                           breaks = c("+surprise",
                                                      "no surprise", 
                                                      "-surprise", 
                                                      NA_character_),
                                           type = c("+surprise" = "#de2d26",
                                                    "no surprise" = "#ffffff", 
                                                    "-surprise" = "#3182bd", 
                                                    "NA" = "#f7f7f7")) 
  } else {
    gg = gg + ggplot2::scale_fill_gradient2(low = "blue", 
                                            high = "red", 
                                            na.value = "grey60", 
                                            name = "",
                                            limits = rng)
  }
  
  gg
}



#' Compute surprise index from a single vector or for all numeric columns in a
#' data frame (suitable for the 'export')
#' 
#' @param datain numeric vector or a data.frame (tibble) with numeric-type variables
#' @param win numeric, width of the sliding window
#' @return numeric vector of surprise indices (standardize departures)
#'  or if the input is a data.frame (tibble) then the input with mutated columns
surprise <- function(datain = read_export(by = 'year'), win = 30){
  
  if (inherits(datain, "data.frame")){
      dataout = dplyr::ungroup(datain) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) surprise(x, win = win)))
      return(dataout)
  }
  

  nx = length(datain)
  if (nx <= win) stop("input must be longer than window")
  x = seq_len(nx)
  idx = seq_len(win) - 1L
  
  # a temporary function applied for each iteration
  # @param i iteration number
  # @param dat essentially datain
  # @param x the sequential index alon dat
  # @param win window width
  # @param idx the sequence along the window
  # @return numeric surprise index
  surprise_one = function(i, dat = NULL, x = NULL, win = NULL, idx = NULL){
    idx = idx + (i - win)  
    this_y = dat[idx]
    isnotna = !is.na(this_y)
    if (sum(isnotna) < 10) return(NA_real_)
    fit <- lm(y ~ x, data = data.frame(x = x[idx], y = this_y), na.action = na.exclude)
    dev <- datain[i] - predict(fit, newdata = data.frame(x=i))
    dev / sd(fit$residuals)
  }
  dataout = c(rep(NA_real_, win),
        sapply(seq.int(from = win+1L, to = length(datain)), 
                   surprise_one, dat = datain, x = x, win = win, idx = idx) |>
          unname())
  return(dataout)
  
  
  #dataout <- matrix(data=NA,nrow=length(datain),ncol=1)
  #for (i in (win+1):length(datain))
  #{
  #  IDX <- (i-win):(i-1)
  #  df <- data.frame(x = x[IDX], y = datain[IDX])
  #  fit <- lm(y ~ x, data=df)
  #  dev <- datain[i] - predict(fit,newdata = data.frame(x=i))
  #  dataout[i] <- dev / sd(fit$residuals)
  #}
  #return(dataout[,1, drop = TRUE])
}



#' Compute departure from a single vector or for all numeric columns in a
#' data frame (suitable for the 'export')
#' 
#' @param datain numeric vector or a data.frame (tibble) with numeric-type variables
#' @param win numeric, width of the sliding window
#' @param fun function name (unquoted) to compute the departure from
#' @param na.rm logical, if TRUE drop NAs before computing departure
#' @return numeric vector of surprise indices (standardize departures)
#'  or if the input is a data.frame (tibble) then the input with mutated columns
departure <- function(datain = read_export(by = 'year'), 
                      win = 30, 
                      fun = mean, 
                      na.rm = TRUE){
  
  if (inherits(datain, "data.frame")){
    dataout = dplyr::ungroup(datain) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                                  \(x) departure(x, win = win, fun = fun, na.rm = na.rm)))
    return(dataout)
  }
  
  
  nx = length(datain)
  if (nx <= win) stop("input must be longer than window")
  x = seq_len(nx)
  idx = seq_len(win) - 1L
  
  # a temporary function applied for each iteration
  # @param i iteration number
  # @param dat essentially datain
  # @param x the sequential index alon dat
  # @param win window width
  # @param idx the sequence along the window
  # @return numeric departure
  depart_one = function(i, dat = NULL, x = NULL, win = NULL, idx = NULL, fun = mean, na.rm = TRUE){
    idx = idx + (i - win)  
    this_y = dat[idx]
    isnotna = !is.na(this_y)
    if (sum(isnotna) < 10) return(NA_real_)
    
    center = fun(this_y, na.rm = na.rm)
    stdev = sd(this_y, na.rm = na.rm)
    dev = datain[i] - center
    dev/stdev
  }
  dataout = c(rep(NA_real_, win),
              sapply(seq.int(from = win+1L, to = length(datain)), 
                     depart_one, dat = datain, x = x, win = win, idx = idx, fun = fun, na.rm = na.rm) |>
                unname())
  return(dataout)
}
