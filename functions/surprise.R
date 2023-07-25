#' Plot an export data frame
#'
#' @param x data frame or tibble of exported data
#' @param purge_empty, logical if TRUE drop empty columns (populated by NAs only)
#' @return ggplot object
plot_surprise = function(x = read_export(by = 'year') |>
                           dplyr::filter(date >= as.Date("1970-01-01")) |>
                           surprise(win = 20), 
                         surprise = 2,
                         purge_empty = TRUE,
                         title = NULL){
  
  period = if(diff(x$date[1:2]) > 32){
    'Year'
  } else {
    'Month'
  }
  
  if (is.null(title)) title = paste("Surprise by", period)
  
  if (purge_empty) x = purge_export(x)
  
  if (!is.null(surprise)){
    # leaves a wide empty -1 to 1 spot
    isurprise = c(-surprise, surprise)
    recode_surprise = function(x, vals = c(-1, 1)){
      iy = !is.na(x)
      ix = findInterval(x[iy], vals) + 1
      newvals = c("-surprise", "no surprise", "+surprise")[ix]
      #vals[vals == -1] <- 0
      r = rep(NA_integer_, length(x))
      r[which(iy)] = newvals
      factor(r, levels = c("-surprise", "no surprise", "+surprise"))
    }
    x = dplyr::ungroup(x) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), recode_surprise, vals = isurprise))
    
  }
  
  cnames = dplyr::select(x, -date) |> colnames()
  long = long_export(x) |>
    dplyr::mutate(name = factor(name, levels = cnames))
  
  gg = ggplot2::ggplot(long, ggplot2::aes(date, name)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "grey60") +
    ggplot2::labs(x = "Date", 
                  y = "", 
                  title = title) +
    ggplot2::scale_y_discrete(name = "Parameters", 
                              #breaks = ggplot2::waiver(), 
                              #labels = levels(cnames),
                              guide = guide_axis(angle = 0)) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = element_text(size=10))
  
  if (!is.null(surprise)){
    gg = gg + ggplot2::scale_fill_discrete(drop = FALSE,
                                           type = c("-surprise" = "#3182bd", 
                                                    "no surprise" = "#ffffff", 
                                                    "+surprise" = "#de2d26",
                                                    NA_charcater_ = "#f7f7f7")) 
  } else {
    gg = gg + ggplot2::scale_fill_gradient2(low="blue", high="red", na.value="grey60", name="")
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
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), surprise, win = win))
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


if (FALSE){
  x = read_export(by = 'year')
  nms = colnames(x)
  for (i in 2:ncol(x)) {
    cat(nms[i], "\n")
    x[[i]] = surprise(x[[i]])
  }
}