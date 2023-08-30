#' Given a single surprise time series - show as plot, highlighting one or
#' more dates
plot_surprise_series = function(x = compute_surprise_series(),
                                highlight = dplyr::slice(x, floor(nrow(x)/2)) |> pull(date),
                                surprise_window = attr(x, "surprise_window"),
                                surprise_threshold = 2,
                                title = NULL,
                                caption = sprintf("window: %i, threshold: %i",
                                                  surprise_window,
                                                  surprise_threshold)){
  
  if (FALSE){
    x = compute_surprise_series()
    highlight = as.Date("2015-01-01") #dplyr::slice(x, floor(nrow(x)/2)) |> pull(date)
    surprise_window = attr(x, "surprise_window")
    surprise_threshold = 2
    title = NULL
    caption = NULL
  }
  
  if (inherits(highlight, "numeric")) highlight = sprintf("%0.4i-01-01", highlight)
  if (!inherits(highlight, 'Date')) highlight = as.Date(highlight)
  
  name = names(x)[2]
  
  gg = ggplot(data = x, aes(date, .data[[name]])) +
    
    ggplot2::geom_point(show.legend = FALSE, 
                        shape = 1, 
                        size = 0.5) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::geom_point(data = dplyr::filter(x, !between(surprise, -surprise_threshold, surprise_threshold)), 
                        ggplot2::aes(date, .data[[name]]),
                        shape = 16,
                        size = 3,
                        show.legend = FALSE) +
    ggplot2::labs(
      x = NULL, 
      y = name, 
      title = title,
      caption = caption) + 
    ggplot2::theme(legend.title=ggplot2::element_blank())
  
  make_segment = function(x, the_date, surprise_window = 20, surprise_threshold = 2){
    
    name = colnames(x)[2]
    r = dplyr::slice(x, which(findInterval(x$date, the_date) == 0))
    if (nrow(r) > surprise_window) r = dplyr::slice_tail(r, n = surprise_window)
    
    y = dplyr::filter(x, date == the_date)
    v = predict(y$fit[[1]], newdata = dplyr::bind_rows(r,y))
    n = nrow(r)
    
    line = ggplot2::geom_segment(data = tibble(x1 = r$date[1], 
                                               x2 = y$date, 
                                               y1 = v[1], 
                                               y2 = v[n]),
                                 aes(x = x1,
                                     y = y1,
                                     xend = x2,
                                     yend = y2),
                                 show.legend = FALSE,
                                 color = 'blue')
    
    upper = ggplot2::geom_segment(data = tibble(x1 = r$date[1], 
                                                x2 = y$date, 
                                                y1 = v[1] + y$surprise, 
                                                y2 = v[n] + y$surprise),
                                  aes(x = x1,
                                      y = y1,
                                      xend = x2,
                                      yend = y2),
                                  linetype = 'dotted',
                                  show.legend = FALSE,
                                  color = 'blue')
    
    upper_thresh = ggplot2::geom_segment(data = tibble(x1 = r$date[1], 
                                                       x2 = y$date, 
                                                       y1 = v[1] + surprise_threshold, 
                                                       y2 = v[n] + surprise_threshold),
                                         aes(x = x1,
                                             y = y1,
                                             xend = x2,
                                             yend = y2),
                                         linetype = 'dashed',
                                         show.legend = FALSE,
                                         color = 'orange')
                          
    lower = ggplot2::geom_segment(data = tibble(x1 = r$date[1], 
                                                x2 = y$date, 
                                                y1 = v[1] - y$surprise, 
                                                y2 = v[n] - y$surprise),
                                  aes(x = x1,
                                      y = y1,
                                      xend = x2,
                                      yend = y2),
                                  linetype = 'dotted',
                                  show.legend = FALSE,
                                  color = 'blue')
    
    lower_thresh = ggplot2::geom_segment(data = tibble(x1 = r$date[1], 
                                                       x2 = y$date, 
                                                       y1 = v[1] - surprise_threshold, 
                                                       y2 = v[n] - surprise_threshold),
                                         aes(x = x1,
                                             y = y1,
                                             xend = x2,
                                             yend = y2),
                                         linetype = 'dashed',
                                         show.legend = FALSE,
                                         color = 'orange')
    
    point = ggplot2::geom_point(data = y,
                                aes(date , .data[[name]]),
                                shape = 1,
                                size = 4,
                                color = 'blue')
    
    list(line, upper, upper_thresh, lower, lower_thresh, point)
  }
    
  if (!is.null(highlight)){
    
    #y = dplyr::select(x, dplyr::all_of(1:3)) |>
    #  na.omit()
    
    dd = lapply(seq_along(highlight),
                function(idate){
                  make_segment(x, highlight[idate], surprise_window = surprise_window,
                               surprise_threshold = surprise_threshold)
                })
    
    for (i in seq_along(dd)) {
      for (j in seq_along(dd[[i]])) gg = gg + dd[[i]][[j]]
    }    
    
  } # highlight
  gg
}

#' Given one time series (date and variable) compute surprise 
#' 
#' @param x table of data with date and variable (only one) 
#' @param surprise_window numeric, window width
#' @param min_present, numeric, the minimum number of non-NA points required
#' @return tibble with date, variable, dev, fit (lm object) and surprise 
compute_surprise_series = function(x = read_export(by = 'year', 
                                                   selection = read_target_vars(treatment = c("median")),
                                                   replace_names = TRUE, 
                                                   standardize = FALSE) |>
                                     dplyr::filter(date >= as.Date("1950-01-01")) |>
                                     dplyr::select(dplyr::all_of(c("date", "WMCC (HAB)"))),
                                   surprise_window = 20,
                                   min_present = 10){
  
  if (nrow(x) < surprise_window) stop("input must have more rows than window size")
  
  #select only for date and one variable
  idx = sapply(x, is.numeric)
  ix = which(!idx)[1]
  iy = which(idx)[1]
  x = dplyr::select(x, dplyr::all_of(c(ix, iy)))
  
  # compute surprise, 
  surprise_one = function(x, n = surprise_window, min_nonna = min_present){
    y = dplyr::slice_head(x, n=n) # the window
    x = dplyr::slice_tail(x, n=1) # the next one after the window
    
    isnotna = !is.na(y[[2]])
    if (sum(isnotna) < min_nonna) {
      x = mutate(x, fit = list(NULL), dev = NA_real_, surprise = NA_real_)
    } else {
      name = names(y)[2]
      formulah = paste0("`", name, "`", " ~ date")
      fit <- lm(as.formula(formulah), data = y, na.action = na.exclude)
      dev <- x[[name]] - predict(fit, newdata = dplyr::select(x,1))
      surprise = dev / sd(fit$residuals)
      x = mutate(x, fit = list(fit), dev = dev, surprise = surprise)
    } 
    x
  }
  
  index = seq_len(surprise_window + 1) - 1
  r = lapply(seq(from = surprise_window + 1, to = nrow(x), by = 1 ),
             function(i){
               index <<- index + 1
               surprise_one(dplyr::slice(x, index))
             }) |>
    dplyr::bind_rows()
  
  attr(r, "surprise_window" ) <- surprise_window
  r
}