#' Given a LONG assembly, make a time-series plot
#'
#' @param x long assembly table
#' @param caption NULL or char, for adding a caption
#' @param title NULL or char, for adding a title
#' @param what char or of 'std' or 'raw' to control what gets plotted
#' @param facet logical, if TRUE facet by name
#' @return ggplot object
plot_departure_surprise_series = function(x = assemble_departure_surprise()$long |>
                                            dplyr::filter(grepl("SST", name, fixed = TRUE)),
                                          caption = NULL,
                                          title = NULL,
                                          what = c('std', 'raw')[1],
                                          facet = FALSE){

  
  gg = ggplot2::ggplot(x, ggplot2::aes(date, .data[[what]])) +
    ggplot2::geom_point(ggplot2::aes(color = name), 
               show.legend = FALSE, 
               shape = 1, 
               size = 0.5) +
    ggplot2::geom_smooth(ggplot2::aes(group = name, color = name), 
                se = TRUE, 
                linewidth = 0.75, 
                show.legend = TRUE,
                method = 'loess', formula = 'y ~ x') +
  
    ggplot2::geom_point(data = dplyr::filter(x, surprise), 
                        ggplot2::aes(date, .data[[what]], color = name),
               shape = 16,
               size = 2) +
    ggplot2::labs(
      x = NULL, 
      y = NULL, 
      title = title,
      caption = caption) + 
    
    ggplot2::theme(legend.title=ggplot2::element_blank())
  
  if (facet) gg = gg + facet_wrap(~name)
  
  gg
  
}



#' Quickly assemble the all important long-form tally by data and name of
#' the standardized score, the surprise label and a logical of surprise
#' @param x tibble, not-standarized (so we can compute surprise) with human-readable names
#' @param surprise_window numeric, the width of sliding window
#' @param surprise_threshold numeric, number of standard deviatiosn that define a surprise
#' @param display_names char, the name in order to display 
assemble_departure_surprise = function(x = read_export(by = 'year', 
                                                       selection = read_target_vars(treatment = c("median")),
                                                       replace_names = TRUE, 
                                                       standardize = FALSE) |>
                                         dplyr::filter(date >= as.Date("1950-01-01")),
                                       surprise_window = 20, 
                                       surprise_threshold = 2,
                                       display_names =  get_display_names()){
  
  x <- x[c("date", display_names)]
  y = long_export(x) |>
    dplyr::rename(raw = value)
  
  s = surprise(x, win = surprise_window)
  
  z = recode_surprise(s, surprise_threshold = surprise_threshold)
  labels = long_export(z$labeled_data) |>
    dplyr::rename(label = value) |>
    dplyr::mutate(surprise = label %in% c("-surprise", "+surprise"))
  
  long = standardize_export(x) |>
    long_export() |>
    dplyr::left_join(labels, by = c("date", "name")) |>
    dplyr::left_join(y, by = c("date", "name")) |>
    dplyr::rename(std = value) |>
    dplyr::relocate(raw, .before = std ) |>
    dplyr::mutate(name = factor(name, levels = rev(display_names)))
  
  rng = range(long$std, na.rm = TRUE) |> abs() |> max()
  rng = rng * c(-1, 1)
  
  list(
    surprise_window = surprise_window, 
    surprise_threshold = surprise_threshold,
    range = rng, 
    long = long
  )
}


#' This plots departures (standardized long term) with surprises shown as dots
#'
#' @param x tibble, not-standarized (so we can compute surprise) with human-readable names
#' @param surprise_window numeric, the width of sliding window
#' @param min_n numeric, the number of non-NA values in a window
#' @param surprise_threshold numeric, number of standard deviation that define a surprise
#' @param replace_names logical, if TRUE then use simple names
#' @param clip_window logical, if TRUE then clip columns to left that are all NA
#' @param pruge_empty logical if TRUE clip empty rows
#' @param title char or NULL  if NULL then suppress title
#' @param y_text_angle numeric, degress to rotate the y-text labels
#' @param display_names char, the name in order to display 
#' @param delimit_surprise logical, if TRUE add a small vertical line on each row
#'   that delimits where the window allows the surprise computation to begin
#' @param mask_surprise logical, if TRUE mask out cells before the surprise can be computed
#' @return ggplot object
plot_departure_surprise = function(x = read_export(by = 'year', 
                                          selection = read_target_vars(treatment = c("median")),
                                          replace_names = TRUE, 
                                          standardize = FALSE) |>
                            dplyr::filter(date >= as.Date("1900-01-01")),
                          surprise_window = 20, 
                          min_n = 10, 
                          surprise_threshold = 2,
                          clip_window = TRUE,
                          purge_empty = FALSE,
                          title = NULL,
                          caption = "auto",
                          y_text_angle = 0,
                          display_names =  get_display_names(),
                          delimit_surprise = TRUE,
                          mask_surprise = !delimit_surprise){
  if (FALSE){
    x = read_export(by = 'year', 
                    selection = read_target_vars(treatment = c("median")),
                    replace_names = TRUE, 
                    standardize = FALSE) |>
      dplyr::filter(date >= as.Date("1900-01-01"))
    surprise_window = 20
    min_n = 10
    surprise_threshold = 2
    clip_window = TRUE
    purge_empty = FALSE
    title = NULL
    caption = "auto"
    y_text_angle = 0
    display_names =  get_display_names()
    delimit_surprise = FALSE
    mask_surprise = !delimit_surprise
  }
  
  
  if (!is.null(caption) && caption[1] == "auto"){
    caption = sprintf("window: %i, threshold: %i", surprise_window, surprise_threshold)
  }
  
  x <- dplyr::select(x, dplyr::any_of(c("date", display_names)))

  s = surprise(x, win = surprise_window)
  z = recode_surprise(s, surprise_threshold = surprise_threshold)
  labels = long_export(z$labeled_data) |>
    dplyr::rename(label = value) |>
    dplyr::mutate(surprise = label %in% c("-surprise", "+surprise"))
  
  long = standardize_export(x)
  long = long |>
    long_export() |>
    dplyr::left_join(labels, by = c("date", "name")) |>
    dplyr::mutate(name = factor(name, levels = rev(display_names)))
  
  rng = range(long$value, na.rm = TRUE) |> abs() |> max()
  rng = rng * c(-1, 1)
  
  as_year = function(x) format(x, "%Y")
  gg = ggplot2::ggplot(long, ggplot2::aes(date, name)) +
    ggplot2::scale_x_date(breaks = seq(from = as.Date("1900-01-01"),
                                            to = as.Date("2020-01-01"),
                                            by = "10 years"), 
                          labels = as_year) 
  if (mask_surprise){
    dat = dplyr::group_by(long, name) |>
      dplyr::group_map(
        function(tbl, key, win = 20, min_n = 10){
          ix = which(!is.na(tbl$value))
          if (length(ix > 0) ){
            tbl$value[seq_len(ix[1] + win - min_n - 1)] <- NA
          }
          tbl
        }, win = surprise_window, min_n = min_n, .keep = TRUE) |>
      dplyr::bind_rows()
    gg = gg +  
      ggplot2::geom_tile(data = dat, ggplot2::aes(fill = value), colour = get_color("grey90")) 
  } else {
    gg = gg +  
      ggplot2::geom_tile(ggplot2::aes(fill = value), colour = get_color("grey90")) 
  }
  
  gg = gg +
    ggplot2::scale_fill_gradient2(low = get_color("blue"), 
                                  high = get_color("red"), 
                                  na.value = get_color("grey75"), 
                                  name = "value",
                                  limits = rng) + 
    ggplot2::geom_point(data = dplyr::filter(long, surprise), # these are the surprises
                   aes(date, name),
                   color = get_color("black"),
                   alpha = 0.8,
                   size = 1,
                   show.legend = FALSE) 
  if (delimit_surprise){
    boxes = dplyr::select(long, dplyr::all_of(c("date", "name", "value"))) |>
      dplyr::group_by(name) |>
      dplyr::group_map(
        function(tbl, key){
          ix = which(!is.na(tbl$value))              # assumed first record
          dplyr::slice(tbl, ix[1] + surprise_window - min_n) # assumed first (possible) surprise
        }, .keep = TRUE) |>
      dplyr::bind_rows() |>
      dplyr::mutate(width = 365, height = 1)
    gg = gg + 
      geom_tile(data = boxes, 
                aes(x = date, y = name, width = width, height = height, fill = value),
                color = get_color("black"),
                linewidth = 0.2)
  }
    gg + ggside::geom_xsidecol(mapping = aes(date, surprise),  # this is the histogram of counts along top
                          data = z$profile_data,
                          show.legend = FALSE,
                          orientation = 'x') + 
    ggplot2::labs(x = NULL, 
                  y = NULL,
                  title = title,
                  caption = caption) +
    ggplot2::scale_y_discrete(name = NULL, 
                              guide = guide_axis(angle = y_text_angle)) + 
    ggplot2::theme_gray() + 
    ggplot2::theme(axis.text.x = element_text(size=10),
                   legend.title=element_blank()) 
}



#' This is a wrapper around plot_surprise, but the surprise argument is always NULL
#' 
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
plot_surprise = function(x = read_export(by = 'year', 
                                         selection = read_target_vars(treatment = c("median")),
                                         replace_names = TRUE, 
                                         standardize = FALSE) |>
                           dplyr::filter(date >= as.Date("1950-01-01")) |>
                           dplyr::select(date, dplyr::contains(c("nao", "amo", "gsi"))) |>
                           surprise(win = win) |>
                           dplyr::filter(date >= as.Date("1980-01-01")) , 
                         surprise = 2,
                         clip_window = TRUE,
                         purge_empty = FALSE,
                         title = 'auto',
                         y_text_angle = 0,
                         cnames = get_display_names()){
  
  if (FALSE){
    x = read_export(by = 'year', 
                    selection = read_target_vars(treatment = c("median")),
                    replace_names = TRUE, 
                    standardize = FALSE) |>
      dplyr::filter(date >= as.Date("1950-01-01")) |>
      dplyr::select(date, dplyr::contains(c("nao", "amo", "gsi"))) |>
      surprise(win = win) |>
      dplyr::filter(date >= as.Date("1980-01-01"))
    
    
    surprise = 2
    clip_window = TRUE
    purge_empty = FALSE
    title = 'auto'
    y_text_angle = 0
    cnames =  get_display_names()
  }
  
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
      
    dat = as.matrix(dplyr::select(x, -date)) |>
      apply(1, function(x){
        sum(findInterval(x, isurprise) != 1, na.rm = TRUE)/length(x)
      })
      
    profile_data = dplyr::select(x, date) |>
      dplyr::mutate(surprise = dat)
  
    x = dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), \(x) recode_surprise(x, vals = isurprise)))
    rng = c(-surprise, surprise)

    
  } else {
    rng = table_range(dplyr::ungroup(x), na.rm = TRUE, collapse = TRUE)
    if (sign(rng[1]) < 0 && sign(rng[2]) > 0){
      rng = rep(max(rng),2) * c(-1, 1)    
    } 
  } # surprise or departure?
  
  #cnames = dplyr::select(x, -date) |> colnames()
  long = long_export(x) |>
    dplyr::mutate(name = factor(name, levels = rev(cnames))) |>
    tidyr::drop_na("name")
  
  gg = ggplot2::ggplot(long, ggplot2::aes(date, name)) +
    ggplot2::scale_x_date(date_minor_breaks = "10 years") +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "grey60") +
    ggplot2::labs(x = "", 
                  y = "", 
                  title = title) +
    ggplot2::scale_y_discrete(name = NULL, 
                              guide = guide_axis(angle = y_text_angle)) + 
    ggplot2::theme_gray() + 
    ggplot2::theme(axis.text.x = element_text(size=10),
                   legend.title=element_blank())
   
  
  if (!is.null(surprise)){
    gg = gg + 
      
      ggside::geom_xsidecol(mapping = aes(date, surprise),
                             data = profile_data,
                             show.legend = FALSE,
                            orientation = 'x') +
      
      ggplot2::scale_fill_discrete(drop = FALSE,
                                           breaks = c("+surprise",
                                                      "no surprise", 
                                                      "-surprise", 
                                                      NA_character_),
                                           type = c("+surprise" = get_color("red"),
                                                    "no surprise" = get_color("white"), 
                                                    "-surprise" = get_color("blue"), 
                                                    "NA" = get_color("grey60"))) 
  } else {
    gg = gg + ggplot2::scale_fill_gradient2(low = get_color("blue"), 
                                            high = get_color("red"), 
                                            na.value = get_color("grey60"), 
                                            name = "",
                                            limits = rng)
  }
  
  gg
}


#' Retrieve a list of ordered human-friendly names
#' 
#' @return character vector of names
get_display_names = function(){
  c("AMO", "NAO", "GSI", 
    "ERSST", #"Salinity",
    "WMCC (SST)", "EMCC (SST)", "GBK (SST)", "GBN (SST)", 
    "JBN (SST)", "WBN (SST)", 
    "Durham Tmin", "Blue Hill Tmin", "Corinna Tmin", 
    "Durham Tmax", "Blue Hill Tmax", "Corinna Tmax", 
    "Androscoggin River", "Narraguagus River",
    "WMCC (Chl)", "EMCC (Chl)", "GBK (Chl)", "GBN (Chl)", "JBN (Chl)", "WBN (Chl)", 
    "WMCC (HAB)", "EMCC (HAB)", 
    "PCI spring", "PCI fall", 
    "Cal spring", "Cal fall")
}

#' Given a table of surprise, threshold and compute a label
#' 
#' @param x a table of surprises (plus a column of date)
#' @param surprise_threshold numeric, defines +/- what constitutes a surprise
#' @param surprise_names char, the labels to attach in order of (low, none, high)
#' @return a list of profile_data (number of surprises per year),
#'   then labeled surprise data, the surprise_threshold, the surprise labels and the range of surprise values
recode_surprise = function(x, 
                           surprise_threshold = 2,
                           surprise_names = c("-surprise", "no surprise", "+surprise")){
  
  threshold = surprise_threshold * c(-1,1)
  
  recode_surprise_one = function(x, vals = c(-1, 1)){
    iy = !is.na(x)
    ix = findInterval(x[iy], vals) + 1
    newvals = surprise_names[ix]
    r = rep(NA_integer_, length(x))
    r[which(iy)] = newvals
    factor(r, levels = rev(surprise_names))
  }

  dat = as.matrix(dplyr::select(x, -date)) |>
    apply(1, function(x){
      sum(findInterval(x, threshold) != 1, na.rm = TRUE)/length(x)
    })
  
  profile_data = dplyr::select(x, date) |>
    dplyr::mutate(surprise = dat)
  
  labeled_data = dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), 
                                                \(x) recode_surprise_one(x, vals = threshold)))
  
  list(
    profile_data = profile_data,
    labeled_data = labeled_data,
    surprise = list(
      range = threshold,
      names = surprise_names
      )
  )
  
}


#' Compute surprise index from a single vector or for all numeric columns in a
#' data frame (suitable for the 'export')
#' 
#' @param datain numeric vector or a data.frame (tibble) with numeric-type variables
#' @param win numeric, width of the sliding window
#' @param min_n numeric, the minimum number of non-NA values to compute surprise
#' @return numeric vector of surprise indices (standardize departures)
#'  or if the input is a data.frame (tibble) then the input with mutated columns
surprise <- function(datain = read_export(by = 'year'), 
                     win = 30,
                     min_n = 10){
  
  if (FALSE){
    datain = read_export(by = 'year', 
                selection = read_target_vars(treatment = c("median")),
                replace_names = TRUE, 
                standardize = FALSE) |>
      dplyr::filter(date >= as.Date("1900-01-01"))
    min_n = 10
    win = 20
  }
  
  
  if (inherits(datain, "data.frame")){
      dataout = dplyr::ungroup(datain) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) surprise(x, win = win)))
      return(dataout)
  }
  
  if (min_n > win) stop(sprintf("min_n (%i) must be less than or equal to win (%i)",
                                min_n, win))
  nx = length(datain)
  if (nx <= win) stop("input must be longer than window")
  x = seq_len(nx)
  idx = seq_len(win) - 1L
  
  
  # a temporary function applied for each iteration applied per variable 
  # @param i iteration number
  # @param dat essentially datain
  # @param x the sequential index along dat
  # @param win window width
  # @param idx the sequence along the window
  # @return numeric surprise index
  surprise_one = function(i, dat = NULL, x = NULL, win = NULL, idx = NULL){
    ix = idx + (i - win)  
    this_y = dat[ix]
    isnotna = !is.na(this_y)
    if (sum(isnotna) < min_n) return(NA_real_)
    fit <- lm(y ~ x, data = data.frame(x = x[ix], y = this_y), na.action = na.exclude)
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
