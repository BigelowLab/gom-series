Figure 4
================

``` r
source("../setup.R")
```

    ## here() starts at /mnt/s1/projects/ecocast/projects/nrecord/gom-series

Below is a plot that shows standardized departures relative to the long
term mean with “surprises” indicated with a dot.

``` r
surprise_window = 20
surprise_threshold = 2
x = read_export(by = 'year', 
                selection = read_target_vars(treatment = c("median")),
                replace_names = TRUE, 
                standardize = FALSE) |>
    dplyr::filter(date >= as.Date("1900-01-01"))
```

``` r
plot_departure_surprise(x, surprise_window = surprise_window, delimit_surprise = FALSE)
```

![](README-fig4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Group bar graphs

``` r
vmeta = read_var_meta()
lut = vmeta$group
names(lut) = vmeta[['human_readable_name']]
surprise_window = 20
surprise_threshold = 2
x = read_export(by = 'year', 
                selection = read_target_vars(treatment = c("median")),
                replace_names = TRUE, 
                standardize = FALSE) |>
    dplyr::filter(date >= as.Date("1900-01-01"))
sites = colnames(x)[-1]
s = surprise(x, win = surprise_window) 
z = recode_surprise(s, surprise_threshold = surprise_threshold)$labeled_data |>
  tidyr::pivot_longer(dplyr::any_of(sites), names_to = "name", values_to = "surprise") |>
  dplyr::mutate(group = lut[name],
                year = format(date, "%Y") |> as.numeric())
```

So who belongs to what group?

``` r
dplyr::summarise(z, items = paste(unique(.data$name), collapse = ", "), .by = "group") 
```

    ## # A tibble: 6 × 2
    ##   group items                                                                   
    ##   <chr> <chr>                                                                   
    ## 1 fwi   Androscoggin River, Narraguagus River                                   
    ## 2 wx    Durham Tmax, Durham Tmin, Blue Hill Tmax, Blue Hill Tmin, Corinna Tmax,…
    ## 3 index NAO, AMO, GSI                                                           
    ## 4 sst   ERSST, EMCC (SST), GBK (SST), GBN (SST), JBN (SST), WMCC (SST), WBN (SS…
    ## 5 chl   EMCC (Chl), GBK (Chl), GBN (Chl), JBN (Chl), WMCC (Chl), WBN (Chl)      
    ## 6 bio   PCI spring, PCI fall, WMCC (HAB), EMCC (HAB), Cal spring, Cal fall

Make a stacked series of barplots.

``` r
ns = z |>
  dplyr::filter(!is.na(surprise) & surprise != "no surprise") |>
  dplyr::summarise(surprises = n(), .by = dplyr::all_of(c("year", "group")))

ggplot(data = ns, aes(x = year, y = surprises)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ group,  ncol = 1)
```

![](README-fig4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Looking at time series - blech

``` r
sites = c("ERSST", "EMCC (HAB)")
y = x |>
  dplyr::select(dplyr::any_of(c("date", sites)))

s = surprise(y, win = surprise_window) 

z = recode_surprise(s, surprise_threshold = surprise_threshold)$labeled_data |>
  tidyr::pivot_longer(dplyr::any_of(sites), names_to = "name", values_to = "surprise") 

s = s |>
  tidyr::pivot_longer(dplyr::any_of(sites), names_to = "name", values_to = "value") |>
  mutate(surprise = z$surprise) |>
  na.omit()

 
ggplot(data = s, aes(x = date, y = value)) + 
  lims(x = as.Date(c("1970-01-01", "2022-01-01"))) + 
  labs(y = "Standarized Departure") + 
  geom_point() + 
  geom_point(data = droplevels(s, exclude = "no surprise") |> na.omit(),
             aes(x = date, y = value, color = surprise), size = 2) + 
    ggplot2::geom_smooth(aes(x = date, y = value), 
                         se = FALSE,  
                         linewidth = 0.75, 
                         show.legend = TRUE,
                         method = 'loess', 
                         formula = 'y ~ x') +
    geom_point(data = droplevels(s, exclude = "no surprise") |> na.omit(),
             aes(x = date, y = value, color = surprise), size = 2) + 
  facet_wrap(~name, ncol = 1)
```

![](README-fig4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
