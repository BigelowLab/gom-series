GOM-Series Buoys
================

``` r
suppressPackageStartupMessages({
  library(cofbb)
  library(ggOceanMaps)
})
source("setup.R")
```

## Buoy data aggregated to monthly means

5 buoys from across the Gulf of Maine were selected for inclusion. A
listing of these is saved in `data/buoy-listings.csv` and can be read as
simple look-up table (lut).

``` r
buoys = buoy_lut()
buoys
```

    ## # A tibble: 6 × 5
    ##   name  longname            id      lon   lat
    ##   <chr> <chr>               <chr> <dbl> <dbl>
    ## 1 wms   Western Maine Shelf B01   -70.4  43.2
    ## 2 cms   Central Maine Shelf E01   -69.4  43.7
    ## 3 pb    Penobscot Bay       F01   -69.0  44.1
    ## 4 ems   Eastern Maine Shelf I01   -68.1  44.1
    ## 5 jb    Jordan Basin        M01   -67.9  43.5
    ## 6 nec   Northeast Channel   N01   -65.9  42.3

``` r
bb <- cofbb::get_bb("gom")
basemap(limits = bb, bathymetry = TRUE) + 
  geom_point(data = buoys, aes(x = lon, y = lat), 
             color = "orange") +
  geom_text(data = buoys, aes(x = lon, y = lat, 
                              label = id,
                              hjust = 0.5, 
                              vjust = 1.2))
```

![](README-buoys_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## MET

### Fetch meteorological (met) data

Meteorological data for these buoys can be fetched using
`fetch_buoy_met()`. This function downloads high temporal resolution
data per buoy, and aggregates into monthly means, and saved to disk a
simple table. Run this as needed to update data.

    met <- lapply(buoys$id, fetch_buoy_met)

### Read and display met data

Read one or more buoy met data files using `read_buoy_met()`. By default
all buoys are read and boundinto one table.

``` r
x <- read_buoy_met() |>
  dplyr::mutate(month = format(date, "%b"), .after = date) |>
  dplyr::group_by(buoy)

ggplot(data = x, aes(x = date, y = wind_speed, color = buoy, shape = buoy)) +
  geom_line()
```

![](README-buoys_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(data = filter(x, month == 'Aug'), 
       aes(x = date, y = air_temperature, color = buoy)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(x = "year", y = 'Air Temp (C)', title = "August - mean monthly air temperature")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README-buoys_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## CTD at depth (temp, salinity, sigma_t)

### Fetch CTD data

CTD data for these buoys can be fetched using `fetch_buoy_ctd()`. This
function downloads high temporal resolution data per buoy at various
depths, and aggregates into monthly means, and saved to disk a simple
table. Run this as needed to update data.

    ctd <- lapply(buoys$id, fetch_buoy_ctd)

### Read and display CTD data

Read one or more buoy CTD data files using `read_buoy_ctd()`. By default
all buoys are read and bound into one table.

``` r
x <- read_buoy_ctd() |>
  dplyr::mutate(month = format(date, "%b"), .after = date) |>
  dplyr::group_by(buoy)

ggplot(data = x, aes(x = date, y = temperature, color = depth)) +
  scale_y_reverse()  + 
  geom_line() + 
  facet_wrap(~buoy)
```

![](README-buoys_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
