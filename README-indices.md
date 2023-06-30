GOM-series NOAA Interannual Climate Indices
================

[NOAA Interannual Climate
Indices](https://psl.noaa.gov/data/climateindices/list/)

``` r
source("setup.R")
```

## Atlantic Multidecadal Oscillation (AMO) Index - long version

Enfield, D.B., A. M. Mestas-Nunez and P.J. Trimble, 2001: The Atlantic
multidecadal oscillation and it’s relation to rainfall and river flows
in the continental U.S.. Geophysical Research Letters, Vol. 28,
2077-2080.

``` r
amo_wide <- fetch_amo(form="wide") |>
  glimpse()
```

    ## Rows: 167
    ## Columns: 13
    ## $ year <dbl> 1856, 1857, 1858, 1859, 1860, 1861, 1862, 1863, 1864, 1865, 1866,…
    ## $ jan  <dbl> 0.243, 0.238, -0.197, -0.109, 0.138, 0.023, -0.006, -0.125, -0.18…
    ## $ feb  <dbl> 0.176, -0.035, -0.290, -0.097, -0.089, -0.087, -0.142, -0.023, -0…
    ## $ mar  <dbl> 0.248, -0.050, -0.046, 0.083, 0.084, 0.016, -0.051, 0.049, -0.161…
    ## $ apr  <dbl> 0.167, 0.032, 0.235, 0.173, -0.046, 0.270, -0.104, 0.071, -0.015,…
    ## $ may  <dbl> 0.219, -0.008, 0.113, 0.110, 0.192, 0.217, -0.160, 0.044, 0.093, …
    ## $ jun  <dbl> 0.241, 0.124, -0.016, -0.078, 0.326, 0.314, -0.023, -0.021, 0.136…
    ## $ jul  <dbl> 0.255, 0.156, -0.137, -0.158, 0.252, 0.436, -0.272, -0.273, 0.281…
    ## $ aug  <dbl> 0.232, 0.022, -0.047, 0.086, 0.062, 0.324, -0.246, -0.279, 0.368,…
    ## $ sep  <dbl> 0.299, 0.043, 0.094, 0.103, 0.018, 0.281, -0.235, -0.137, 0.215, …
    ## $ oct  <dbl> 0.149, -0.110, 0.172, 0.151, 0.132, 0.236, -0.244, -0.213, 0.146,…
    ## $ nov  <dbl> 0.159, -0.144, 0.299, 0.083, 0.015, 0.238, -0.331, -0.157, 0.207,…
    ## $ dec  <dbl> 0.253, -0.259, 0.146, 0.105, 0.164, 0.137, -0.337, -0.095, 0.209,…

``` r
amo_long <- fetch_amo(form = "long") |>
  glimpse()
```

    ## Rows: 2,004
    ## Columns: 2
    ## $ date <date> 1856-01-01, 1856-02-01, 1856-03-01, 1856-04-01, 1856-05-01, 1856…
    ## $ amo  <dbl> 0.243, 0.176, 0.248, 0.167, 0.219, 0.241, 0.255, 0.232, 0.299, 0.…

``` r
ggplot(amo_long, aes(x=date, y=amo)) +
  geom_line()
```

![](README-indices_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## North Atlantic Oscillation (NAO) Index

Hurrell, J.W., 1995: Decadal trends in the North Atlantic Oscillation
and relationships to regional temperature and precipitation. Science
269, 676-679.

Jones, P.D., Jónsson, T. and Wheeler, D., 1997: Extension to the North
Atlantic Oscillation using early instrumental pressure observations from
Gibraltar and South-West Iceland. Int. J. Climatol. 17, 1433-1450.

``` r
fetch_nao(form="wide") |>
  glimpse()
```

    ## Rows: 73
    ## Columns: 13
    ## $ year <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,…
    ## $ jan  <dbl> 0.56, -0.42, 0.57, -0.12, -0.08, -2.65, -0.76, 0.71, -1.14, -1.52…
    ## $ feb  <dbl> 0.01, 0.35, -1.38, -1.00, 0.40, -1.71, -1.71, -0.32, -1.64, 0.33,…
    ## $ mar  <dbl> -0.78, -1.47, -1.97, -0.45, -1.27, -0.96, -0.46, -1.73, -2.46, -0…
    ## $ apr  <dbl> 0.65, -0.38, 0.95, -1.96, 1.31, -0.60, -1.30, 0.39, 0.26, 0.25, 1…
    ## $ may  <dbl> -0.50, -0.50, -0.99, -0.56, -0.03, -0.26, 2.10, -0.68, -0.17, 0.4…
    ## $ jun  <dbl> 0.25, -1.35, -0.10, 1.41, 0.06, -0.80, 0.41, -0.42, -1.08, 0.71, …
    ## $ jul  <dbl> -1.23, 1.39, -0.06, 0.43, -0.57, 1.78, -0.72, -1.16, -1.69, 0.77,…
    ## $ aug  <dbl> -0.19, -0.41, -0.49, -1.04, -2.57, 1.25, -1.89, -0.83, -2.13, -0.…
    ## $ sep  <dbl> 0.39, -1.18, -0.38, -0.19, -0.28, 0.46, 0.38, -1.47, 0.08, 1.00, …
    ## $ oct  <dbl> 1.43, 2.54, -0.28, 1.95, 1.16, -1.09, 1.47, 1.95, 0.68, 1.48, -1.…
    ## $ nov  <dbl> -1.46, -0.54, -1.32, 0.96, 0.29, -1.49, 0.40, 0.63, 1.59, 0.30, -…
    ## $ dec  <dbl> -1.03, 1.13, -0.49, -0.52, 0.55, 0.07, 0.00, 0.02, -0.74, 0.32, -…

``` r
nao_long <- fetch_nao(form = "long") |>
  glimpse()
```

    ## Rows: 876
    ## Columns: 2
    ## $ date <date> 1950-01-01, 1950-02-01, 1950-03-01, 1950-04-01, 1950-05-01, 1950…
    ## $ nao  <dbl> 0.56, 0.01, -0.78, 0.65, -0.50, 0.25, -1.23, -0.19, 0.39, 1.43, -…

``` r
ggplot(nao_long, aes(x=date, y=nao)) +
  geom_line()
```

![](README-indices_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
