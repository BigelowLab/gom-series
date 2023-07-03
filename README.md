GOM-Series
================

# Aggregating monthly oceanographic time series from the Gulf of Maine

# Base functionality for a dataset

-   `fetch_xyz()`

-   `read_xyz()`

-   `complete_intervals_xyz(by = c("month", "year", ...))`

-   `aggregate_xyz(by = c("month", "year", ...))` (`sixnum`)

-   `export_xyz(variable = "something", form = c("long", "wide"))`

# Merge exported datasets one wide annual

-   [ ] sst mean `year, oisst.geobasin.mean, oisst.geobank.mean, ...`
-   [ ] sst q25
-   [ ] sst q75
-   [ ] chlor mean `year, cmems.geobasin.mean, cmems.geobank.mean, ...`
-   [ ] chlor q25
-   [ ] chlor q75
-   [ ] usgs `year, usgs.123456.mean, usgs.123457.mean, ...`
-   [ ] buoy `year, buoy.A01.mean.sst, buoy.E01.mean.sst, ...`
