GOM-Series
================

# Code Base

All code is written in [R language](https://www.r-project.org/). The
[here](https://CRAN.R-project.org/package=here) is used to navigate the
project data directory and to make the project highly portable. One
primary file, `setup.R`, is sourced in order to install and load any
required libraries. Files located in the `functions` subdirectory are
automatically sourced.

A number of dataset specific READMEs are stored at the top level of the
projects.

# Local data storage

We minimize local data storage demand by opting to dynamically fetching
data online when it is reasonable to do so, and to compress all stored
files.

# Datasets

# Base functionality for each dataset

-   `fetch_xyz()` downloads a dataset, possibly aggregating to
    monthly/yearly values, and possibly storing locally

-   `read_xyz()` reads a dataset - possibly by fetching with
    `fetch_xyz()`

-   `complete_intervals_xyz(by = c("month", "year"))` clips a dataset to
    complete intervals only. A month is considered complete with 28 or
    more days. A year is considered complete with 336 days (daily data)
    or 12 months (monthly data).

-   `aggregate_xyz(by = c("month", "year", ...))` aggregates by month or
    year, when possible each parameter is summarized with the `sixnum`
    function which yields `min`, `q25`,`median`, `'mean`, `q75` and
    `max`).

-   `export_xyz(by = c("month", "year"))` exports data into a
    wide-format suitable for joining with other datasets
