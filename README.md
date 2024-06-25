
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sepa

<!-- badges: start -->

[![R-CMD-check](https://github.com/simonmoulds/sepa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonmoulds/sepa/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The sepa package provides access to environmental data collected by the
Scottish Environment Protection Agency (SEPA).

## Installation

You can install the development version of sepa from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("simonmoulds/sepa")
#> Skipping install of 'sepa' from a github remote, the SHA1 (0f85c57e) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(sepa)
```

The steps required to download historical streamflow data are outlined
below:

``` r
grps <- sepa_group_list()
q_grp <- grps |> filter(group_name %in% "StationsWithFlow") |> pull(group_id)
# List stations that measure flow
stns <- sepa_station_list(group_id = q_grp)
# Choose arbitrary station
stn_id <- stns$station_id[1]
available_ts <- sepa_timeseries_list(stn_id)
# Get the timeseries ID
ts_id <- available_ts |> 
  filter(stationparameter_name == "Flow" & ts_name == "Day.Mean") |> 
  pull(ts_id)
# Retrieve data
ts <- sepa_timeseries_values(ts_id, start_date = as.Date("2000-01-01"), end_date = Sys.Date())
```

Once we have obtained the data we can plot the timeseries:

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
