# Download data for all stations in one or more provinces

Iterates over all stations in one or more selected provinces and
downloads hourly data for each.

## Usage

``` r
download_station_province(
  province,
  station_data = NULL,
  out_dir = NULL,
  first_year = NULL,
  last_year = NULL,
  parallel = FALSE,
  confirm = FALSE
)
```

## Arguments

- province:

  Character. Province name(s) or abbreviation(s). Can be a single
  province or a vector of provinces.

- station_data:

  Data frame of station metadata. Optional.

- out_dir:

  Character. Base output directory. If not supplied, defaults to
  `file.path(getwd(), "drifloon_output")`.

- first_year:

  Numeric. Optional starting year.

- last_year:

  Numeric. Optional ending year.

- parallel:

  Logical. If `TRUE`, stations are downloaded in parallel using
  [`furrr::future_walk`](https://furrr.futureverse.org/reference/future_map.html).
  Default is `FALSE`.

- confirm:

  Logical. If `FALSE` (default), displays estimated download size and
  requests user confirmation. If `TRUE`, skips the confirmation prompt
  and proceeds with download.

## Value

Invisibly returns `NULL`.

## Details

Could trigger a very large number of downloads when using multiple
provinces. By default, an estimated file count and space requirement is
shown before downloading begins. Set `confirm = TRUE` to skip this
warning.

When multiple provinces are provided, provinces that have no stations or
fail to match will issue a warning and skip to the next province.
