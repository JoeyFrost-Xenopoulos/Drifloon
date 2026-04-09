# Download full available hourly dataset for a single station

Internal download engine used by exported wrappers such as
\[download_station_by_name()\].

## Usage

``` r
download_station(
  station,
  out_dir = NULL,
  first_year = NULL,
  last_year = NULL,
  parallel = FALSE
)
```

## Arguments

- station:

  List or named vector containing station metadata. Must include
  `Station.ID`, `Name`, `Province`, `HLY.First.Year`, and
  `HLY.Last.Year`.

- out_dir:

  Character. Base directory where station folders will be created. If
  not supplied, defaults to `file.path(getwd(), "drifloon_output")`.

- first_year:

  Numeric. First year to download (default: station metadata).

- last_year:

  Numeric. Last year to download (default: station metadata).

- parallel:

  Logical. If `TRUE`, downloads months in parallel using
  [`furrr::future_pwalk`](https://furrr.futureverse.org/reference/future_map2.html).
  Default is `FALSE`.

## Value

Invisibly returns `NULL`.

## Details

Year ranges are validated against station metadata. If requested years
exceed available data, an error is thrown.

Data are downloaded for each year-month pair (months 1 through 12),
sequentially by default or in parallel when `parallel = TRUE`.
