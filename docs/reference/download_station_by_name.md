# Download station data by name or station ID

Wrapper to download data for a station identified by either its name or
station ID.

## Usage

``` r
download_station_by_name(
  station_name = NULL,
  station_id = NULL,
  out_dir = NULL,
  station_data = NULL,
  first_year = NULL,
  last_year = NULL,
  parallel = FALSE
)
```

## Arguments

- station_name:

  Character. Station name (optional).

- station_id:

  Numeric. Station ID (optional).

- out_dir:

  Character. Base output directory. If not supplied, defaults to
  `file.path(getwd(), "drifloon_output")`.

- station_data:

  Data frame containing station metadata. Optional; if omitted, packaged
  metadata is loaded automatically.

- first_year:

  Numeric. First year to download (optional).

- last_year:

  Numeric. Last year to download (optional).

- parallel:

  Logical. If `TRUE`, months are downloaded in parallel using
  [`furrr::future_pwalk`](https://furrr.futureverse.org/reference/future_map2.html).
  Default is `FALSE`.

## Value

Invisibly returns `NULL`.

## Details

Either `station_name` or `station_id` must be provided. If both are
provided, `station_name` takes precedence. If `first_year` or
`last_year` are outside the station metadata range (`HLY.First.Year` to
`HLY.Last.Year`), an error is thrown.
