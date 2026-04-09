# Download hourly climate data for a single station-year-month

Downloads one month of hourly climate data for a station selected by
name or station ID.

## Usage

``` r
download_station_month(
  station_name = NULL,
  out_dir = NULL,
  year,
  month,
  station_id = NULL,
  station_data = NULL,
  downloader = download.file,
  sleeper = Sys.sleep
)
```

## Arguments

- station_name:

  Character. Station name (optional).

- out_dir:

  Character. Base output directory. If not supplied, defaults to
  `file.path(getwd(), "drifloon_output")`.

- year:

  Numeric. Year to download.

- month:

  Numeric. Month to download (1–12).

- station_id:

  Numeric. Station ID (optional).

- station_data:

  Data frame containing station metadata. Optional; if omitted, packaged
  metadata is loaded automatically.

- downloader:

  Function. Download function (default: `download.file`). Useful for
  testing/mocking (testthat).

- sleeper:

  Function. Sleep function for throttling (default: `Sys.sleep`).

## Value

Invisibly returns `TRUE` if successful, `FALSE` otherwise.

## Details

Either `station_name` or `station_id` must be provided. If both are
provided, `station_name` takes precedence.
