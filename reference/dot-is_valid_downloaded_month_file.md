# Download one station file by station-year-month

Internal helper used by wrappers and station-level downloader.

## Usage

``` r
.is_valid_downloaded_month_file(path)
```

## Arguments

- station_id:

  Numeric. The station ID.

- station_name:

  Character. Station name (used for file naming).

- station_folder:

  Character. Directory where files will be saved.

- year:

  Numeric. Year to download.

- month:

  Numeric. Month to download (1–12).

- downloader:

  Function. Download function.

- sleeper:

  Function. Sleep function for throttling.

## Value

Invisibly returns `TRUE` if successful, `FALSE` otherwise.
