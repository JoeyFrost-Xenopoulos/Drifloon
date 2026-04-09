# Sync station metadata from official inventory CSV

Downloads the current station inventory CSV, applies filtering, compares
it to the current local metadata, and updates local CSV/RDS files only
when content has changed (or when forced).

## Usage

``` r
.sync_station_metadata(
  station_csv_url =
    "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
  csv_path = file.path("extdata", "HLY_station_info.csv"),
  rds_path = file.path("data", "HLY_station_info.rds"),
  force = FALSE,
  create_backup = TRUE,
  max_backup_bytes = 25 * 1024^2,
  require_hourly = TRUE,
  provinces = NULL,
  min_year = 1980,
  max_year = NULL,
  downloader = download.file
)
```

## Arguments

- station_csv_url:

  Character. Source URL for station inventory CSV.

- csv_path:

  Character. Local output path for filtered CSV metadata.

- rds_path:

  Character. Local output path for filtered RDS metadata.

- force:

  Logical. If TRUE, writes files even when data are unchanged.

- create_backup:

  Logical. If TRUE, backup current metadata before update when current
  metadata is valid and small enough.

- max_backup_bytes:

  Numeric. Maximum combined size of current CSV+RDS files eligible for
  backup. Default is 25 MB.

- require_hourly:

  Logical. If TRUE, keep only rows with hourly ranges.

- provinces:

  Character vector of provinces to keep (optional).

- min_year:

  Numeric. Minimum year overlap filter. Default is 1980.

- max_year:

  Numeric. Optional maximum year overlap filter.

- downloader:

  Function used to download files (default: download.file).

## Value

Invisibly returns a list with update status, modified-date text, and
output paths.
