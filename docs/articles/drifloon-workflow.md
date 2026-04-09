# Introduction to Drifloon

## Overview

The Drifloon package helps you download Canadian hourly weather station
data from the Environment and Climate Change Canada portal.

Core functions:

- [`load_metadata()`](../reference/load_metadata.md): Load station
  inventory used by all download wrappers.
- [`sync_metadata()`](../reference/sync_metadata.md): Checks and updates
  metadata from the official station inventory CSV.
- [`download_station_month()`](../reference/download_station_month.md):
  Download one station, one year, one month.
- [`download_station_by_name()`](../reference/download_station_by_name.md):
  Download one station for a year range.
- [`download_station_province()`](../reference/download_station_province.md):
  Download all stations in one province.
- [`download_all_station()`](../reference/download_all_station.md):
  Download all stations in your metadata.
- [`download_metadata()`](../reference/download_metadata.md): Copy
  packaged metadata `.rds` to your output folder.

## Load metadata

``` r
library(Drifloon)

meta <- load_metadata()
head(meta[, c("Name", "Province", "Station.ID", "HLY.First.Year", "HLY.Last.Year")])
##                    Name         Province Station.ID HLY.First.Year
## 1     ESQUIMALT HARBOUR BRITISH COLUMBIA         52           1994
## 2               MALAHAT BRITISH COLUMBIA         65           1994
## 3     SATURNA ISLAND CS BRITISH COLUMBIA         96           1994
## 4 VICTORIA GONZALES HTS BRITISH COLUMBIA        113           1953
## 5  VICTORIA GONZALES CS BRITISH COLUMBIA        114           1994
## 6      VICTORIA INT'L A BRITISH COLUMBIA        118           1953
##   HLY.Last.Year
## 1          2026
## 2          2025
## 3          2026
## 4          2000
## 5          2026
## 6          2013
```

The metadata frame is expected to include these key columns:

- `Name`: The name of the station. Note: some stations have the same
  name but different station ids.
- `Province`: The Province of the station.
- `Station.ID`: The unique identifier for each station
- `HLY.First.Year`: The first year with available hourly data
- `HLY.Last.Year`: The last year with available hourly data

## Sync metadata from source inventory

Use [`sync_metadata()`](../reference/sync_metadata.md) to pull the most
recent station metadata and update local metadata. This will only change
when new station metadata is available.

``` r
# Typical sync with defaults
sync_result <- sync_metadata()
str(sync_result)

# Example with tighter filtering and backup control
sync_result <- sync_metadata(
  provinces = c("ON", "QC"),
  min_year = 2000,
  create_backup = TRUE,
  max_backup_bytes = 25 * 1024^2
)
```

If you want this to happen during load, use:

``` r
meta <- load_metadata(sync = TRUE)
```

## Download metadata snapshot

If you want a local copy of packaged metadata, use
[`download_metadata()`](../reference/download_metadata.md).

``` r
metadata_path <- download_metadata(out_dir = "drifloon_output", overwrite = TRUE)
metadata_path
```

## Download a single month first

Start small to verify names, IDs, output paths. The default download
directory is in the root of your current working directory in a folder
`drifloon_output/`.

``` r
# By station name
ok <- download_station_month(
  station_name = "TORONTO CITY",
  year = 2020,
  month = 1,
  out_dir = "drifloon_output"
)

# By station ID (useful when names are duplicated)
ok <- download_station_month(
  station_id = 31688,
  year = 2020,
  month = 1,
  out_dir = "drifloon_output"
)
```

## Download a station across a year range

Here we are asking for the station data for every month of the year
range 2018 to 2020. Note you can use the `parallel=TRUE` argument if you
want to parallelize download, which could make it faster.

``` r
download_station_by_name(
  station_name = "TORONTO CITY",
  first_year = 2018,
  last_year = 2020,
  out_dir = "drifloon_output",
  parallel = FALSE
)
```

There are multiple stations with the same name. In this case pass both
`station_name` and `station_id` to force a specific match.

## Download by province

Because of how large the download of
[`download_station_province()`](../reference/download_station_province.md)
typically is, it estimates total files and storage first.

``` r
# Interactive confirmation (default)
download_station_province(
  province = "ON",
  first_year = 2019,
  last_year = 2020,
  out_dir = "drifloon_output"
)

# Non-interactive workflows (scripts/CI)
download_station_province(
  province = "ON",
  first_year = 2019,
  last_year = 2020,
  out_dir = "drifloon_output",
  confirm = TRUE
)
```

## Download all stations

Use cautiously: this can generate very large downloads.

``` r
download_all_station(
  first_year = 2019,
  last_year = 2020,
  out_dir = "drifloon_output",
  confirm = TRUE
)
```

## Helper internals and how they support the API

Drifloon includes internal helpers (not exported) that make wrappers
safer and more predictable:

- Metadata and paths:
  - [`.load_station_metadata()`](../reference/dot-load_station_metadata.md)
  - [`.resolve_out_dir()`](../reference/dot-resolve_out_dir.md)
  - [`.sync_station_metadata()`](../reference/dot-sync_station_metadata.md)
- Name and province resolution:
  - [`.normalize_station_name_key()`](../reference/dot-normalize_station_name_key.md)
  - [`.find_station_name_index()`](../reference/dot-find_station_name_index.md)
  - [`.province_normalize()`](../reference/dot-province_normalize.md)
- Validation and safety:
  - [`.validate_year()`](../reference/dot-validate_year.md)
  - [`.estimate_download_size()`](../reference/dot-estimate_download_size.md)
  - `.check_disk_space()`
  - [`.is_valid_downloaded_month_file()`](../reference/dot-is_valid_downloaded_month_file.md)

You can inspect internals for debugging with triple-colon access:

``` r
Drifloon:::.normalize_station_name_key(c("Montréal/Trudeau", "Montreal Trudeau"))
Drifloon:::.province_normalize(c("on", "Ontario", "QC"))
```

## Typical end-to-end workflow

``` r
library(Drifloon)

# 1) Refresh metadata
sync_metadata(provinces = c("ON", "QC"), min_year = 2000)

# 2) Load filtered metadata
meta <- load_metadata()

# 3) Validate one month first
download_station_month(
  station_name = "TORONTO CITY",
  year = 2020,
  month = 1,
  out_dir = "drifloon_output"
)

# 4) Scale to a station range
download_station_by_name(
  station_name = "TORONTO CITY",
  first_year = 2018,
  last_year = 2020,
  out_dir = "drifloon_output"
)

# 5) Scale to province only after checks
download_station_province(
  province = "ON",
  first_year = 2019,
  last_year = 2020,
  out_dir = "drifloon_output",
  confirm = TRUE
)
```
