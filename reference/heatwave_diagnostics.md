# Detect Heatwaves Using Provincial Baseline Temperature Ranges

Detects heatwave events from daily temperatures in the Observation
table. A heatwave day is defined as a day where either daily maximum
temperature is at least \\\mu + 5\sigma\\ or daily average temperature
is at least \\\mu + 3\sigma\\, where \\\mu\\ and \\\sigma\\ are taken
from the baseline range profiles. A heatwave event is two or more
consecutive heatwave days.

## Usage

``` r
heatwave_diagnostics(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  province = NULL,
  years = NULL,
  min_consecutive_days = 2L,
  max_sd_threshold = 5,
  avg_sd_threshold = 3,
  canada_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_Canada_1980-2020.rds"),
  province_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_province_1980-2020.rds"),
  fallback_to_canada = TRUE,
  write_csv = FALSE,
  verbose = TRUE,
  max_rows_print = 20L
)
```

## Arguments

- base_dir:

  Character. Root directory containing the database folder.

- db_name:

  Character. SQLite database file name under
  `file.path(base_dir, "database")`.

- out_dir:

  Character. Directory where diagnostic CSV files are written when
  `write_csv = TRUE`.

- station_ids:

  Optional vector of Station IDs to include.

- province:

  Optional character vector of province names to include.

- years:

  Optional single year or two-value year range.

- min_consecutive_days:

  Integer. Minimum consecutive heatwave days for an event. Defaults to
  `2`.

- max_sd_threshold:

  Numeric. SD multiplier for the daily-maximum rule. Defaults to `5`.

- avg_sd_threshold:

  Numeric. SD multiplier for the daily-average rule. Defaults to `3`.

- canada_ranges_rds_path:

  Character. Path to Canada-wide baseline range RDS file (1980-2020).

- province_ranges_rds_path:

  Character. Path to province baseline range RDS file (1980-2020).

- fallback_to_canada:

  Logical. If `TRUE`, stations without a matching province baseline use
  the Canada-wide baseline for Temp_C.

- write_csv:

  Logical. If `TRUE`, writes diagnostic CSV files.

- verbose:

  Logical. If `TRUE`, prints summary tables.

- max_rows_print:

  Integer. Maximum rows to print from detailed tables.

## Value

A named list with `scope_summary`, `station_summary`, `heatwave_events`,
`heatwave_days`, and baseline metadata. The return value is invisible.

## Details

Provincial baseline ranges are used by default; when a station province
is missing from the province baseline file, the function can fall back
to the Canada-wide baseline.
