# Diagnose Out-of-Range Values Using Baseline Variable Ranges

Flags unexpected values by comparing Observation variables against a
baseline \\\mu \pm 3\sigma\\ range from a reference profile (for
example, historical 1980-2020 data). This is intended as a general
warning tool, not a strict physical-validity check.

## Usage

``` r
out_of_range_diagnostics(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  province = NULL,
  years = NULL,
  variables = c("Temp_C", "Dew_Point_C", "Rel_Hum", "Wind_Dir_deg", "Wind_Spd_kmh",
    "Visibility_km", "Stn_Press_kPa", "Hmdx", "Wind_Chill"),
  canada_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_Canada_1980-2020.rds"),
  province_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_province_1980-2020.rds"),
  include_station_breakdown = FALSE,
  include_outlier_rows = FALSE,
  write_csv = FALSE,
  verbose = TRUE,
  max_rows_print = 20L,
  baseline_rds_path = NULL
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

- variables:

  Character vector of Observation variable names to evaluate.

- canada_ranges_rds_path:

  Character. Path to Canada-wide baseline range RDS file (1980-2020).

- province_ranges_rds_path:

  Character. Path to province baseline range RDS file (1980-2020).

- include_station_breakdown:

  Logical. If `TRUE`, includes a larger table with one row per
  station-variable pair.

- include_outlier_rows:

  Logical. If `TRUE`, includes a row-level table of observations outside
  the \\\mu \pm 3\sigma\\ range.

- write_csv:

  Logical. If `TRUE`, writes diagnostic CSV files. Default is `FALSE`.

- verbose:

  Logical. If `TRUE`, prints diagnostic tables to the R console.
  Defaults to `TRUE`.

- max_rows_print:

  Integer. Maximum rows to print for detailed tables shown in detailed
  printed tables. Defaults to `20`.

- baseline_rds_path:

  Character. Optional override RDS path for the baseline ranges used in
  the diagnostics.

## Value

A named list with `range_scope_summary`, `out_of_range_by_variable`, and
(optionally) `out_of_range_by_station_variable` and
`out_of_range_observations`. The return value is invisible.
