# Diagnose Missing Observations in a Drifloon Database

Runs missing-data diagnostics on the Observation table for all data or a
user-selected subset (by Climate ID, Station ID, and/or year range).

## Usage

``` r
missing_observations_diagnostics(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  climate_ids = NULL,
  station_ids = NULL,
  years = NULL,
  missing_columns = c("Temp_C", "Dew_Point_C", "Rel_Hum", "Wind_Dir_deg", "Wind_Spd_kmh",
    "Visibility_km", "Stn_Press_kPa", "Hmdx", "Wind_Chill", "Weather"),
  write_csv = FALSE,
  print_tables = TRUE,
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

- climate_ids:

  Optional character vector of Climate IDs to include.

- station_ids:

  Optional vector of Station IDs to include.

- years:

  Optional single year or two-value year range.

- missing_columns:

  Character vector of Observation columns to include in missingness
  calculations.

- write_csv:

  Logical. If `TRUE`, writes diagnostic CSV files. Default is `FALSE`.

- print_tables:

  Logical. If `TRUE`, prints the diagnostic tables to the R console.

- max_rows_print:

  Integer. Maximum rows to print for each detailed table when
  `print_tables = TRUE`. Defaults to `20`.

## Value

A named list with `missing_by_station`, `missing_by_station_year`, and
`missing_summary` data frames using readable column titles. The return
value is invisible to avoid duplicate console output when
`print_tables = TRUE`.
