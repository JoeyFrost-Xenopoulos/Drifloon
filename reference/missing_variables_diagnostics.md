# Diagnose Missingness by Variable in a Drifloon Database

Computes missing-cell percentages for each variable in the Observation
table using the selected diagnostic scope. Optionally returns a larger
table with a station-level breakdown for each variable.

## Usage

``` r
missing_variables_diagnostics(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  years = NULL,
  missing_columns = c("Temp_C", "Dew_Point_C", "Rel_Hum", "Wind_Dir_deg", "Wind_Spd_kmh",
    "Visibility_km", "Stn_Press_kPa", "Hmdx", "Wind_Chill", "Precip_Amount"),
  include_station_breakdown = FALSE,
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

- years:

  Optional single year or two-value year range.

- missing_columns:

  Character vector of Observation columns to include in missingness
  calculations.

- include_station_breakdown:

  Logical. If `TRUE`, includes a larger table with one row per
  station-variable pair.

- write_csv:

  Logical. If `TRUE`, writes diagnostic CSV files. Default is `FALSE`.

- verbose:

  Logical. If `TRUE`, prints diagnostic tables to the R console.
  Defaults to `TRUE`.

- max_rows_print:

  Integer. Maximum rows to print for detailed tables shown in detailed
  printed tables. Defaults to `20`.

## Value

A named list with `missing_by_variable`, `missing_variable_summary`, and
(optionally) `missing_by_station_variable`. The return value is
invisible.
