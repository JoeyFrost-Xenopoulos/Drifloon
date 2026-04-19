# Plot Station Observations Against Baseline SD Bounds

Creates one panel per variable for a selected station and highlights
observations outside a baseline \\\mu \pm k\sigma\\ range in red.

## Usage

``` r
plot_station_outliers(
  station,
  base_dir = getwd(),
  db_name = "climate_database.db",
  province = NULL,
  years = NULL,
  variables = c("Temp_C", "Dew_Point_C", "Rel_Hum", "Wind_Dir_deg", "Wind_Spd_kmh",
    "Visibility_km", "Stn_Press_kPa", "Hmdx", "Wind_Chill"),
  sd_threshold = 3,
  panels_per_row = 3L,
  canada_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_Canada_1980-2020.rds"),
  province_ranges_rds_path = file.path(base_dir, "data",
    "variable_ranges_province_1980-2020.rds"),
  baseline_rds_path = NULL,
  point_cex = 0.6,
  verbose = TRUE
)
```

## Arguments

- station:

  Station identifier. Either a single Station ID (numeric) or station
  name (character).

- base_dir:

  Character. Root directory containing the database folder.

- db_name:

  Character. SQLite database file name under
  `file.path(base_dir, "database")`.

- province:

  Optional character vector of province names to include.

- years:

  Optional single year or two-value year range.

- variables:

  Character vector of Observation variable names to plot.

- sd_threshold:

  Numeric. SD multiplier for the bounds. Defaults to `3`.

- panels_per_row:

  Integer. Number of variable panels per row.

- canada_ranges_rds_path:

  Character. Path to Canada-wide baseline range RDS file (1980-2020).

- province_ranges_rds_path:

  Character. Path to province baseline range RDS file (1980-2020).

- baseline_rds_path:

  Character. Optional override RDS path for the baseline ranges used in
  the diagnostics.

- point_cex:

  Numeric. Point size used for plotting.

- verbose:

  Logical. If `TRUE`, prints a short summary table.

## Value

Invisibly returns a list with station metadata, selected baseline
profile, and a per-variable summary of observations outside bounds.
