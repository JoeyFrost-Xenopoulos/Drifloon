# Filter station inventory rows

Internal helper that filters inventory rows to stations with valid
hourly coverage and optional province/year constraints.

## Usage

``` r
.filter_station_inventory(
  station_data,
  require_hourly = TRUE,
  provinces = NULL,
  min_year = NULL,
  max_year = NULL
)
```

## Arguments

- station_data:

  Data frame read from station inventory CSV.

- require_hourly:

  Logical. If TRUE, keep only rows with non-missing hourly year bounds.

- provinces:

  Character vector of provinces to keep (optional).

- min_year:

  Numeric. Optional minimum year that must overlap station hourly
  coverage.

- max_year:

  Numeric. Optional maximum year that must overlap station hourly
  coverage.

## Value

Filtered data frame sorted by Station.ID then Name.
