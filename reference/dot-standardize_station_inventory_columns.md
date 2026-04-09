# Read station inventory CSV with preamble support

Handles station inventory files that include disclaimer/preamble text
before the actual CSV header row.

## Usage

``` r
.standardize_station_inventory_columns(data)
```

## Arguments

- csv_path:

  Character. Path to CSV file.

## Value

List with parsed data frame and optional modified-date text.
