# Build station output folder label

Uses a normalized station name by default, and appends station ID when
metadata indicates duplicate station names.

## Usage

``` r
.station_output_label(station_name, station_id, station_data = NULL)
```

## Arguments

- station_name:

  Character. Station name.

- station_id:

  Numeric. Station ID.

- station_data:

  Data frame with station metadata.

## Value

Character folder label for station output.
