# Compare station inventory frames

Internal helper that compares two station inventories after stable
sorting.

## Usage

``` r
.same_station_inventory(new_data, current_data)
```

## Arguments

- new_data:

  Data frame with candidate metadata.

- current_data:

  Data frame with current metadata.

## Value

Logical. TRUE when data are equivalent.
