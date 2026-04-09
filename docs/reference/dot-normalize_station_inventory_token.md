# Find header row in station inventory CSV

Internal helper to locate the station inventory table header, even when
the file starts with disclaimer text lines.

## Usage

``` r
.normalize_station_inventory_token(x)
```

## Arguments

- lines:

  Character vector of file lines.

## Value

Integer row index of the header line.
