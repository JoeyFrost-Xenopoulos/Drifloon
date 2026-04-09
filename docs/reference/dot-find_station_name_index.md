# Find station row index from station name

Internal helper for resolving a station by name with exact normalized
matching and fallback fuzzy matching.

## Usage

``` r
.find_station_name_index(
  station_name,
  station_data,
  station_id = NULL,
  max_distance = 2L
)
```

## Arguments

- station_name:

  Character. User-provided station name.

- station_data:

  Data frame containing station metadata.

- station_id:

  Numeric. Optional station ID used to disambiguate.

- max_distance:

  Integer. Maximum edit distance for fuzzy matching.

## Value

Integer vector of candidate row indices.
