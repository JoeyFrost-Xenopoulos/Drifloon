# Download station data by name or station ID

Wrapper to download data for one or more stations identified by their
name(s) and/or station ID(s).

## Usage

``` r
download_station_by_name(
  station_name = NULL,
  station_id = NULL,
  out_dir = NULL,
  station_data = NULL,
  first_year = NULL,
  last_year = NULL,
  parallel = FALSE
)
```

## Arguments

- station_name:

  Character. Station name(s) (optional). Can be a single station name or
  a vector of names.

- station_id:

  Numeric. Station ID(s) (optional). Can be a single ID or a vector of
  IDs.

- out_dir:

  Character. Base output directory. If not supplied, defaults to
  `file.path(getwd(), "drifloon_output")`.

- station_data:

  Data frame containing station metadata. Optional; if omitted, packaged
  metadata is loaded automatically.

- first_year:

  Numeric. First year to download (optional).

- last_year:

  Numeric. Last year to download (optional).

- parallel:

  Logical. If `TRUE`, months are downloaded in parallel using
  [`furrr::future_pwalk`](https://furrr.futureverse.org/reference/future_map2.html).
  Default is `FALSE`.

## Value

Invisibly returns `NULL`.

## Details

Either `station_name` or `station_id` must be provided (both can be used
together).

When vectorizing (passing multiple stations):

- If only `station_name` is a vector, each name is downloaded
  separately.

- If only `station_id` is a vector, each ID is downloaded separately.

- If both are vectors, they must be the same length for pairwise
  matching, or one can have length 1 and will be recycled.

If `first_year` or `last_year` are outside the station metadata range
(`HLY.First.Year` to `HLY.Last.Year`), an error is thrown.

Station lookups that result in multiple matches or no matches will issue
warnings and skip to the next station.
