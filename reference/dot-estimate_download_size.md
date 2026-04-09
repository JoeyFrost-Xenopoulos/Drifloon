# Estimate download size for a batch of downloads

Calculates estimated number of files and space (in MB) that will be
downloaded for a given set of stations and year range.

## Usage

``` r
.estimate_download_size(
  stations,
  first_year = NULL,
  last_year = NULL,
  bytes_per_file = 130000
)
```

## Arguments

- stations:

  Data frame containing station metadata with columns HLY.First.Year and
  HLY.Last.Year.

- first_year:

  Numeric. Starting year. If NULL, uses station metadata.

- last_year:

  Numeric. Ending year. If NULL, uses station metadata.

- bytes_per_file:

  Numeric. Expected size of each .csv file in bytes. Default is 130000
  (roughly 130 KB).

## Value

List with elements: count (number of files), size_mb (estimated MB),
size_gb (estimated GB), years (number of years).
