# Load station metadata

Loads the hourly station metadata included with the package.

## Usage

``` r
load_metadata(sync = FALSE, ...)
```

## Arguments

- sync:

  Logical. If `TRUE`, metadata is synced from the official station
  inventory before loading. Default is `FALSE`.

- ...:

  Additional arguments passed to \[sync_metadata()\] when `sync = TRUE`.

## Value

Data frame of station metadata.

## Details

Metadata is loaded using internal package/local metadata resolution. Set
`sync = TRUE` to refresh metadata first.
