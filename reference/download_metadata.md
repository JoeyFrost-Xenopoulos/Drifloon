# Copy packaged metadata file to disk

Copies the packaged `HLY_station_info.rds` file to a target directory.
If no directory is supplied, the file is copied to the current working
directory.

## Usage

``` r
download_metadata(out_dir = NULL, overwrite = FALSE)
```

## Arguments

- out_dir:

  Character. Output directory for the metadata file. Defaults to
  `file.path(getwd(), "drifloon_output")`.

- overwrite:

  Logical. If `TRUE`, overwrite an existing output file. Default is
  `FALSE`.

## Value

Character path to the copied `.rds` file (invisibly).
