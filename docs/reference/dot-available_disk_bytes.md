# Check available disk space

Verifies that the output directory has sufficient free space for the
estimated download.

## Usage

``` r
.available_disk_bytes(out_dir, os_type = .Platform$OS.type)
```

## Arguments

- out_dir:

  Character. Path to output directory.

- estimated_bytes:

  Numeric. Estimated download size in bytes.

- buffer_percent:

  Numeric. Extra buffer to require (default: 10, meaning 10% extra).

## Value

Logical TRUE if sufficient space available, otherwise stops with error.
