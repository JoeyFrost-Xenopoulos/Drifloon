# Parse Modified Date from station inventory preamble

Internal helper that extracts a modified date stamp when present in the
top-left preamble cell (for example: Modified Date: 2026-01-07 23:30
UTC).

## Usage

``` r
.station_inventory_modified_date(lines)
```

## Arguments

- lines:

  Character vector of file lines.

## Value

Character scalar with parsed modified-date text, or NA_character\_.
