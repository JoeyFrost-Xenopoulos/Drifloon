# Validate year inputs

Checks that year values are valid (numeric, not negative, not far in
future).

## Usage

``` r
.validate_year(year, allow_null = TRUE)
```

## Arguments

- year:

  Numeric or NULL. Year value(s) to validate.

- allow_null:

  Logical. If TRUE, NULL is accepted. Default is TRUE.

## Value

The validated year as integer, or NULL if allow_null=TRUE and input is
NULL.
