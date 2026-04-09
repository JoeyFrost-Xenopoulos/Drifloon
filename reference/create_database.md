# Create a SQLite weather database from local files

Builds a SQLite database using station metadata and hourly station CSV
files.

## Usage

``` r
create_database(
  base_dir = file.path(getwd()),
  db_name = "climate_database.db",
  overwrite = FALSE,
  batch_size = 50L
)
```

## Arguments

- base_dir:

  Character. Root directory that contains input files and where metadata
  and hourly data files are located.

- db_name:

  Character. SQLite file name to create under
  `file.path(base_dir, "database")`. Default is `"climate_database.db"`.

- overwrite:

  Logical. If `TRUE`, replaces an existing database file.

- batch_size:

  Numeric. Number of source files to accumulate before each
  observation-table write. Default is `50`.

## Value

Invisibly returns a list with created path and row/file counts.

## Details

Expected file structure under `base_dir`:

- `data/HLY_station_info.rds`

- `drifloon_output/` (recursive station CSV files)

Station metadata is read from
`file.path(base_dir, "data", "HLY_station_info.rds")`. Column names are
matched permissively to support common variants (for example,
`Station.ID` and `Station_ID`).

Hourly rows are matched by Climate ID using a climate-id column in each
CSV (for example, `Climate.ID` or `Climate ID`).

The SQLite database file is created under
`file.path(base_dir, "database")`, not directly under `base_dir`.

If the current working directory contains more than 1000 CSV files, an
interactive confirmation prompt is shown because the resulting database
may be large.

Station records are pre-filtered to only Climate IDs found in downloaded
hourly CSV files before insertion.

If no station metadata rows are present, the function still creates the
database schema and weather lookup table.
