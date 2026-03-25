<img src="sticker/drifloon_hex.png" align="right" width="170" alt="Drifloon hex sticker" />

# Drifloon

---

Drifloon helps you discover and download historical hourly weather data from Environment and Climate Change Canada (ECCC), then optionally build a local SQLite database for analysis.

https://climate.weather.gc.ca/historical_data/search_historic_data_e.html

<br clear="right"/>

## What You Can Do

---

- Load station metadata (name, station ID, province, hourly year range).
- Sync station metadata from the official station inventory.
- Download one month, one station, one province, or all stations.
- Build a SQLite database from downloaded CSV files.

## Quick Start

---

Before the downloads it is smart to look at the list of stations, as each download requires either station name, station_id, or province. 

```r
library(Drifloon)

stations <- load_metadata()
head(stations)
```

## Metadata

---

You can update the current metadata to the most recent version.

```r
stations <- load_metadata(sync = TRUE)
```

Or call the sync wrapper directly:

```r
sync_metadata()
```

By default, sync applies no year floor or year cap. Set min_year and/or max_year only when you want explicit year filtering.

## Downloads

---

By default, downloads are written to a drifloon_output folder in your working directory.

### Download One Station-Month

---

Use either station_id or station_name:

```r
download_station_month(
	station_id = 31688,
	year = 2020,
	month = 7
)
```

### Download a Single Station (All Available Months/Years)

---

```r
download_station_by_name(
	station_name = "Discovery Island",
	first_year = 2018,
	last_year = 2020,
	parallel = FALSE
)
```

### Download All Stations in a Province

---

The function prompts for confirmation by default because this can be large.

```r
download_station_province(
	province = "ON",
	first_year = 2019,
	last_year = 2020,
	parallel = FALSE,
	confirm = TRUE
)
```

### Download All Stations

---

Use with caution; this can generate very large downloads.

```r
download_all_station(
	first_year = 2020,
	last_year = 2020,
	parallel = FALSE,
	confirm = TRUE
)
```

## Save Packaged Metadata File

---

If you want a local copy of the packaged metadata file:

```r
download_metadata(out_dir = "data", overwrite = TRUE)
```

## Create SQLite Database

---

After downloading CSVs, build a SQLite database:

```r
create_database(
	base_dir = getwd(),
	db_name = "climate_database.db",
	overwrite = TRUE
)
```

Expected structure under base_dir:

- data/HLY_station_info.rds
- drifloon_output/ (downloaded station CSVs)

The database is created at:

- database/climate_database.db

## Notes

---

- Station name matching is tolerant to case and punctuation differences.
- Ambiguous station names can be disambiguated with station_id.
- Disk-space checks are warning-only; downloads continue even when free space appears low.

## License

---

MIT