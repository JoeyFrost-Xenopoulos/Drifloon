#' Create a SQLite weather database from local files
#'
#' Builds a SQLite database using station metadata and hourly station CSV files.
#'
#' Expected file structure under \code{base_dir}:
#' \itemize{
#'   \item \code{data/HLY_station_info.rds}
#'   \item \code{drifloon_output/} (recursive station CSV files)
#' }
#'
#' @param base_dir Character. Root directory that contains input files and where
#'   metadata and hourly data files are located.
#' @param db_name Character. SQLite file name to create under
#'   \code{file.path(base_dir, "database")}.
#'   Default is \code{"climate_database.db"}.
#' @param overwrite Logical. If \code{TRUE}, replaces an existing database file.
#' @param batch_size Numeric. Number of source files to accumulate before each
#'   observation-table write. Default is \code{50}.
#'
#' @return Invisibly returns a list with created path and row/file counts.
#'
#' @details
#' Station metadata is read from
#' \code{file.path(base_dir, "data", "HLY_station_info.rds")}. Column names
#' are matched permissively to support common variants (for example,
#' \code{Station.ID} and \code{Station_ID}).
#'
#' Hourly rows are matched by Climate ID using a climate-id column in each CSV
#' (for example, \code{Climate.ID} or \code{Climate ID}).
#'
#' The SQLite database file is created under
#' \code{file.path(base_dir, "database")}, not directly under \code{base_dir}.
#'
#' If the current working directory contains more than 1000 CSV files, an
#' interactive confirmation prompt is shown because the resulting database may
#' be large.
#'
#' Station records are pre-filtered to only Climate IDs found in downloaded
#' hourly CSV files before insertion.
#'
#' If no station metadata rows are present, the function still creates the
#' database schema and weather lookup table.
#'
#' @export
create_database <- function(base_dir = file.path(getwd()),
                            db_name = "climate_database.db",
                            overwrite = FALSE,
                            batch_size = 50L) {
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("base_dir must be a single, non-empty character path.")
  }
  if (!is.character(db_name) || length(db_name) != 1 || !nzchar(db_name)) {
    stop("db_name must be a single, non-empty character value.")
  }
  if (!is.numeric(batch_size) || is.na(batch_size) || batch_size < 1) {
    stop("batch_size must be a positive number.")
  }

  wd_csv_files <- list.files(
    path = getwd(),
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(wd_csv_files) > 1000L) {
    warning_text <- paste0(
      "Working directory contains ",
      length(wd_csv_files),
      " CSV files. The resulting database may be large."
    )

    if (interactive()) {
      message(warning_text)
      response <- readline(prompt = "Continue creating database? (yes/no): ")
      if (!tolower(trimws(response)) %in% c("yes", "y")) {
        stop("Database creation cancelled by user.")
      }
    } else {
      warning(warning_text, call. = FALSE)
    }
  }

  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  metadata_dir <- file.path(base_dir, "data")
  hourly_dir <- file.path(base_dir, "drifloon_output")
  station_info_path <- file.path(metadata_dir, "HLY_station_info.rds")
  db_out_dir <- file.path(base_dir, "database")

  if (!dir.exists(hourly_dir)) {
    stop("Missing expected directory: ", hourly_dir)
  }
  if (!file.exists(station_info_path)) {
    stop("Missing expected metadata file: ", station_info_path)
  }

  if (!dir.exists(db_out_dir)) {
    dir.create(db_out_dir, recursive = TRUE)
  }

  db_path <- file.path(db_out_dir, db_name)
  if (file.exists(db_path)) {
    if (!isTRUE(overwrite)) {
      stop("Database already exists at ", db_path, ". Use overwrite = TRUE to replace it.")
    }
    file.remove(db_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbExecute(con, "
    CREATE TABLE Station (
      Climate_ID TEXT PRIMARY KEY,
      Station_ID INTEGER UNIQUE,
      Station_Name TEXT,
      Province_Name TEXT,
      Latitude REAL,
      Longitude REAL,
      Elevation REAL,
      HLY_First_Year INTEGER,
      HLY_Last_Year INTEGER
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE Weather (
      Weather_ID INTEGER PRIMARY KEY,
      Weather_Condition TEXT UNIQUE
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE Observation (
      Station_ID INTEGER,
      Year INTEGER,
      Month INTEGER,
      Day INTEGER,
      Time_LST TEXT,
      Temp_C REAL,
      Dew_Point_C REAL,
      Rel_Hum REAL,
      Precip_Amount REAL,
      Wind_Dir_deg REAL,
      Wind_Spd_kmh REAL,
      Visibility_km REAL,
      Stn_Press_kPa REAL,
      Hmdx REAL,
      Wind_Chill REAL,
      Weather INTEGER,
      PRIMARY KEY (Station_ID, Year, Month, Day, Time_LST),
      FOREIGN KEY (Station_ID) REFERENCES Station(Station_ID),
      FOREIGN KEY (Weather) REFERENCES Weather(Weather_ID)
    );
  ")

  weather_table <- .weather_lookup_table()
  weather_id_by_condition <- weather_table$Weather_ID
  names(weather_id_by_condition) <- weather_table$Weather_Condition
  DBI::dbWriteTable(con, "Weather", weather_table, append = TRUE, row.names = FALSE)

  station_raw <- readRDS(station_info_path)
  if (!is.data.frame(station_raw)) {
    stop("Metadata RDS must contain a data.frame.")
  }

  find_col <- function(candidates, required = TRUE) {
    idx <- which(names(station_raw) %in% candidates)
    if (length(idx) == 0) {
      if (required) {
        stop("station metadata is missing required columns: ", paste(candidates, collapse = ", "))
      }
      return(NULL)
    }
    names(station_raw)[idx[[1]]]
  }

  col_climate <- find_col(c("Climate.ID", "Climate_ID", "Climate ID"), required = TRUE)
  col_station_id <- find_col(c("Station.ID", "Station_ID", "Station ID"), required = TRUE)
  col_station_name <- find_col(c("stationName", "Name", "Station_Name", "Station Name"), required = TRUE)

  col_province <- find_col(c("Province", "Province_Name", "Province Name"), required = FALSE)
  col_latitude <- find_col(c("Latitude"), required = FALSE)
  col_longitude <- find_col(c("Longitude"), required = FALSE)
  col_elevation <- find_col(c("Elevation..m.", "Elevation", "Elevation_m"), required = FALSE)
  col_hly_first <- find_col(c("HLY.First.Year", "HLY_First_Year", "HLY First Year"), required = FALSE)
  col_hly_last <- find_col(c("HLY.Last.Year", "HLY_Last_Year", "HLY Last Year"), required = FALSE)

  get_col_or_na <- function(col_name, n, mode = c("character", "numeric", "integer")) {
    mode <- match.arg(mode)
    if (is.null(col_name)) {
      if (mode == "character") return(rep(NA_character_, n))
      if (mode == "numeric") return(rep(NA_real_, n))
      return(rep(NA_integer_, n))
    }

    values <- station_raw[[col_name]]
    if (mode == "character") return(as.character(values))
    if (mode == "numeric") return(suppressWarnings(as.numeric(values)))
    suppressWarnings(as.integer(values))
  }

  n_station <- nrow(station_raw)
  station_table <- data.frame(
    Climate_ID = as.character(station_raw[[col_climate]]),
    Station_ID = suppressWarnings(as.integer(station_raw[[col_station_id]])),
    Station_Name = as.character(station_raw[[col_station_name]]),
    Province_Name = get_col_or_na(col_province, n_station, mode = "character"),
    Latitude = get_col_or_na(col_latitude, n_station, mode = "numeric"),
    Longitude = get_col_or_na(col_longitude, n_station, mode = "numeric"),
    Elevation = get_col_or_na(col_elevation, n_station, mode = "numeric"),
    HLY_First_Year = get_col_or_na(col_hly_first, n_station, mode = "integer"),
    HLY_Last_Year = get_col_or_na(col_hly_last, n_station, mode = "integer"),
    stringsAsFactors = FALSE
  )

  station_table <- station_table[
    !is.na(station_table$Station_ID) & nzchar(station_table$Climate_ID),
    ,
    drop = FALSE
  ]
  station_table <- station_table[!duplicated(station_table$Climate_ID), , drop = FALSE]
  rownames(station_table) <- NULL

  csv_files <- list.files(hourly_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

  extract_climate_ids <- function(df) {
    climate_id_candidates <- c("Climate.ID", "Climate_ID", "Climate ID", "ClimateID")
    id_col <- climate_id_candidates[climate_id_candidates %in% names(df)]

    if (length(id_col) == 0) {
      return(rep(NA_character_, nrow(df)))
    }

    as.character(df[[id_col[[1]]]])
  }

  climate_ids_in_downloads <- character(0)
  if (length(csv_files) > 0) {
    for (path in csv_files) {
      header_probe <- tryCatch(
        suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE, nrows = 5)),
        error = function(e) NULL
      )
      if (is.null(header_probe)) {
        next
      }

      climate_ids_in_downloads <- c(climate_ids_in_downloads, extract_climate_ids(header_probe))
    }
  }

  climate_ids_in_downloads <- unique(climate_ids_in_downloads[!is.na(climate_ids_in_downloads) & nzchar(climate_ids_in_downloads)])
  if (length(csv_files) > 0 && length(climate_ids_in_downloads) == 0) {
    stop(
      "No Climate ID column found in hourly CSV files. ",
      "Expected one of: Climate.ID, Climate_ID, Climate ID, ClimateID."
    )
  }

  if (length(climate_ids_in_downloads) > 0) {
    station_table <- station_table[station_table$Climate_ID %in% climate_ids_in_downloads, , drop = FALSE]
    rownames(station_table) <- NULL
  } else {
    station_table <- station_table[0, , drop = FALSE]
  }

  if (nrow(station_table) > 0) {
    DBI::dbWriteTable(con, "Station", station_table, append = TRUE, row.names = FALSE)
  }

  station_id_lookup <- station_table$Station_ID
  names(station_id_lookup) <- station_table$Climate_ID

  files_processed <- 0L
  rows_written <- 0L
  batch <- NULL
  batch_size <- as.integer(batch_size)

  if (length(csv_files) > 0 && nrow(station_table) > 0) {
    DBI::dbBegin(con)
    committed <- FALSE
    
    progressr::with_progress({
      p <- progressr::progressor(steps = length(csv_files))
      
      tryCatch({
        for (path in csv_files) {
        hourly <- tryCatch(
          suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE)),
          error = function(e) NULL
        )
        if (is.null(hourly)) {
          next
        }

        required_hourly_cols <- c("Year", "Month", "Day", "Time..LST.")
        if (length(setdiff(required_hourly_cols, names(hourly))) > 0) {
          next
        }

        row_climate_id <- extract_climate_ids(hourly)
        if (length(row_climate_id) != nrow(hourly)) {
          next
        }

        row_station_id <- as.integer(unname(station_id_lookup[row_climate_id]))

        weather_text <- if ("Weather" %in% names(hourly)) as.character(hourly$Weather) else rep(NA_character_, nrow(hourly))
        weather_id <- as.integer(unname(weather_id_by_condition[weather_text]))

        rows <- data.frame(
          Station_ID = row_station_id,
          Year = suppressWarnings(as.integer(hourly$Year)),
          Month = suppressWarnings(as.integer(hourly$Month)),
          Day = suppressWarnings(as.integer(hourly$Day)),
          Time_LST = as.character(hourly$Time..LST.),
          Temp_C = if ("Temp...C." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Temp...C.)) else NA_real_,
          Dew_Point_C = if ("Dew.Point.Temp...C." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Dew.Point.Temp...C.)) else NA_real_,
          Rel_Hum = if ("Rel.Hum...." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Rel.Hum....)) else NA_real_,
          Precip_Amount = if ("Precip..Amount..mm." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Precip..Amount..mm.)) else NA_real_,
          Wind_Dir_deg = if ("Wind.Dir..10s.deg." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Wind.Dir..10s.deg.)) else NA_real_,
          Wind_Spd_kmh = if ("Wind.Spd..km.h." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Wind.Spd..km.h.)) else NA_real_,
          Visibility_km = if ("Visibility..km." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Visibility..km.)) else NA_real_,
          Stn_Press_kPa = if ("Stn.Press..kPa." %in% names(hourly)) suppressWarnings(as.numeric(hourly$Stn.Press..kPa.)) else NA_real_,
          Hmdx = if ("Hmdx" %in% names(hourly)) suppressWarnings(as.numeric(hourly$Hmdx)) else NA_real_,
          Wind_Chill = if ("Wind.Chill" %in% names(hourly)) suppressWarnings(as.numeric(hourly$Wind.Chill)) else NA_real_,
          Weather = weather_id,
          stringsAsFactors = FALSE
        )

        rows <- rows[
          !is.na(rows$Station_ID) & !is.na(rows$Year) & !is.na(rows$Month) &
            !is.na(rows$Day) & nzchar(rows$Time_LST),
          ,
          drop = FALSE
        ]
        rownames(rows) <- NULL

        if (!is.null(rows) && nrow(rows) > 0) {
          if (is.null(batch)) {
            batch <- rows
          } else {
            batch <- rbind(batch, rows)
          }
        }

        files_processed <- files_processed + 1L
        p(message = sprintf("Processed %d files", files_processed))

        if (!is.null(batch) && (files_processed %% batch_size == 0L)) {
          DBI::dbWriteTable(con, "Observation", batch, append = TRUE, row.names = FALSE)
          rows_written <- rows_written + nrow(batch)
          batch <- NULL
        }
      }

      if (!is.null(batch) && nrow(batch) > 0) {
        DBI::dbWriteTable(con, "Observation", batch, append = TRUE, row.names = FALSE)
        rows_written <- rows_written + nrow(batch)
      }

        DBI::dbCommit(con)
        committed <- TRUE
      }, error = function(e) {
        if (!committed) {
          DBI::dbRollback(con)
        }
        stop("Failed while building observations: ", conditionMessage(e))
      })
    })
  }

  invisible(list(
    db_path = db_path,
    station_rows = nrow(station_table),
    observation_rows = rows_written,
    files_processed = files_processed
  ))
}

#' @keywords internal
.weather_lookup_table <- function() {
  conditions <- c(
    "Blowing Dust", "Blowing Sand", "Blowing Snow",
    "Clear", "Cloudy",
    "Drizzle", "Dust",
    "Fog", "Freezing Drizzle", "Freezing Fog", "Freezing Rain", "Funnel Cloud",
    "Hail", "Haze", "Heavy Drizzle", "Heavy Freezing Drizzle", "Heavy Freezing Rain",
    "Heavy Hail", "Heavy Ice Pellet Showers", "Heavy Ice Pellets", "Heavy Rain",
    "Heavy Rain Showers", "Heavy Snow", "Heavy Snow Grains", "Heavy Snow Pellets",
    "Heavy Snow Showers", "Heavy Thunderstorms",
    "Ice Crystals", "Ice Fog", "Ice Pellet Showers", "Ice Pellets",
    "Mainly Clear", "Moderate Drizzle", "Moderate Freezing Drizzle",
    "Moderate Freezing Rain", "Moderate Hail", "Moderate Ice Pellet Showers",
    "Moderate Ice Pellets", "Moderate Rain", "Moderate Rain Showers",
    "Moderate Snow", "Moderate Snow Grains", "Moderate Snow Pellets",
    "Moderate Snow Showers", "Mostly Cloudy",
    "Rain", "Rain Showers",
    "Smoke", "Snow", "Snow Grains", "Snow Pellets", "Snow Showers",
    "Thunderstorms", "Tornado"
  )

  unique_conditions <- sort(unique(conditions))
  data.frame(
    Weather_ID = seq_along(unique_conditions),
    Weather_Condition = unique_conditions,
    stringsAsFactors = FALSE
  )
}
