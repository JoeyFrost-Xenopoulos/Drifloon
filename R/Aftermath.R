#' Create a SQLite weather database from local files
#'
#' Builds a SQLite database using station metadata and hourly station CSV files.
#' The function can also scaffold the expected directory layout.
#'
#' Expected file structure under \code{base_dir}:
#' \itemize{
#'   \item \code{metadata/HLY_station_info.csv}
#'   \item \code{hourly_station_data/} (recursive station CSV files)
#' }
#'
#' @param base_dir Character. Root directory that contains input files and where
#'   metadata and hourly data files are located.
#' @param db_name Character. SQLite file name to create under \code{base_dir}.
#'   Default is \code{"climate_database.db"}.
#' @param overwrite Logical. If \code{TRUE}, replaces an existing database file.
#' @param batch_size Numeric. Number of source files to accumulate before each
#'   observation-table write. Default is \code{50}.
#'
#' @return Invisibly returns a list with created path and row/file counts.
#'
#' @details
#' Station metadata is read from
#' \code{file.path(base_dir, "metadata", "HLY_station_info.csv")}. Column names
#' are matched permissively to support common variants (for example,
#' \code{Climate.ID} and \code{Climate_ID}).
#'
#' If the current working directory contains more than 1000 CSV files, an
#' interactive confirmation prompt is shown because the resulting database may
#' be large.
#'
#' If no station metadata rows are present, the function still creates the
#' database schema and weather lookup table.
#'
#' @export
create_database <- function(base_dir = file.path(getwd(), "drifloon_output"),
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
  metadata_dir <- file.path(base_dir, "metadata")
  hourly_dir <- file.path(base_dir, "hourly_station_data")
  station_info_path <- file.path(metadata_dir, "HLY_station_info.csv")

  if (!dir.exists(hourly_dir)) {
    stop("Missing expected directory: ", hourly_dir)
  }
  if (!file.exists(station_info_path)) {
    stop("Missing expected metadata file: ", station_info_path)
  }

  db_path <- file.path(base_dir, db_name)
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

  station_raw <- utils::read.csv(station_info_path, stringsAsFactors = FALSE)

  required_station_cols <- c("Climate.ID", "Station.ID", "stationName")
  missing_station_cols <- setdiff(required_station_cols, names(station_raw))
  if (length(missing_station_cols) > 0) {
    stop(
      "station metadata is missing required columns: ",
      paste(missing_station_cols, collapse = ", ")
    )
  }

  station_table <- data.frame(
    Climate_ID = as.character(station_raw$Climate.ID),
    Station_ID = suppressWarnings(as.integer(station_raw$Station.ID)),
    Station_Name = as.character(station_raw$stationName),
    Province_Name = if ("Province" %in% names(station_raw)) as.character(station_raw$Province) else NA_character_,
    Latitude = if ("Latitude" %in% names(station_raw)) suppressWarnings(as.numeric(station_raw$Latitude)) else NA_real_,
    Longitude = if ("Longitude" %in% names(station_raw)) suppressWarnings(as.numeric(station_raw$Longitude)) else NA_real_,
    Elevation = if ("Elevation..m." %in% names(station_raw)) suppressWarnings(as.numeric(station_raw$Elevation..m.)) else NA_real_,
    HLY_First_Year = if ("HLY.First.Year" %in% names(station_raw)) suppressWarnings(as.integer(station_raw$HLY.First.Year)) else NA_integer_,
    HLY_Last_Year = if ("HLY.Last.Year" %in% names(station_raw)) suppressWarnings(as.integer(station_raw$HLY.Last.Year)) else NA_integer_,
    stringsAsFactors = FALSE
  )

  station_table <- station_table[
    !is.na(station_table$Station_ID) & nzchar(station_table$Climate_ID),
    ,
    drop = FALSE
  ]
  station_table <- station_table[!duplicated(station_table$Station_ID), , drop = FALSE]
  rownames(station_table) <- NULL

  if (nrow(station_table) > 0) {
    DBI::dbWriteTable(con, "Station", station_table, append = TRUE, row.names = FALSE)
  }

  station_id_lookup <- station_table$Station_ID
  names(station_id_lookup) <- station_table$Climate_ID

  csv_files <- list.files(hourly_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

  files_processed <- 0L
  rows_written <- 0L
  batch <- NULL
  batch_size <- as.integer(batch_size)

  if (length(csv_files) > 0 && nrow(station_table) > 0) {
    DBI::dbBegin(con)
    committed <- FALSE
    tryCatch({
      for (path in csv_files) {
        hourly <- tryCatch(
          suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE)),
          error = function(e) NULL
        )
        if (is.null(hourly)) {
          next
        }

        required_hourly_cols <- c("Climate.ID", "Year", "Month", "Day", "Time..LST.")
        if (length(setdiff(required_hourly_cols, names(hourly))) > 0) {
          next
        }

        weather_text <- if ("Weather" %in% names(hourly)) as.character(hourly$Weather) else rep(NA_character_, nrow(hourly))
        weather_id <- as.integer(unname(weather_id_by_condition[weather_text]))

        station_id <- as.integer(unname(station_id_lookup[as.character(hourly$Climate.ID)]))

        rows <- data.frame(
          Station_ID = station_id,
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
