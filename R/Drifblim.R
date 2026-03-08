#' Download hourly climate data for a single station-year-month
#'
#' Downloads one month of hourly climate data for a given station from
#' Environment and Climate Change Canada's bulk data endpoint.
#' Main download function which is passed parameters to deal with the downloads
#'
#' @param station_id Numeric. The station ID.
#' @param station_name Character. Station name (used for file naming).
#' @param station_folder Character. Directory where files will be saved.
#' @param year Numeric. Year to download (1980-2020).
#' @param month Numeric. Month to download (1–12).
#' @param downloader Function. Download function (default: \code{download.file}).
#'   Useful for testing/mocking (testthat).
#' @param sleeper Function. Sleep function for throttling (default: \code{Sys.sleep}).
#'
#' @return Invisibly returns \code{TRUE} if successful, \code{FALSE} otherwise.
#' @details
#' Skips download if file already exists and is non-empty.
#' Includes basic error handling and self-throttling (0.3 seconds).
#'
#' @export
download_station_month <- function(station_id, station_name, station_folder,
                               year, month, downloader = download.file,
                               sleeper = Sys.sleep) {
  url <- paste0(
    "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
    "&stationID=", station_id,
    "&Year=", year,
    "&Month=", month,
    "&Day=14&timeframe=1"
  )

  dest_file <- file.path(
    station_folder,
    paste0(station_name, "-", station_id, "-", year, "-", month, ".csv")
  )

  # If file already exists skip it
  if (file.exists(dest_file) && file.info(dest_file)$size > 0) {
    message("Skipping existing file: ", basename(dest_file))
    return(invisible(TRUE))
  }

  # download the governments files :3 (with error handling)
  success <- tryCatch({
    downloader(url, destfile = dest_file, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("Download failed: ", basename(dest_file))
    message("Reason: ", e$message)
    FALSE
  })
  # check if download is valid
  if (!success) {
    message("Invalid or corrupted file: ", basename(dest_file))
    if (file.exists(dest_file)) file.remove(dest_file)
    return(invisible(FALSE))
  }
  # self throttling to not get banned
  sleeper(0.3)
  invisible(TRUE)
}

#' Download full available hourly dataset for a single station
#'
#' @param station List or named vector containing station metadata.
#'   Must include \code{Station.ID}, \code{Name}, \code{Province},
#'   \code{HLY.First.Year}, and \code{HLY.Last.Year}.
#' @param out_dir Character. Base directory where station folders will be created.
#' @param first_year Numeric. First year to download (default: station metadata).
#' @param last_year Numeric. Last year to download (default: station metadata).
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Year ranges are validated against station metadata. If requested years
#' exceed available data, they are automatically adjusted.
#'
#' Data are downloaded month-by-month using \code{purrr::pwalk}.
#' Better than apply() because it coerces to a matrix
#'
#' @export
download_station <- function(station, out_dir, first_year = NULL, last_year = NULL) {

  station_id   <- station$Station.ID
  station_name <- gsub(" ", "_", station$Name)

  # Metadata range
  meta_first <- as.numeric(station$HLY.First.Year)
  meta_last  <- as.numeric(station$HLY.Last.Year)

  # Use metadata if not provided
  begin_year <- if (is.null(first_year)) meta_first else as.numeric(first_year)
  end_year   <- if (is.null(last_year))  meta_last  else as.numeric(last_year)

  # Range validation
  if (begin_year < meta_first) {
    message(
      "Requested start year (", begin_year,
      ") is earlier than available data (", meta_first,
      "). Defaulting to ", meta_first, "."
    )
    begin_year <- meta_first
  }

  if (end_year > meta_last) {
    message(
      "Requested end year (", end_year,
      ") is later than available data (", meta_last,
      "). Defaulting to ", meta_last, "."
    )
    end_year <- meta_last
  }
  # If user does something stupid
  if (begin_year > end_year) {
    stop("No valid years to download after adjusting to station data range.")
  }

  years <- seq(begin_year, end_year)
  months <- 1:12

  station_folder <- file.path(out_dir, paste0(station_name, "-", station_id))
  if (!dir.exists(station_folder)) dir.create(station_folder, recursive = TRUE)

  combos <- expand.grid(year = years, month = months)

  purrr::pwalk(
    combos,
    function(year, month) {
      download_station_month(
        station_id,
        station_name,
        station_folder,
        year,
        month)
    }
  )
}

#' Download station data by name or station ID
#'
#' Wrapper to download data for a station identified by
#' either its name or station ID.
#'
#' @param station_data Data frame containing station metadata. Optional;
#'   if omitted, packaged metadata is loaded automatically.
#' @param out_dir Character. Base output directory.
#' @param station_name Character. Station name (optional).
#' @param station_id Numeric. Station ID (optional).
#' @param first_year Numeric. First year to download (optional).
#' @param last_year Numeric. Last year to download (optional).
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Either \code{station_name} or \code{station_id} must be provided.
#' If both are provided, \code{station_name} takes precedence.
#'
#' @export
.load_station_metadata <- function() {
  data_path <- system.file("data", "HLY_station_info.rds", package = "Drifloon")

  if (nzchar(data_path) && file.exists(data_path)) {
    return(readRDS(data_path))}

  local_path <- file.path("data", "HLY_station_info.rds")

  if (file.exists(local_path)) {
    return(readRDS(local_path))}
  stop(
    "Could not find station metadata. Provide station_data explicitly or add data/HLY_station_info.rds."
    )
}

download_station_by_name <- function(station_data = NULL, out_dir,
                               station_name = NULL,
                               station_id = NULL,
                               first_year = NULL,
                               last_year = NULL) {
  if (is.null(station_data)) {
    station_data <- .load_station_metadata()
  }

  if (!is.null(station_name)) {
    idx <- which(gsub(" ", "_", station_data$Name) == gsub(" ", "_", station_name))
    if (length(idx) > 1) {
      matches <- station_data[idx, c("Name", "Station.ID")]
      options <- paste0(matches$Name, " (Station.ID: ", matches$Station.ID, ")")
      stop(
        paste0(
          "Multiple stations matched '", station_name, "'. ",
          "Please clarify by providing station_id. Matches: ",
          paste(options, collapse = "; ")
        )
      )
    }
    } else if (!is.null(station_id)) {
    idx <- which(station_data$Station.ID == station_id)
    } else {
    stop("Must provide station_name or station_id.")
    }

  # check if station exists
  if (length(idx) == 0) stop("Station not found.")
  station_row <- as.list(station_data[idx[1], ])
  download_station(station_row, out_dir, first_year, last_year)
}


#' Load station metadata into the user's environment
#'
#' Loads the hourly station metadata included with the package and assigns
#' it to the user's global environment as \code{station_metadata}.
#'
#' @return Invisibly returns the metadata data frame.
#'
#' @details
#' The metadata is loaded from the packaged file
#' \code{data/HLY_station_info.csv}. If the package file is not found,
#' a local file in the working directory is used as a fallback.
#'
#' @export
load_metadata <- function() {
  data_path <- system.file("data", "HLY_station_info.rds", package = "Drifloon")

  if (!nzchar(data_path) || !file.exists(data_path)) {
    stop("Could not find station metadata.")
  }

  readRDS(data_path)
}

#' Download hourly data for all stations
#'
#' Iterates over all stations in a metadata data frame and downloads
#' hourly data for each.
#'
#' @param station_data Data frame of station metadata.
#' @param out_dir Character. Base output directory.
#' @param first_year Numeric. Optional starting year.
#' @param last_year Numeric. Optional ending year.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Use with caution — this will trigger a very large number of downloads.
#'
#' @export
download_all_station <- function(station_data, out_dir, first_year = NULL, last_year = NULL) {
  purrr::pwalk(station_data, function(...) {
    download_station(list(...), out_dir, first_year, last_year)
  })
}
