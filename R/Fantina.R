#' Download one station file by station-year-month
#'
#' Internal helper used by wrappers and station-level downloader.
#'
#' @param station_id Numeric. The station ID.
#' @param station_name Character. Station name (used for file naming).
#' @param station_folder Character. Directory where files will be saved.
#' @param year Numeric. Year to download.
#' @param month Numeric. Month to download (1–12).
#' @param downloader Function. Download function.
#' @param sleeper Function. Sleep function for throttling.
#'
#' @return Invisibly returns \code{TRUE} if successful, \code{FALSE} otherwise.
#'
#' @keywords internal
.is_valid_downloaded_month_file <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    return(FALSE)
  }

  preview <- tryCatch(
    readLines(path, n = 5, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )

  if (length(preview) == 0) {
    return(FALSE)
  }

  preview_text <- tolower(paste(preview, collapse = "\n"))
  if (grepl("<html", preview_text, fixed = TRUE) || grepl("<!doctype", preview_text, fixed = TRUE)) {
    return(FALSE)
  }

  TRUE
}

#' @keywords internal
.download_station_month_file <- function(station_id, station_name, station_folder,
                                         year, month, downloader = download.file,
                                         sleeper = Sys.sleep) {
  year <- as.integer(year)
  month <- as.integer(month)

  if (is.na(year) || is.na(month) || month < 1 || month > 12) {
    stop("year/month must be valid integers and month must be in 1:12.")
  }

  url <- paste0(
    "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
    "&stationID=", station_id,
    "&Year=", year,
    "&Month=", month,
    "&Day=14&timeframe=1"
  )

  dest_file <- file.path(
    station_folder,
    paste0(station_name, "_", year, "_", sprintf("%02d", as.integer(month)), ".csv")
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

  if (!.is_valid_downloaded_month_file(dest_file)) {
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
#' Internal download engine used by exported wrappers such as
#' [download_station_by_name()].
#'
#' @param station List or named vector containing station metadata.
#'   Must include \code{Station.ID}, \code{Name}, \code{Province},
#'   \code{HLY.First.Year}, and \code{HLY.Last.Year}.
#' @param out_dir Character. Base directory where station folders will be created.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param first_year Numeric. First year to download (default: station metadata).
#' @param last_year Numeric. Last year to download (default: station metadata).
#' @param parallel Logical. If \code{TRUE}, downloads months in parallel using
#'   \code{furrr::future_pwalk}. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Year ranges are validated against station metadata. If requested years
#' exceed available data, an error is thrown.
#'
#' Data are downloaded for each year-month pair (months 1 through 12),
#' sequentially by default or in parallel when \code{parallel = TRUE}.
#'
#' @keywords internal
download_station <- function(station, out_dir = NULL, first_year = NULL, last_year = NULL,
                             parallel = FALSE) {

  out_dir <- .resolve_out_dir(out_dir)

  station_id   <- station$Station.ID
  station_name <- gsub(" ", "_", station$Name)
  station_folder_name <- station[["Folder.Name"]]
  if (is.null(station_folder_name) || !is.character(station_folder_name) || !nzchar(station_folder_name)) {
    station_folder_name <- station_name
  }

  # Metadata range
  meta_first <- as.numeric(station$HLY.First.Year)
  meta_last  <- as.numeric(station$HLY.Last.Year)

  if (is.na(meta_first) || is.na(meta_last)) {
    stop("Station metadata must include valid HLY.First.Year and HLY.Last.Year values.")
  }

  # Use metadata if not provided
  begin_year <- if (is.null(first_year)) meta_first else .validate_year(first_year, allow_null = FALSE)
  end_year   <- if (is.null(last_year))  meta_last  else .validate_year(last_year, allow_null = FALSE)

  # Range validation
  if (begin_year < meta_first) {
    stop(
      "Requested start year (", begin_year,
      ") is earlier than available data (", meta_first, ") for station ",
      station_name, "."
    )
  }

  if (end_year > meta_last) {
    stop(
      "Requested end year (", end_year,
      ") is later than available data (", meta_last, ") for station ",
      station_name, "."
    )
  }
  # If user does something stupid
  if (begin_year > end_year) {
    stop("No valid years to download after adjusting to station data range.")
  }

  years <- seq(begin_year, end_year)
  months <- 1:12

  station_folder <- file.path(out_dir, station_folder_name)
  if (!dir.exists(station_folder)) dir.create(station_folder, recursive = TRUE)

  combos <- expand.grid(
    year = years,
    month = months,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Use progressr for progress tracking
  use_progressr <- requireNamespace("progressr", quietly = TRUE)

  if (isTRUE(parallel)) {
    if (use_progressr) {
      progressr::with_progress({
        p <- progressr::progressor(steps = nrow(combos), label = paste("Downloading", station_name))
        furrr::future_walk(seq_len(nrow(combos)), function(i) {
          .download_station_month_file(
            station_id,
            station_name,
            station_folder,
            combos$year[[i]],
            combos$month[[i]]
          )
          p()
        })
      })
    } else {
      furrr::future_walk(seq_len(nrow(combos)), function(i) {
        .download_station_month_file(
          station_id,
          station_name,
          station_folder,
          combos$year[[i]],
          combos$month[[i]]
        )
      })
    }
  } else {
    if (use_progressr) {
      progressr::with_progress({
        p <- progressr::progressor(steps = length(years) * 12, label = paste("Downloading", station_name))
        for (year_i in years) {
          for (month_i in months) {
            .download_station_month_file(
              station_id,
              station_name,
              station_folder,
              year_i,
              month_i
            )
            p()
          }
        }
      })
    } else {
      for (year_i in years) {
        for (month_i in months) {
          .download_station_month_file(
            station_id,
            station_name,
            station_folder,
            year_i,
            month_i
          )
        }
      }
    }
  }
}
