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
#' exceed available data, they are automatically adjusted.
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

  station_folder <- file.path(out_dir, station_name)
  if (!dir.exists(station_folder)) dir.create(station_folder, recursive = TRUE)

  combos <- expand.grid(
    year = years,
    month = months,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  if (isTRUE(parallel)) {
    furrr::future_walk(seq_len(nrow(combos)), function(i) {
      .download_station_month_file(
        station_id,
        station_name,
        station_folder,
        combos$year[[i]],
        combos$month[[i]]
      )
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
