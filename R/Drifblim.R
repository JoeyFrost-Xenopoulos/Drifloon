#' Download hourly climate data for a single station-year-month
#'
#' Downloads one month of hourly climate data for a station selected by name
#' or station ID.
#' @param station_name Character. Station name (optional).
#' @param out_dir Character. Base output directory.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param year Numeric. Year to download.
#' @param month Numeric. Month to download (1–12).
#' @param station_id Numeric. Station ID (optional).
#' @param station_data Data frame containing station metadata. Optional;
#'   if omitted, packaged metadata is loaded automatically.
#' @param downloader Function. Download function (default: \code{download.file}).
#'   Useful for testing/mocking (testthat).
#' @param sleeper Function. Sleep function for throttling (default: \code{Sys.sleep}).
#'
#' @return Invisibly returns \code{TRUE} if successful, \code{FALSE} otherwise.
#'
#' @details
#' Either \code{station_name} or \code{station_id} must be provided.
#' If both are provided, \code{station_name} takes precedence.
#'
#' @export
download_station_month <- function(station_name = NULL,
                                   out_dir = NULL,
                                   year,
                                   month,
                                   station_id = NULL,
                                   station_data = NULL,
                                   downloader = download.file,
                                   sleeper = Sys.sleep) {
  year <- .validate_year(year, allow_null = FALSE)
  month <- as.integer(month)
  if (is.na(month) || month < 1 || month > 12) {
    stop("Invalid month! Month must be between 1 and 12.")
  }

  out_dir <- .resolve_out_dir(out_dir)

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
    stop("Please provide either station_name or station_id.")
  }

  if (length(idx) == 0) {
    stop("No matching station found.")
  }

  station <- station_data[idx[1], ]
  station_id <- station$Station.ID
  station_name <- gsub(" ", "_", station$Name)
  meta_first <- as.numeric(station$HLY.First.Year)
  meta_last <- as.numeric(station$HLY.Last.Year)

  if (is.na(meta_first) || is.na(meta_last)) {
    stop("Station metadata must include valid HLY.First.Year and HLY.Last.Year values.")
  }

  if (year < meta_first || year > meta_last) {
    stop(
      "Requested year (", year, ") is outside available range (",
      meta_first, "-", meta_last, ") for station ", station_name, "."
    )
  }

  station_folder <- file.path(out_dir, station_name)
  if (!dir.exists(station_folder)) dir.create(station_folder, recursive = TRUE)

  .download_station_month_file(
    station_id = station_id,
    station_name = station_name,
    station_folder = station_folder,
    year = year,
    month = month,
    downloader = downloader,
    sleeper = sleeper
  )
}

#' Download station data by name or station ID
#'
#' Wrapper to download data for a station identified by
#' either its name or station ID.
#'
#' @param station_name Character. Station name (optional).
#' @param station_id Numeric. Station ID (optional).
#' @param out_dir Character. Base output directory.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param station_data Data frame containing station metadata. Optional;
#'   if omitted, packaged metadata is loaded automatically.
#' @param first_year Numeric. First year to download (optional).
#' @param last_year Numeric. Last year to download (optional).
#' @param parallel Logical. If \code{TRUE}, months are downloaded in parallel
#'   using \code{furrr::future_pwalk}. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Either \code{station_name} or \code{station_id} must be provided.
#' If both are provided, \code{station_name} takes precedence.
#' If \code{first_year} or \code{last_year} are outside the station metadata
#' range (\code{HLY.First.Year} to \code{HLY.Last.Year}), an error is thrown.
#'
#' @export
download_station_by_name <- function(station_name = NULL,
                               station_id = NULL,
                               out_dir = NULL,
                               station_data = NULL,
                               first_year = NULL,
                               last_year = NULL,
                               parallel = FALSE) {
  out_dir <- .resolve_out_dir(out_dir)

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
  download_station(
    station_row,
    out_dir,
    first_year,
    last_year,
    parallel = parallel
  )
}

#' Download data for all stations in a province
#'
#' Iterates over all stations in a selected province and downloads
#' hourly data for each.
#'
#' @param province Character. Province name or abbreviation.
#' @param station_data Data frame of station metadata. Optional.
#' @param out_dir Character. Base output directory.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param first_year Numeric. Optional starting year.
#' @param last_year Numeric. Optional ending year.
#' @param parallel Logical. If \code{TRUE}, stations are downloaded in parallel
#'   using \code{furrr::future_walk}. Default is \code{FALSE}.
#' @param confirm Logical. If \code{FALSE} (default), displays estimated download
#'   size and requests user confirmation. If \code{TRUE}, skips the confirmation
#'   prompt and proceeds with download.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Could trigger a very large number of downloads.
#' By default, an estimated file count and space requirement is shown before
#' downloading begins. Set \code{confirm = TRUE} to skip this warning.
#'
#'
#' @export
download_station_province <- function(province,
                                      station_data = NULL,
                                      out_dir = NULL,
                                      first_year = NULL,
                                      last_year = NULL,
                                      parallel = FALSE,
                                      confirm = FALSE) {

  # Backward-compatible support for legacy positional signature:
  # download_station_province(station_data, out_dir, province, ...)
  if (!is.character(province) && is.character(out_dir) && length(out_dir) == 1) {
    legacy_station_data <- province
    legacy_out_dir <- station_data
    legacy_province <- out_dir

    station_data <- legacy_station_data
    out_dir <- legacy_out_dir
    province <- legacy_province

    warning(
      "Deprecated positional call detected. Use download_station_province(province, station_data = ..., out_dir = ..., ...) instead.",
      call. = FALSE
    )
  }

  if (missing(province) || is.null(province) || !is.character(province) || length(province) != 1) {
    stop("province must be provided as a single character value.")
  }

  out_dir <- .resolve_out_dir(out_dir)

  if (is.null(station_data)) {
    station_data <- .load_station_metadata()
  }

  province_name <- .province_normalize(province)
  station_province <- .province_normalize(station_data$Province, strict = FALSE)
  stations <- station_data[!is.na(station_province) & station_province == province_name, ]

  if (nrow(stations) == 0) {
    stop("No stations found for province: ", province_name)
  }

  # Estimate and warn about download size
  if (!isTRUE(confirm)) {
    estimate <- .estimate_download_size(stations, first_year, last_year)
    message("\n=== Download Estimate ===")
    message("Province: ", province_name)
    message("Stations: ", nrow(stations))
    message("Years: ", estimate$years)
    message("Estimated files: ", estimate$count)
    if (estimate$size_gb >= 1) {
      message("Estimated space: ", estimate$size_gb, " GB")
    } else {
      message("Estimated space: ", estimate$size_mb, " MB")
    }
    message("=====================\n")
    response <- readline(prompt = "Continue with download? (yes/no): ")
    if (!tolower(trimws(response)) %in% c("yes", "y")) {
      message("Download cancelled.")
      return(invisible(NULL))
    }
  }

  station_rows <- lapply(seq_len(nrow(stations)), function(i) {
    as.list(stations[i, , drop = FALSE])
  })

  if (isTRUE(parallel)) {
    furrr::future_walk(station_rows, function(station_row) {
      # Avoid nested futures by keeping per-station downloads sequential here.
      download_station(station_row, out_dir, first_year, last_year, parallel = FALSE)
    })
  } else {
    purrr::walk(station_rows, function(station_row) {
      download_station(station_row, out_dir, first_year, last_year, parallel = FALSE)
    })
  }

  invisible(NULL)
}

#' Download hourly data for all stations
#'
#' Iterates over all stations in a metadata data frame and downloads
#' hourly data for each from Environment Canada website.
#'
#' @param station_data Data frame of station metadata. Optional;
#'   if omitted, packaged metadata is loaded automatically.
#' @param out_dir Character. Base output directory.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param first_year Numeric. Optional starting year.
#' @param last_year Numeric. Optional ending year.
#' @param parallel Logical. If \code{TRUE}, stations are downloaded in parallel
#'   using \code{furrr::future_pwalk}. Default is \code{FALSE}.
#' @param confirm Logical. If \code{FALSE} (default), displays estimated download
#'   size and requests user confirmation. If \code{TRUE}, skips the confirmation
#'   prompt and proceeds with download.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Use with caution, this will trigger a very large number of downloads.
#' By default, an estimated file count and space requirement is shown before
#' downloading begins. Set \code{confirm = TRUE} to skip this warning.
#'
#' @export
download_all_station <- function(station_data = NULL, out_dir = NULL, first_year = NULL,
                                 last_year = NULL, parallel = FALSE, confirm = FALSE) {
  out_dir <- .resolve_out_dir(out_dir)

  if (is.null(station_data)) {
    station_data <- .load_station_metadata()
  }

  # Estimate and warn about download size
  if (!isTRUE(confirm)) {
    estimate <- .estimate_download_size(station_data, first_year, last_year)
    message("\n=== Download Estimate ===")
    message("Total stations: ", nrow(station_data))
    message("Years: ", estimate$years)
    message("Estimated files: ", estimate$count)
    if (estimate$size_gb >= 1) {
      message("Estimated space: ", estimate$size_gb, " GB")
    } else {
      message("Estimated space: ", estimate$size_mb, " MB")
    }
    message("=====================\n")
    response <- readline(prompt = "Continue with download? (yes/no): ")
    if (!tolower(trimws(response)) %in% c("yes", "y")) {
      message("Download cancelled.")
      return(invisible(NULL))
    }
  }

  walker <- if (isTRUE(parallel)) furrr::future_pwalk else purrr::pwalk

  walker(station_data, function(...) {
    download_station(list(...), out_dir, first_year, last_year, parallel = FALSE)
  })
}

#' Sync station metadata from official inventory
#'
#' User-facing wrapper around internal sync logic.
#'
#' @param ... Additional arguments passed to [Drifloon:::.sync_station_metadata()].
#'
#' @return Invisibly returns sync status details as a list.
#'
#' @export
sync_metadata <- function(...) {
  .sync_station_metadata(...)
}

#' Load station metadata
#'
#' Loads the hourly station metadata included with the package.
#'
#' @param sync Logical. If \code{TRUE}, metadata is synced from the official
#'   station inventory before loading. Default is \code{FALSE}.
#' @param ... Additional arguments passed to [sync_metadata()] when
#'   \code{sync = TRUE}.
#'
#' @return Data frame of station metadata.
#'
#' @details
#' Metadata is loaded using internal package/local metadata resolution.
#' Set \code{sync = TRUE} to refresh metadata first.
#'
#' @export
load_metadata <- function(sync = FALSE, ...) {
  if (!is.logical(sync) || length(sync) != 1 || is.na(sync)) {
    stop("sync must be TRUE or FALSE.")
  }

  if (isTRUE(sync)) {
    sync_metadata(...)
  }

  .load_station_metadata()
}

#' Copy packaged metadata file to disk
#'
#' Copies the packaged \code{HLY_station_info.rds} file to a target directory.
#' If no directory is supplied, the file is copied to the current working
#' directory.
#'
#' @param out_dir Character. Output directory for the metadata file.
#'   Defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param overwrite Logical. If \code{TRUE}, overwrite an existing output file.
#'   Default is \code{FALSE}.
#'
#' @return Character path to the copied \code{.rds} file (invisibly).
#'
#' @export
download_metadata <- function(out_dir = NULL, overwrite = FALSE) {
  out_dir <- .resolve_out_dir(out_dir)

  data_path <- system.file("data", "HLY_station_info.rds", package = "Drifloon")
  if (!nzchar(data_path) || !file.exists(data_path)) {
    stop("Could not find packaged metadata file: data/HLY_station_info.rds")
  }

  dest_path <- file.path(out_dir, "HLY_station_info.rds")
  if (file.exists(dest_path) && !isTRUE(overwrite)) {
    stop(
      "Metadata file already exists at destination. Set overwrite = TRUE to replace it."
    )
  }

  success <- file.copy(from = data_path, to = dest_path, overwrite = overwrite)
  if (!isTRUE(success)) {
    stop("Failed to copy metadata file to destination.")
  }

  invisible(normalizePath(dest_path, winslash = "/", mustWork = FALSE))
}
