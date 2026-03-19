#' Download hourly climate data for a single station-year-month
#'
#' Downloads one month of hourly climate data for a station selected by name
#' or station ID.
#'
#' @param station_data Data frame containing station metadata. Optional;
#'   if omitted, packaged metadata is loaded automatically.
#' @param out_dir Character. Base output directory.
#'   If not supplied, defaults to \code{file.path(getwd(), "drifloon_output")}.
#' @param year Numeric. Year to download.
#' @param month Numeric. Month to download (1–12).
#' @param station_name Character. Station name (optional).
#' @param station_id Numeric. Station ID (optional).
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
download_station_month <- function(station_data = NULL,
                                   out_dir = NULL,
                                   year,
                                   month,
                                   station_name = NULL,
                                   station_id = NULL,
                                   downloader = download.file,
                                   sleeper = Sys.sleep) {
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

#' Resolve output directory
#'
#' Internal helper that resolves and creates the default output directory when
#' one is not provided.
#'
#' @param out_dir Character. User-provided output directory or \code{NULL}.
#'
#' @return Character path to a valid output directory.
#'
#' @keywords internal
.resolve_out_dir <- function(out_dir = NULL) {
  if (is.null(out_dir)) {
    out_dir <- file.path(getwd(), "drifloon_output")
  }

  if (!is.character(out_dir) || length(out_dir) != 1 || !nzchar(out_dir)) {
    stop("out_dir must be a single, non-empty character path.")
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  out_dir
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

#' Load packaged station metadata
#'
#' Internal helper used by download wrappers when 
#' \code{station_data} is not supplied.
#'
#' @return Data frame of station metadata.
#'
#' @keywords internal
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

#' Normalize province input
#'
#' Converts province names or abbreviations to standardized
#' uppercase full province or territory names.
#'
#' @param province Character vector of province names or abbreviations.
#'
#' @return Character vector of normalized province names in uppercase
#'   (for example, \code{"BRITISH COLUMBIA"}).
#'
#' @keywords internal
.province_normalize <- function(province, strict = TRUE) {
  province_lookup <- c(
    "AB" = "ALBERTA",
    "ALBERTA" = "ALBERTA",
    "BC" = "BRITISH COLUMBIA",
    "BRITISH COLOMBIA" = "BRITISH COLUMBIA",
    "BRITISH COLUMBIA" = "BRITISH COLUMBIA",
    "MB" = "MANITOBA",
    "MANITOBA" = "MANITOBA",
    "NB" = "NEW BRUNSWICK",
    "NEW BRUNSWICK" = "NEW BRUNSWICK",
    "NL" = "NEWFOUNDLAND AND LABRADOR",
    "NF" = "NEWFOUNDLAND AND LABRADOR",
    "NEWFOUNDLAND" = "NEWFOUNDLAND AND LABRADOR",
    "NEWFOUNDLAND AND LABRADOR" = "NEWFOUNDLAND AND LABRADOR",
    "NS" = "NOVA SCOTIA",
    "NOVA SCOTIA" = "NOVA SCOTIA",
    "ON" = "ONTARIO",
    "ONTARIO" = "ONTARIO",
    "PE" = "PRINCE EDWARD ISLAND",
    "PEI" = "PRINCE EDWARD ISLAND",
    "PRINCE EDWARD ISLAND" = "PRINCE EDWARD ISLAND",
    "QC" = "QUEBEC",
    "PQ" = "QUEBEC",
    "QUEBEC" = "QUEBEC",
    "SK" = "SASKATCHEWAN",
    "SASKATCHEWAN" = "SASKATCHEWAN",
    "NU" = "NUNAVUT",
    "NUNAVUT" = "NUNAVUT",
    "NT" = "NORTHWEST TERRITORIES",
    "NWT" = "NORTHWEST TERRITORIES",
    "NORTHWEST TERRITORIES" = "NORTHWEST TERRITORIES",
    "YT" = "YUKON",
    "YUKON" = "YUKON",
    "YUKON TERRITORY" = "YUKON"
  )

  province_key <- trimws(toupper(province))
  province_key <- gsub("[[:space:]]+", " ", province_key)

  normalized <- unname(province_lookup[province_key])
  if (isTRUE(strict) && any(is.na(normalized))) {
    canonical <- unique(unname(province_lookup[nchar(names(province_lookup)) > 2]))
    stop(
      paste0(
        "Invalid province. Choose one of: ",
        paste(canonical, collapse = ", ")
      )
    )
  }

  normalized
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
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Could trigger a very large number of downloads.
#' Backward compatibility note: old positional calls of the form
#' \code{download_station_province(station_data, out_dir, province, ...)}
#' are still supported with a deprecation warning.
#'
#' @export
download_station_province <- function(province,
                                      station_data = NULL,
                                      out_dir = NULL,
                                      first_year = NULL,
                                      last_year = NULL,
                                      parallel = FALSE) {

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
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details
#' Use with caution — this will trigger a very large number of downloads.
#'
#' @export
download_all_station <- function(station_data = NULL, out_dir = NULL, first_year = NULL,
                                 last_year = NULL, parallel = FALSE) {
  out_dir <- .resolve_out_dir(out_dir)

  if (is.null(station_data)) {
    station_data <- .load_station_metadata()
  }

  walker <- if (isTRUE(parallel)) furrr::future_pwalk else purrr::pwalk

  walker(station_data, function(...) {
    # Avoid nested futures by keeping per-station downloads sequential here.
    download_station(list(...), out_dir, first_year, last_year, parallel = FALSE)
  })
}

#' Load station metadata
#'
#' Loads the hourly station metadata included with the package.
#'
#' @return Data frame of station metadata.
#'
#' @details
#' The metadata is loaded from the packaged file
#' \code{data/HLY_station_info.rds}.
#'
#' @export
load_metadata <- function() {
  data_path <- system.file("data", "HLY_station_info.rds", package = "Drifloon")

  if (!nzchar(data_path) || !file.exists(data_path)) {
    stop("Could not find station metadata.")
  }

  readRDS(data_path)
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
