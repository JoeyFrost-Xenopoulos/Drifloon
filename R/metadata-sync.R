#' Find header row in station inventory CSV
#'
#' Internal helper to locate the station inventory table header, even when the
#' file starts with disclaimer text lines.
#'
#' @param lines Character vector of file lines.
#'
#' @return Integer row index of the header line.
#'
#' @keywords internal
.normalize_station_inventory_token <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(x)))
}

#' @keywords internal
.station_inventory_header_row <- function(lines) {
  if (length(lines) == 0) {
    stop("Station inventory CSV is empty.")
  }

  clean <- sub("^\\ufeff", "", lines)
  is_header_row <- vapply(clean, function(line) {
    cells <- strsplit(line, ",", fixed = TRUE)[[1]]
    norm <- .normalize_station_inventory_token(cells)

    has_name <- "name" %in% norm
    has_province <- "province" %in% norm
    has_station_id <- any(norm == "stationid" | grepl("^stationid", norm))
    has_hly_first <- any(grepl("^hlyfirst", norm))
    has_hly_last <- any(grepl("^hlylast", norm))

    has_name && has_province && has_station_id && has_hly_first && has_hly_last
  }, logical(1))

  idx <- which(is_header_row)
  if (length(idx) == 0L) {
    stop("Could not find station inventory header row in downloaded CSV.")
  }

  idx[[1]]
}

#' Parse Modified Date from station inventory preamble
#'
#' Internal helper that extracts a modified date stamp when present in the
#' top-left preamble cell (for example: Modified Date: 2026-01-07 23:30 UTC).
#'
#' @param lines Character vector of file lines.
#'
#' @return Character scalar with parsed modified-date text, or NA_character_.
#'
#' @keywords internal
.station_inventory_modified_date <- function(lines) {
  if (length(lines) == 0) {
    return(NA_character_)
  }

  first_nonempty <- lines[nzchar(trimws(lines))]
  if (length(first_nonempty) == 0) {
    return(NA_character_)
  }

  first_cell <- strsplit(sub("^\\ufeff", "", first_nonempty[[1]]), ",", fixed = TRUE)[[1]][1]
  first_cell <- trimws(first_cell)

  if (!grepl("^Modified Date:\\s*", first_cell)) {
    return(NA_character_)
  }

  sub("^Modified Date:\\s*", "", first_cell)
}

#' Read station inventory CSV with preamble support
#'
#' Handles station inventory files that include disclaimer/preamble text before
#' the actual CSV header row.
#'
#' @param csv_path Character. Path to CSV file.
#'
#' @return List with parsed data frame and optional modified-date text.
#'
#' @keywords internal
.standardize_station_inventory_columns <- function(data) {
  col_norm <- .normalize_station_inventory_token(names(data))

  find_col <- function(predicate) {
    idx <- which(predicate(col_norm))
    if (length(idx) == 0L) {
      return(NA_integer_)
    }
    idx[[1]]
  }

  idx_name <- find_col(function(x) x == "name")
  idx_province <- find_col(function(x) x == "province")
  idx_station <- find_col(function(x) x == "stationid" | grepl("^stationid", x))
  idx_hly_first <- find_col(function(x) grepl("^hlyfirst", x))
  idx_hly_last <- find_col(function(x) grepl("^hlylast", x))

  if (any(is.na(c(idx_name, idx_province, idx_station, idx_hly_first, idx_hly_last)))) {
    stop(
      "Station inventory is missing required columns after normalization. Found columns: ",
      paste(names(data), collapse = ", ")
    )
  }

  names(data)[idx_name] <- "Name"
  names(data)[idx_province] <- "Province"
  names(data)[idx_station] <- "Station.ID"
  names(data)[idx_hly_first] <- "HLY.First.Year"
  names(data)[idx_hly_last] <- "HLY.Last.Year"

  data
}

#' @keywords internal
.is_station_inventory_format <- function(data) {
  required_cols <- c("Name", "Province", "Station.ID", "HLY.First.Year", "HLY.Last.Year")
  is.data.frame(data) && all(required_cols %in% names(data))
}

#' @keywords internal
.read_current_station_metadata <- function(csv_path, rds_path) {
  messages <- character(0)
  csv_data <- NULL
  rds_data <- NULL

  if (file.exists(csv_path)) {
    parsed_csv <- tryCatch(
      .read_station_inventory_csv(csv_path),
      error = function(e) e
    )

    if (inherits(parsed_csv, "error")) {
      messages <- c(messages, paste0("Current CSV metadata could not be parsed: ", parsed_csv$message))
    } else if (.is_station_inventory_format(parsed_csv$data)) {
      csv_data <- parsed_csv$data
    } else {
      messages <- c(messages, "Current CSV metadata exists but is not in the expected station format.")
    }
  } else {
    messages <- c(messages, paste0("Current CSV metadata file is missing: ", csv_path))
  }

  if (file.exists(rds_path)) {
    parsed_rds <- tryCatch(
      readRDS(rds_path),
      error = function(e) e
    )

    if (inherits(parsed_rds, "error")) {
      messages <- c(messages, paste0("Current RDS metadata could not be read: ", parsed_rds$message))
    } else if (.is_station_inventory_format(parsed_rds)) {
      rds_data <- parsed_rds
    } else {
      messages <- c(messages, "Current RDS metadata exists but is not in the expected station format.")
    }
  } else {
    messages <- c(messages, paste0("Current RDS metadata file is missing: ", rds_path))
  }

  current_data <- if (!is.null(csv_data)) csv_data else rds_data

  if (!is.null(csv_data) && !is.null(rds_data) && !.same_station_inventory(csv_data, rds_data)) {
    messages <- c(messages, "Current CSV and RDS metadata differ; CSV will be used as the comparison source.")
  }

  list(
    data = current_data,
    valid = !is.null(current_data),
    messages = messages
  )
}

#' @keywords internal
.backup_station_metadata_files <- function(csv_path,
                                           rds_path,
                                           backup_dir = file.path(dirname(rds_path), "metadata_backups"),
                                           max_backup_bytes = 25 * 1024 ^ 2) {
  files <- c(csv_path, rds_path)
  files <- files[file.exists(files)]

  if (length(files) == 0) {
    return(character(0))
  }

  total_bytes <- sum(vapply(files, function(path) file.info(path)$size, numeric(1), USE.NAMES = FALSE), na.rm = TRUE)
  if (is.na(total_bytes) || total_bytes > max_backup_bytes) {
    message(
      "Skipping metadata backup: existing metadata size (", round(total_bytes / 1024 ^ 2, 2),
      " MB) exceeds max_backup_bytes (", round(max_backup_bytes / 1024 ^ 2, 2), " MB)."
    )
    return(character(0))
  }

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backups <- character(0)

  for (src in files) {
    dest <- file.path(backup_dir, paste0(tools::file_path_sans_ext(basename(src)), "_", stamp, ".", tools::file_ext(src)))
    if (isTRUE(file.copy(src, dest, overwrite = TRUE))) {
      backups <- c(backups, dest)
    }
  }

  backups
}

#' @keywords internal
.read_station_inventory_csv <- function(csv_path) {
  if (!file.exists(csv_path)) {
    stop("CSV path does not exist: ", csv_path)
  }

  lines <- readLines(csv_path, warn = FALSE, encoding = "UTF-8")
  header_row <- .station_inventory_header_row(lines)
  modified_date <- .station_inventory_modified_date(lines)

  table_lines <- lines[header_row:length(lines)]
  table_lines <- sub("^\\ufeff", "", table_lines)

  con <- textConnection(table_lines)
  on.exit(close(con), add = TRUE)

  data <- tryCatch(
    utils::read.csv(con, stringsAsFactors = FALSE, check.names = TRUE),
    error = function(e) stop("Failed to parse station inventory table: ", e$message)
  )

  data <- .standardize_station_inventory_columns(data)

  list(data = data, modified_date = modified_date)
}

#' Filter station inventory rows
#'
#' Internal helper that filters inventory rows to stations with valid hourly
#' coverage and optional province/year constraints.
#'
#' @param station_data Data frame read from station inventory CSV.
#' @param require_hourly Logical. If TRUE, keep only rows with non-missing
#'   hourly year bounds.
#' @param provinces Character vector of provinces to keep (optional).
#' @param min_year Numeric. Optional minimum year that must overlap station
#'   hourly coverage.
#' @param max_year Numeric. Optional maximum year that must overlap station
#'   hourly coverage.
#'
#' @return Filtered data frame sorted by Station.ID then Name.
#'
#' @keywords internal
.filter_station_inventory <- function(station_data,
                                      require_hourly = TRUE,
                                      provinces = NULL,
                                      min_year = NULL,
                                      max_year = NULL) {
  if (!is.data.frame(station_data)) {
    stop("station_data must be a data frame.")
  }

  required_cols <- c("Name", "Province", "Station.ID", "HLY.First.Year", "HLY.Last.Year")
  missing_cols <- setdiff(required_cols, names(station_data))
  if (length(missing_cols) > 0) {
    stop("Station inventory is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  station_data$Station.ID <- suppressWarnings(as.numeric(station_data$Station.ID))
  station_data$HLY.First.Year <- suppressWarnings(as.numeric(station_data$HLY.First.Year))
  station_data$HLY.Last.Year <- suppressWarnings(as.numeric(station_data$HLY.Last.Year))

  keep <- !is.na(station_data$Station.ID) & !is.na(station_data$Name) & nzchar(station_data$Name)

  if (isTRUE(require_hourly)) {
    keep <- keep & !is.na(station_data$HLY.First.Year) & !is.na(station_data$HLY.Last.Year)
  }

  if (!is.null(provinces)) {
    normalized_target <- .province_normalize(provinces)
    normalized_station <- .province_normalize(station_data$Province, strict = FALSE)
    keep <- keep & !is.na(normalized_station) & normalized_station %in% normalized_target
  }

  if (!is.null(min_year)) {
    min_year <- .validate_year(min_year, allow_null = FALSE)
    keep <- keep & station_data$HLY.Last.Year >= min_year
  }

  if (!is.null(max_year)) {
    max_year <- .validate_year(max_year, allow_null = FALSE)
    keep <- keep & station_data$HLY.First.Year <= max_year
  }

  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    stop("min_year must be less than or equal to max_year.")
  }

  filtered <- station_data[keep, , drop = FALSE]
  if (nrow(filtered) == 0) {
    return(filtered)
  }

  # Keep a stable ordering so comparison against current metadata is deterministic.
  filtered <- filtered[order(filtered$Station.ID, filtered$Name), , drop = FALSE]
  rownames(filtered) <- NULL
  filtered
}

#' Compare station inventory frames
#'
#' Internal helper that compares two station inventories after stable sorting.
#'
#' @param new_data Data frame with candidate metadata.
#' @param current_data Data frame with current metadata.
#'
#' @return Logical. TRUE when data are equivalent.
#'
#' @keywords internal
.same_station_inventory <- function(new_data, current_data) {
  normalize_df <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (nrow(x) > 0 && all(c("Station.ID", "Name") %in% names(x))) {
      x <- x[order(suppressWarnings(as.numeric(x$Station.ID)), as.character(x$Name)), , drop = FALSE]
    }
    rownames(x) <- NULL
    x[] <- lapply(x, function(col) {
      if (is.factor(col)) as.character(col) else col
    })
    x
  }

  isTRUE(all.equal(normalize_df(new_data), normalize_df(current_data), check.attributes = FALSE))
}

#' Sync station metadata from official inventory CSV
#'
#' Downloads the current station inventory CSV, applies filtering, compares it
#' to the current local metadata, and updates local CSV/RDS files only when
#' content has changed (or when forced).
#'
#' @param station_csv_url Character. Source URL for station inventory CSV.
#' @param csv_path Character. Local output path for filtered CSV metadata.
#' @param rds_path Character. Local output path for filtered RDS metadata.
#' @param force Logical. If TRUE, writes files even when data are unchanged.
#' @param create_backup Logical. If TRUE, backup current metadata before update
#'   when current metadata is valid and small enough.
#' @param max_backup_bytes Numeric. Maximum combined size of current CSV+RDS
#'   files eligible for backup. Default is 25 MB.
#' @param require_hourly Logical. If TRUE, keep only rows with hourly ranges.
#' @param provinces Character vector of provinces to keep (optional).
#' @param min_year Numeric. Minimum year overlap filter.
#' @param max_year Numeric. Optional maximum year overlap filter.
#' @param downloader Function used to download files (default: download.file).
#'
#' @return Invisibly returns a list with update status, modified-date text,
#'   and output paths.
#'
#' @keywords internal
.sync_station_metadata <- function(
    station_csv_url = "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
    csv_path = file.path("extdata", "HLY_station_info.csv"),
    rds_path = file.path("data", "HLY_station_info.rds"),
    force = FALSE,
    create_backup = TRUE,
    max_backup_bytes = 25 * 1024 ^ 2,
    require_hourly = TRUE,
    provinces = NULL,
    min_year = NULL,
    max_year = NULL,
    downloader = download.file) {
  if (!is.character(station_csv_url) || length(station_csv_url) != 1 || !nzchar(station_csv_url)) {
    stop("station_csv_url must be a single non-empty URL string.")
  }

  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    stop("force must be TRUE or FALSE.")
  }

  if (!is.logical(create_backup) || length(create_backup) != 1 || is.na(create_backup)) {
    stop("create_backup must be TRUE or FALSE.")
  }

  if (!is.numeric(max_backup_bytes) || length(max_backup_bytes) != 1 || is.na(max_backup_bytes) || max_backup_bytes <= 0) {
    stop("max_backup_bytes must be a single positive number.")
  }

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)

  tryCatch({
    downloader(station_csv_url, destfile = temp_csv, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    stop("Failed to download station inventory CSV: ", e$message)
  })

  parsed_download <- .read_station_inventory_csv(temp_csv)

  filtered <- .filter_station_inventory(
    parsed_download$data,
    require_hourly = require_hourly,
    provinces = provinces,
    min_year = min_year,
    max_year = max_year
  )

  if (nrow(filtered) == 0) {
    stop("Filtering removed all station rows; no metadata update was written.")
  }

  current <- .read_current_station_metadata(csv_path, rds_path)
  if (length(current$messages) > 0) {
    for (msg in current$messages) {
      message(msg)
    }
  }

  is_current <- !is.null(current$data) && .same_station_inventory(filtered, current$data)
  updated <- isTRUE(force) || !is_current
  backup_files <- character(0)

  if (updated) {
    csv_dir <- dirname(csv_path)
    rds_dir <- dirname(rds_path)

    if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
    if (!dir.exists(rds_dir)) dir.create(rds_dir, recursive = TRUE)

    if (isTRUE(create_backup) && isTRUE(current$valid)) {
      backup_files <- .backup_station_metadata_files(
        csv_path = csv_path,
        rds_path = rds_path,
        max_backup_bytes = max_backup_bytes
      )
      if (length(backup_files) > 0) {
        message("Backed up existing metadata files: ", paste(basename(backup_files), collapse = ", "))
      }
    }

    utils::write.csv(filtered, csv_path, row.names = FALSE, na = "")
    saveRDS(filtered, rds_path)
    message("Station metadata updated. Rows written: ", nrow(filtered))
  } else {
    message("Station metadata is already up to date. No files were changed.")
  }

  invisible(list(
    updated = updated,
    rows = nrow(filtered),
    csv_path = csv_path,
    rds_path = rds_path,
    source_url = station_csv_url,
    source_modified_date = parsed_download$modified_date,
    backup_files = backup_files
  ))
}
