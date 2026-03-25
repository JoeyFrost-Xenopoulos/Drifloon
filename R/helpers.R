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

#' Normalize station name key
#'
#' Internal helper to normalize station names for matching while being tolerant
#' of case, spacing, and punctuation differences.
#'
#' @param x Character vector of station names.
#'
#' @return Character vector of normalized station-name keys.
#'
#' @keywords internal
.normalize_station_name_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)

  # Transliterate accents when possible (for example, e -> e)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""

  # Normalize symbols and punctuation to stable alphanumeric keys
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]", "", x)
  x
}

#' Find station row index from station name
#'
#' Internal helper for resolving a station by name with exact normalized
#' matching and fallback fuzzy matching.
#'
#' @param station_name Character. User-provided station name.
#' @param station_data Data frame containing station metadata.
#' @param station_id Numeric. Optional station ID used to disambiguate.
#' @param max_distance Integer. Maximum edit distance for fuzzy matching.
#'
#' @return Integer vector of candidate row indices.
#'
#' @keywords internal
.find_station_name_index <- function(station_name,
                                     station_data,
                                     station_id = NULL,
                                     max_distance = 2L) {
  if (!is.data.frame(station_data) || is.null(station_data$Name) || is.null(station_data$Station.ID)) {
    stop("station_data must contain Name and Station.ID columns.")
  }

  query_key <- .normalize_station_name_key(station_name)
  if (!nzchar(query_key)) {
    stop("station_name must be a non-empty string.")
  }

  name_keys <- .normalize_station_name_key(station_data$Name)
  idx_name <- which(name_keys == query_key)

  # Fuzzy fallback: choose rows at the minimum edit distance when close enough.
  if (length(idx_name) == 0) {
    d <- as.integer(utils::adist(query_key, name_keys))
    d[is.na(d)] <- .Machine$integer.max
    min_d <- suppressWarnings(min(d))

    if (is.finite(min_d) && min_d <= as.integer(max_distance)) {
      idx_name <- which(d == min_d)
    }
  }

  if (!is.null(station_id)) {
    idx <- idx_name[station_data$Station.ID[idx_name] == station_id]
    return(idx)
  }

  idx_name
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

#' Estimate download size for a batch of downloads
#'
#' Calculates estimated number of files and space (in MB) that will be
#' downloaded for a given set of stations and year range.
#'
#' @param stations Data frame containing station metadata with columns
#'   HLY.First.Year and HLY.Last.Year.
#' @param first_year Numeric. Starting year. If NULL, uses station metadata.
#' @param last_year Numeric. Ending year. If NULL, uses station metadata.
#' @param bytes_per_file Numeric. Expected size of each .csv file in bytes.
#'   Default is 130000 (roughly 130 KB).
#'
#' @return List with elements: count (number of files), size_mb (estimated MB),
#'   size_gb (estimated GB), years (number of years).
#'
#' @keywords internal
.estimate_download_size <- function(stations, first_year = NULL, last_year = NULL,
                                    bytes_per_file = 130000) {
  if (is.null(stations) || nrow(stations) == 0) {
    return(list(count = 0, size_mb = 0, size_gb = 0, years = 0))
  }

  # Ensure stations is a data frame
  if (!is.data.frame(stations)) {
    stations <- as.data.frame(stations, stringsAsFactors = FALSE)
  }

  required_cols <- c("HLY.First.Year", "HLY.Last.Year")
  missing_cols <- setdiff(required_cols, names(stations))
  if (length(missing_cols) > 0) {
    stop("stations is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  req_first <- if (is.null(first_year)) NULL else .validate_year(first_year, allow_null = FALSE)
  req_last <- if (is.null(last_year)) NULL else .validate_year(last_year, allow_null = FALSE)

  if (!is.null(req_first) && !is.null(req_last) && req_first > req_last) {
    stop("first_year must be less than or equal to last_year.")
  }

  station_first <- suppressWarnings(as.numeric(stations$HLY.First.Year))
  station_last <- suppressWarnings(as.numeric(stations$HLY.Last.Year))

  effective_first <- if (is.null(req_first)) station_first else pmax(station_first, req_first)
  effective_last <- if (is.null(req_last)) station_last else pmin(station_last, req_last)

  valid_range <- !is.na(effective_first) & !is.na(effective_last) & (effective_first <= effective_last)
  years_per_station <- ifelse(valid_range, effective_last - effective_first + 1, 0)

  n_files <- sum(years_per_station, na.rm = TRUE) * 12  # 12 months per year
  non_zero_years <- years_per_station[years_per_station > 0]
  n_years <- if (length(non_zero_years) == 0) {
    0
  } else if (length(unique(non_zero_years)) == 1) {
    as.integer(non_zero_years[[1]])
  } else {
    paste0(min(non_zero_years), "-", max(non_zero_years), " (station-specific)")
  }

  total_bytes <- n_files * bytes_per_file
  size_mb <- total_bytes / (1024 ^ 2)
  size_gb <- total_bytes / (1024 ^ 3)

  list(
    count = n_files,
    size_mb = round(size_mb, 2),
    size_gb = round(size_gb, 2),
    years = n_years
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

#' Validate year inputs
#'
#' Checks that year values are valid (numeric, not negative, not far in future).
#'
#' @param year Numeric or NULL. Year value(s) to validate.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default is TRUE.
#'
#' @return The validated year as integer, or NULL if allow_null=TRUE and input is NULL.
#'
#' @keywords internal
.validate_year <- function(year, allow_null = TRUE) {
  if (is.null(year)) {
    if (isTRUE(allow_null)) {
      return(NULL)
    } else {
      stop("Year cannot be NULL.")
    }
  }

  year_int <- suppressWarnings(as.integer(year))

  if (is.na(year_int)) {
    stop("Year must be a valid integer.")
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (year_int < 1800) {
    stop("Year must be 1800 or later (received: ", year_int, ").")
  }

  if (year_int > current_year + 1) {
    warning(
      "Year ", year_int, " is in the future. Station data may not be available. ",
      "Proceeding with caution.",
      call. = FALSE
    )
  }

  year_int
}

#' Build station output folder label
#'
#' Uses a normalized station name by default, and appends station ID when
#' metadata indicates duplicate station names.
#'
#' @param station_name Character. Station name.
#' @param station_id Numeric. Station ID.
#' @param station_data Data frame with station metadata.
#'
#' @return Character folder label for station output.
#'
#' @keywords internal
.station_output_label <- function(station_name, station_id, station_data = NULL) {
  label <- gsub(" ", "_", station_name)

  if (is.null(station_data) || !is.data.frame(station_data) || is.null(station_data$Name)) {
    return(label)
  }

  all_labels <- gsub(" ", "_", station_data$Name)
  if (sum(all_labels == label, na.rm = TRUE) > 1) {
    return(paste0(label, "-", station_id))
  }

  label
}

#' Check available disk space
#'
#' Verifies that the output directory has sufficient free space for the estimated download.
#'
#' @param out_dir Character. Path to output directory.
#' @param estimated_bytes Numeric. Estimated download size in bytes.
#' @param buffer_percent Numeric. Extra buffer to require (default: 10, meaning 10\% extra).
#'
#' @return Logical TRUE if sufficient space available, otherwise stops with error.
#'
#' @keywords internal
.available_disk_bytes <- function(out_dir, os_type = .Platform$OS.type) {
  tryCatch({
    if (identical(os_type, "windows")) {
      # Windows: use fsutil against drive root (for example, C:)
      normalized_out_dir <- normalizePath(out_dir, winslash = "\\", mustWork = FALSE)
      drive_root <- sub("^([A-Za-z]:).*$", "\\1", normalized_out_dir)
      if (!grepl("^[A-Za-z]:$", drive_root)) {
        return(NULL)
      }

      cmd_result <- suppressWarnings(
        system2("fsutil", c("volume", "diskfree", drive_root), stdout = TRUE, stderr = FALSE)
      )

      if (length(cmd_result) == 0) {
        return(NULL)
      }

      # Extract bytes available from fsutil output.
      matches <- regmatches(cmd_result, regexpr("[0-9]+$", cmd_result))
      if (length(matches) == 0) {
        return(NULL)
      }

      match_values <- suppressWarnings(as.numeric(matches))
      if (all(is.na(match_values))) {
        return(NULL)
      }

      return(max(match_values, na.rm = TRUE))
    }

    # Unix/macOS/Linux: use POSIX output format with 1K blocks.
    normalized_out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
    cmd_result <- suppressWarnings(
      system2("df", c("-Pk", normalized_out_dir), stdout = TRUE, stderr = FALSE)
    )

    lines <- cmd_result[nzchar(trimws(cmd_result))]
    if (length(lines) < 2) {
      return(NULL)
    }

    # With -P, the last line is guaranteed to be a single POSIX table row.
    fields <- strsplit(trimws(lines[[length(lines)]]), "[[:space:]]+")[[1]]
    if (length(fields) < 4) {
      return(NULL)
    }

    available_kb <- suppressWarnings(as.numeric(fields[[4]]))
    if (is.na(available_kb) || available_kb < 0) {
      return(NULL)
    }

    available_kb * 1024
  }, error = function(e) {
    NULL
  })
}

#' @keywords internal
.check_disk_space <- function(out_dir, estimated_bytes, buffer_percent = 10) {
  if (!dir.exists(out_dir)) {
    tryCatch({
      dir.create(out_dir, recursive = TRUE)
    }, error = function(e) {
      stop("Could not create output directory: ", out_dir, "\n", e$message)
    })
  }

  disk_info <- .available_disk_bytes(out_dir)

  if (is.null(disk_info) || disk_info <= 0) {
    warning(
      "Could not determine available disk space. Proceeding without disk space check.",
      call. = FALSE
    )
    return(TRUE)
  }

  required_bytes <- estimated_bytes * (1 + buffer_percent / 100)

  if (disk_info < required_bytes) {
    available_gb <- round(disk_info / (1024 ^ 3), 2)
    required_gb <- round(required_bytes / (1024 ^ 3), 2)
    warning(
      "Insufficient disk space in '", out_dir, "'.\n",
      "  Available: ", available_gb, " GB\n",
      "  Required (with buffer): ", required_gb, " GB\n",
      "Proceeding anyway; downloads may fail if disk fills up.",
      call. = FALSE
    )
    return(FALSE)
  }

  invisible(TRUE)
}
