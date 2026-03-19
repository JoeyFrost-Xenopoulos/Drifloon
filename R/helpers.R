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

  n_stations <- nrow(stations)
  start_year <- if (is.null(first_year)) {
    min(as.numeric(stations$HLY.First.Year), na.rm = TRUE)
  } else {
    max(as.numeric(first_year), min(as.numeric(stations$HLY.First.Year), na.rm = TRUE))
  }

  end_year <- if (is.null(last_year)) {
    max(as.numeric(stations$HLY.Last.Year), na.rm = TRUE)
  } else {
    min(as.numeric(last_year), max(as.numeric(stations$HLY.Last.Year), na.rm = TRUE))
  }

  n_years <- max(0, end_year - start_year + 1)
  n_files <- n_stations * n_years * 12  # 12 months per year
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

#' Check available disk space
#'
#' Verifies that the output directory has sufficient free space for the estimated download.
#'
#' @param out_dir Character. Path to output directory.
#' @param estimated_bytes Numeric. Estimated download size in bytes.
#' @param buffer_percent Numeric. Extra buffer to require (default: 10, meaning 10% extra).
#'
#' @return Logical TRUE if sufficient space available, otherwise stops with error.
#'
#' @keywords internal
.check_disk_space <- function(out_dir, estimated_bytes, buffer_percent = 10) {
  if (!dir.exists(out_dir)) {
    tryCatch({
      dir.create(out_dir, recursive = TRUE)
    }, error = function(e) {
      stop("Could not create output directory: ", out_dir, "\n", e$message)
    })
  }

  # Get disk info using shell command (works on Windows/Linux/Mac)
  disk_info <- tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows: use fsutil
      cmd_result <- system(paste('fsutil volume diskfree', gsub("/", "\\", out_dir, fixed = TRUE)),
                          intern = TRUE, ignore.stderr = TRUE)
      if (length(cmd_result) > 0) {
        # Extract bytes available from fsutil output
        matches <- regmatches(cmd_result, regexpr("[0-9]+$", cmd_result))
        if (length(matches) > 0) {
          as.numeric(matches[[1]])
        } else {
          NULL
        }
      } else {
        NULL
      }
    } else {
      # Unix/Linux/Mac: use df
      df_output <- system(paste("df", out_dir), intern = TRUE)
      lines <- strsplit(df_output[[length(df_output)]], "[[:space:]]+")[[1]]
      as.numeric(lines[4]) * 1024  # Convert to bytes
    }
  }, error = function(e) {
    NULL
  })

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
    stop(
      "Insufficient disk space in '", out_dir, "'.\n",
      "  Available: ", available_gb, " GB\n",
      "  Required (with buffer): ", required_gb, " GB"
    )
  }

  invisible(TRUE)
}
