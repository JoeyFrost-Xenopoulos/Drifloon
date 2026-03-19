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
