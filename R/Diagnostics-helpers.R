.build_in_clause <- function(con, values) {
  if (length(values) == 0) {
    return("(NULL)")
  }
  quoted <- as.character(DBI::dbQuoteLiteral(con, values))
  paste0("(", paste(quoted, collapse = ", "), ")")
}

.assert_required_tables <- function(con, required = c("Station", "Observation")) {
  tables <- DBI::dbListTables(con)
  missing_tables <- setdiff(required, tables)
  if (length(missing_tables) > 0) {
    stop(
      "Missing required table(s): ",
      paste(missing_tables, collapse = ", "),
      call. = FALSE
    )
  }
}

.assert_required_columns <- function(con, table_name, required_columns) {
  present <- DBI::dbListFields(con, table_name)
  missing <- setdiff(required_columns, present)
  if (length(missing) > 0) {
    stop(
      "Table '",
      table_name,
      "' is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.resolve_diagnostic_scope <- function(
  con,
  station_ids = NULL,
  province_names = NULL,
  years = NULL
) {
  where_parts <- character(0)

  if (!is.null(station_ids)) {
    station_ids <- unique(station_ids)
    if (length(station_ids) == 0) {
      stop("station_ids cannot be an empty vector.", call. = FALSE)
    }

    present_station <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT Station_ID FROM Station WHERE Station_ID IN %s",
        .build_in_clause(con, station_ids)
      )
    )$Station_ID

    missing_station <- setdiff(as.character(station_ids), as.character(present_station))
    if (length(missing_station) > 0) {
      stop(
        "Unknown Station_ID value(s): ",
        paste(missing_station, collapse = ", "),
        call. = FALSE
      )
    }

    where_parts <- c(
      where_parts,
      sprintf("o.Station_ID IN %s", .build_in_clause(con, station_ids))
    )
  }

  if (!is.null(province_names)) {
    province_names <- unique(as.character(province_names))
    province_names <- trimws(province_names)
    province_names <- province_names[nzchar(province_names)]
    if (length(province_names) == 0) {
      stop("province_names cannot be an empty vector.", call. = FALSE)
    }

    province_names_norm <- .province_normalize(province_names, strict = TRUE)

    station_provinces <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT Province_Name FROM Station WHERE Province_Name IS NOT NULL AND TRIM(Province_Name) <> ''"
    )$Province_Name

    station_provinces <- as.character(station_provinces)
    station_provinces_norm <- .province_normalize(station_provinces, strict = FALSE)

    has_match <- province_names_norm %in% station_provinces_norm
    missing_provinces <- province_names[!has_match]
    if (length(missing_provinces) > 0) {
      stop(
        "No stations found for Province_Name value(s): ",
        paste(missing_provinces, collapse = ", "),
        call. = FALSE
      )
    }

    canonical_provinces <- unique(
      station_provinces[station_provinces_norm %in% province_names_norm]
    )
    where_parts <- c(
      where_parts,
      sprintf("s.Province_Name IN %s", .build_in_clause(con, canonical_provinces))
    )
  }

  if (!is.null(years)) {
    years <- as.integer(years)
    if (length(years) == 1) {
      years <- c(years, years)
    }
    if (length(years) != 2 || any(is.na(years))) {
      stop("years must be NULL, a single year, or c(start_year, end_year).", call. = FALSE)
    }

    year_min <- min(years)
    year_max <- max(years)
    where_parts <- c(where_parts, sprintf("o.Year BETWEEN %d AND %d", year_min, year_max))
  }

  if (length(where_parts) == 0) {
    return("")
  }

  paste0("WHERE ", paste(where_parts, collapse = " AND "))
}

.prettify_missing_column_name <- function(column_names) {
  display_names <- gsub("_", " ", column_names, fixed = TRUE)
  tools::toTitleCase(tolower(display_names))
}

.load_variable_ranges <- function(
  ranges_rds_path,
  required_cols = c("Variable", "Mean_Value", "Sd_Value"),
  preserve_cols = character(0)
) {
  if (file.exists(ranges_rds_path)) {
    ranges <- readRDS(ranges_rds_path)
    source_path <- ranges_rds_path
  } else {
    stop(
      "No baseline range file found. Checked: ",
      ranges_rds_path,
      call. = FALSE
    )
  }

  if (!is.data.frame(ranges)) {
    stop("Baseline ranges file must contain a data frame.", call. = FALSE)
  }

  missing_cols <- setdiff(required_cols, names(ranges))
  if (length(missing_cols) > 0) {
    stop(
      "Baseline ranges file is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  keep_cols <- unique(c(required_cols, preserve_cols))
  ranges <- ranges[, intersect(keep_cols, names(ranges)), drop = FALSE]
  ranges$Variable <- as.character(ranges$Variable)
  numeric_cols <- intersect(c("Expected_Lower", "Expected_Upper", "Mean_Value", "Sd_Value"), required_cols)
  for (col_name in numeric_cols) {
    ranges[[col_name]] <- suppressWarnings(as.numeric(ranges[[col_name]]))
  }
  if ("Province_Name" %in% names(ranges)) {
    ranges$Province_Name <- as.character(ranges$Province_Name)
  }
  if (length(numeric_cols) > 0) {
    keep <- rep(TRUE, nrow(ranges))
    for (col_name in numeric_cols) {
      keep <- keep & !is.na(ranges[[col_name]])
    }
    ranges <- ranges[keep, , drop = FALSE]
  }

  if (nrow(ranges) == 0) {
    stop("Baseline ranges file has no usable rows with expected bounds.", call. = FALSE)
  }

  list(data = ranges, source = source_path)
}

.select_out_of_range_baseline <- function(
  con,
  station_ids,
  province = NULL,
  canada_ranges_rds_path,
  province_ranges_rds_path,
  baseline_rds_path = NULL
) {
  selected_province <- NULL

  if (!is.null(province)) {
    province <- unique(trimws(as.character(province)))
    province <- province[nzchar(province)]
    if (length(province) == 1) {
      province_norm <- .province_normalize(province, strict = TRUE)
      station_provinces <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT Province_Name FROM Station WHERE Province_Name IS NOT NULL AND TRIM(Province_Name) <> ''"
      )$Province_Name
      station_provinces <- as.character(station_provinces)
      station_provinces_norm <- .province_normalize(station_provinces, strict = FALSE)
      matched_idx <- which(station_provinces_norm == province_norm)
      if (length(matched_idx) > 0) {
        selected_province <- as.character(station_provinces[matched_idx[1]])
      } else {
        selected_province <- as.character(province_norm[1])
      }
    }
  } else if (!is.null(station_ids)) {
    station_ids <- unique(station_ids)
    station_info <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT Province_Name FROM Station WHERE Station_ID IN %s AND Province_Name IS NOT NULL AND TRIM(Province_Name) <> ''",
        .build_in_clause(con, unique(station_ids))
      )
    )
    station_provinces <- as.character(station_info$Province_Name)
    station_provinces <- station_provinces[nzchar(station_provinces)]
    if (length(unique(station_provinces)) == 1) {
      station_province_norm <- .province_normalize(station_provinces[1], strict = FALSE)
      if (!is.na(station_province_norm)) {
        selected_province <- as.character(station_province_norm)
      } else {
        selected_province <- as.character(station_provinces[1])
      }
    }
  }

  ranges_rds_path <- canada_ranges_rds_path
  profile_label <- "Canada 1980-2020"
  if (!is.null(selected_province)) {
    ranges_rds_path <- province_ranges_rds_path
    profile_label <- paste0(as.character(selected_province), " 1980-2020")
  }

  if (!is.null(baseline_rds_path)) {
    ranges_rds_path <- baseline_rds_path
    profile_label <- tools::file_path_sans_ext(basename(ranges_rds_path))
  }

  ranges_info <- .load_variable_ranges(
    ranges_rds_path,
    preserve_cols = "Province_Name"
  )

  ranges <- ranges_info$data
  if ("Province_Name" %in% names(ranges)) {
    ranges$Province_Name <- trimws(as.character(ranges$Province_Name))
    ranges <- ranges[nzchar(ranges$Province_Name), , drop = FALSE]

    if (is.null(selected_province)) {
      distinct_provinces <- unique(ranges$Province_Name)
      if (length(distinct_provinces) == 1) {
        selected_province <- distinct_provinces[1]
      } else {
        stop(
          "Province-specific baseline ranges require a province selection or a prefiltered single-province RDS file.",
          call. = FALSE
        )
      }
    }

    selected_province_norm <- .province_normalize(selected_province, strict = FALSE)
    province_data_norm <- .province_normalize(ranges$Province_Name, strict = FALSE)
    province_idx <- which(province_data_norm == selected_province_norm)
    if (length(province_idx) == 0) {
      stop(
        "No baseline ranges found for Province_Name value(s): ",
        selected_province,
        call. = FALSE
      )
    }

    ranges <- ranges[
      province_idx,
      c("Variable", "Mean_Value", "Sd_Value"),
      drop = FALSE
    ]
    profile_label <- paste0(as.character(selected_province_norm[1]), " 1980-2020")
  } else {
    ranges <- ranges[, c("Variable", "Mean_Value", "Sd_Value"), drop = FALSE]
  }

  list(
    ranges = ranges,
    source = ranges_info$source,
    profile_label = profile_label
  )
}

.print_diagnostic_table <- function(x) {
  if (!is.data.frame(x)) {
    print(x)
    return(invisible(NULL))
  }

  if (ncol(x) == 0) {
    print.data.frame(x, row.names = FALSE)
    return(invisible(NULL))
  }

  x_chr <- as.data.frame(
    lapply(x, function(col) {
      as.character(col)
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  col_names <- names(x_chr)
  col_widths <- vapply(
    seq_along(x_chr),
    function(i) {
      max(nchar(c(col_names[i], x_chr[[i]]), type = "width"), na.rm = TRUE)
    },
    FUN.VALUE = numeric(1)
  )

  format_cell <- function(value, width, align_left) {
    if (align_left) {
      sprintf("%-*s", width, value)
    } else {
      sprintf("%*s", width, value)
    }
  }

  header <- vapply(
    seq_along(col_names),
    function(i) format_cell(col_names[i], col_widths[i], align_left = (i == 1)),
    FUN.VALUE = character(1)
  )
  cat(paste(header, collapse = "  "), "\n", sep = "")

  if (nrow(x_chr) > 0) {
    for (row_idx in seq_len(nrow(x_chr))) {
      row_values <- vapply(
        seq_along(col_names),
        function(i) format_cell(x_chr[row_idx, i], col_widths[i], align_left = (i == 1)),
        FUN.VALUE = character(1)
      )
      cat(paste(row_values, collapse = "  "), "\n", sep = "")
    }
  }

  invisible(NULL)
}
