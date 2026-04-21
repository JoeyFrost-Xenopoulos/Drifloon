#' Diagnose Out-of-Range Values Using Baseline Variable Ranges
#'
#' Flags unexpected values by comparing Observation variables against a baseline
#' standard deviation range of +- 3 from a reference profile (for example,
#' historical 1980-2020 data). This is intended as a general warning tool, not
#' a strict physical-validity check. The summary tables also include counts
#' beyond standard deviation +- 5}.
#'
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param out_dir Character. Directory where diagnostic CSV files are written
#'   when \code{write_csv = TRUE}.
#' @param station_ids Optional vector of Station IDs to include.
#' @param province Optional character vector of province names to include.
#' @param years Optional single year or two-value year range.
#' @param variables Character vector of Observation variable names to evaluate.
#' @param canada_ranges_rds_path Character. Path to Canada-wide baseline range
#'   RDS file (1980-2020).
#' @param province_ranges_rds_path Character. Path to province baseline range
#'   RDS file (1980-2020).
#' @param baseline_rds_path Character. Optional override RDS path for the
#'   baseline ranges used in the diagnostics.
#' @param include_station_breakdown Logical. If \code{TRUE}, includes a larger
#'   table with one row per station-variable pair.
#' @param include_outlier_rows Logical. If \code{TRUE}, includes a row-level
#'   table of observations outside the 3 SD range.
#' @param write_csv Logical. If \code{TRUE}, writes diagnostic CSV files.
#'   Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints diagnostic tables to the
#'   R console. Defaults to \code{TRUE}.
#' @param max_rows_print Integer. Maximum rows to print for detailed tables
#'   shown in detailed printed tables. Defaults to \code{20}.
#'
#' @return A named list with \code{range_scope_summary},
#'   \code{out_of_range_by_variable}, and (optionally)
#'   \code{out_of_range_by_station_variable} and
#'   \code{out_of_range_observations}. The return value is invisible.
#' @export
range_diagnostics <- function(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  province = NULL,
  years = NULL,
  variables = c(
    "Temp_C",
    "Dew_Point_C",
    "Rel_Hum",
    "Wind_Dir_deg",
    "Wind_Spd_kmh",
    "Visibility_km",
    "Stn_Press_kPa",
    "Hmdx",
    "Wind_Chill"
  ),
  canada_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds"),
  province_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds"),
  include_station_breakdown = FALSE,
  include_outlier_rows = FALSE,
  write_csv = FALSE,
  verbose = TRUE,
  max_rows_print = 20L,
  baseline_rds_path = NULL
) {
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("base_dir must be a single, non-empty character path.", call. = FALSE)
  }
  if (!is.character(db_name) || length(db_name) != 1 || !nzchar(db_name)) {
    stop("db_name must be a single, non-empty character value.", call. = FALSE)
  }
  if (!is.character(out_dir) || length(out_dir) != 1 || !nzchar(out_dir)) {
    stop("out_dir must be a single, non-empty character path.", call. = FALSE)
  }
  if (!is.character(canada_ranges_rds_path) || length(canada_ranges_rds_path) != 1 ||
      !nzchar(canada_ranges_rds_path)) {
    stop("canada_ranges_rds_path must be a single, non-empty character path.", call. = FALSE)
  }
  if (!is.character(province_ranges_rds_path) || length(province_ranges_rds_path) != 1 ||
      !nzchar(province_ranges_rds_path)) {
    stop("province_ranges_rds_path must be a single, non-empty character path.", call. = FALSE)
  }
  if (!is.null(baseline_rds_path) &&
      (!is.character(baseline_rds_path) || length(baseline_rds_path) != 1 || !nzchar(baseline_rds_path))) {
    stop("baseline_rds_path must be a single, non-empty character path when provided.", call. = FALSE)
  }
  if (!is.character(variables) || length(variables) == 0) {
    stop("variables must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.null(province) && (!is.character(province) || length(province) == 0)) {
    stop("province must be a non-empty character vector when provided.", call. = FALSE)
  }
  if (!is.logical(include_station_breakdown) || length(include_station_breakdown) != 1 ||
      is.na(include_station_breakdown)) {
    stop("include_station_breakdown must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_outlier_rows) || length(include_outlier_rows) != 1 ||
      is.na(include_outlier_rows)) {
    stop("include_outlier_rows must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(write_csv) || length(write_csv) != 1 || is.na(write_csv)) {
    stop("write_csv must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_rows_print) || length(max_rows_print) != 1 ||
      is.na(max_rows_print) || max_rows_print < 1) {
    stop("max_rows_print must be a single positive number.", call. = FALSE)
  }
  max_rows_print <- as.integer(max_rows_print)

  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  db_path <- file.path(base_dir, "database", db_name)
  canada_ranges_rds_path <- normalizePath(canada_ranges_rds_path, winslash = "/", mustWork = FALSE)
  province_ranges_rds_path <- normalizePath(province_ranges_rds_path, winslash = "/", mustWork = FALSE)

  if (!file.exists(db_path)) {
    stop("Database file not found: ", db_path, call. = FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .assert_required_tables(con)
  .assert_required_columns(con, "Station", c("Station_ID", "Station_Name", "Climate_ID"))

  observation_fields <- DBI::dbListFields(con, "Observation")
  variables <- unique(as.character(variables))

  baseline_info <- .select_out_of_range_baseline(
    con = con,
    station_ids = station_ids,
    province = province,
    canada_ranges_rds_path = canada_ranges_rds_path,
    province_ranges_rds_path = province_ranges_rds_path,
    baseline_rds_path = baseline_rds_path
  )
  baseline_ranges <- baseline_info$ranges

  missing_in_observation <- setdiff(variables, observation_fields)
  if (length(missing_in_observation) > 0) {
    stop(
      "Observation is missing variable column(s): ",
      paste(missing_in_observation, collapse = ", "),
      call. = FALSE
    )
  }

  baseline_ranges <- baseline_ranges[baseline_ranges$Variable %in% variables, , drop = FALSE]
  missing_in_baseline <- setdiff(variables, baseline_ranges$Variable)
  if (length(missing_in_baseline) > 0) {
    stop(
      "Baseline ranges are missing variable(s): ",
      paste(missing_in_baseline, collapse = ", "),
      call. = FALSE
    )
  }

  baseline_ranges <- baseline_ranges[order(match(baseline_ranges$Variable, variables)), , drop = FALSE]

  baseline_ranges$Mean_Value <- suppressWarnings(as.numeric(baseline_ranges$Mean_Value))
  baseline_ranges$Sd_Value <- suppressWarnings(as.numeric(baseline_ranges$Sd_Value))
  invalid_baseline <- !is.finite(baseline_ranges$Mean_Value) |
    !is.finite(baseline_ranges$Sd_Value) |
    baseline_ranges$Sd_Value < 0
  if (any(invalid_baseline)) {
    stop(
      "Baseline ranges contain invalid Mean_Value/Sd_Value for variable(s): ",
      paste(baseline_ranges$Variable[invalid_baseline], collapse = ", "),
      ". Mean_Value and Sd_Value must be finite numbers, and Sd_Value must be >= 0.",
      call. = FALSE
    )
  }

  where_clause <- .resolve_diagnostic_scope(
    con,
    station_ids = station_ids,
    province_names = province,
    years = years
  )

  scope_columns <- unique(c(
    "Year",
    if (isTRUE(include_outlier_rows)) c("Month", "Day", "Time_LST") else character(0),
    variables
  ))
  scope_column_sql <- paste(sprintf("o.%s AS %s", scope_columns, scope_columns), collapse = ",\n        ")

  DBI::dbExecute(
    con,
    sprintf(
      "
      CREATE TEMP TABLE scoped_observations AS
      SELECT
        s.Station_Name AS station_name,
        s.Station_ID AS station_id,
        %s
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s;
      ",
      scope_column_sql,
      where_clause
    )
  )

  range_scope_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      o.station_name AS \"Station Name\",
      o.station_id AS \"Station ID\",
      CAST(MIN(o.Year) AS TEXT) || '-' || CAST(MAX(o.Year) AS TEXT) AS \"Year Range\"
    FROM scoped_observations o
    GROUP BY o.station_name, o.station_id
    ORDER BY o.station_name;
    "
  )

  variable_sql <- vapply(
    seq_len(nrow(baseline_ranges)),
    function(i) {
      col_name <- baseline_ranges$Variable[i]
      lower_3sd <- baseline_ranges$Mean_Value[i] - 3 * baseline_ranges$Sd_Value[i]
      upper_3sd <- baseline_ranges$Mean_Value[i] + 3 * baseline_ranges$Sd_Value[i]
      lower_5sd <- baseline_ranges$Mean_Value[i] - 5 * baseline_ranges$Sd_Value[i]
      upper_5sd <- baseline_ranges$Mean_Value[i] + 5 * baseline_ranges$Sd_Value[i]

      sprintf(
        "
        SELECT
          '%s' AS variable_key,
          SUM(CASE WHEN o.%s IS NOT NULL THEN 1 ELSE 0 END) AS observations_checked,
          SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_3sd_cells,
          SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_5sd_cells
        FROM scoped_observations o
        ",
        col_name,
        col_name,
        col_name,
        col_name,
        lower_3sd,
        col_name,
        upper_3sd,
        col_name,
        col_name,
        lower_5sd,
        col_name,
        upper_5sd
      )
    },
    FUN.VALUE = character(1)
  )

  out_of_range_by_variable <- DBI::dbGetQuery(
    con,
    paste(variable_sql, collapse = "\nUNION ALL\n")
  )

  out_of_range_by_variable[["Outside 3 SD +- Percent"]] <- ifelse(
    out_of_range_by_variable$observations_checked > 0,
    round(
      100.0 * out_of_range_by_variable$outside_3sd_cells /
        out_of_range_by_variable$observations_checked,
      2
    ),
    NA_real_
  )

  out_of_range_by_variable <- out_of_range_by_variable[
    order(out_of_range_by_variable[["Outside 3 SD +- Percent"]], decreasing = TRUE),
    c(
      "variable_key",
      "observations_checked",
      "outside_3sd_cells",
      "Outside 3 SD +- Percent",
      "outside_5sd_cells"
    )
  ]

  names(out_of_range_by_variable) <- c(
    "Variable",
    "Observations Checked",
    "Outside 3 SD +-",
    "Outside 3 SD +- Percent",
    "Outside 5 SD +-"
  )
  out_of_range_by_variable[["Variable"]] <-
    .prettify_missing_column_name(out_of_range_by_variable[["Variable"]])

  out_of_range_by_station_variable <- NULL
  if (isTRUE(include_station_breakdown)) {
    station_variable_sql <- vapply(
      seq_len(nrow(baseline_ranges)),
      function(i) {
        col_name <- baseline_ranges$Variable[i]
        lower_3sd <- baseline_ranges$Mean_Value[i] - 3 * baseline_ranges$Sd_Value[i]
        upper_3sd <- baseline_ranges$Mean_Value[i] + 3 * baseline_ranges$Sd_Value[i]
        lower_5sd <- baseline_ranges$Mean_Value[i] - 5 * baseline_ranges$Sd_Value[i]
        upper_5sd <- baseline_ranges$Mean_Value[i] + 5 * baseline_ranges$Sd_Value[i]

        sprintf(
          "
          SELECT
            o.station_name AS station_name,
            o.station_id AS station_id,
            '%s' AS variable_key,
            SUM(CASE WHEN o.%s IS NOT NULL THEN 1 ELSE 0 END) AS observations_checked,
            SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_3sd_cells,
            SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_5sd_cells
          FROM scoped_observations o
          GROUP BY o.station_name, o.station_id
          ",
          col_name,
          col_name,
          col_name,
          col_name,
          lower_3sd,
          col_name,
          upper_3sd,
          col_name,
          col_name,
          lower_5sd,
          col_name,
          upper_5sd
        )
      },
      FUN.VALUE = character(1)
    )

    out_of_range_by_station_variable <- DBI::dbGetQuery(
      con,
      paste(station_variable_sql, collapse = "\nUNION ALL\n")
    )

    out_of_range_by_station_variable[["Outside 3 SD +- Percent"]] <- ifelse(
      out_of_range_by_station_variable$observations_checked > 0,
      round(
        100.0 * out_of_range_by_station_variable$outside_3sd_cells /
          out_of_range_by_station_variable$observations_checked,
        2
      ),
      NA_real_
    )

    out_of_range_by_station_variable <- out_of_range_by_station_variable[
      order(
        out_of_range_by_station_variable$station_name,
        -out_of_range_by_station_variable[["Outside 3 SD +- Percent"]],
        out_of_range_by_station_variable$variable_key
      ),
      c(
        "station_name",
        "station_id",
        "variable_key",
        "observations_checked",
        "outside_3sd_cells",
        "Outside 3 SD +- Percent",
        "outside_5sd_cells"
      )
    ]

    names(out_of_range_by_station_variable) <- c(
      "Station Name",
      "Station ID",
      "Variable",
      "Observations Checked",
      "Outside 3 SD +-",
      "Outside 3 SD +- Percent",
      "Outside 5 SD +-"
    )
    out_of_range_by_station_variable[["Variable"]] <-
      .prettify_missing_column_name(out_of_range_by_station_variable[["Variable"]])
  }

  out_of_range_observations <- NULL
  if (isTRUE(include_outlier_rows)) {
    observation_sql <- vapply(
      seq_len(nrow(baseline_ranges)),
      function(i) {
        col_name <- baseline_ranges$Variable[i]
        lower_3sd <- baseline_ranges$Mean_Value[i] - 3 * baseline_ranges$Sd_Value[i]
        upper_3sd <- baseline_ranges$Mean_Value[i] + 3 * baseline_ranges$Sd_Value[i]

        sprintf(
          "
          SELECT
            o.station_name AS \"Station Name\",
            o.station_id AS \"Station ID\",
            o.Year AS \"Year\",
            o.Month AS \"Month\",
            o.Day AS \"Day\",
            o.Time_LST AS \"Time LST\",
            '%s' AS \"Variable\",
            CAST(o.%s AS REAL) AS \"Observed Value\",
            %.15g AS \"Lower 3 SD\",
            %.15g AS \"Upper 3 SD\"
          FROM scoped_observations o
          WHERE o.%s IS NOT NULL
          AND (o.%s < %.15g OR o.%s > %.15g)
          ",
          col_name,
          col_name,
          lower_3sd,
          upper_3sd,
          col_name,
          col_name,
          lower_3sd,
          col_name,
          upper_3sd
        )
      },
      FUN.VALUE = character(1)
    )

    out_of_range_observations <- DBI::dbGetQuery(
      con,
      paste(observation_sql, collapse = "\nUNION ALL\n")
    )

    if (nrow(out_of_range_observations) > 0) {
      out_of_range_observations <- out_of_range_observations[
        order(
          out_of_range_observations[["Station Name"]],
          out_of_range_observations[["Variable"]],
          out_of_range_observations[["Year"]],
          out_of_range_observations[["Month"]],
          out_of_range_observations[["Day"]]
        ),
        ,
        drop = FALSE
      ]
    }
  }

  no_scope <- sum(out_of_range_by_variable[["Observations Checked"]], na.rm = TRUE) == 0
  if (isTRUE(no_scope) || nrow(range_scope_summary) == 0) {
    range_scope_summary <- data.frame(
      "Summary" = "No observations matched the selected diagnostic scope.",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (isTRUE(write_csv)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    write.csv(
      range_scope_summary,
      file.path(out_dir, "out_of_range_scope_summary.csv"),
      row.names = FALSE
    )
    write.csv(
      out_of_range_by_variable,
      file.path(out_dir, "out_of_range_by_variable.csv"),
      row.names = FALSE
    )

    if (isTRUE(include_station_breakdown)) {
      write.csv(
        out_of_range_by_station_variable,
        file.path(out_dir, "out_of_range_by_station_variable.csv"),
        row.names = FALSE
      )
    }

    if (isTRUE(include_outlier_rows)) {
      write.csv(
        out_of_range_observations,
        file.path(out_dir, "out_of_range_observations.csv"),
        row.names = FALSE
      )
    }
  }

  if (isTRUE(no_scope)) {
    warning("No observations matched the selected diagnostic scope.", call. = FALSE)
  }

  result <- list(
    range_scope_summary = range_scope_summary,
    out_of_range_by_variable = out_of_range_by_variable,
    out_of_range_by_station_variable = out_of_range_by_station_variable,
    out_of_range_observations = out_of_range_observations,
    baseline_profile = baseline_info$profile_label,
    baseline_source = baseline_info$source
  )

  if (isTRUE(verbose)) {
    cat("\nBaseline profile:\n")
    cat(result$baseline_profile, "\n")

    cat("\nOut-of-range diagnostics summary\n")
    .print_diagnostic_table(result$range_scope_summary)

    cat("\nBy variable\n")
    .print_diagnostic_table(result$out_of_range_by_variable)

    if (isTRUE(include_station_breakdown) && !is.null(result$out_of_range_by_station_variable)) {
      cat("\nBy station and variable (top rows)\n")
      .print_diagnostic_table(utils::head(result$out_of_range_by_station_variable, max_rows_print))
      if (nrow(result$out_of_range_by_station_variable) > max_rows_print) {
        cat("...", nrow(result$out_of_range_by_station_variable) - max_rows_print, "more rows\n")
      }
    }

    if (isTRUE(include_outlier_rows) && !is.null(result$out_of_range_observations)) {
      cat("\nOut-of-range observations (top rows)\n")
      .print_diagnostic_table(utils::head(result$out_of_range_observations, max_rows_print))
      if (nrow(result$out_of_range_observations) > max_rows_print) {
        cat("...", nrow(result$out_of_range_observations) - max_rows_print, "more rows\n")
      }
    }

    cat("\nBaseline ranges source:\n")
    cat(result$baseline_source, "\n")
  }

  invisible(result)
}

#' Plot Station Observations Against Baseline SD Bounds
#'
#' Creates one panel per variable for a selected station and highlights
#' observations outside a baseline \eqn{\mu \pm k\sigma} range in red.
#'
#' @param station Station identifier. Either a single Station ID (numeric) or
#'   station name (character).
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param province Optional character vector of province names to include.
#' @param years Optional single year or two-value year range.
#' @param variables Character vector of Observation variable names to plot.
#' @param sd_threshold Numeric. SD multiplier for the bounds. Defaults to
#'   \code{3}.
#' @param panels_per_row Integer. Number of variable panels per row.
#' @param canada_ranges_rds_path Character. Path to Canada-wide baseline range
#'   RDS file (1980-2020).
#' @param province_ranges_rds_path Character. Path to province baseline range
#'   RDS file (1980-2020).
#' @param baseline_rds_path Character. Optional override RDS path for the
#'   baseline ranges used in the diagnostics.
#' @param point_cex Numeric. Point size used for plotting.
#' @param verbose Logical. If \code{TRUE}, prints a short summary table.
#'
#' @return Invisibly returns a list with station metadata, selected baseline
#'   profile, and a per-variable summary of observations outside bounds.
#' @export
plot_station_outliers <- function(
  station,
  base_dir = getwd(),
  db_name = "climate_database.db",
  province = NULL,
  years = NULL,
  variables = c(
    "Temp_C",
    "Dew_Point_C",
    "Rel_Hum",
    "Wind_Dir_deg",
    "Wind_Spd_kmh",
    "Visibility_km",
    "Stn_Press_kPa",
    "Hmdx",
    "Wind_Chill"
  ),
  sd_threshold = 3,
  panels_per_row = 3L,
  canada_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds"),
  province_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds"),
  baseline_rds_path = NULL,
  point_cex = 0.6,
  verbose = TRUE
) {
  if (missing(station)) {
    stop("station must be provided (Station ID or Station Name).", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("base_dir must be a single, non-empty character path.", call. = FALSE)
  }
  if (!is.character(db_name) || length(db_name) != 1 || !nzchar(db_name)) {
    stop("db_name must be a single, non-empty character value.", call. = FALSE)
  }
  if (!is.character(variables) || length(variables) == 0) {
    stop("variables must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(sd_threshold) || length(sd_threshold) != 1 || is.na(sd_threshold) || sd_threshold <= 0) {
    stop("sd_threshold must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(panels_per_row) || length(panels_per_row) != 1 || is.na(panels_per_row) || panels_per_row < 1) {
    stop("panels_per_row must be a single positive number.", call. = FALSE)
  }
  panels_per_row <- as.integer(panels_per_row)
  if (!is.numeric(point_cex) || length(point_cex) != 1 || is.na(point_cex) || point_cex <= 0) {
    stop("point_cex must be a single positive number.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.", call. = FALSE)
  }

  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  db_path <- file.path(base_dir, "database", db_name)

  if (!file.exists(db_path)) {
    stop("Database file not found: ", db_path, call. = FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .assert_required_tables(con)
  .assert_required_columns(con, "Station", c("Station_ID", "Station_Name", "Climate_ID"))

  station_table <- DBI::dbGetQuery(
    con,
    "SELECT Station_ID, Station_Name FROM Station ORDER BY Station_Name"
  )
  if (nrow(station_table) == 0) {
    stop("Station table is empty.", call. = FALSE)
  }

  station_id <- NA_integer_
  station_name <- NA_character_

  if (is.numeric(station) && length(station) == 1 && !is.na(station)) {
    station_id <- as.integer(station)
    idx <- which(as.integer(station_table$Station_ID) == station_id)
    if (length(idx) != 1) {
      stop("Unknown station ID: ", station_id, call. = FALSE)
    }
    station_name <- as.character(station_table$Station_Name[idx[1]])
  } else if (is.character(station) && length(station) == 1 && nzchar(station)) {
    query_key <- .normalize_station_name_key(station)
    station_keys <- .normalize_station_name_key(station_table$Station_Name)
    idx <- which(station_keys == query_key)
    if (length(idx) == 0) {
      stop("No station matched name: ", station, call. = FALSE)
    }
    if (length(idx) > 1) {
      stop(
        "Multiple stations matched name '",
        station,
        "'. Please pass station ID instead.",
        call. = FALSE
      )
    }
    station_id <- as.integer(station_table$Station_ID[idx[1]])
    station_name <- as.character(station_table$Station_Name[idx[1]])
  } else {
    stop("station must be a single Station ID (numeric) or Station Name (character).", call. = FALSE)
  }

  observation_fields <- DBI::dbListFields(con, "Observation")
  missing_in_observation <- setdiff(variables, observation_fields)
  if (length(missing_in_observation) > 0) {
    stop(
      "Observation is missing variable column(s): ",
      paste(missing_in_observation, collapse = ", "),
      call. = FALSE
    )
  }

  baseline_info <- .select_out_of_range_baseline(
    con = con,
    station_ids = station_id,
    province = province,
    canada_ranges_rds_path = canada_ranges_rds_path,
    province_ranges_rds_path = province_ranges_rds_path,
    baseline_rds_path = baseline_rds_path
  )
  baseline_ranges <- baseline_info$ranges
  baseline_ranges <- baseline_ranges[baseline_ranges$Variable %in% variables, , drop = FALSE]

  missing_in_baseline <- setdiff(variables, baseline_ranges$Variable)
  if (length(missing_in_baseline) > 0) {
    stop(
      "Baseline ranges are missing variable(s): ",
      paste(missing_in_baseline, collapse = ", "),
      call. = FALSE
    )
  }

  baseline_ranges <- baseline_ranges[order(match(baseline_ranges$Variable, variables)), , drop = FALSE]
  baseline_ranges$Mean_Value <- suppressWarnings(as.numeric(baseline_ranges$Mean_Value))
  baseline_ranges$Sd_Value <- suppressWarnings(as.numeric(baseline_ranges$Sd_Value))
  invalid_baseline <- !is.finite(baseline_ranges$Mean_Value) |
    !is.finite(baseline_ranges$Sd_Value) |
    baseline_ranges$Sd_Value < 0
  if (any(invalid_baseline)) {
    stop(
      "Baseline ranges contain invalid Mean_Value/Sd_Value for variable(s): ",
      paste(baseline_ranges$Variable[invalid_baseline], collapse = ", "),
      ". Mean_Value and Sd_Value must be finite numbers, and Sd_Value must be >= 0.",
      call. = FALSE
    )
  }

  where_clause <- .resolve_diagnostic_scope(
    con,
    station_ids = station_id,
    province_names = province,
    years = years
  )

  variable_sql <- paste(sprintf("o.%s AS %s", variables, variables), collapse = ",\n        ")
  station_data <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        o.Year,
        o.Month,
        o.Day,
        o.Time_LST,
        %s
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s
      ORDER BY o.Time_LST;
      ",
      variable_sql,
      where_clause
    )
  )

  if (nrow(station_data) == 0) {
    stop("No observations matched the selected station/scope.", call. = FALSE)
  }

  large_plot_warning_threshold <- 40000L
  observation_count <- nrow(station_data)
  if (observation_count > large_plot_warning_threshold) {
    warning(
      "plot_station_outliers() is about to plot ",
      observation_count,
      " observations (>",
      large_plot_warning_threshold,
      "). This may take a while. Consider narrowing years or variables.",
      call. = FALSE
    )
  }

  safe_parse_time <- function(x, formats) {
    parsed <- rep(as.POSIXct(NA), length(x))
    for (fmt in formats) {
      idx <- which(is.na(parsed) & !is.na(x) & nzchar(x))
      if (length(idx) == 0) {
        break
      }
      trial <- tryCatch(
        suppressWarnings(as.POSIXct(x[idx], format = fmt, tz = "UTC")),
        error = function(e) rep(as.POSIXct(NA), length(idx))
      )
      parsed[idx] <- trial
    }
    parsed
  }

  time_raw <- as.character(station_data$Time_LST)
  time_raw[is.na(time_raw)] <- ""

  time_x <- safe_parse_time(
    time_raw,
    c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M:%S",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M"
    )
  )

  # Fallback for time-only entries (for example, "13:00") using Year/Month/Day.
  needs_time_only <- is.na(time_x) & grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", time_raw)
  if (any(needs_time_only)) {
    date_prefix <- sprintf(
      "%04d-%02d-%02d",
      as.integer(station_data$Year[needs_time_only]),
      as.integer(station_data$Month[needs_time_only]),
      as.integer(station_data$Day[needs_time_only])
    )
    datetime_text <- paste(date_prefix, time_raw[needs_time_only])
    repaired <- safe_parse_time(
      datetime_text,
      c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")
    )
    time_x[needs_time_only] <- repaired
  }

  valid_time_n <- sum(!is.na(time_x))
  use_time_axis <- valid_time_n >= max(1L, floor(0.8 * nrow(station_data)))
  x_values <- if (use_time_axis) time_x else seq_len(nrow(station_data))
  x_label <- if (use_time_axis) "Time (LST)" else "Observation Index"

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  n_vars <- length(variables)
  n_rows <- ceiling(n_vars / panels_per_row)
  graphics::par(mfrow = c(n_rows, panels_per_row), mar = c(3.5, 3.8, 2.6, 1.2), oma = c(0, 0, 2, 0))

  summary_rows <- vector("list", length = n_vars)

  for (i in seq_len(n_vars)) {
    var_name <- variables[i]
    y <- suppressWarnings(as.numeric(station_data[[var_name]]))

    mu <- baseline_ranges$Mean_Value[i]
    sigma <- baseline_ranges$Sd_Value[i]
    lower <- mu - sd_threshold * sigma
    upper <- mu + sd_threshold * sigma

    checked <- sum(!is.na(y))
    flagged <- sum(!is.na(y) & (y < lower | y > upper))

    plot_title <- sprintf("%s (%d/%d)", .prettify_missing_column_name(var_name), flagged, checked)

    graphics::plot(
      x_values,
      y,
      pch = 16,
      cex = point_cex,
      col = "black",
      xlab = x_label,
      ylab = .prettify_missing_column_name(var_name),
      main = plot_title
    )
    graphics::abline(h = c(lower, upper), col = "steelblue", lty = 2)

    out_idx <- which(!is.na(y) & (y < lower | y > upper))
    if (length(out_idx) > 0) {
      graphics::points(x_values[out_idx], y[out_idx], pch = 16, cex = point_cex, col = "red")
    }

    summary_rows[[i]] <- data.frame(
      Variable = .prettify_missing_column_name(var_name),
      `Observations Checked` = checked,
      `Outside SD` = flagged,
      `Outside SD Percent` = if (checked > 0) round(100 * flagged / checked, 2) else NA_real_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  graphics::mtext(
    sprintf(
      "%s (ID %d) | Baseline: %s | Threshold: +/- %.2f SD",
      station_name,
      station_id,
      baseline_info$profile_label,
      sd_threshold
    ),
    outer = TRUE,
    cex = 0.9,
    line = 0.3
  )

  summary_df <- do.call(rbind, summary_rows)
  if (isTRUE(verbose)) {
    cat("\nStation outlier summary\n")
    .print_diagnostic_table(summary_df)
  }

  invisible(list(
    station_id = station_id,
    station_name = station_name,
    baseline_profile = baseline_info$profile_label,
    baseline_source = baseline_info$source,
    sd_threshold = sd_threshold,
    summary = summary_df
  ))
}

# Backward-compatible alias for older scripts.
out_of_range_diagnostics <- function(...) {
  range_diagnostics(...)
}
