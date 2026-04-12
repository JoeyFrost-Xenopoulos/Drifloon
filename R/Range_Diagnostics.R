#' Diagnose Out-of-Range Values Using Baseline Variable Ranges
#'
#' Flags unexpected values by comparing Observation variables against a baseline
#' \eqn{\mu \pm 3\sigma} range from a reference profile (for example,
#' historical 1980-2020 data). This is intended as a general warning tool, not
#' a strict physical-validity check.
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
out_of_range_diagnostics <- function(
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

  where_clause <- .resolve_diagnostic_scope(
    con,
    station_ids = station_ids,
    province_names = province,
    years = years
  )

  range_scope_summary <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        s.Station_Name AS \"Station Name\",
        s.Station_ID AS \"Station ID\",
        CAST(MIN(o.Year) AS TEXT) || '-' || CAST(MAX(o.Year) AS TEXT) AS \"Year Range\"
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s
      GROUP BY s.Station_Name, s.Station_ID
      ORDER BY s.Station_Name;
      ",
      where_clause
    )
  )

  variable_sql <- vapply(
    seq_len(nrow(baseline_ranges)),
    function(i) {
      col_name <- baseline_ranges$Variable[i]
      lower_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) - 3 * as.numeric(baseline_ranges$Sd_Value[i])
      upper_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) + 3 * as.numeric(baseline_ranges$Sd_Value[i])

      sprintf(
        "
        SELECT
          '%s' AS variable_key,
          SUM(CASE WHEN o.%s IS NOT NULL THEN 1 ELSE 0 END) AS observations_checked,
          SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_3sd_cells
        FROM Observation o
        INNER JOIN Station s ON s.Station_ID = o.Station_ID
        %s
        ",
        col_name,
        col_name,
        col_name,
        col_name,
        lower_3sd,
        col_name,
        upper_3sd,
        where_clause
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
      "outside_3sd_cells",
      "observations_checked",
      "Outside 3 SD +- Percent"
    )
  ]

  names(out_of_range_by_variable) <- c(
    "Variable",
    "Outside 3 SD +-",
    "Observations Checked",
    "Outside 3 SD +- Percent"
  )
  out_of_range_by_variable[["Variable"]] <-
    .prettify_missing_column_name(out_of_range_by_variable[["Variable"]])

  out_of_range_by_station_variable <- NULL
  if (isTRUE(include_station_breakdown)) {
    station_variable_sql <- vapply(
      seq_len(nrow(baseline_ranges)),
      function(i) {
        col_name <- baseline_ranges$Variable[i]
        lower_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) - 3 * as.numeric(baseline_ranges$Sd_Value[i])
        upper_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) + 3 * as.numeric(baseline_ranges$Sd_Value[i])

        sprintf(
          "
          SELECT
            s.Station_Name AS station_name,
            s.Station_ID AS station_id,
            '%s' AS variable_key,
            SUM(CASE WHEN o.%s IS NOT NULL THEN 1 ELSE 0 END) AS observations_checked,
            SUM(CASE WHEN o.%s IS NOT NULL AND (o.%s < %.15g OR o.%s > %.15g) THEN 1 ELSE 0 END) AS outside_3sd_cells
          FROM Observation o
          INNER JOIN Station s ON s.Station_ID = o.Station_ID
          %s
          GROUP BY s.Station_Name, s.Station_ID
          ",
          col_name,
          col_name,
          col_name,
          col_name,
          lower_3sd,
          col_name,
          upper_3sd,
          where_clause
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
        "outside_3sd_cells",
        "observations_checked",
        "Outside 3 SD +- Percent"
      )
    ]

    names(out_of_range_by_station_variable) <- c(
      "Station Name",
      "Station ID",
      "Variable",
      "Outside 3 SD +-",
      "Observations Checked",
      "Outside 3 SD +- Percent"
    )
    out_of_range_by_station_variable[["Variable"]] <-
      .prettify_missing_column_name(out_of_range_by_station_variable[["Variable"]])
  }

  scope_filter <- if (nzchar(where_clause)) {
    sub("^WHERE ", "", where_clause)
  } else {
    "1 = 1"
  }

  out_of_range_observations <- NULL
  if (isTRUE(include_outlier_rows)) {
    observation_sql <- vapply(
      seq_len(nrow(baseline_ranges)),
      function(i) {
        col_name <- baseline_ranges$Variable[i]
        lower_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) - 3 * as.numeric(baseline_ranges$Sd_Value[i])
        upper_3sd <- as.numeric(baseline_ranges$Mean_Value[i]) + 3 * as.numeric(baseline_ranges$Sd_Value[i])

        sprintf(
          "
          SELECT
            s.Station_Name AS \"Station Name\",
            s.Station_ID AS \"Station ID\",
            o.Year AS \"Year\",
            o.Month AS \"Month\",
            o.Day AS \"Day\",
            o.Time_LST AS \"Time LST\",
            '%s' AS \"Variable\",
            CAST(o.%s AS REAL) AS \"Observed Value\",
            %.15g AS \"Lower 3 SD\",
            %.15g AS \"Upper 3 SD\"
          FROM Observation o
          INNER JOIN Station s ON s.Station_ID = o.Station_ID
          WHERE %s
          AND o.%s IS NOT NULL
          AND (o.%s < %.15g OR o.%s > %.15g)
          ",
          col_name,
          col_name,
          lower_3sd,
          upper_3sd,
          scope_filter,
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