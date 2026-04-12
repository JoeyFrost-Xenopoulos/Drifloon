#' Diagnose Missing Observations in a Drifloon Database
#'
#' Runs missing-data diagnostics on the Observation table for all data or a
#' user-selected subset (by Station ID and/or year range).
#'
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param out_dir Character. Directory where diagnostic CSV files are written
#'   when \code{write_csv = TRUE}.
#' @param station_ids Optional vector of Station IDs to include.
#' @param years Optional single year or two-value year range.
#' @param missing_columns Character vector of Observation columns to include in
#'   missingness calculations.
#' @param write_csv Logical. If \code{TRUE}, writes diagnostic CSV files.
#'   Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints diagnostic tables to the
#'   R console. Defaults to \code{TRUE}.
#' @param max_rows_print Integer. Maximum rows to print for each detailed table
#'   shown in detailed printed tables. Defaults to \code{20}.
#'
#' @return A named list with \code{missing_by_station},
#'   \code{missing_by_station_year}, \code{missing_hour_gaps}, and
#'   \code{missing_summary} data frames using readable column titles. The
#'   return value is invisible.
#' @export
missing_observations_diagnostics <- function(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  years = NULL,
  missing_columns = c(
    "Temp_C",
    "Dew_Point_C",
    "Rel_Hum",
    "Wind_Dir_deg",
    "Wind_Spd_kmh",
    "Visibility_km",
    "Stn_Press_kPa",
    "Hmdx",
    "Wind_Chill",
    "Precip_Amount"
  ),
  write_csv = FALSE,
  verbose = TRUE,
  max_rows_print = 20L
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
  if (!is.character(missing_columns) || length(missing_columns) == 0) {
    stop("missing_columns must be a non-empty character vector.", call. = FALSE)
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
  .assert_required_columns(
    con,
    "Observation",
    c("Station_ID", "Year", "Time_LST", missing_columns)
  )

  where_clause <- .resolve_diagnostic_scope(
    con,
    station_ids = station_ids,
    years = years
  )

  missing_expr <- paste(
    sprintf("CASE WHEN o.%s IS NULL THEN 1 ELSE 0 END", missing_columns),
    collapse = " + "
  )

  missing_by_station <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        s.Station_Name AS \"Station Name\",
        s.Station_ID AS \"Station ID\",
        COUNT(*) AS \"Total Observations\",
        SUM(%s) AS \"Missing Cells\",
        ROUND(100.0 * SUM(%s) / NULLIF(COUNT(*) * %d, 0), 2) AS \"Missing Percent\"
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s
      GROUP BY s.Station_Name, s.Station_ID
      ORDER BY \"Missing Percent\" DESC, \"Station Name\";
      ",
      missing_expr,
      missing_expr,
      length(missing_columns),
      where_clause
    )
  )

  missing_by_station_year <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        s.Station_Name AS \"Station Name\",
        s.Station_ID AS \"Station ID\",
        o.Year AS \"Year\",
        COUNT(*) AS \"Total Observations (Year)\",
        SUM(%s) AS \"Missing Cells (Year)\",
        ROUND(100.0 * SUM(%s) / NULLIF(COUNT(*) * %d, 0), 2) AS \"Missing Percent (Year)\"
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s
      GROUP BY s.Station_Name, s.Station_ID, o.Year
      ORDER BY \"Station Name\", \"Year\";
      ",
      missing_expr,
      missing_expr,
      length(missing_columns),
      where_clause
    )
  )

  missing_overall <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        SUM(%s) AS missing_cells_all,
        COUNT(*) AS total_rows_all
      FROM Observation o
      INNER JOIN Station s ON s.Station_ID = o.Station_ID
      %s;
      ",
      missing_expr,
      where_clause
    )
  )

  missing_hour_gaps <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      WITH ordered_observations AS (
        SELECT
          s.Station_Name AS station_name,
          s.Station_ID AS station_id,
          o.Time_LST AS current_time_lst,
          LAG(o.Time_LST) OVER (
            PARTITION BY s.Station_ID
            ORDER BY o.Time_LST
          ) AS previous_time_lst
        FROM Observation o
        INNER JOIN Station s ON s.Station_ID = o.Station_ID
        %s
      ),
      hour_gaps AS (
        SELECT
          station_name,
          station_id,
          previous_time_lst,
          current_time_lst,
          CAST(ROUND((julianday(current_time_lst) - julianday(previous_time_lst)) * 24, 0) AS INTEGER) AS gap_hours
        FROM ordered_observations
        WHERE previous_time_lst IS NOT NULL
      )
      SELECT
        station_name AS \"Station Name\",
        station_id AS \"Station ID\",
        previous_time_lst AS \"Previous Time LST\",
        current_time_lst AS \"Current Time LST\",
        gap_hours AS \"Gap Hours\",
        gap_hours - 1 AS \"Missing Hours\"
      FROM hour_gaps
      WHERE gap_hours > 1
      ORDER BY \"Station Name\", \"Previous Time LST\", \"Current Time LST\";
      ",
      where_clause
    )
  )

  final_avg_missing_percent <- if (nrow(missing_by_station_year) > 0) {
    mean(missing_by_station_year[["Missing Percent (Year)"]], na.rm = TRUE)
  } else {
    NA_real_
  }

  global_missing_percent <- NA_real_
  if (nrow(missing_overall) == 1) {
    total_rows_all <- as.numeric(missing_overall$total_rows_all[1])
    missing_cells_all <- as.numeric(missing_overall$missing_cells_all[1])
    total_cells_all <- total_rows_all * length(missing_columns)
    if (total_cells_all > 0) {
      global_missing_percent <- round(100.0 * missing_cells_all / total_cells_all, 2)
    }
  }

  missing_hour_gap_count <- nrow(missing_hour_gaps)
  missing_hour_count <- if (missing_hour_gap_count > 0) {
    sum(missing_hour_gaps[["Missing Hours"]], na.rm = TRUE)
  } else {
    0
  }

  missing_summary <- data.frame(
    "Average Missing Percent (Year)" = round(final_avg_missing_percent, 2),
    "Global Missing Percent" = global_missing_percent,
    "Missing Hour Gaps" = missing_hour_gap_count,
    "Missing Hour Rows" = missing_hour_count,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  no_scope <- nrow(missing_by_station) == 0
  if (isTRUE(no_scope)) {
    missing_summary <- data.frame(
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
      missing_by_station,
      file.path(out_dir, "missing_by_station.csv"),
      row.names = FALSE
    )
    write.csv(
      missing_by_station_year,
      file.path(out_dir, "missing_by_station_year.csv"),
      row.names = FALSE
    )
    write.csv(
      missing_hour_gaps,
      file.path(out_dir, "missing_hour_gaps.csv"),
      row.names = FALSE
    )
    write.csv(
      missing_summary,
      file.path(out_dir, "missing_summary.csv"),
      row.names = FALSE
    )
  }

  if (isTRUE(no_scope)) {
    warning("No observations matched the selected diagnostic scope.", call. = FALSE)
  }

  result <- list(
    missing_by_station = missing_by_station,
    missing_by_station_year = missing_by_station_year,
    missing_hour_gaps = missing_hour_gaps,
    missing_summary = missing_summary
  )

  if (isTRUE(verbose)) {
    cat("\nMissing-data diagnostics summary\n")
    .print_diagnostic_table(result$missing_summary)

    cat("\nBy station (top rows)\n")
    .print_diagnostic_table(utils::head(result$missing_by_station, max_rows_print))
    if (nrow(result$missing_by_station) > max_rows_print) {
      cat("...", nrow(result$missing_by_station) - max_rows_print, "more rows\n")
    }

    cat("\nBy station and year (top rows)\n")
    .print_diagnostic_table(utils::head(result$missing_by_station_year, max_rows_print))
    if (nrow(result$missing_by_station_year) > max_rows_print) {
      cat("...", nrow(result$missing_by_station_year) - max_rows_print, "more rows\n")
    }

    cat("\nHour gaps (top rows)\n")
    if (nrow(result$missing_hour_gaps) == 0) {
      cat("There are no hour gaps detected.\n")
    } else {
      .print_diagnostic_table(utils::head(result$missing_hour_gaps, max_rows_print))
      if (nrow(result$missing_hour_gaps) > max_rows_print) {
        cat("...", nrow(result$missing_hour_gaps) - max_rows_print, "more rows\n")
      }
    }
  }

  invisible(result)
}

#' Diagnose Missingness by Variable in a Drifloon Database
#'
#' Computes missing-cell percentages for each variable in the Observation table
#' using the selected diagnostic scope. Optionally returns a larger table with
#' a station-level breakdown for each variable.
#'
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param out_dir Character. Directory where diagnostic CSV files are written
#'   when \code{write_csv = TRUE}.
#' @param station_ids Optional vector of Station IDs to include.
#' @param years Optional single year or two-value year range.
#' @param missing_columns Character vector of Observation columns to include in
#'   missingness calculations.
#' @param include_station_breakdown Logical. If \code{TRUE}, includes a larger
#'   table with one row per station-variable pair.
#' @param write_csv Logical. If \code{TRUE}, writes diagnostic CSV files.
#'   Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints diagnostic tables to the
#'   R console. Defaults to \code{TRUE}.
#' @param max_rows_print Integer. Maximum rows to print for detailed tables
#'   shown in detailed printed tables. Defaults to \code{20}.
#'
#' @return A named list with \code{missing_by_variable},
#'   \code{missing_variable_summary}, and (optionally)
#'   \code{missing_by_station_variable}. The return value is invisible.
#' @export
missing_variables_diagnostics <- function(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  station_ids = NULL,
  years = NULL,
  missing_columns = c(
    "Temp_C",
    "Dew_Point_C",
    "Rel_Hum",
    "Wind_Dir_deg",
    "Wind_Spd_kmh",
    "Visibility_km",
    "Stn_Press_kPa",
    "Hmdx",
    "Wind_Chill",
    "Precip_Amount"
  ),
  include_station_breakdown = FALSE,
  write_csv = FALSE,
  verbose = TRUE,
  max_rows_print = 20L
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
  if (!is.logical(include_station_breakdown) || length(include_station_breakdown) != 1 ||
      is.na(include_station_breakdown)) {
    stop("include_station_breakdown must be TRUE or FALSE.", call. = FALSE)
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
  if (!is.character(missing_columns) || length(missing_columns) == 0) {
    stop("missing_columns must be a non-empty character vector.", call. = FALSE)
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
  .assert_required_columns(
    con,
    "Observation",
    c("Station_ID", "Year", missing_columns)
  )

  where_clause <- .resolve_diagnostic_scope(
    con,
    station_ids = station_ids,
    years = years
  )

  per_variable_sql <- vapply(
    missing_columns,
    function(col_name) {
      sprintf(
        "
        SELECT
          '%s' AS variable_key,
          SUM(CASE WHEN o.%s IS NULL THEN 1 ELSE 0 END) AS missing_cells,
          COUNT(*) AS total_observations
        FROM Observation o
        INNER JOIN Station s ON s.Station_ID = o.Station_ID
        %s
        ",
        col_name,
        col_name,
        where_clause
      )
    },
    FUN.VALUE = character(1)
  )

  missing_by_variable <- DBI::dbGetQuery(
    con,
    paste(per_variable_sql, collapse = "\nUNION ALL\n")
  )

  missing_by_variable[["Missing Percent"]] <- ifelse(
    missing_by_variable$total_observations > 0,
    round(100.0 * missing_by_variable$missing_cells / missing_by_variable$total_observations, 2),
    NA_real_
  )

  missing_by_variable <- missing_by_variable[
    order(missing_by_variable[["Missing Percent"]], decreasing = TRUE),
    c("variable_key", "missing_cells", "total_observations", "Missing Percent")
  ]
  names(missing_by_variable) <- c(
    "Variable",
    "Missing Observations",
    "Total Observations",
    "Missing Percent"
  )
  missing_by_variable[["Variable"]] <- .prettify_missing_column_name(missing_by_variable[["Variable"]])

  missing_by_station_variable <- NULL
  if (isTRUE(include_station_breakdown)) {
    per_station_variable_sql <- vapply(
      missing_columns,
      function(col_name) {
        sprintf(
          "
          SELECT
            s.Station_Name AS station_name,
            s.Station_ID AS station_id,
            '%s' AS variable_key,
            SUM(CASE WHEN o.%s IS NULL THEN 1 ELSE 0 END) AS missing_cells,
            COUNT(*) AS total_observations
          FROM Observation o
          INNER JOIN Station s ON s.Station_ID = o.Station_ID
          %s
          GROUP BY s.Station_Name, s.Station_ID
          ",
          col_name,
          col_name,
          where_clause
        )
      },
      FUN.VALUE = character(1)
    )

    missing_by_station_variable <- DBI::dbGetQuery(
      con,
      paste(per_station_variable_sql, collapse = "\nUNION ALL\n")
    )

    missing_by_station_variable[["Missing Percent"]] <- ifelse(
      missing_by_station_variable$total_observations > 0,
      round(
        100.0 * missing_by_station_variable$missing_cells /
          missing_by_station_variable$total_observations,
        2
      ),
      NA_real_
    )

    missing_by_station_variable <- missing_by_station_variable[
      order(
        missing_by_station_variable$station_name,
        -missing_by_station_variable[["Missing Percent"]],
        missing_by_station_variable$variable_key
      ),
      c("station_name", "station_id", "variable_key", "missing_cells", "total_observations", "Missing Percent")
    ]
    names(missing_by_station_variable) <- c(
      "Station Name",
      "Station ID",
      "Variable",
      "Missing Observations",
      "Total Observations",
      "Missing Percent"
    )
    missing_by_station_variable[["Variable"]] <-
      .prettify_missing_column_name(missing_by_station_variable[["Variable"]])
  }

  missing_variable_summary <- DBI::dbGetQuery(
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

  no_scope <- sum(missing_by_variable[["Total Observations"]], na.rm = TRUE) == 0
  if (isTRUE(no_scope) || nrow(missing_variable_summary) == 0) {
    missing_variable_summary <- data.frame(
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
      missing_by_variable,
      file.path(out_dir, "missing_by_variable.csv"),
      row.names = FALSE
    )
    write.csv(
      missing_variable_summary,
      file.path(out_dir, "missing_variable_summary.csv"),
      row.names = FALSE
    )

    if (isTRUE(include_station_breakdown)) {
      write.csv(
        missing_by_station_variable,
        file.path(out_dir, "missing_by_station_variable.csv"),
        row.names = FALSE
      )
    }
  }

  if (isTRUE(no_scope)) {
    warning("No observations matched the selected diagnostic scope.", call. = FALSE)
  }

  result <- list(
    missing_by_variable = missing_by_variable,
    missing_variable_summary = missing_variable_summary,
    missing_by_station_variable = missing_by_station_variable
  )

  if (isTRUE(verbose)) {
    cat("\nVariable-level missingness summary\n")
    .print_diagnostic_table(result$missing_variable_summary)

    cat("\nBy variable\n")
    .print_diagnostic_table(result$missing_by_variable)

    if (isTRUE(include_station_breakdown) && !is.null(result$missing_by_station_variable)) {
      cat("\nBy station and variable (top rows)\n")
      .print_diagnostic_table(utils::head(result$missing_by_station_variable, max_rows_print))
      if (nrow(result$missing_by_station_variable) > max_rows_print) {
        cat("...", nrow(result$missing_by_station_variable) - max_rows_print, "more rows\n")
      }
    }
  }

  invisible(result)
}