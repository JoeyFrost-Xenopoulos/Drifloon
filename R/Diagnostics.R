#' Diagnose Missing Observations in a Drifloon Database
#'
#' Runs missing-data diagnostics on the Observation table for all data or a
#' user-selected subset (by Climate ID, Station ID, and/or year range).
#'
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param out_dir Character. Directory where diagnostic CSV files are written
#'   when \code{write_csv = TRUE}.
#' @param climate_ids Optional character vector of Climate IDs to include.
#' @param station_ids Optional vector of Station IDs to include.
#' @param years Optional single year or two-value year range.
#' @param missing_columns Character vector of Observation columns to include in
#'   missingness calculations.
#' @param write_csv Logical. If \code{TRUE}, writes diagnostic CSV files.
#'   Default is \code{FALSE}.
#' @param print_tables Logical. If \code{TRUE}, prints the diagnostic tables to
#'   the R console.
#' @param max_rows_print Integer. Maximum rows to print for each detailed table
#'   when \code{print_tables = TRUE}. Defaults to \code{20}.
#'
#' @return A named list with \code{missing_by_station},
#'   \code{missing_by_station_year}, and \code{missing_summary} data frames
#'   using readable column titles. The return value is invisible to avoid
#'   duplicate console output when \code{print_tables = TRUE}.
#' @export
missing_observations_diagnostics <- function(
  base_dir = getwd(),
  db_name = "climate_database.db",
  out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
  climate_ids = NULL,
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
    "Weather"
  ),
  write_csv = FALSE,
  print_tables = TRUE,
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
  if (!is.logical(print_tables) || length(print_tables) != 1 || is.na(print_tables)) {
    stop("print_tables must be TRUE or FALSE.", call. = FALSE)
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
    climate_ids = climate_ids,
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

  final_avg_missing_percent <- mean(
    missing_by_station_year[["Missing Percent (Year)"]],
    na.rm = TRUE
  )

  global_missing_percent <- NA_real_
  if (nrow(missing_overall) == 1) {
    total_rows_all <- as.numeric(missing_overall$total_rows_all[1])
    missing_cells_all <- as.numeric(missing_overall$missing_cells_all[1])
    total_cells_all <- total_rows_all * length(missing_columns)
    if (total_cells_all > 0) {
      global_missing_percent <- round(100.0 * missing_cells_all / total_cells_all, 2)
    }
  }

  missing_summary <- data.frame(
    "Average Missing Percent (Year)" = round(final_avg_missing_percent, 2),
    "Global Missing Percent" = global_missing_percent,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

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
      missing_summary,
      file.path(out_dir, "missing_summary.csv"),
      row.names = FALSE
    )
  }

  if (nrow(missing_by_station) == 0) {
    warning("No observations matched the selected diagnostic scope.", call. = FALSE)
  }

  result <- list(
    missing_by_station = missing_by_station,
    missing_by_station_year = missing_by_station_year,
    missing_summary = missing_summary
  )

  if (isTRUE(print_tables)) {
    cat("\nMissing-data diagnostics summary\n")
    print(result$missing_summary)

    cat("\nBy station (top rows)\n")
    print(utils::head(result$missing_by_station, max_rows_print))
    if (nrow(result$missing_by_station) > max_rows_print) {
      cat("...", nrow(result$missing_by_station) - max_rows_print, "more rows\n")
    }

    cat("\nBy station and year (top rows)\n")
    print(utils::head(result$missing_by_station_year, max_rows_print))
    if (nrow(result$missing_by_station_year) > max_rows_print) {
      cat("...", nrow(result$missing_by_station_year) - max_rows_print, "more rows\n")
    }
  }

  invisible(result)
}

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
  climate_ids = NULL,
  station_ids = NULL,
  years = NULL
) {
  where_parts <- character(0)

  if (!is.null(climate_ids)) {
    climate_ids <- unique(as.character(climate_ids))
    if (length(climate_ids) == 0) {
      stop("climate_ids cannot be an empty vector.", call. = FALSE)
    }

    present_climate <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT Climate_ID FROM Station WHERE Climate_ID IN %s",
        .build_in_clause(con, climate_ids)
      )
    )$Climate_ID

    missing_climate <- setdiff(climate_ids, as.character(present_climate))
    if (length(missing_climate) > 0) {
      stop(
        "Unknown Climate_ID value(s): ",
        paste(missing_climate, collapse = ", "),
        call. = FALSE
      )
    }

    where_parts <- c(
      where_parts,
      sprintf("s.Climate_ID IN %s", .build_in_clause(con, climate_ids))
    )
  }

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
