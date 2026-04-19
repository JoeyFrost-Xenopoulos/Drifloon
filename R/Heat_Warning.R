#' Detect Heatwaves Using Provincial Baseline Temperature Ranges
#'
#' Detects heatwave events from daily temperatures in the Observation table.
#' A heatwave day is defined as a day where either daily maximum temperature is
#' at least 
#' \eqn{\mu + 5\sigma} or daily average temperature is at least
#' \eqn{\mu + 3\sigma}, where \eqn{\mu} and \eqn{\sigma} are taken from the
#' baseline range profiles. A heatwave event is two or more consecutive
#' heatwave days.
#'
#' Provincial baseline ranges are used by default; when a station province is
#' missing from the province baseline file, the function can fall back to the
#' Canada-wide baseline.
#'
#' @param base_dir Character. Root directory containing the database folder.
#' @param db_name Character. SQLite database file name under
#'   \code{file.path(base_dir, "database")}.
#' @param out_dir Character. Directory where diagnostic CSV files are written
#'   when \code{write_csv = TRUE}.
#' @param station_ids Optional vector of Station IDs to include.
#' @param province Optional character vector of province names to include.
#' @param years Optional single year or two-value year range.
#' @param min_consecutive_days Integer. Minimum consecutive heatwave days for an
#'   event. Defaults to \code{2}.
#' @param max_sd_threshold Numeric. SD multiplier for the daily-maximum rule.
#'   Defaults to \code{5}.
#' @param avg_sd_threshold Numeric. SD multiplier for the daily-average rule.
#'   Defaults to \code{3}.
#' @param canada_ranges_rds_path Character. Path to Canada-wide baseline range
#'   RDS file (1980-2020).
#' @param province_ranges_rds_path Character. Path to province baseline range
#'   RDS file (1980-2020).
#' @param fallback_to_canada Logical. If \code{TRUE}, stations without a
#'   matching province baseline use the Canada-wide baseline for Temp_C.
#' @param write_csv Logical. If \code{TRUE}, writes diagnostic CSV files.
#' @param verbose Logical. If \code{TRUE}, prints summary tables.
#' @param max_rows_print Integer. Maximum rows to print from detailed tables.
#'
#' @return A named list with \code{scope_summary}, \code{station_summary},
#'   \code{heatwave_events}, \code{heatwave_days}, and baseline metadata.
#'   The return value is invisible.
#' @export
heatwave_diagnostics <- function(
	base_dir = getwd(),
	db_name = "climate_database.db",
	out_dir = file.path(base_dir, "drifloon_output", "diagnostics"),
	station_ids = NULL,
	province = NULL,
	years = NULL,
	min_consecutive_days = 2L,
	max_sd_threshold = 5,
	avg_sd_threshold = 3,
	canada_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds"),
	province_ranges_rds_path = file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds"),
	fallback_to_canada = TRUE,
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
	if (!is.numeric(min_consecutive_days) || length(min_consecutive_days) != 1 ||
			is.na(min_consecutive_days) || min_consecutive_days < 2) {
		stop("min_consecutive_days must be a single integer >= 2.", call. = FALSE)
	}
	min_consecutive_days <- as.integer(min_consecutive_days)
	if (!is.numeric(max_sd_threshold) || length(max_sd_threshold) != 1 ||
			is.na(max_sd_threshold) || max_sd_threshold <= 0) {
		stop("max_sd_threshold must be a single positive number.", call. = FALSE)
	}
	if (!is.numeric(avg_sd_threshold) || length(avg_sd_threshold) != 1 ||
			is.na(avg_sd_threshold) || avg_sd_threshold <= 0) {
		stop("avg_sd_threshold must be a single positive number.", call. = FALSE)
	}
	if (!is.character(canada_ranges_rds_path) || length(canada_ranges_rds_path) != 1 ||
			!nzchar(canada_ranges_rds_path)) {
		stop("canada_ranges_rds_path must be a single, non-empty character path.", call. = FALSE)
	}
	if (!is.character(province_ranges_rds_path) || length(province_ranges_rds_path) != 1 ||
			!nzchar(province_ranges_rds_path)) {
		stop("province_ranges_rds_path must be a single, non-empty character path.", call. = FALSE)
	}
	if (!is.logical(fallback_to_canada) || length(fallback_to_canada) != 1 || is.na(fallback_to_canada)) {
		stop("fallback_to_canada must be TRUE or FALSE.", call. = FALSE)
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
	.assert_required_columns(con, "Station", c("Station_ID", "Station_Name", "Climate_ID", "Province_Name"))
	.assert_required_columns(con, "Observation", c("Station_ID", "Year", "Temp_C"))

	observation_fields <- DBI::dbListFields(con, "Observation")
	has_ymd <- all(c("Year", "Month", "Day") %in% observation_fields)
	has_time_lst <- "Time_LST" %in% observation_fields

	if (has_ymd) {
		obs_date_sql <- "date(printf('%04d-%02d-%02d', o.Year, o.Month, o.Day))"
		non_missing_date_filter <- "o.Month IS NOT NULL AND o.Day IS NOT NULL"
	} else if (has_time_lst) {
		obs_date_sql <- "date(o.Time_LST)"
		non_missing_date_filter <- "o.Time_LST IS NOT NULL AND TRIM(o.Time_LST) <> ''"
		warning(
			"Observation date is derived from Time_LST because Year/Month/Day columns were not all available.",
			call. = FALSE
		)
	} else {
		stop(
			"Observation must have either Year/Month/Day columns or a parseable Time_LST column for daily aggregation.",
			call. = FALSE
		)
	}

	where_clause <- .resolve_diagnostic_scope(
		con,
		station_ids = station_ids,
		province_names = province,
		years = years
	)

	daily <- DBI::dbGetQuery(
		con,
		sprintf(
			"
			SELECT
				s.Station_Name AS station_name,
				s.Station_ID AS station_id,
				s.Province_Name AS province_name,
				%s AS obs_date,
				MAX(CAST(o.Temp_C AS REAL)) AS daily_max_temp_c,
				AVG(CAST(o.Temp_C AS REAL)) AS daily_avg_temp_c,
				SUM(CASE WHEN o.Temp_C IS NOT NULL THEN 1 ELSE 0 END) AS daily_observation_count
			FROM Observation o
			INNER JOIN Station s ON s.Station_ID = o.Station_ID
			%s
			%s
			GROUP BY s.Station_Name, s.Station_ID, s.Province_Name, %s
			HAVING %s IS NOT NULL
			ORDER BY s.Station_Name, obs_date;
			",
			obs_date_sql,
			where_clause,
			if (nzchar(where_clause)) {
				paste("AND o.Temp_C IS NOT NULL AND", non_missing_date_filter)
			} else {
				paste("WHERE o.Temp_C IS NOT NULL AND", non_missing_date_filter)
			},
			obs_date_sql,
			obs_date_sql
		)
	)

	scope_summary <- DBI::dbGetQuery(
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

	province_ranges_info <- .load_variable_ranges(
		ranges_rds_path = province_ranges_rds_path,
		required_cols = c("Variable", "Mean_Value", "Sd_Value", "Province_Name"),
		preserve_cols = "Province_Name"
	)
	province_ranges <- province_ranges_info$data
	province_ranges <- province_ranges[province_ranges$Variable == "Temp_C", , drop = FALSE]
	if (nrow(province_ranges) == 0) {
		stop("Province baseline ranges do not include Temp_C.", call. = FALSE)
	}
	province_ranges$Province_Name <- trimws(as.character(province_ranges$Province_Name))
	province_ranges <- province_ranges[nzchar(province_ranges$Province_Name), , drop = FALSE]
	province_ranges$province_norm <- .province_normalize(province_ranges$Province_Name, strict = FALSE)
	province_ranges <- province_ranges[!is.na(province_ranges$province_norm), , drop = FALSE]

	if (nrow(province_ranges) == 0) {
		stop("Province baseline ranges contain no usable Temp_C rows.", call. = FALSE)
	}

	duplicate_province <- duplicated(province_ranges$province_norm)
	if (any(duplicate_province)) {
		warning(
			"Duplicate Temp_C province baseline rows found; using the first row per province.",
			call. = FALSE
		)
		province_ranges <- province_ranges[!duplicate_province, , drop = FALSE]
	}

	canada_ranges_info <- .load_variable_ranges(
		ranges_rds_path = canada_ranges_rds_path,
		required_cols = c("Variable", "Mean_Value", "Sd_Value")
	)
	canada_ranges <- canada_ranges_info$data
	canada_ranges <- canada_ranges[canada_ranges$Variable == "Temp_C", , drop = FALSE]
	if (nrow(canada_ranges) == 0) {
		stop("Canada baseline ranges do not include Temp_C.", call. = FALSE)
	}

	canada_mean <- as.numeric(canada_ranges$Mean_Value[1])
	canada_sd <- as.numeric(canada_ranges$Sd_Value[1])
	if (!is.finite(canada_mean) || !is.finite(canada_sd) || canada_sd < 0) {
		stop("Canada Temp_C baseline Mean_Value/Sd_Value are invalid.", call. = FALSE)
	}

	heatwave_days <- daily
	if (nrow(heatwave_days) > 0) {
		heatwave_days$obs_date <- as.Date(heatwave_days$obs_date)
		heatwave_days$province_name <- as.character(heatwave_days$province_name)
		heatwave_days$province_norm <- .province_normalize(heatwave_days$province_name, strict = FALSE)

		mean_by_province <- setNames(province_ranges$Mean_Value, province_ranges$province_norm)
		sd_by_province <- setNames(province_ranges$Sd_Value, province_ranges$province_norm)

		heatwave_days$baseline_mean <- as.numeric(mean_by_province[heatwave_days$province_norm])
		heatwave_days$baseline_sd <- as.numeric(sd_by_province[heatwave_days$province_norm])
		heatwave_days$baseline_scope <- ifelse(is.na(heatwave_days$baseline_mean), "Canada", "Province")

		if (isTRUE(fallback_to_canada)) {
			fallback_idx <- which(is.na(heatwave_days$baseline_mean) | is.na(heatwave_days$baseline_sd))
			if (length(fallback_idx) > 0) {
				heatwave_days$baseline_mean[fallback_idx] <- canada_mean
				heatwave_days$baseline_sd[fallback_idx] <- canada_sd
			}
		}

		missing_baseline <- is.na(heatwave_days$baseline_mean) | is.na(heatwave_days$baseline_sd)
		if (any(missing_baseline)) {
			warning(
				"Some daily rows have no matching province baseline and were excluded (set fallback_to_canada = TRUE to include them).",
				call. = FALSE
			)
			heatwave_days <- heatwave_days[!missing_baseline, , drop = FALSE]
		}

		if (nrow(heatwave_days) > 0) {
			heatwave_days$threshold_daily_max <- heatwave_days$baseline_mean + max_sd_threshold * heatwave_days$baseline_sd
			heatwave_days$threshold_daily_avg <- heatwave_days$baseline_mean + avg_sd_threshold * heatwave_days$baseline_sd
			heatwave_days$hit_daily_max_rule <- heatwave_days$daily_max_temp_c >= heatwave_days$threshold_daily_max
			heatwave_days$hit_daily_avg_rule <- heatwave_days$daily_avg_temp_c >= heatwave_days$threshold_daily_avg
			heatwave_days$is_heatwave_day <- heatwave_days$hit_daily_max_rule | heatwave_days$hit_daily_avg_rule
		}
	}

	events_list <- list()
	if (nrow(heatwave_days) > 0) {
		by_station <- split(heatwave_days, heatwave_days$station_id)
		events_list <- lapply(by_station, function(station_df) {
			station_df <- station_df[order(station_df$obs_date), , drop = FALSE]
			flagged <- station_df[which(station_df$is_heatwave_day), , drop = FALSE]
			if (nrow(flagged) == 0) {
				return(NULL)
			}

			day_gap <- c(NA_integer_, as.integer(diff(flagged$obs_date)))
			run_start <- is.na(day_gap) | day_gap != 1L
			run_id <- cumsum(run_start)
			runs <- split(flagged, run_id)
			runs <- runs[vapply(runs, nrow, FUN.VALUE = integer(1)) >= min_consecutive_days]

			if (length(runs) == 0) {
				return(NULL)
			}

			do.call(
				rbind,
				lapply(seq_along(runs), function(i) {
					run_df <- runs[[i]]
					data.frame(
						"Station Name" = as.character(run_df$station_name[1]),
						"Station ID" = as.integer(run_df$station_id[1]),
						"Province" = as.character(run_df$province_name[1]),
						"Baseline Scope" = as.character(run_df$baseline_scope[1]),
						"Event Start Date" = as.Date(min(run_df$obs_date)),
						"Event End Date" = as.Date(max(run_df$obs_date)),
						"Consecutive Days" = as.integer(nrow(run_df)),
						"Days By Max Rule" = as.integer(sum(run_df$hit_daily_max_rule, na.rm = TRUE)),
						"Days By Avg Rule" = as.integer(sum(run_df$hit_daily_avg_rule, na.rm = TRUE)),
						"Event Peak Daily Max C" = max(run_df$daily_max_temp_c, na.rm = TRUE),
						"Event Peak Daily Avg C" = max(run_df$daily_avg_temp_c, na.rm = TRUE),
						stringsAsFactors = FALSE,
						check.names = FALSE
					)
				})
			)
		})
		events_list <- events_list[!vapply(events_list, is.null, FUN.VALUE = logical(1))]
	}

	if (length(events_list) > 0) {
		heatwave_events <- do.call(rbind, events_list)
		rownames(heatwave_events) <- NULL
		heatwave_events <- heatwave_events[
			order(
				heatwave_events[["Station Name"]],
				heatwave_events[["Event Start Date"]]
			),
			,
			drop = FALSE
		]
	} else {
		heatwave_events <- data.frame(
			"Station Name" = character(0),
			"Station ID" = integer(0),
			"Province" = character(0),
			"Baseline Scope" = character(0),
			"Event Start Date" = as.Date(character(0)),
			"Event End Date" = as.Date(character(0)),
			"Consecutive Days" = integer(0),
			"Days By Max Rule" = integer(0),
			"Days By Avg Rule" = integer(0),
			"Event Peak Daily Max C" = numeric(0),
			"Event Peak Daily Avg C" = numeric(0),
			stringsAsFactors = FALSE,
			check.names = FALSE
		)
	}

	if (nrow(heatwave_days) > 0) {
		station_summary <- do.call(
			rbind,
			lapply(split(heatwave_days, heatwave_days$station_id), function(station_df) {
				station_events <- heatwave_events[heatwave_events[["Station ID"]] == station_df$station_id[1], , drop = FALSE]
				data.frame(
					"Station Name" = as.character(station_df$station_name[1]),
					"Station ID" = as.integer(station_df$station_id[1]),
					"Province" = as.character(station_df$province_name[1]),
					"Daily Rows" = as.integer(nrow(station_df)),
					"Heatwave Days" = as.integer(sum(station_df$is_heatwave_day, na.rm = TRUE)),
					"Heatwave Events" = as.integer(nrow(station_events)),
					stringsAsFactors = FALSE,
					check.names = FALSE
				)
			})
		)
		rownames(station_summary) <- NULL
		station_summary <- station_summary[
			order(station_summary[["Heatwave Events"]], station_summary[["Heatwave Days"]], decreasing = TRUE),
			,
			drop = FALSE
		]
	} else {
		station_summary <- data.frame(
			"Station Name" = character(0),
			"Station ID" = integer(0),
			"Province" = character(0),
			"Daily Rows" = integer(0),
			"Heatwave Days" = integer(0),
			"Heatwave Events" = integer(0),
			stringsAsFactors = FALSE,
			check.names = FALSE
		)
	}

	heatwave_days_report <- heatwave_days
	if (nrow(heatwave_days_report) > 0) {
		heatwave_days_report <- heatwave_days_report[
			order(heatwave_days_report$station_name, heatwave_days_report$obs_date),
			c(
				"station_name",
				"station_id",
				"province_name",
				"obs_date",
				"daily_max_temp_c",
				"daily_avg_temp_c",
				"daily_observation_count",
				"baseline_scope",
				"baseline_mean",
				"baseline_sd",
				"threshold_daily_max",
				"threshold_daily_avg",
				"hit_daily_max_rule",
				"hit_daily_avg_rule",
				"is_heatwave_day"
			),
			drop = FALSE
		]

		names(heatwave_days_report) <- c(
			"Station Name",
			"Station ID",
			"Province",
			"Date",
			"Daily Max Temp C",
			"Daily Avg Temp C",
			"Daily Observation Count",
			"Baseline Scope",
			"Baseline Mean",
			"Baseline SD",
			"Threshold Daily Max",
			"Threshold Daily Avg",
			"Hit Daily Max Rule",
			"Hit Daily Avg Rule",
			"Heatwave Day"
		)
	}

	if (isTRUE(write_csv)) {
		if (!dir.exists(out_dir)) {
			dir.create(out_dir, recursive = TRUE)
		}

		write.csv(scope_summary, file.path(out_dir, "heatwave_scope_summary.csv"), row.names = FALSE)
		write.csv(station_summary, file.path(out_dir, "heatwave_station_summary.csv"), row.names = FALSE)
		write.csv(heatwave_events, file.path(out_dir, "heatwave_events.csv"), row.names = FALSE)
		write.csv(heatwave_days_report, file.path(out_dir, "heatwave_days.csv"), row.names = FALSE)
	}

	if (nrow(scope_summary) == 0) {
		scope_summary <- data.frame(
			"Summary" = "No observations matched the selected heatwave scope.",
			stringsAsFactors = FALSE,
			check.names = FALSE
		)
	}

	result <- list(
		scope_summary = scope_summary,
		station_summary = station_summary,
		heatwave_events = heatwave_events,
		heatwave_days = heatwave_days_report,
		baseline_source = list(
			province = province_ranges_info$source,
			canada = canada_ranges_info$source
		),
		thresholds = list(
			min_consecutive_days = min_consecutive_days,
			max_sd_threshold = max_sd_threshold,
			avg_sd_threshold = avg_sd_threshold
		)
	)

	if (isTRUE(verbose)) {
		cat("\nHeatwave diagnostics summary\n")
		.print_diagnostic_table(result$scope_summary)

		cat("\nBy station\n")
		.print_diagnostic_table(result$station_summary)

		cat("\nHeatwave events\n")
		if (nrow(result$heatwave_events) == 0) {
			cat("No heatwave detected.\n")
		} else {
			.print_diagnostic_table(utils::head(result$heatwave_events, max_rows_print))
			if (nrow(result$heatwave_events) > max_rows_print) {
				cat("...", nrow(result$heatwave_events) - max_rows_print, "more rows\n")
			}
		}

		cat("\nBaseline ranges source\n")
		cat("Province:", result$baseline_source$province, "\n")
		cat("Canada:", result$baseline_source$canada, "\n")
	}

	invisible(result)
}
