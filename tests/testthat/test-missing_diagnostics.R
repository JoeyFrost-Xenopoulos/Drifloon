test_that("missing_observations_diagnostics reports missing hourly rows", {
  skip_if_not_installed("RSQLite")

  base_dir <- tempfile("missing-diagnostics-")
  dir.create(file.path(base_dir, "database"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  db_path <- file.path(base_dir, "database", "climate_database.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station <- data.frame(
    Station_ID = 1L,
    Station_Name = "Gap Station",
    Climate_ID = 999999L,
    stringsAsFactors = FALSE
  )

  observations <- data.frame(
    Station_ID = c(1L, 1L),
    Year = c(2020L, 2020L),
    Time_LST = c("2020-01-01 13:00", "2020-01-01 15:00"),
    Temp_C = c(1, 1),
    Dew_Point_C = c(1, 1),
    Rel_Hum = c(1, 1),
    Wind_Dir_deg = c(1, 1),
    Wind_Spd_kmh = c(1, 1),
    Visibility_km = c(1, 1),
    Stn_Press_kPa = c(1, 1),
    Hmdx = c(1, 1),
    Wind_Chill = c(1, 1),
    Weather = c("Clear", "Clear"),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "Station", station, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "Observation", observations, overwrite = TRUE, row.names = FALSE)

  result <- missing_observations_diagnostics(
    base_dir = base_dir,
    write_csv = FALSE,
    verbose = FALSE,
    max_rows_print = 5L
  )

  expect_s3_class(result$missing_hour_gaps, "data.frame")
  expect_equal(nrow(result$missing_hour_gaps), 1L)
  expect_equal(result$missing_hour_gaps[["Previous Time LST"]][1], "2020-01-01 13:00")
  expect_equal(result$missing_hour_gaps[["Current Time LST"]][1], "2020-01-01 15:00")
  expect_equal(result$missing_hour_gaps[["Gap Hours"]][1], 2L)
  expect_equal(result$missing_hour_gaps[["Missing Hours"]][1], 1L)
  expect_equal(result$missing_summary[["Missing Hour Gaps"]][1], 1L)
  expect_equal(result$missing_summary[["Missing Hour Rows"]][1], 1L)
})

test_that("missing_observations_diagnostics leaves sequential hours untouched", {
  skip_if_not_installed("RSQLite")

  base_dir <- tempfile("missing-diagnostics-")
  dir.create(file.path(base_dir, "database"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  db_path <- file.path(base_dir, "database", "climate_database.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station <- data.frame(
    Station_ID = 1L,
    Station_Name = "No Gap Station",
    Climate_ID = 999998L,
    stringsAsFactors = FALSE
  )

  observations <- data.frame(
    Station_ID = c(1L, 1L, 1L),
    Year = c(2020L, 2020L, 2020L),
    Time_LST = c("2020-01-01 13:00", "2020-01-01 14:00", "2020-01-01 15:00"),
    Temp_C = c(1, 1, 1),
    Dew_Point_C = c(1, 1, 1),
    Rel_Hum = c(1, 1, 1),
    Wind_Dir_deg = c(1, 1, 1),
    Wind_Spd_kmh = c(1, 1, 1),
    Visibility_km = c(1, 1, 1),
    Stn_Press_kPa = c(1, 1, 1),
    Hmdx = c(1, 1, 1),
    Wind_Chill = c(1, 1, 1),
    Weather = c("Clear", "Clear", "Clear"),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "Station", station, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "Observation", observations, overwrite = TRUE, row.names = FALSE)

  result <- missing_observations_diagnostics(
    base_dir = base_dir,
    write_csv = FALSE,
    verbose = FALSE,
    max_rows_print = 5L
  )

  expect_s3_class(result$missing_hour_gaps, "data.frame")
  expect_equal(nrow(result$missing_hour_gaps), 0L)
  expect_equal(result$missing_summary[["Missing Hour Gaps"]][1], 0L)
  expect_equal(result$missing_summary[["Missing Hour Rows"]][1], 0L)
})
