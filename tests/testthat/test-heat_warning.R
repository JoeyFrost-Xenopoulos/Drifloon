test_that("heatwave_diagnostics detects a 2-day heatwave event", {
  skip_if_not_installed("RSQLite")

  base_dir <- tempfile("heatwave-diagnostics-")
  dir.create(file.path(base_dir, "database"), recursive = TRUE)
  dir.create(file.path(base_dir, "data"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  db_path <- file.path(base_dir, "database", "climate_database.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station <- data.frame(
    Station_ID = 1L,
    Station_Name = "Heatwave Station",
    Climate_ID = 1001L,
    Province_Name = "ONTARIO",
    stringsAsFactors = FALSE
  )

  observation <- data.frame(
    Station_ID = rep(1L, 6),
    Year = rep(2020L, 6),
    Time_LST = c(
      "2020-07-01 01:00", "2020-07-01 13:00",
      "2020-07-02 01:00", "2020-07-02 13:00",
      "2020-07-03 01:00", "2020-07-03 13:00"
    ),
    Temp_C = c(31, 29, 27, 25, 24, 22),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "Station", station, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "Observation", observation, overwrite = TRUE, row.names = FALSE)

  province_ranges <- data.frame(
    Province_Name = "ONTARIO",
    Variable = "Temp_C",
    Mean_Value = 20,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )
  canada_ranges <- data.frame(
    Variable = "Temp_C",
    Mean_Value = 19,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )

  province_rds <- file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds")
  canada_rds <- file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds")
  saveRDS(province_ranges, province_rds)
  saveRDS(canada_ranges, canada_rds)

  result <- heatwave_diagnostics(
    base_dir = base_dir,
    province_ranges_rds_path = province_rds,
    canada_ranges_rds_path = canada_rds,
    write_csv = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result$heatwave_events), 1L)
  expect_equal(result$heatwave_events[["Consecutive Days"]][1], 2L)
  expect_equal(result$heatwave_events[["Days By Max Rule"]][1], 1L)
  expect_equal(result$heatwave_events[["Days By Avg Rule"]][1], 2L)
  expect_equal(sum(result$heatwave_days[["Heatwave Day"]]), 2)
})

test_that("heatwave_diagnostics does not report events for non-consecutive hot days", {
  skip_if_not_installed("RSQLite")

  base_dir <- tempfile("heatwave-diagnostics-")
  dir.create(file.path(base_dir, "database"), recursive = TRUE)
  dir.create(file.path(base_dir, "data"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  db_path <- file.path(base_dir, "database", "climate_database.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station <- data.frame(
    Station_ID = 1L,
    Station_Name = "Split Heat Station",
    Climate_ID = 1002L,
    Province_Name = "ONTARIO",
    stringsAsFactors = FALSE
  )

  observation <- data.frame(
    Station_ID = rep(1L, 6),
    Year = rep(2020L, 6),
    Time_LST = c(
      "2020-07-01 01:00", "2020-07-01 13:00",
      "2020-07-02 01:00", "2020-07-02 13:00",
      "2020-07-03 01:00", "2020-07-03 13:00"
    ),
    Temp_C = c(31, 29, 23, 22, 31, 29),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "Station", station, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "Observation", observation, overwrite = TRUE, row.names = FALSE)

  province_ranges <- data.frame(
    Province_Name = "ONTARIO",
    Variable = "Temp_C",
    Mean_Value = 20,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )
  canada_ranges <- data.frame(
    Variable = "Temp_C",
    Mean_Value = 19,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )

  province_rds <- file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds")
  canada_rds <- file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds")
  saveRDS(province_ranges, province_rds)
  saveRDS(canada_ranges, canada_rds)

  result <- heatwave_diagnostics(
    base_dir = base_dir,
    province_ranges_rds_path = province_rds,
    canada_ranges_rds_path = canada_rds,
    write_csv = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result$heatwave_events), 0L)
  expect_equal(sum(result$heatwave_days[["Heatwave Day"]]), 2)
})

test_that("heatwave_diagnostics falls back to Canada baseline when province is missing", {
  skip_if_not_installed("RSQLite")

  base_dir <- tempfile("heatwave-diagnostics-")
  dir.create(file.path(base_dir, "database"), recursive = TRUE)
  dir.create(file.path(base_dir, "data"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  db_path <- file.path(base_dir, "database", "climate_database.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station <- data.frame(
    Station_ID = 1L,
    Station_Name = "Fallback Station",
    Climate_ID = 1003L,
    Province_Name = "ATLANTIS",
    stringsAsFactors = FALSE
  )

  observation <- data.frame(
    Station_ID = rep(1L, 4),
    Year = rep(2020L, 4),
    Time_LST = c(
      "2020-07-01 01:00", "2020-07-01 13:00",
      "2020-07-02 01:00", "2020-07-02 13:00"
    ),
    Temp_C = c(25, 25, 26, 24),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "Station", station, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "Observation", observation, overwrite = TRUE, row.names = FALSE)

  province_ranges <- data.frame(
    Province_Name = "ONTARIO",
    Variable = "Temp_C",
    Mean_Value = 20,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )
  canada_ranges <- data.frame(
    Variable = "Temp_C",
    Mean_Value = 19,
    Sd_Value = 2,
    stringsAsFactors = FALSE
  )

  province_rds <- file.path(base_dir, "data", "variable_ranges_province_1980-2020.rds")
  canada_rds <- file.path(base_dir, "data", "variable_ranges_Canada_1980-2020.rds")
  saveRDS(province_ranges, province_rds)
  saveRDS(canada_ranges, canada_rds)

  result <- heatwave_diagnostics(
    base_dir = base_dir,
    province_ranges_rds_path = province_rds,
    canada_ranges_rds_path = canada_rds,
    fallback_to_canada = TRUE,
    write_csv = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result$heatwave_events), 1L)
  expect_equal(result$heatwave_events[["Baseline Scope"]][1], "Canada")
})