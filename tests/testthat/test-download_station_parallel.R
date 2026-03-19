test_that("download_station uses purrr::pwalk when parallel is FALSE", {
  calls <- 0

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    calls <<- calls + 1
    invisible(TRUE)
  }

  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2001
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .download_station_month_file = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon:::download_station(station, out_dir, parallel = FALSE)

  # 2 years * 12 months
  expect_equal(calls, 24)
})

test_that("download_station uses furrr::future_walk when parallel is TRUE", {
  testthat::skip_if_not_installed("furrr")

  calls <- 0
  used_future <- FALSE

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    calls <<- calls + 1
    invisible(TRUE)
  }

  fake_future_walk <- function(.x, .f, ...) {
    used_future <<- TRUE
    for (i in .x) {
      .f(i)
    }
    invisible(NULL)
  }

  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2001
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .download_station_month_file = fake_download_station_month,
    .package = "Drifloon"
  )

  local_mocked_bindings(
    future_walk = fake_future_walk,
    .package = "furrr"
  )

  Drifloon:::download_station(station, out_dir, parallel = TRUE)

  expect_true(used_future)
  expect_equal(calls, 24)
})

test_that("download_station clamps years to station metadata bounds", {
  years_seen <- integer(0)

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    years_seen <<- c(years_seen, year)
    invisible(TRUE)
  }

  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2002
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .download_station_month_file = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon:::download_station(
    station = station,
    out_dir = out_dir,
    first_year = 1995,
    last_year = 2010,
    parallel = FALSE
  )

  expect_true(all(years_seen %in% 2000:2002))
  expect_equal(length(years_seen), 36)
})

test_that("download_station errors when adjusted range is invalid", {
  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    Drifloon:::download_station(
      station = station,
      out_dir = out_dir,
      first_year = 2010,
      last_year = 2011
    ),
    "No valid years"
  )
})

test_that("download_station uses station-name folder and file naming inputs", {
  station_folders <- character(0)
  station_names <- character(0)

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    station_folders <<- c(station_folders, station_folder)
    station_names <<- c(station_names, station_name)
    invisible(TRUE)
  }

  station <- list(
    Station.ID = 123,
    Name = "Discovery Island",
    Province = "BRITISH COLUMBIA",
    HLY.First.Year = 1997,
    HLY.Last.Year = 1997
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .download_station_month_file = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon:::download_station(station = station, out_dir = out_dir)

  expect_true(all(station_names == "Discovery_Island"))
  expect_true(all(station_folders == file.path(out_dir, "Discovery_Island")))
  expect_true(dir.exists(file.path(out_dir, "Discovery_Island")))
})
