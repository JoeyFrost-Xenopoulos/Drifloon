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
    download_station_month = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon::download_station(station, out_dir, parallel = FALSE)

  # 2 years * 12 months
  expect_equal(calls, 24)
})

test_that("download_station uses furrr::future_pwalk when parallel is TRUE", {
  testthat::skip_if_not_installed("furrr")

  calls <- 0
  used_future <- FALSE

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    calls <<- calls + 1
    invisible(TRUE)
  }

  fake_future_pwalk <- function(.l, .f, ...) {
    used_future <<- TRUE
    for (i in seq_len(nrow(.l))) {
      .f(.l$year[[i]], .l$month[[i]])
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
    download_station_month = fake_download_station_month,
    .package = "Drifloon"
  )

  local_mocked_bindings(
    future_pwalk = fake_future_pwalk,
    .package = "furrr"
  )

  Drifloon::download_station(station, out_dir, parallel = TRUE)

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
    download_station_month = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon::download_station(
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
    Drifloon::download_station(
      station = station,
      out_dir = out_dir,
      first_year = 2010,
      last_year = 2011
    ),
    "No valid years"
  )
})
