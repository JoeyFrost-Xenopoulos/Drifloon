test_that("download_station_by_name forwards parallel argument", {
  captured_parallel <- NULL

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_parallel <<- parallel
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = "Test Station",
    Station.ID = 99,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2001,
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon:::download_station_by_name(
    station_data = station_data,
    out_dir = out_dir,
    station_name = "Test Station",
    parallel = TRUE
  )

  expect_true(captured_parallel)
})

test_that("download_station_province passes through furrr and disables nested parallel", {
  testthat::skip_if_not_installed("furrr")

  used_future <- FALSE
  captured_parallel <- logical(0)

  fake_future_walk <- function(.x, .f, ...) {
    used_future <<- TRUE
    for (i in seq_along(.x)) {
      .f(.x[[i]])
    }
    invisible(NULL)
  }

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_parallel <<- c(captured_parallel, parallel)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    Province = c("ON", "ON"),
    HLY.First.Year = c(2000, 2001),
    HLY.Last.Year = c(2002, 2003),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    future_walk = fake_future_walk,
    .package = "furrr"
  )

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_station_province(
    station_data = station_data,
    out_dir = out_dir,
    province = "ON",
    parallel = TRUE,
    confirm = TRUE
  )

  expect_true(used_future)
  expect_true(all(captured_parallel == FALSE))
  expect_equal(length(captured_parallel), 2)
})

test_that("download_all_station passes through furrr and disables nested parallel", {
  testthat::skip_if_not_installed("furrr")

  used_future <- FALSE
  captured_parallel <- logical(0)

  fake_future_pwalk <- function(.l, .f, ...) {
    used_future <<- TRUE
    for (i in seq_len(nrow(.l))) {
      .f(.l$Name[[i]], .l$Station.ID[[i]], .l$Province[[i]], .l$HLY.First.Year[[i]], .l$HLY.Last.Year[[i]])
    }
    invisible(NULL)
  }

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_parallel <<- c(captured_parallel, parallel)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(1, 2, 3),
    Province = c("ON", "ON", "QC"),
    HLY.First.Year = c(2000, 2001, 2002),
    HLY.Last.Year = c(2002, 2003, 2004),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    future_pwalk = fake_future_pwalk,
    .package = "furrr"
  )

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_all_station(
    station_data = station_data,
    out_dir = out_dir,
    parallel = TRUE,
    confirm = TRUE
  )

  expect_true(used_future)
  expect_true(all(captured_parallel == FALSE))
  expect_equal(length(captured_parallel), 3)
})

test_that("download_station_province sequential mode calls matching stations", {
  captured_ids <- integer(0)

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    expect_false(parallel)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(1, 2, 3),
    Province = c("ON", "QC", "ON"),
    HLY.First.Year = c(2000, 2001, 2002),
    HLY.Last.Year = c(2003, 2004, 2005),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_station_province(
    station_data = station_data,
    out_dir = out_dir,
    province = "ON",
    parallel = FALSE,
    confirm = TRUE
  )

  expect_equal(sort(captured_ids), c(1, 3))
})

test_that("download_station_province accepts province as first positional argument", {
  captured_ids <- integer(0)

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(1, 2, 3),
    Province = c("ON", "QC", "ON"),
    HLY.First.Year = c(2000, 2001, 2002),
    HLY.Last.Year = c(2003, 2004, 2005),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_station_province(
    "ON",
    station_data = station_data,
    out_dir = out_dir,
    parallel = FALSE,
    confirm = TRUE
  )

  expect_equal(sort(captured_ids), c(1, 3))
})

test_that("download_station_province supports legacy positional order with warning", {
  captured_ids <- integer(0)

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(1, 2, 3),
    Province = c("ON", "QC", "ON"),
    HLY.First.Year = c(2000, 2001, 2002),
    HLY.Last.Year = c(2003, 2004, 2005),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  expect_warning(
    Drifloon::download_station_province(
      station_data,
      out_dir,
      "ON",
      parallel = FALSE,
      confirm = TRUE
    ),
    "Deprecated positional call"
  )

  expect_equal(sort(captured_ids), c(1, 3))
})

test_that("download_station_province loads metadata when station_data is NULL", {
  captured_ids <- integer(0)

  fake_download_station <- function(station, ...) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    invisible(NULL)
  }

  loaded <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    Province = c("ON", "QC"),
    HLY.First.Year = c(2000, 2001),
    HLY.Last.Year = c(2003, 2004),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .load_station_metadata = function() loaded,
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_station_province(
    station_data = NULL,
    out_dir = out_dir,
    province = "ON",
    confirm = TRUE
  )

  expect_equal(captured_ids, 1)
})

test_that("download_station_province errors when no station matches province", {
  station_data <- data.frame(
    Name = "S1",
    Station.ID = 1,
    Province = "QC",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2003,
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    Drifloon::download_station_province(
      station_data = station_data,
      out_dir = out_dir,
      province = "ON"
    ),
    "No stations found"
  )
})

test_that("download_station_province requests every month for every year", {
  calls <- data.frame(year = integer(0), month = integer(0))

  fake_download_station_month <- function(station_id, station_name, station_folder, year, month,
                                          downloader = download.file, sleeper = Sys.sleep) {
    calls <<- rbind(calls, data.frame(year = as.integer(year), month = as.integer(month)))
    invisible(TRUE)
  }

  station_data <- data.frame(
    Name = "Discovery Island",
    Station.ID = 123,
    Province = "BRITISH COLUMBIA",
    HLY.First.Year = 1997,
    HLY.Last.Year = 1998,
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .download_station_month_file = fake_download_station_month,
    .package = "Drifloon"
  )

  Drifloon::download_station_province(
    station_data = station_data,
    out_dir = out_dir,
    province = "BC",
    parallel = FALSE,
    confirm = TRUE
  )

  expect_equal(nrow(calls), 24)
  expect_equal(sort(unique(calls$year)), c(1997L, 1998L))
  expect_equal(sort(unique(calls$month)), 1:12)
})

test_that("download_all_station sequential mode calls all stations", {
  captured_ids <- integer(0)

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    expect_false(parallel)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(1, 2, 3),
    Province = c("ON", "QC", "AB"),
    HLY.First.Year = c(2000, 2001, 2002),
    HLY.Last.Year = c(2003, 2004, 2005),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon::download_all_station(
    station_data = station_data,
    out_dir = out_dir,
    parallel = FALSE,
    confirm = TRUE
  )

  expect_equal(sort(captured_ids), c(1, 2, 3))
})

test_that("download_all_station runs disk-space safeguard before downloading", {
  checked_bytes <- NULL
  captured_ids <- integer(0)

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured_ids <<- c(captured_ids, station$Station.ID)
    invisible(NULL)
  }

  fake_check_disk_space <- function(out_dir, estimated_bytes, buffer_percent = 10) {
    checked_bytes <<- estimated_bytes
    TRUE
  }

  station_data <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    Province = c("ON", "QC"),
    HLY.First.Year = c(2000, 2000),
    HLY.Last.Year = c(2000, 2000),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .check_disk_space = fake_check_disk_space,
    .package = "Drifloon"
  )

  Drifloon::download_all_station(
    station_data = station_data,
    out_dir = out_dir,
    parallel = FALSE,
    confirm = TRUE
  )

  expect_true(checked_bytes > 0)
  expect_equal(sort(captured_ids), c(1, 2))
})

test_that("download_station_province stops when disk-space safeguard fails", {
  fake_check_disk_space <- function(...) {
    stop("Insufficient disk space")
  }

  fake_download_station <- function(...) {
    stop("download_station should not run when disk check fails")
  }

  station_data <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    Province = c("ON", "ON"),
    HLY.First.Year = c(2000, 2000),
    HLY.Last.Year = c(2000, 2000),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .check_disk_space = fake_check_disk_space,
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  expect_error(
    Drifloon::download_station_province(
      station_data = station_data,
      out_dir = out_dir,
      province = "ON",
      parallel = FALSE,
      confirm = TRUE
    ),
    "Insufficient disk space"
  )
})

test_that("download_station_province does not run disk check when user cancels", {
  fake_check_disk_space <- function(...) {
    stop("disk check should not run when user cancels")
  }

  station_data <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    Province = c("ON", "ON"),
    HLY.First.Year = c(2000, 2000),
    HLY.Last.Year = c(2000, 2000),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("drifloon-test-", as.integer(Sys.time())))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    .check_disk_space = fake_check_disk_space,
    readline = function(...) "no",
    .package = "Drifloon"
  )

  expect_no_error(
    Drifloon::download_station_province(
      station_data = station_data,
      out_dir = out_dir,
      province = "ON",
      parallel = FALSE,
      confirm = FALSE
    )
  )
})
