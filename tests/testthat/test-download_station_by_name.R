test_that("download_station_by_name selects by station_name", {
  captured <- NULL

  fake_download_station <- function(station, out_dir, first_year = NULL, last_year = NULL,
                                    parallel = FALSE) {
    captured <<- list(station = station, out_dir = out_dir, parallel = parallel)
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("A Station", "B Station"),
    Station.ID = c(10, 20),
    Province = c("ON", "QC"),
    HLY.First.Year = c(2000, 2001),
    HLY.Last.Year = c(2005, 2006),
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("drifloon-byname-")
  dir.create(out_dir)

  local_mocked_bindings(download_station = fake_download_station, .package = "Drifloon")

  Drifloon:::download_station_by_name(
    station_data = station_data,
    out_dir = out_dir,
    station_name = "A_Station"
  )

  expect_equal(captured$station$Station.ID, 10)
  expect_equal(captured$out_dir, out_dir)
  expect_false(captured$parallel)
})

test_that("download_station_by_name selects by station_id", {
  captured_id <- NULL

  fake_download_station <- function(station, ...) {
    captured_id <<- station$Station.ID
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = c("A Station", "B Station"),
    Station.ID = c(10, 20),
    Province = c("ON", "QC"),
    HLY.First.Year = c(2000, 2001),
    HLY.Last.Year = c(2005, 2006),
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("drifloon-byid-")
  dir.create(out_dir)

  local_mocked_bindings(download_station = fake_download_station, .package = "Drifloon")

  Drifloon:::download_station_by_name(
    station_data = station_data,
    out_dir = out_dir,
    station_id = 20
  )

  expect_equal(captured_id, 20)
})

test_that("download_station_by_name errors on ambiguous station name", {
  station_data <- data.frame(
    Name = c("A Station", "A Station"),
    Station.ID = c(10, 11),
    Province = c("ON", "QC"),
    HLY.First.Year = c(2000, 2001),
    HLY.Last.Year = c(2005, 2006),
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("drifloon-ambig-")
  dir.create(out_dir)

  expect_error(
    Drifloon:::download_station_by_name(
      station_data = station_data,
      out_dir = out_dir,
      station_name = "A_Station"
    ),
    "Multiple stations matched"
  )
})

test_that("download_station_by_name errors when no identifier provided", {
  station_data <- data.frame(
    Name = "A Station",
    Station.ID = 10,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("drifloon-noid-")
  dir.create(out_dir)

  expect_error(
    Drifloon:::download_station_by_name(
      station_data = station_data,
      out_dir = out_dir
    ),
    "Must provide station_name or station_id"
  )
})

test_that("download_station_by_name errors when station is missing", {
  station_data <- data.frame(
    Name = "A Station",
    Station.ID = 10,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("drifloon-missing-")
  dir.create(out_dir)

  expect_error(
    Drifloon:::download_station_by_name(
      station_data = station_data,
      out_dir = out_dir,
      station_id = 999
    ),
    "Station not found"
  )
})

test_that("download_station_by_name loads metadata when station_data is NULL", {
  loaded <- data.frame(
    Name = "A Station",
    Station.ID = 10,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  captured_id <- NULL

  fake_download_station <- function(station, ...) {
    captured_id <<- station$Station.ID
    invisible(NULL)
  }

  local_mocked_bindings(
    .load_station_metadata = function() loaded,
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  out_dir <- tempfile("drifloon-loadmeta-")
  dir.create(out_dir)

  Drifloon:::download_station_by_name(
    station_data = NULL,
    out_dir = out_dir,
    station_name = "A_Station"
  )

  expect_equal(captured_id, 10)
})

test_that("download_station_by_name defaults out_dir to working directory folder", {
  captured_out_dir <- NULL

  fake_download_station <- function(station, out_dir, ...) {
    captured_out_dir <<- out_dir
    invisible(NULL)
  }

  station_data <- data.frame(
    Name = "A Station",
    Station.ID = 10,
    Province = "ONTARIO",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  temp_wd <- tempfile("drifloon-default-outdir-")
  dir.create(temp_wd)
  setwd(temp_wd)
  on.exit(setwd(old_wd), add = TRUE)

  local_mocked_bindings(
    download_station = fake_download_station,
    .package = "Drifloon"
  )

  Drifloon:::download_station_by_name(
    station_data = station_data,
    station_id = 10
  )

  expect_equal(
    normalizePath(captured_out_dir, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(temp_wd, "drifloon_output"), winslash = "/", mustWork = FALSE)
  )
  expect_true(dir.exists(captured_out_dir))
})
