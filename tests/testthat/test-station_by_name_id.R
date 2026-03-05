test_that("station_by_name_id errors when station_name is duplicated", {
  station_data <- data.frame(
    Name = c("Twin Station", "Twin Station", "Other"),
    Station.ID = c(101, 202, 303),
    Province = c("ON", "QC", "AB"),
    HLY.First.Year = c(2000, 2005, 2010),
    HLY.Last.Year = c(2001, 2006, 2011),
    stringsAsFactors = FALSE
  )

  expect_error(
    station_by_name_id(
      station_data = station_data,
      out_dir = withr::local_tempdir(),
      station_name = "Twin_Station"
    ),
    "Multiple stations matched"
  )
})

test_that("station_by_name_id selects unique station_name", {
  captured_station_id <- NULL

  fake_station_full <- function(station, out_dir, first_year = NULL, last_year = NULL) {
    captured_station_id <<- station$Station.ID
  }

  station_data <- data.frame(
    Name = c("Unique Station"),
    Station.ID = c(999),
    Province = c("ON"),
    HLY.First.Year = c(2000),
    HLY.Last.Year = c(2001),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    station_full = fake_station_full
  )

  station_by_name_id(
    station_data = station_data,
    out_dir = withr::local_tempdir(),
    station_name = "Unique_Station"
  )

  expect_equal(captured_station_id, 999)
})

test_that("station_by_name_id loads default metadata when station_data is omitted", {
  captured_station_id <- NULL

  fake_station_full <- function(station, out_dir, first_year = NULL, last_year = NULL) {
    captured_station_id <<- station$Station.ID
  }

  fake_loader <- function() {
    data.frame(
      Name = c("Auto Station"),
      Station.ID = c(444),
      Province = c("ON"),
      HLY.First.Year = c(2000),
      HLY.Last.Year = c(2001),
      stringsAsFactors = FALSE
    )
  }

  local_mocked_bindings(
    station_full = fake_station_full,
    .load_default_station_data = fake_loader
  )

  station_by_name_id(
    out_dir = withr::local_tempdir(),
    station_name = "Auto_Station"
  )

  expect_equal(captured_station_id, 444)
})

test_that("all_stations calls station_full once per station", {
  called_ids <- integer(0)

  fake_station_full <- function(station, out_dir, first_year = NULL, last_year = NULL) {
    called_ids <<- c(called_ids, station$Station.ID)
  }

  station_data <- data.frame(
    Name = c("S1", "S2", "S3"),
    Station.ID = c(11, 22, 33),
    Province = c("ON", "QC", "AB"),
    HLY.First.Year = c(2000, 2000, 2000),
    HLY.Last.Year = c(2001, 2001, 2001),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    station_full = fake_station_full
  )

  all_stations(
    station_data = station_data,
    out_dir = withr::local_tempdir()
  )

  expect_equal(called_ids, c(11, 22, 33))
})
