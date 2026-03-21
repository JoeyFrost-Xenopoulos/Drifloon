test_that("download_station_month downloads file and sleeps", {
  tmp <- tempfile("drifloon-month-")
  dir.create(tmp, recursive = TRUE)

  station_data <- data.frame(
    Name = "MyStation",
    Station.ID = 31688,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2024,
    stringsAsFactors = FALSE
  )

  got_sleep <- NULL
  got_url <- NULL
  got_dest <- NULL

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    got_url <<- url
    got_dest <<- destfile
    writeLines("ok", con = destfile)
    invisible(0)
  }

  fake_sleeper <- function(seconds) {
    got_sleep <<- seconds
    invisible(NULL)
  }

  result <- Drifloon::download_station_month(
    station_data = station_data,
    out_dir = tmp,
    year = 2020,
    month = 7,
    station_id = 31688,
    downloader = fake_downloader,
    sleeper = fake_sleeper
  )

  expect_true(isTRUE(result))
  expect_true(file.exists(got_dest))
  expect_match(got_url, "stationID=31688")
  expect_match(got_url, "Year=2020")
  expect_match(got_url, "Month=7")
  expect_equal(got_sleep, 0.3)
})

test_that("download_station_month skips existing non-empty file", {
  tmp <- tempfile("drifloon-month-")
  dir.create(tmp, recursive = TRUE)

  station_data <- data.frame(
    Name = "MyStation",
    Station.ID = 31688,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2024,
    stringsAsFactors = FALSE
  )

  station_folder <- file.path(tmp, "MyStation")
  dir.create(station_folder, recursive = TRUE)
  dest <- file.path(station_folder, "MyStation_2020_07.csv")
  writeLines("already-here", con = dest)

  downloader_called <- FALSE
  sleeper_called <- FALSE

  fake_downloader <- function(...) {
    downloader_called <<- TRUE
    invisible(0)
  }

  fake_sleeper <- function(...) {
    sleeper_called <<- TRUE
    invisible(NULL)
  }

  result <- Drifloon::download_station_month(
    station_data = station_data,
    out_dir = tmp,
    year = 2020,
    month = 7,
    station_id = 31688,
    downloader = fake_downloader,
    sleeper = fake_sleeper
  )

  expect_true(isTRUE(result))
  expect_false(downloader_called)
  expect_false(sleeper_called)
})

test_that("download_station_month removes partial file on error", {
  tmp <- tempfile("drifloon-month-")
  dir.create(tmp, recursive = TRUE)

  station_data <- data.frame(
    Name = "MyStation",
    Station.ID = 31688,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2024,
    stringsAsFactors = FALSE
  )

  station_folder <- file.path(tmp, "MyStation")
  dir.create(station_folder, recursive = TRUE)
  dest <- file.path(station_folder, "MyStation_2020_07.csv")
  sleeper_called <- FALSE

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    writeLines("partial", con = destfile)
    stop("network broke")
  }

  fake_sleeper <- function(...) {
    sleeper_called <<- TRUE
    invisible(NULL)
  }

  result <- Drifloon::download_station_month(
    station_data = station_data,
    out_dir = tmp,
    year = 2020,
    month = 7,
    station_id = 31688,
    downloader = fake_downloader,
    sleeper = fake_sleeper
  )

  expect_false(isTRUE(result))
  expect_false(file.exists(dest))
  expect_false(sleeper_called)
})

test_that("download_station_month errors when year is outside station metadata range", {
  tmp <- tempfile("drifloon-month-")
  dir.create(tmp, recursive = TRUE)

  station_data <- data.frame(
    Name = "MyStation",
    Station.ID = 31688,
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  expect_error(
    Drifloon::download_station_month(
      station_data = station_data,
      out_dir = tmp,
      year = 1999,
      month = 7,
      station_id = 31688
    ),
    "outside available range"
  )
})
