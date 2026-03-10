test_that("download_station_month downloads file and sleeps", {
  tmp <- tempfile("drifloon-month-")
  dir.create(tmp, recursive = TRUE)

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
    station_id = 31688,
    station_name = "MyStation",
    station_folder = tmp,
    year = 2020,
    month = 7,
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

  dest <- file.path(tmp, "MyStation-31688-2020-7.csv")
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
    station_id = 31688,
    station_name = "MyStation",
    station_folder = tmp,
    year = 2020,
    month = 7,
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

  dest <- file.path(tmp, "MyStation-31688-2020-7.csv")
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
    station_id = 31688,
    station_name = "MyStation",
    station_folder = tmp,
    year = 2020,
    month = 7,
    downloader = fake_downloader,
    sleeper = fake_sleeper
  )

  expect_false(isTRUE(result))
  expect_false(file.exists(dest))
  expect_false(sleeper_called)
})
