# Skips existing files
test_that("station_year_month skips existing files", {
  
  tmp <- withr::local_tempdir()
  
  # Create fake existing file
  existing_file <- file.path(tmp, "TestStation-123-2020-1.csv")
  writeLines("already exists", existing_file)
  
  station_year_month(
    station_id = 123,
    station_name = "TestStation",
    station_folder = tmp,
    year = 2020,
    month = 1,
    downloader = function(...) stop("Downloader should not run"),
    sleeper = function(...) NULL
  )
  
  expect_true(file.exists(existing_file))
})

# download creates files
test_that("successful download creates file", {
  
  tmp <- withr::local_tempdir()
  
  fake_downloader <- function(url, destfile, ...) {
    writeLines("fake weather data", destfile)
  }
  
  station_year_month(
    station_id = 123,
    station_name = "TestStation",
    station_folder = tmp,
    year = 2020,
    month = 1,
    downloader = fake_downloader,
    sleeper = function(...) NULL
  )
  
  expect_true(
    file.exists(file.path(tmp, "TestStation-123-2020-1.csv"))
  )
})

# delete corrupted files
test_that("failed download removes corrupted file", {
  
  tmp <- withr::local_tempdir()
  
  fake_downloader <- function(...) {
    stop("network error")
  }
  
  station_year_month(
    station_id = 123,
    station_name = "TestStation",
    station_folder = tmp,
    year = 2020,
    month = 1,
    downloader = fake_downloader,
    sleeper = function(...) NULL
  )
  
  expect_false(
    file.exists(file.path(tmp, "TestStation-123-2020-1.csv"))
  )
})