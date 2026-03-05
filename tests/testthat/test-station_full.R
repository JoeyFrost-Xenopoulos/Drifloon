# We want to verify:
# Start year is adjusted upward
# End year is adjusted downward
# Error when invalid range
# Correct number of calls made

test_that("station_full adjusts start year to metadata minimum", {
  
  calls <- list()
  
  fake_station_year_month <- function(station_id,
                                      station_name,
                                      station_folder,
                                      year,
                                      month) {
    calls <<- append(calls, list(year))
  }
  
  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2002
  )
  
  withr::local_tempdir() -> tmp
  
  local_mocked_bindings(
    station_year_month = fake_station_year_month
  )
  
  station_full(
    station,
    tmp,
    first_year = 1990,   # too early
    last_year = 2000
  )
  
  # Should only use year 2000
  expect_true(all(unlist(calls) == 2000))
})

test_that("station_full adjusts end year to metadata maximum", {
  
  calls <- list()
  
  fake_station_year_month <- function(station_id,
                                      station_name,
                                      station_folder,
                                      year,
                                      month) {
    calls <<- append(calls, list(year))
  }
  
  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2002
  )
  
  withr::local_tempdir() -> tmp
  
  local_mocked_bindings(
    station_year_month = fake_station_year_month
  )
  
  station_full(
    station,
    tmp,
    first_year = 2002,
    last_year = 2010  # too late
  )
  
  expect_true(all(unlist(calls) == 2002))
})

test_that("station_full errors when adjusted range is invalid", {
  
  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005
  )
  
  tmp <- withr::local_tempdir()
  
  expect_error(
    station_full(
      station,
      tmp,
      first_year = 2010,
      last_year = 2011
    ),
    "No valid years"
  )
})

test_that("station_full calls 12 months per valid year", {
  
  call_count <- 0
  
  fake_station_year_month <- function(...) {
    call_count <<- call_count + 1
  }
  
  station <- list(
    Station.ID = 1,
    Name = "Test Station",
    Province = "ON",
    HLY.First.Year = 2000,
    HLY.Last.Year = 2001
  )
  
  tmp <- withr::local_tempdir()
  
  local_mocked_bindings(
    station_year_month = fake_station_year_month
  )
  
  station_full(station, tmp)
  
  expect_equal(call_count, 24)
})

