test_that("estimate_download_size sums per-station year coverage", {
  stations <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    HLY.First.Year = c(2000, 2000),
    HLY.Last.Year = c(2001, 2004),
    stringsAsFactors = FALSE
  )

  estimate <- Drifloon:::.estimate_download_size(stations)

  expect_equal(estimate$count, 84)
  expect_equal(estimate$years, "2-5 (station-specific)")
})

test_that("estimate_download_size applies requested year bounds per station", {
  stations <- data.frame(
    Name = c("S1", "S2"),
    Station.ID = c(1, 2),
    HLY.First.Year = c(2000, 2000),
    HLY.Last.Year = c(2001, 2004),
    stringsAsFactors = FALSE
  )

  estimate <- Drifloon:::.estimate_download_size(
    stations = stations,
    first_year = 2001,
    last_year = 2002
  )

  expect_equal(estimate$count, 36)
  expect_equal(estimate$years, "1-2 (station-specific)")
})

test_that("estimate_download_size errors on reversed year bounds", {
  stations <- data.frame(
    Name = "S1",
    Station.ID = 1,
    HLY.First.Year = 2000,
    HLY.Last.Year = 2002,
    stringsAsFactors = FALSE
  )

  expect_error(
    Drifloon:::.estimate_download_size(stations, first_year = 2005, last_year = 2004),
    "first_year must be less than or equal to last_year"
  )
})
