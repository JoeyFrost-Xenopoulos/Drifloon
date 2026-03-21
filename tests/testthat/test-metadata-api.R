test_that("sync_metadata forwards arguments to internal sync helper", {
  captured <- NULL

  fake_sync <- function(...) {
    captured <<- list(...)
    invisible(list(updated = TRUE))
  }

  local_mocked_bindings(
    .sync_station_metadata = fake_sync,
    .package = "Drifloon"
  )

  result <- Drifloon::sync_metadata(min_year = 1980, require_hourly = TRUE)

  expect_equal(captured$min_year, 1980)
  expect_true(captured$require_hourly)
  expect_true(result$updated)
})

test_that("load_metadata syncs first when sync is TRUE", {
  synced <- FALSE
  loaded <- data.frame(Name = "A", Station.ID = 1, stringsAsFactors = FALSE)

  local_mocked_bindings(
    sync_metadata = function(...) { synced <<- TRUE; invisible(NULL) },
    .load_station_metadata = function() loaded,
    .package = "Drifloon"
  )

  result <- Drifloon::load_metadata(sync = TRUE, min_year = 1980)

  expect_true(synced)
  expect_equal(result$Station.ID[[1]], 1)
})

test_that("load_metadata does not sync by default", {
  synced <- FALSE
  loaded <- data.frame(Name = "A", Station.ID = 1, stringsAsFactors = FALSE)

  local_mocked_bindings(
    sync_metadata = function(...) { synced <<- TRUE; invisible(NULL) },
    .load_station_metadata = function() loaded,
    .package = "Drifloon"
  )

  result <- Drifloon::load_metadata()

  expect_false(synced)
  expect_equal(result$Station.ID[[1]], 1)
})

test_that("load_metadata validates sync argument", {
  expect_error(
    Drifloon::load_metadata(sync = "yes"),
    "sync must be TRUE or FALSE"
  )
})
