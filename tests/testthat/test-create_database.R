test_that("create_database errors when expected structure is missing", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- tempfile("drifloon-db-")

  expect_error(
    Drifloon::create_database(root),
    "Missing expected directory"
  )
})


test_that("create_database ingests station and observation rows", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- tempfile("drifloon-db-")
  dir.create(file.path(root, "data"), recursive = TRUE)
  dir.create(file.path(root, "drifloon_output", "Station_123"), recursive = TRUE)

  station_info <- data.frame(
    Climate.ID = "CXTEST",
    Station.ID = 123,
    Name = "Test Station",
    Province = "Ontario",
    Latitude = 44.1,
    Longitude = -78.3,
    Elevation..m. = 188,
    HLY.First.Year = 2024,
    HLY.Last.Year = 2024,
    stringsAsFactors = FALSE
  )
  saveRDS(
    station_info,
    file.path(root, "data", "HLY_station_info.rds")
  )

  hourly <- data.frame(
    Station.ID = 123,
    Climate.ID = "CXTEST",
    Year = 2024,
    Month = 1,
    Day = 1,
    Time..LST. = "00:00",
    Temp...C. = -1,
    Dew.Point.Temp...C. = -4,
    Rel.Hum.... = 80,
    Precip..Amount..mm. = 0,
    Wind.Dir..10s.deg. = 12,
    Wind.Spd..km.h. = 15,
    Visibility..km. = 20,
    Stn.Press..kPa. = 101.2,
    Hmdx = NA,
    Wind.Chill = NA,
    Weather = "Clear",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    hourly,
    file.path(root, "drifloon_output", "Station_123", "Test_2024_01.csv"),
    row.names = FALSE
  )

  result <- Drifloon::create_database(root)
  expect_equal(result$station_rows, 1)
  expect_equal(result$observation_rows, 1)

  con <- DBI::dbConnect(RSQLite::SQLite(), result$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  station_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM Station")$n[[1]]
  obs_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM Observation")$n[[1]]

  expect_equal(station_n, 1)
  expect_equal(obs_n, 1)
})
