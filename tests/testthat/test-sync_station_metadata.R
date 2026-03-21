test_that(".sync_station_metadata updates files when metadata is new", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  remote_csv <- file.path(tmp_root, "remote.csv")
  remote_data <- data.frame(
    Name = c("A Station", "B Station"),
    Province = c("ONTARIO", "QUEBEC"),
    Station.ID = c(1, 2),
    HLY.First.Year = c(2000, NA),
    HLY.Last.Year = c(2005, NA),
    stringsAsFactors = FALSE
  )
  utils::write.csv(remote_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")

  result <- expect_message(
    Drifloon:::.sync_station_metadata(
      station_csv_url = "https://example.com/stations.csv",
      csv_path = csv_path,
      rds_path = rds_path,
      require_hourly = TRUE,
      downloader = fake_downloader
    ),
    "Station metadata updated"
  )

  expect_true(result$updated)
  expect_true(file.exists(csv_path))
  expect_true(file.exists(rds_path))

  saved <- readRDS(rds_path)
  expect_equal(nrow(saved), 1)
  expect_equal(saved$Station.ID[[1]], 1)
})

test_that(".sync_station_metadata skips write when metadata is unchanged", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")
  dir.create(dirname(csv_path), recursive = TRUE)
  dir.create(dirname(rds_path), recursive = TRUE)

  stable_data <- data.frame(
    Name = "A Station",
    Province = "ONTARIO",
    Station.ID = 1,
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )

  utils::write.csv(stable_data, csv_path, row.names = FALSE)
  saveRDS(stable_data, rds_path)

  remote_csv <- file.path(tmp_root, "remote.csv")
  utils::write.csv(stable_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  result <- expect_message(
    Drifloon:::.sync_station_metadata(
      station_csv_url = "https://example.com/stations.csv",
      csv_path = csv_path,
      rds_path = rds_path,
      require_hourly = TRUE,
      downloader = fake_downloader
    ),
    "already up to date"
  )

  expect_false(result$updated)
})

test_that(".sync_station_metadata supports province and year filtering", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  remote_csv <- file.path(tmp_root, "remote.csv")
  remote_data <- data.frame(
    Name = c("A Station", "B Station", "C Station"),
    Province = c("ONTARIO", "QUEBEC", "ONTARIO"),
    Station.ID = c(1, 2, 3),
    HLY.First.Year = c(1990, 2005, 2015),
    HLY.Last.Year = c(2000, 2010, 2020),
    stringsAsFactors = FALSE
  )
  utils::write.csv(remote_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")

  result <- Drifloon:::.sync_station_metadata(
    station_csv_url = "https://example.com/stations.csv",
    csv_path = csv_path,
    rds_path = rds_path,
    require_hourly = TRUE,
    provinces = "ON",
    min_year = 1998,
    max_year = 2018,
    downloader = fake_downloader
  )

  expect_true(result$updated)

  saved <- readRDS(rds_path)
  expect_equal(saved$Station.ID, c(1, 3))
})

test_that(".sync_station_metadata backs up existing metadata before update", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")
  dir.create(dirname(csv_path), recursive = TRUE)
  dir.create(dirname(rds_path), recursive = TRUE)

  old_data <- data.frame(
    Name = "A Station",
    Province = "ONTARIO",
    Station.ID = 1,
    HLY.First.Year = 1990,
    HLY.Last.Year = 1991,
    stringsAsFactors = FALSE
  )
  utils::write.csv(old_data, csv_path, row.names = FALSE)
  saveRDS(old_data, rds_path)

  remote_csv <- file.path(tmp_root, "remote.csv")
  new_data <- old_data
  new_data$HLY.Last.Year <- 2005
  utils::write.csv(new_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  result <- Drifloon:::.sync_station_metadata(
    station_csv_url = "https://example.com/stations.csv",
    csv_path = csv_path,
    rds_path = rds_path,
    require_hourly = TRUE,
    create_backup = TRUE,
    max_backup_bytes = 1024 * 1024,
    downloader = fake_downloader
  )

  expect_true(result$updated)
  expect_true(length(result$backup_files) >= 1)
  expect_true(all(file.exists(result$backup_files)))
})

test_that(".sync_station_metadata continues when current metadata files are missing", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")

  remote_csv <- file.path(tmp_root, "remote.csv")
  remote_data <- data.frame(
    Name = "A Station",
    Province = "ONTARIO",
    Station.ID = 1,
    HLY.First.Year = 2000,
    HLY.Last.Year = 2005,
    stringsAsFactors = FALSE
  )
  utils::write.csv(remote_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  result <- expect_message(
    Drifloon:::.sync_station_metadata(
      station_csv_url = "https://example.com/stations.csv",
      csv_path = csv_path,
      rds_path = rds_path,
      require_hourly = TRUE,
      downloader = fake_downloader
    ),
    "Current CSV metadata file is missing"
  )

  expect_true(result$updated)
  expect_true(file.exists(csv_path))
  expect_true(file.exists(rds_path))
})

test_that(".read_station_inventory_csv handles disclaimer preamble and extracts modified date", {
  tmp_csv <- tempfile(fileext = ".csv")

  preamble <- c(
    "Modified Date: 2026-01-07 23:30 UTC,,,,",
    "Station Inventory Disclaimer: snapshot statement,,,,",
    "Station ID Disclaimer: ids may change,,,,",
    "",
    "Name,Province,Station.ID,HLY.First.Year,HLY.Last.Year",
    "A Station,ONTARIO,1,2000,2005"
  )

  writeLines(preamble, con = tmp_csv)

  parsed <- Drifloon:::.read_station_inventory_csv(tmp_csv)

  expect_equal(parsed$modified_date, "2026-01-07 23:30 UTC")
  expect_equal(nrow(parsed$data), 1)
  expect_equal(parsed$data$Station.ID[[1]], 1)
})

test_that(".sync_station_metadata works with downloaded CSV preamble lines", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  remote_csv <- file.path(tmp_root, "remote-with-preamble.csv")
  preamble <- c(
    "Modified Date: 2026-01-07 23:30 UTC,,,,",
    "Station Inventory Disclaimer: snapshot statement,,,,",
    "Station ID Disclaimer: ids may change,,,,",
    "",
    "Name,Province,Station.ID,HLY.First.Year,HLY.Last.Year",
    "A Station,ONTARIO,1,2000,2005",
    "B Station,QUEBEC,2,,,"
  )
  writeLines(preamble, con = remote_csv)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")

  result <- Drifloon:::.sync_station_metadata(
    station_csv_url = "https://example.com/stations.csv",
    csv_path = csv_path,
    rds_path = rds_path,
    require_hourly = TRUE,
    downloader = fake_downloader
  )

  expect_true(result$updated)
  expect_equal(result$source_modified_date, "2026-01-07 23:30 UTC")
  saved <- readRDS(rds_path)
  expect_equal(saved$Station.ID[[1]], 1)
})

test_that(".read_station_inventory_csv normalizes spaced header variants", {
  tmp_csv <- tempfile(fileext = ".csv")

  lines <- c(
    "Modified Date: 2026-01-07 23:30 UTC,,,,",
    "Station Inventory Disclaimer: snapshot statement,,,,",
    "Station ID Disclaimer: ids may change,,,,",
    "",
    "Name,Province,Climate ID,Station ID,HLY First Y,HLY Last Y",
    "A Station,ONTARIO,101,1,2000,2005"
  )

  writeLines(lines, con = tmp_csv)

  parsed <- Drifloon:::.read_station_inventory_csv(tmp_csv)

  expect_true(all(c("Name", "Province", "Station.ID", "HLY.First.Year", "HLY.Last.Year") %in% names(parsed$data)))
  expect_equal(parsed$data$Station.ID[[1]], 1)
  expect_equal(parsed$data$HLY.First.Year[[1]], 2000)
  expect_equal(parsed$data$HLY.Last.Year[[1]], 2005)
})

test_that(".sync_station_metadata defaults to min_year 1980 with no max_year cap", {
  tmp_root <- tempfile("drifloon-sync-")
  dir.create(tmp_root, recursive = TRUE)

  remote_csv <- file.path(tmp_root, "remote.csv")
  remote_data <- data.frame(
    Name = c("Old Station", "Modern Station", "Recent Station"),
    Province = c("ONTARIO", "ONTARIO", "ONTARIO"),
    Station.ID = c(1, 2, 3),
    HLY.First.Year = c(1970, 1980, 2015),
    HLY.Last.Year = c(1979, 1990, 2024),
    stringsAsFactors = FALSE
  )
  utils::write.csv(remote_data, remote_csv, row.names = FALSE)

  fake_downloader <- function(url, destfile, mode = "wb", quiet = TRUE) {
    file.copy(remote_csv, destfile, overwrite = TRUE)
    invisible(0)
  }

  csv_path <- file.path(tmp_root, "extdata", "HLY_station_info.csv")
  rds_path <- file.path(tmp_root, "data", "HLY_station_info.rds")

  result <- Drifloon:::.sync_station_metadata(
    station_csv_url = "https://example.com/stations.csv",
    csv_path = csv_path,
    rds_path = rds_path,
    require_hourly = TRUE,
    downloader = fake_downloader
  )

  expect_true(result$updated)
  saved <- readRDS(rds_path)
  expect_false(1 %in% saved$Station.ID)
  expect_true(2 %in% saved$Station.ID)
  expect_true(3 %in% saved$Station.ID)
})
