test_that("available_disk_bytes parses POSIX df output", {
  fake_system2 <- function(command, args, stdout = TRUE, stderr = FALSE) {
    expect_equal(command, "df")
    expect_true("-Pk" %in% args)
    c(
      "Filesystem 1024-blocks Used Available Capacity Mounted on",
      "/dev/disk1s5s1 976490576 123 456789 1% /"
    )
  }

  local_mocked_bindings(
    system2 = fake_system2,
    normalizePath = function(path, winslash = "/", mustWork = FALSE) path,
    .package = "Drifloon"
  )

  bytes <- Drifloon:::.available_disk_bytes("/tmp/path with spaces", os_type = "unix")
  expect_equal(bytes, 456789 * 1024)
})

test_that("available_disk_bytes returns NULL on malformed POSIX df output", {
  fake_system2 <- function(command, args, stdout = TRUE, stderr = FALSE) {
    c("Filesystem 1024-blocks Used Available Capacity Mounted on", "bad output")
  }

  local_mocked_bindings(
    system2 = fake_system2,
    normalizePath = function(path, winslash = "/", mustWork = FALSE) path,
    .package = "Drifloon"
  )

  bytes <- Drifloon:::.available_disk_bytes("/tmp", os_type = "unix")
  expect_null(bytes)
})
