test_that(".province_normalize accepts names and abbreviations", {
  expect_equal(Drifloon:::.province_normalize("Ontario"), "ON")
  expect_equal(Drifloon:::.province_normalize("ontario"), "ON")
  expect_equal(Drifloon:::.province_normalize("  on  "), "ON")
  expect_equal(Drifloon:::.province_normalize("QC"), "QC")
  expect_equal(Drifloon:::.province_normalize("british columbia"), "BC")
})

test_that(".province_normalize errors on invalid province", {
  expect_error(
    Drifloon:::.province_normalize("Atlantis"),
    "Invalid province"
  )
})
