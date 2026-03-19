test_that(".province_normalize accepts names and abbreviations", {
  expect_equal(Drifloon:::.province_normalize("Ontario"), "ONTARIO")
  expect_equal(Drifloon:::.province_normalize("ontario"), "ONTARIO")
  expect_equal(Drifloon:::.province_normalize("  on  "), "ONTARIO")
  expect_equal(Drifloon:::.province_normalize("QC"), "QUEBEC")
  expect_equal(
    Drifloon:::.province_normalize("british columbia"),
    "BRITISH COLUMBIA"
  )
  expect_equal(
    Drifloon:::.province_normalize("British Colombia"),
    "BRITISH COLUMBIA"
  )
  expect_equal(Drifloon:::.province_normalize("Yukon Territory"), "YUKON")
})

test_that(".province_normalize errors on invalid province", {
  expect_error(
    Drifloon:::.province_normalize("Atlantis"),
    "Invalid province"
  )
})

test_that(".province_normalize can be tolerant for metadata vectors", {
  expect_equal(
    Drifloon:::.province_normalize(c("BC", "YUKON TERRITORY", "UNKNOWN"), strict = FALSE),
    c("BRITISH COLUMBIA", "YUKON", NA)
  )
})
