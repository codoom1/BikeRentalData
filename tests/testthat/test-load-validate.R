test_that("valid processed data pass validation", {
  expect_true(validate_bike_rentals(make_processed_fixture()))
})

test_that("invalid totals are rejected", {
  data <- make_processed_fixture()
  data$total_rentals[[1]] <- 99L
  expect_error(validate_bike_rentals(data), "registered \\+ casual")
})

test_that("load_bike_rentals standardizes a legacy index", {
  data <- make_processed_fixture()
  names(data)[[1]] <- ""
  path <- tempfile(fileext = ".csv")
  readr::write_csv(data, path)

  loaded <- load_bike_rentals(path)
  expect_s3_class(loaded, "data.frame")
  expect_true("instant" %in% names(loaded))
  expect_s3_class(loaded$date, "Date")
})

test_that("data dictionary covers required fields", {
  dictionary <- data_dictionary()
  expect_true(all(names(make_processed_fixture()) %in% dictionary$variable))
})
