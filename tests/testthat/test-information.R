test_that("archive planning covers annual and monthly formats", {
  plan <- bikerentaldata:::.archive_plan("2017-10-01", "2018-02-28")
  expect_equal(plan$archive_id, c("2017", "201801", "201802"))
  expect_equal(plan$format, c("annual", "monthly", "monthly"))
})

test_that("trip location summaries support legacy and modern station fields", {
  trip_dir <- tempfile("station-files-")
  dir.create(trip_dir)

  readr::write_csv(
    data.frame(
      "Start station number" = c("1", "2", "1"),
      check.names = FALSE
    ),
    file.path(trip_dir, "2010-capitalbikeshare-tripdata.csv")
  )
  readr::write_csv(
    data.frame(start_station_id = c("2", "3")),
    file.path(trip_dir, "201801-capitalbikeshare-tripdata.csv")
  )

  summary <- summarize_trip_locations(trip_dir)
  by_file <- summarize_trip_locations(trip_dir, by = "file")

  expect_equal(summary$unique_station_locations, 3L)
  expect_equal(by_file$station_locations, c(2L, 2L))
})

test_that("system information has readable structured output", {
  info <- structure(
    list(
      station_locations = 847L,
      jurisdiction_count = 2L,
      jurisdiction_names = c("Washington, DC", "Arlington, VA"),
      feed_url = "https://example.com"
    ),
    class = "bikerental_system_info"
  )

  output <- capture.output(print(info))
  expect_true(any(grepl("Station locations: 847", output)))
  expect_true(any(grepl("Washington, DC", output)))
  expect_true(any(grepl("Arlington, VA", output)))
  expect_equal(info$jurisdiction_count, 2L)
})
