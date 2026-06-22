test_that("prepare_bike_rentals aggregates and joins daily records", {
  trip_dir <- tempfile("trips-")
  dir.create(trip_dir)

  trips <- data.frame(
    ride_id = 1:6,
    rideable_type = "classic_bike",
    started_at = rep(
      c("2023-01-01 08:00:00", "2023-01-02 08:00:00", "2023-01-03 08:00:00"),
      each = 2
    ),
    start_lat = 38.9,
    start_lng = -77.0,
    member_casual = rep(c("member", "casual"), 3)
  )
  readr::write_csv(
    trips,
    file.path(trip_dir, "202301-capitalbikeshare-tripdata.csv")
  )

  weather <- data.frame(
    date = as.Date("2023-01-01") + 0:2,
    Temp = 50,
    Wind = 5,
    Humidity = 60,
    Barometer = 30,
    Visibility = 10,
    Weather = "Clear"
  )

  result <- prepare_bike_rentals(
    trip_directory = trip_dir,
    weather = weather,
    start_date = "2023-01-01",
    end_date = "2023-01-03"
  )

  expect_equal(nrow(result), 3L)
  expect_equal(result$registered, rep(1L, 3))
  expect_equal(result$casual, rep(1L, 3))
  expect_equal(result$total_rentals, rep(2L, 3))
})
