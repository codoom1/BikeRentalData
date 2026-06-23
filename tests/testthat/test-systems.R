test_that("supported systems are listed", {
  systems <- available_systems()
  expect_equal(
    systems$system,
    c("capital", "citibike", "divvy", "baywheels")
  )
})

test_that("archive keys are parsed across systems", {
  citi <- bikerentaldata:::.parse_archive_period(
    "citibike",
    c("2023-citibike-tripdata.zip", "202401-citibike-tripdata.zip")
  )
  divvy <- bikerentaldata:::.parse_archive_period(
    "divvy",
    c("Divvy_Trips_2019_Q1.zip", "202401-divvy-tripdata.zip")
  )
  bay <- bikerentaldata:::.parse_archive_period(
    "baywheels",
    c(
      "2017-fordgobike-tripdata.csv.zip",
      "202401-baywheels-tripdata.csv.zip"
    )
  )

  expect_equal(citi$format, c("annual", "monthly"))
  expect_equal(divvy$format, c("quarterly", "monthly"))
  expect_equal(bay$format, c("annual", "monthly"))
})

test_that("modern shared schema standardizes for all systems", {
  raw <- data.frame(
    ride_id = "ride-1",
    rideable_type = "electric_bike",
    started_at = "2024-01-01 10:00:00",
    ended_at = "2024-01-01 10:12:00",
    start_station_name = "Origin",
    start_station_id = "1",
    end_station_name = "Destination",
    end_station_id = "2",
    start_lat = "40.0",
    start_lng = "-73.0",
    end_lat = "40.1",
    end_lng = "-73.1",
    member_casual = "member"
  )

  for (system in c("capital", "citibike", "divvy", "baywheels")) {
    result <- standardize_trips(raw, system)
    expect_equal(result$system, system)
    expect_equal(result$bike_type, "electric_bike")
    expect_equal(result$rider_type, "member")
    expect_equal(result$duration_minutes, 12)
  }
})

test_that("common legacy schemas standardize", {
  citi <- data.frame(
    tripduration = "600",
    starttime = "2019-01-01 08:00:00",
    stoptime = "2019-01-01 08:10:00",
    `start station id` = "1",
    `start station name` = "A",
    `start station latitude` = "40",
    `start station longitude` = "-73",
    `end station id` = "2",
    `end station name` = "B",
    `end station latitude` = "40.1",
    `end station longitude` = "-73.1",
    usertype = "Subscriber",
    check.names = FALSE
  )
  divvy <- data.frame(
    trip_id = "2",
    starttime = "2019-01-01 08:00:00",
    stoptime = "2019-01-01 08:10:00",
    from_station_id = "1",
    from_station_name = "A",
    to_station_id = "2",
    to_station_name = "B",
    tripduration = "600",
    usertype = "Customer"
  )

  expect_equal(standardize_trips(citi, "citibike")$rider_type, "member")
  expect_equal(standardize_trips(divvy, "divvy")$rider_type, "casual")
})

test_that("load_trip_data reads a directory", {
  directory <- tempfile("multi-system-")
  dir.create(directory)
  readr::write_csv(
    data.frame(
      ride_id = "A",
      rideable_type = "classic_bike",
      started_at = "2024-01-01 10:00:00",
      ended_at = "2024-01-01 10:05:00",
      member_casual = "casual"
    ),
    file.path(directory, "trips.csv")
  )

  result <- load_trip_data(directory, "divvy")
  expect_equal(nrow(result), 1L)
  expect_equal(result$city, "Chicago")
  expect_equal(result$duration_seconds, 300)
})

test_that("trip dictionary covers the standardized schema", {
  dictionary <- trip_data_dictionary()
  raw <- data.frame(
    started_at = "2024-01-01 10:00:00",
    ended_at = "2024-01-01 10:05:00",
    member_casual = "member"
  )
  standardized <- standardize_trips(raw, "capital")
  expect_true(all(names(standardized) %in% dictionary$variable))
})
