make_standardized_trip <- function(
  system = "capital",
  started_at = "2024-07-04 08:30:00"
) {
  raw <- data.frame(
    started_at = started_at,
    ended_at = "2024-07-04 08:45:00",
    member_casual = "member"
  )
  standardize_trips(raw, system)
}

test_that("calendar variables use each system's local time", {
  capital <- make_standardized_trip("capital", "2024-07-04 23:30:00")
  calendar <- add_calendar_variables(capital)

  expect_equal(calendar$trip_date, as.Date("2024-07-04"))
  expect_equal(calendar$hour, 23L)
  expect_true(calendar$us_federal_holiday)
  expect_equal(calendar$holiday_name, "Independence Day")
  expect_equal(calendar$season, "Summer")
})

test_that("calendar variables identify weekends and time of day", {
  divvy <- make_standardized_trip("divvy", "2024-07-06 21:00:00")
  calendar <- add_calendar_variables(divvy)

  expect_true(calendar$weekend)
  expect_equal(calendar$weekday_name, "Saturday")
  expect_equal(calendar$time_of_day, "evening")
})

test_that("weather enrichment joins system-specific daily values", {
  testthat::local_mocked_bindings(
    download_weather_data = function(
      start_date,
      end_date,
      cache_file,
      station,
      system,
      timezone
    ) {
      tibble::tibble(
        date = as.Date(start_date),
        Temp = 50,
        Wind = 10,
        Humidity = 60,
        Barometer = 30,
        Visibility = 9,
        Precipitation = 0.1,
        Weather = "Rain"
      )
    },
    .package = "bikerentaldata"
  )

  trips <- add_calendar_variables(make_standardized_trip())
  enriched <- add_weather_variables(trips, tempfile("weather-cache-"))

  expect_equal(enriched$weather_station, "DCA")
  expect_equal(enriched$weather_temp_f, 50)
  expect_equal(enriched$weather_precipitation_in, 0.1)
  expect_equal(enriched$weather_conditions, "Rain")
})

test_that("multicity builder combines and enriches supported systems", {
  testthat::local_mocked_bindings(
    download_trip_files = function(
      start_date,
      end_date,
      destination,
      overwrite,
      system
    ) {
      paste0(system, ".csv")
    },
    load_trip_data = function(
      path,
      system,
      start_date,
      end_date,
      n_max
    ) {
      make_standardized_trip(system)
    },
    .package = "bikerentaldata"
  )

  result <- build_multicity_data(
    systems = c("capital", "divvy"),
    start_date = "2024-07-04",
    end_date = "2024-07-04",
    data_dir = tempfile("multicity-"),
    calendar = TRUE,
    weather = FALSE
  )

  expect_equal(result$system, c("capital", "divvy"))
  expect_true("trip_date" %in% names(result))
  expect_true(all(result$us_federal_holiday))
})

test_that("system metadata includes weather defaults", {
  systems <- available_systems()
  expect_equal(
    systems$weather_station,
    c("DCA", "LGA", "ORD", "SFO")
  )
  expect_equal(
    systems$timezone,
    c(
      "America/New_York",
      "America/New_York",
      "America/Chicago",
      "America/Los_Angeles"
    )
  )
})
