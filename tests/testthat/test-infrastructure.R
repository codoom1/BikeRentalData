test_that("bike infrastructure source table covers supported systems", {
  sources <- available_bike_infrastructure_sources()

  expect_equal(
    sort(unique(sources$system)),
    sort(c("capital", "divvy", "citibike", "baywheels"))
  )
  expect_equal(sum(sources$system == "capital"), 4)
  expect_equal(sum(sources$system == "baywheels"), 1)
  expect_true(all(nzchar(sources$url)))
  expect_true(all(nzchar(sources$jurisdiction)))
  expect_true(all(nzchar(sources$facility_column)))
  expect_true(all(grepl("^https://", sources$url)))
})

test_that("infrastructure data dictionary describes generated variables", {
  dictionary <- infrastructure_data_dictionary(
    buffers_m = c(250, 500),
    sides = c("start", "end"),
    format = "tibble"
  )

  expect_true("start_bikeinfra_250m_m" %in% dictionary$variable)
  expect_true("start_bikeinfra_250m_m_per_km2" %in% dictionary$variable)
  expect_true("end_protected_bikeinfra_500m_m" %in% dictionary$variable)
  expect_true("end_protected_bikeinfra_500m_m_per_km2" %in% dictionary$variable)
  expect_true("start_nearest_bikeinfra_m" %in% dictionary$variable)
  expect_true(all(c(
    "variable", "type", "units", "level", "source",
    "description", "interpretation"
  ) %in% names(dictionary)))
  expect_error(
    infrastructure_data_dictionary(buffers_m = 0),
    "positive distances"
  )
  readable <- infrastructure_data_dictionary(sides = "start")
  expect_s3_class(readable, "bikerental_infrastructure_dictionary")
  expect_output(print(readable), "Variables describe station-area exposure")
})

test_that("bike infrastructure exposure adds station-area metrics", {
  testthat::skip_if_not_installed("sf")

  trips <- tibble::tibble(
    system = "capital",
    start_station_id = "A",
    start_station_name = "Start",
    start_lat = 38.9000,
    start_lng = -77.0000,
    end_station_id = "B",
    end_station_name = "End",
    end_lat = 38.9010,
    end_lng = -77.0010
  )
  infrastructure <- sf::st_sf(
    facility_type = c("protected bike lane", "trail"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(
        c(-77.0005, 38.8995, -77.0005, 38.9005),
        ncol = 2,
        byrow = TRUE
      )),
      sf::st_linestring(matrix(
        c(-77.0015, 38.9005, -77.0015, 38.9015),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 4326
    )
  )

  enriched <- add_bike_infrastructure_exposure(
    trips,
    infrastructure,
    buffers_m = c(250, 500),
    facility_column = "facility_type"
  )

  expect_true("start_bikeinfra_250m_m" %in% names(enriched))
  expect_true("start_bikeinfra_250m_m_per_km2" %in% names(enriched))
  expect_true("end_trail_500m_m" %in% names(enriched))
  expect_true("end_trail_500m_m_per_km2" %in% names(enriched))
  expect_true(enriched$start_bikeinfra_250m_m > 0)
  expect_equal(
    enriched$start_bikeinfra_250m_m_per_km2,
    enriched$start_bikeinfra_250m_m / (pi * 0.25^2),
    tolerance = 1e-6
  )
  expect_true(enriched$start_any_protected_500m)
  expect_true(enriched$end_nearest_bikeinfra_m >= 0)
})

test_that("station exposure can be cached and joined", {
  testthat::skip_if_not_installed("sf")

  trips <- tibble::tibble(
    system = "capital",
    start_station_id = c("A", "A"),
    start_station_name = c("Start", "Start"),
    start_lat = c(38.9000, 38.9001),
    start_lng = c(-77.0000, -77.0001),
    end_station_id = c("B", "B"),
    end_station_name = c("End", "End"),
    end_lat = c(38.9010, 38.9011),
    end_lng = c(-77.0010, -77.0011)
  )
  infrastructure <- sf::st_sf(
    facility_type = c("protected bike lane", "trail"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(
        c(-77.0005, 38.8995, -77.0005, 38.9005),
        ncol = 2,
        byrow = TRUE
      )),
      sf::st_linestring(matrix(
        c(-77.0015, 38.9005, -77.0015, 38.9015),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 4326
    )
  )
  cache_file <- tempfile("station-exposure-", fileext = ".csv")

  station_exposure <- build_station_infrastructure_exposure(
    trips,
    infrastructure,
    buffers_m = c(250, 500),
    facility_column = "facility_type",
    cache_file = cache_file
  )
  cached <- build_station_infrastructure_exposure(
    trips,
    infrastructure,
    buffers_m = c(250, 500),
    facility_column = "facility_type",
    cache_file = cache_file
  )
  enriched <- add_station_infrastructure_exposure(trips, cached)
  coverage <- summarize_station_infrastructure_coverage(trips, cached)

  expect_true(file.exists(cache_file))
  expect_equal(nrow(station_exposure), 2)
  expect_equal(nrow(cached), 2)
  expect_true(all(!is.na(enriched$start_bikeinfra_500m_m)))
  expect_equal(dplyr::n_distinct(enriched$start_bikeinfra_500m_m), 1)
  expect_equal(coverage$pct_stations_with_exposure, c(100, 100))
})

test_that("bike infrastructure exposure validates coordinate columns", {
  testthat::skip_if_not_installed("sf")

  infrastructure <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )

  expect_error(
    add_bike_infrastructure_exposure(
      tibble::tibble(start_lat = 1),
      infrastructure,
      sides = "start"
    ),
    "missing required coordinate columns"
  )
})
