#' bikerentaldata: Historical U.S. bike-share trip data tools
#'
#' `bikerentaldata` downloads and standardizes historical trip data from
#' Capital Bikeshare, Citi Bike, Divvy, and Bay Wheels without bundling the
#' full third-party datasets.
#'
#' @section Quick start:
#' 1. Run [available_systems()] to list supported systems.
#' 2. Run [available_trip_data()] to inspect archive coverage.
#' 3. Run [download_trip_files()] for a small date range.
#' 4. Run [load_trip_data()] to read and standardize the downloaded CSV files.
#' 5. Run [trip_data_dictionary()] to inspect the output fields.
#' Use [build_multicity_data()] to perform these steps for several systems in
#' one call.
#'
#' @section Supported systems:
#' Valid system identifiers are `"capital"` (Washington, D.C.),
#' `"citibike"` (New York City), `"divvy"` (Chicago), and `"baywheels"`
#' (San Francisco Bay Area).
#'
#' @section Capital Bikeshare paper workflow:
#' [build_bike_rental_data()] downloads Capital Bikeshare trips and weather,
#' prepares daily records, validates them, and optionally writes a processed
#' CSV. [load_bike_rentals()] and [validate_bike_rentals()] work with that
#' daily dataset.
#'
#' @section Calendar and weather enrichment:
#' [add_calendar_variables()] adds local calendar, time-of-day, season,
#' weekend, and observed U.S. federal holiday fields.
#' [add_weather_variables()] adds daily metro weather from default ASOS
#' stations. Weather represents the metro area, not a rider's exact route.
#'
#' @section Legacy support:
#' Archive structures vary by system and year. The standardizer supports the
#' recent common Lyft schema and common legacy column names. Some legacy files
#' do not include bike type or coordinates, so unavailable fields are `NA`.
#'
#' @section Large downloads:
#' Inspect [available_trip_data()] before downloading. Some archives are large,
#' particularly annual Citi Bike archives. Use a recent one-month range and
#' the `n_max` argument to [load_trip_data()] when testing the package.
#'
#' @section Data licensing:
#' The package's MIT license applies only to original software. Capital
#' Bikeshare, Citi Bike, Divvy, Bay Wheels, and weather records remain subject
#' to their source terms.
#'
#' @seealso
#' [available_systems()], [available_trip_data()], [download_trip_files()],
#' [load_trip_data()], [build_multicity_data()], [trip_data_dictionary()]
#'
#' @examples
#' available_systems()
#' trip_data_dictionary()
#'
#' \dontrun{
#' archives <- available_trip_data("divvy")
#' paths <- download_trip_files(
#'   system = "divvy",
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-31",
#'   destination = "data/divvy"
#' )
#' trips <- load_trip_data(paths, system = "divvy", n_max = 1000)
#' }
#'
#' @docType package
#' @name bikerentaldata-package
#' @aliases bikerentaldata
"_PACKAGE"
