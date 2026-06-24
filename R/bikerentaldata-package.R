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
#' @section Bicycle infrastructure exposure:
#' The package can download official bicycle infrastructure layers and compute
#' station-area exposure variables. Use [download_bike_infrastructure()] to get
#' the supported infrastructure layer for a system, and
#' [available_bike_infrastructure_sources()] to inspect the source, coverage
#' area, jurisdiction, facility-type field, and caveats.
#'
#' Infrastructure exposure is measured around stations, not along the unknown
#' rider route. For example, `start_bikeinfra_500m_m` is the total length of
#' bicycle infrastructure within 500 meters of the trip's start station, and
#' `start_nearest_bikeinfra_m` is the straight-line distance from the start
#' station to the nearest bicycle facility. These variables should be
#' interpreted as nearby infrastructure availability, not proof that a rider
#' used a specific bike lane or trail.
#'
#' For large systems such as Citi Bike, use the station-level cache workflow:
#' build a reusable station exposure table with
#' [build_station_infrastructure_exposure()], join it to trips with
#' [add_station_infrastructure_exposure()], and audit coverage with
#' [summarize_station_infrastructure_coverage()]. The convenience wrapper
#' [add_bike_infrastructure_exposure()] performs the build-and-join workflow in
#' one call and accepts a `cache_file` argument.
#'
#' Run [infrastructure_data_dictionary()] for detailed definitions of the
#' infrastructure variables and [trip_data_dictionary()] for the full
#' standardized trip schema.
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
#' [load_trip_data()], [build_multicity_data()], [trip_data_dictionary()],
#' [download_bike_infrastructure()], [build_station_infrastructure_exposure()],
#' [infrastructure_data_dictionary()]
#'
#' @examples
#' available_systems()
#' trip_data_dictionary()
#' infrastructure_data_dictionary()
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
#'
#' bike_lanes <- download_bike_infrastructure("divvy")
#' station_exposure <- build_station_infrastructure_exposure(
#'   trips,
#'   bike_lanes,
#'   cache_file = "data/cache/divvy_station_exposure.csv"
#' )
#' trips <- add_station_infrastructure_exposure(trips, station_exposure)
#' }
#'
#' @docType package
#' @name bikerentaldata-package
#' @aliases bikerentaldata
"_PACKAGE"
