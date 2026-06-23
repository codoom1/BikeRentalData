#' bikerentaldata: Capital Bikeshare data tools
#'
#' `bikerentaldata` downloads and standardizes historical trip data from
#' Capital Bikeshare, Citi Bike, Divvy, and Bay Wheels without bundling the
#' full third-party datasets.
#'
#' @section Main workflow:
#' Use [build_bike_rental_data()] to download trip archives, retrieve weather
#' observations, prepare daily records, validate them, and save a processed
#' CSV. For more control, use [download_trip_files()],
#' [download_weather_data()], and [prepare_bike_rentals()] separately.
#' Use [load_trip_data()] and [standardize_trips()] for multi-system,
#' trip-level research, and [trip_data_dictionary()] for field definitions.
#'
#' @section Data discovery:
#' Use [available_trip_data()] to inspect official archive coverage,
#' [current_system_info()] to see the current station and jurisdiction
#' footprint, and [summarize_trip_locations()] to count station locations in
#' downloaded historical files.
#'
#' @section Loading and validation:
#' Use [load_bike_rentals()] to read processed data,
#' [validate_bike_rentals()] to check its structure, and [data_dictionary()] to
#' inspect variable definitions.
#'
#' @section Legacy support:
#' Archive structures vary by system and year. The standardizer supports the
#' recent common Lyft schema and common legacy column names. Some legacy files
#' do not include bike type or coordinates, so unavailable fields are `NA`.
#'
#' @section Data licensing:
#' The package's MIT license applies only to original software. Capital
#' Bikeshare-system and weather records remain subject to their source terms.
#'
#' @seealso
#' [build_bike_rental_data()], [available_trip_data()],
#' [current_system_info()], [summarize_trip_locations()]
#'
#' @examples
#' data_dictionary()
#'
#' \dontrun{
#' available_trip_data()
#' current_system_info()
#' }
#'
#' @docType package
#' @name bikerentaldata-package
#' @aliases bikerentaldata
"_PACKAGE"
