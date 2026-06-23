#' bikerentaldata: Capital Bikeshare data tools
#'
#' `bikerentaldata` downloads, prepares, validates, and describes Capital
#' Bikeshare trip data without bundling the full third-party dataset.
#'
#' @section Main workflow:
#' Use [build_bike_rental_data()] to download trip archives, retrieve weather
#' observations, prepare daily records, validate them, and save a processed
#' CSV. For more control, use [download_trip_files()],
#' [download_weather_data()], and [prepare_bike_rentals()] separately.
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
#' The package supports annual or quarterly files from September 20, 2010
#' through 2017 and monthly files from January 2018 onward. Legacy files do not
#' include coordinates, so prepared latitude and longitude values are `NA`.
#'
#' @section Data licensing:
#' The package's MIT license applies only to original software. Capital
#' Bikeshare and weather records remain subject to their source terms.
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
