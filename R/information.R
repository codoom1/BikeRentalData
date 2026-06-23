#' List available historical trip archives
#'
#' Queries the selected system's official archive and returns available annual,
#' quarterly, or monthly trip files.
#'
#' @param system System identifier from [available_systems()]: `"capital"`,
#'   `"citibike"`, `"divvy"`, or `"baywheels"`. Defaults to `"capital"`.
#' @return A tibble containing archive name, format, period, and download URL.
#'
#' @examples
#' \dontrun{
#' archives <- available_trip_data("citibike")
#' head(archives)
#' range(archives$start_date)
#' range(archives$end_date)
#' }
#' @export
available_trip_data <- function(system = "capital") {
  system <- .match_system(system)
  .parse_archive_period(system, .list_archive_keys(system))
}

#' Get current Capital Bikeshare system information
#'
#' Queries the public GBFS feed for the current station count and reports the
#' eight jurisdictions served by Capital Bikeshare.
#'
#' @return A `bikerental_system_info` object. Printing the object displays the
#'   station count and each jurisdiction on a separate line. Its elements can
#'   also be accessed with `$`.
#'
#' @examples
#' \dontrun{
#' info <- current_system_info()
#' info
#' info$station_locations
#' info$jurisdiction_names
#' }
#' @export
current_system_info <- function() {
  endpoint <- paste0(
    "https://gbfs.capitalbikeshare.com/gbfs/en/",
    "station_information.json"
  )
  feed <- jsonlite::fromJSON(endpoint)
  stations <- feed$data$stations
  jurisdiction_names <- c(
    "Washington, DC",
    "Arlington, VA",
    "Alexandria, VA",
    "Montgomery County, MD",
    "Prince George's County, MD",
    "Fairfax County, VA",
    "City of Fairfax, VA",
    "City of Falls Church, VA"
  )

  structure(
    list(
      station_locations = nrow(stations),
      jurisdiction_count = length(jurisdiction_names),
      jurisdiction_names = jurisdiction_names,
      feed_url = endpoint
    ),
    class = "bikerental_system_info"
  )
}

#' Print current Capital Bikeshare system information
#'
#' @param x A `bikerental_system_info` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.bikerental_system_info <- function(x, ...) {
  cat("Capital Bikeshare current system\n")
  cat("  Station locations:", x$station_locations, "\n")
  cat("  Jurisdictions:", x$jurisdiction_count, "\n")
  for (jurisdiction in x$jurisdiction_names) {
    cat("   -", jurisdiction, "\n")
  }
  cat("  Live feed:", x$feed_url, "\n")
  invisible(x)
}

#' Summarize station locations in downloaded trip files
#'
#' Counts distinct start-station identifiers represented in local annual or
#' monthly Capital Bikeshare CSV files.
#'
#' @param trip_directory Directory containing downloaded trip CSV files.
#' @param by Return an overall summary or one row per source file.
#' @param system System identifier used to label the result.
#'
#' @return A tibble containing file and station-location counts.
#'
#' @examples
#' \dontrun{
#' summarize_trip_locations("data/raw")
#' summarize_trip_locations("data/raw", by = "file")
#' }
#' @export
summarize_trip_locations <- function(
  trip_directory = file.path("data", "raw"),
  by = c("overall", "file"),
  system = "capital"
) {
  by <- match.arg(by)
  system <- .match_system(system)
  paths <- sort(list.files(
    trip_directory,
    pattern = "[.]csv$",
    full.names = TRUE,
    recursive = TRUE
  ))

  if (length(paths) == 0L) {
    stop("No Capital Bikeshare trip CSV files found.", call. = FALSE)
  }

  station_sets <- lapply(paths, function(path) {
    header <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      n_max = 0,
      progress = FALSE
    )
    station_column <- .find_column(
      header,
      c(
        "start_station_id", "start_station_number",
        "start_station_name", "start_station"
      ),
      "start station"
    )
    data <- readr::read_csv(
      path,
      col_select = dplyr::all_of(station_column),
      col_types = readr::cols(.default = readr::col_character()),
      progress = FALSE
    )
    stations <- trimws(as.character(data[[station_column]]))
    unique(stations[!is.na(stations) & nzchar(stations)])
  })

  if (by == "file") {
    return(tibble::tibble(
      system = system,
      file = basename(paths),
      station_locations = vapply(station_sets, length, integer(1))
    ))
  }

  tibble::tibble(
    system = system,
    files = length(paths),
    first_file = basename(paths[[1]]),
    last_file = basename(paths[[length(paths)]]),
    unique_station_locations = length(unique(unlist(station_sets)))
  )
}
