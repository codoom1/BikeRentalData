#' List available Capital Bikeshare trip archives
#'
#' Queries the official Capital Bikeshare S3 archive and returns the currently
#' available annual and monthly trip files.
#'
#' @return A tibble containing archive name, format, period, and download URL.
#'
#' @examples
#' \dontrun{
#' archives <- available_trip_data()
#' head(archives)
#' range(archives$start_date)
#' range(archives$end_date)
#' }
#' @export
available_trip_data <- function() {
  endpoint <- "https://capitalbikeshare-data.s3.amazonaws.com/?list-type=2"
  xml <- paste(readLines(endpoint, warn = FALSE), collapse = "")
  matches <- regmatches(
    xml,
    gregexpr("<Key>[^<]+-capitalbikeshare-tripdata[.]zip</Key>", xml)
  )[[1]]

  if (length(matches) == 0L || identical(matches, "")) {
    stop("No Capital Bikeshare archives were found.", call. = FALSE)
  }

  keys <- gsub("</?Key>", "", matches)
  ids <- sub("-capitalbikeshare-tripdata[.]zip$", "", keys)
  annual <- grepl("^[0-9]{4}$", ids)
  start_date <- as.Date(ifelse(
    annual,
    paste0(ids, "-01-01"),
    paste0(substr(ids, 1, 4), "-", substr(ids, 5, 6), "-01")
  ))
  start_date[ids == "2010"] <- as.Date("2010-09-20")
  end_date <- start_date
  end_date[annual] <- as.Date(paste0(ids[annual], "-12-31"))
  end_date[!annual] <- (
    lubridate::ceiling_date(start_date[!annual], "month") - 1
  )

  tibble::tibble(
    archive = keys,
    format = ifelse(annual, "annual", "monthly"),
    start_date = start_date,
    end_date = end_date,
    url = paste0(
      "https://capitalbikeshare-data.s3.amazonaws.com/",
      keys
    )
  ) |>
    dplyr::arrange(start_date)
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
  by = c("overall", "file")
) {
  by <- match.arg(by)
  paths <- sort(list.files(
    trip_directory,
    pattern = "capitalbikeshare-tripdata[.]csv$",
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
      file = basename(paths),
      station_locations = vapply(station_sets, length, integer(1))
    ))
  }

  tibble::tibble(
    files = length(paths),
    first_file = basename(paths[[1]]),
    last_file = basename(paths[[length(paths)]]),
    unique_station_locations = length(unique(unlist(station_sets)))
  )
}
