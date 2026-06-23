.trip_value <- function(data, candidates, default = NA_character_) {
  column <- .find_optional_column(data, candidates)
  if (is.null(column)) {
    return(rep(default, nrow(data)))
  }
  as.character(data[[column]])
}

.numeric_trip_value <- function(data, candidates) {
  suppressWarnings(as.numeric(.trip_value(data, candidates)))
}

.system_timezone <- function(system) {
  system <- .match_system(system)
  .system_metadata$timezone[.system_metadata$system == system]
}

#' Standardize historical bike-share trips
#'
#' Converts recent Lyft-format files and common legacy schemas from Capital
#' Bikeshare, Citi Bike, Divvy, and Bay Wheels into one trip-level structure.
#'
#' @param data A raw trip data frame read from an official source CSV.
#' @param system System identifier from [available_systems()].
#'
#' @return A tibble with standardized trip, station, coordinate, bike-type,
#'   rider-type, and duration fields.
#'
#' @details
#' Recent files from all four systems use a similar schema. The function also
#' recognizes common historical names such as `starttime`, `stoptime`,
#' `usertype`, `from_station_id`, and `tripduration`. Source fields that were
#' not published are returned as `NA`.
#'
#' @examples
#' raw <- data.frame(
#'   ride_id = "ride-1",
#'   rideable_type = "classic_bike",
#'   started_at = "2024-01-01 10:00:00",
#'   ended_at = "2024-01-01 10:12:00",
#'   member_casual = "member"
#' )
#'
#' standardize_trips(raw, system = "capital")
#' @export
standardize_trips <- function(data, system) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  system <- .match_system(system)
  metadata <- .system_metadata[.system_metadata$system == system, ]
  timezone <- .system_timezone(system)

  start_column <- .find_optional_column(
    data,
    c("started_at", "starttime", "start_time", "start_date")
  )
  end_column <- .find_optional_column(
    data,
    c("ended_at", "stoptime", "end_time", "end_date", "stop_time")
  )
  if (is.null(start_column) || is.null(end_column)) {
    stop(
      "Trip data must contain recognizable start and end time columns.",
      call. = FALSE
    )
  }

  started_text <- as.character(data[[start_column]])
  ended_text <- as.character(data[[end_column]])
  started_at <- .parse_trip_datetime(started_text, tz = timezone)
  ended_at <- .parse_trip_datetime(ended_text, tz = timezone)

  duration_seconds <- .numeric_trip_value(
    data,
    c("tripduration", "trip_duration", "duration_sec", "duration")
  )
  calculated_duration <- as.numeric(
    difftime(ended_at, started_at, units = "secs")
  )
  duration_seconds[is.na(duration_seconds)] <- calculated_duration[
    is.na(duration_seconds)
  ]

  rider_raw <- tolower(trimws(.trip_value(
    data,
    c("member_casual", "usertype", "user_type", "member_type", "membership")
  )))
  rider_type <- dplyr::case_when(
    rider_raw %in% c("member", "subscriber", "registered") ~ "member",
    rider_raw %in% c(
      "casual", "customer", "single ride", "day pass",
      "dependent", "casual rider"
    ) ~ "casual",
    TRUE ~ rider_raw
  )
  rider_type[is.na(rider_type) | !nzchar(rider_type)] <- NA_character_

  tibble::tibble(
    system = system,
    system_name = metadata$name,
    city = metadata$city,
    ride_id = .trip_value(data, c("ride_id", "trip_id", "tripid")),
    bike_type = .trip_value(
      data,
      c("rideable_type", "bike_type", "biketype")
    ),
    started_at = started_at,
    ended_at = ended_at,
    duration_seconds = duration_seconds,
    duration_minutes = duration_seconds / 60,
    start_station_id = .trip_value(
      data,
      c("start_station_id", "start_station_number", "from_station_id")
    ),
    start_station_name = .trip_value(
      data,
      c("start_station_name", "start_station", "from_station_name")
    ),
    end_station_id = .trip_value(
      data,
      c("end_station_id", "end_station_number", "to_station_id")
    ),
    end_station_name = .trip_value(
      data,
      c("end_station_name", "end_station", "to_station_name")
    ),
    start_lat = .numeric_trip_value(
      data,
      c("start_lat", "start_station_latitude", "start_latitude")
    ),
    start_lng = .numeric_trip_value(
      data,
      c(
        "start_lng", "start_lon", "start_station_longitude",
        "start_longitude"
      )
    ),
    end_lat = .numeric_trip_value(
      data,
      c("end_lat", "end_station_latitude", "end_latitude")
    ),
    end_lng = .numeric_trip_value(
      data,
      c("end_lng", "end_lon", "end_station_longitude", "end_longitude")
    ),
    rider_type = rider_type
  )
}

#' Load and standardize historical trip files
#'
#' Reads one CSV, a vector of CSVs, or all CSVs in a directory and combines
#' them into the common trip schema returned by [standardize_trips()].
#'
#' @param path A CSV path, vector of CSV paths, or directory containing
#'   extracted trip CSV files.
#' @param system System identifier from [available_systems()].
#' @param start_date Optional first trip date to retain after loading.
#' @param end_date Optional last trip date to retain after loading.
#' @param n_max Maximum rows to read from each file. Use a small value when
#'   testing large archives.
#'
#' @return A standardized trip-level tibble.
#'
#' @examples
#' \dontrun{
#' trips <- load_trip_data(
#'   "data/divvy",
#'   system = "divvy",
#'   n_max = 1000
#' )
#' head(trips)
#' }
#' @export
load_trip_data <- function(
  path,
  system,
  start_date = NULL,
  end_date = NULL,
  n_max = Inf
) {
  if (length(path) == 1L && dir.exists(path)) {
    all_csv <- sort(list.files(
      path,
      pattern = "[.]csv$",
      full.names = TRUE,
      recursive = TRUE
    ))
    trip_csv <- all_csv[
      grepl("(tripdata|trips)", basename(all_csv), ignore.case = TRUE)
    ]
    path <- if (length(trip_csv)) trip_csv else all_csv
  }
  if (length(path) == 0L || any(!file.exists(path))) {
    stop("One or more trip CSV paths do not exist.", call. = FALSE)
  }

  trips <- purrr::map_dfr(path, function(file) {
    message("Reading ", basename(file))
    raw <- readr::read_csv(
      file,
      col_types = readr::cols(.default = readr::col_character()),
      progress = FALSE,
      n_max = n_max
    )
    standardize_trips(raw, system)
  })

  if (!is.null(start_date)) {
    start_date <- .as_date(start_date, "start_date")
    trips <- dplyr::filter(trips, as.Date(started_at) >= start_date)
  }
  if (!is.null(end_date)) {
    end_date <- .as_date(end_date, "end_date")
    trips <- dplyr::filter(trips, as.Date(started_at) <= end_date)
  }

  trips
}

#' Standardized trip-data dictionary
#'
#' @return A tibble describing every field returned by
#'   [standardize_trips()] and [load_trip_data()].
#'
#' @examples
#' trip_data_dictionary()
#' @export
trip_data_dictionary <- function() {
  tibble::tribble(
    ~variable, ~type, ~source, ~description,
    "system", "character", "base", "Short system identifier",
    "system_name", "character", "base", "Public bike-share system name",
    "city", "character", "base", "Primary metropolitan area",
    "ride_id", "character", "base", "Source ride or trip identifier",
    "bike_type", "character", "base", "Published bike or rideable type",
    "started_at", "POSIXct", "base", "Trip start date and local time",
    "ended_at", "POSIXct", "base", "Trip end date and local time",
    "duration_seconds", "double", "base", "Published or calculated duration in seconds",
    "duration_minutes", "double", "base", "Trip duration in minutes",
    "start_station_id", "character", "base", "Origin station identifier",
    "start_station_name", "character", "base", "Origin station name",
    "end_station_id", "character", "base", "Destination station identifier",
    "end_station_name", "character", "base", "Destination station name",
    "start_lat", "double", "base", "Origin latitude",
    "start_lng", "double", "base", "Origin longitude",
    "end_lat", "double", "base", "Destination latitude",
    "end_lng", "double", "base", "Destination longitude",
    "rider_type", "character", "base", "Standardized member or casual category",
    "trip_date", "Date", "calendar", "Local trip-start date",
    "year", "integer", "calendar", "Calendar year",
    "month", "integer", "calendar", "Calendar month",
    "month_name", "character", "calendar", "Calendar month name",
    "day", "integer", "calendar", "Day of month",
    "weekday", "integer", "calendar", "Monday equals 1 and Sunday equals 7",
    "weekday_name", "character", "calendar", "Weekday name",
    "weekend", "logical", "calendar", "Saturday or Sunday indicator",
    "hour", "integer", "calendar", "Local trip-start hour",
    "time_of_day", "character", "calendar", "Trip-start time category",
    "season", "character", "calendar", "Meteorological season",
    "us_federal_holiday", "logical", "calendar", "Observed federal holiday indicator",
    "holiday_name", "character", "calendar", "Observed federal holiday name",
    "weather_station", "character", "weather", "ASOS airport station identifier",
    "weather_temp_f", "double", "weather", "Daily mean temperature in Fahrenheit",
    "weather_wind_mph", "double", "weather", "Daily mean wind speed in miles per hour",
    "weather_humidity_pct", "double", "weather", "Daily mean relative humidity",
    "weather_barometer_inhg", "double", "weather", "Daily mean altimeter pressure",
    "weather_visibility_miles", "double", "weather", "Daily mean visibility",
    "weather_precipitation_in", "double", "weather", "Reported daily precipitation",
    "weather_conditions", "character", "weather", "Most frequent daily conditions"
  )
}
