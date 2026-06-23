.observed_fixed_holiday <- function(date) {
  weekday <- lubridate::wday(date, week_start = 1)
  date + ifelse(weekday == 6L, -1L, ifelse(weekday == 7L, 1L, 0L))
}

.nth_weekday <- function(year, month, weekday, n) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  offset <- (weekday - lubridate::wday(first, week_start = 1) + 7L) %% 7L
  first + offset + 7L * (n - 1L)
}

.last_weekday <- function(year, month, weekday) {
  last <- lubridate::ceiling_date(
    as.Date(sprintf("%04d-%02d-01", year, month)),
    "month"
  ) - 1L
  last - (
    lubridate::wday(last, week_start = 1) - weekday + 7L
  ) %% 7L
}

.federal_holidays <- function(years) {
  years <- sort(unique(as.integer(years)))
  result <- lapply(years, function(year) {
    dates <- c(
      .observed_fixed_holiday(as.Date(sprintf("%04d-01-01", year))),
      .nth_weekday(year, 1, 1, 3),
      .nth_weekday(year, 2, 1, 3),
      .last_weekday(year, 5, 1),
      if (year >= 2021L) {
        .observed_fixed_holiday(as.Date(sprintf("%04d-06-19", year)))
      } else {
        as.Date(NA)
      },
      .observed_fixed_holiday(as.Date(sprintf("%04d-07-04", year))),
      .nth_weekday(year, 9, 1, 1),
      .nth_weekday(year, 10, 1, 2),
      .observed_fixed_holiday(as.Date(sprintf("%04d-11-11", year))),
      .nth_weekday(year, 11, 4, 4),
      .observed_fixed_holiday(as.Date(sprintf("%04d-12-25", year)))
    )
    names <- c(
      "New Year's Day",
      "Martin Luther King Jr. Day",
      "Washington's Birthday",
      "Memorial Day",
      "Juneteenth National Independence Day",
      "Independence Day",
      "Labor Day",
      "Columbus Day",
      "Veterans Day",
      "Thanksgiving Day",
      "Christmas Day"
    )
    keep <- !is.na(dates)
    tibble::tibble(date = dates[keep], holiday_name = names[keep])
  })

  dplyr::bind_rows(result) |>
    dplyr::distinct(date, .keep_all = TRUE)
}

#' Add calendar variables to standardized trips
#'
#' Adds local trip date, year, month, weekday, weekend, hour, time-of-day,
#' season, and U.S. federal holiday variables.
#'
#' @param trips A standardized trip data frame containing `system` and
#'   `started_at`.
#'
#' @return The input data with calendar columns appended.
#'
#' @details
#' Holiday indicators use the observed U.S. federal holiday calendar. They do
#' not include city- or state-specific holidays, school calendars, or special
#' events.
#'
#' @examples
#' raw <- data.frame(
#'   started_at = "2024-07-04 08:30:00",
#'   ended_at = "2024-07-04 08:45:00",
#'   member_casual = "member"
#' )
#' trips <- standardize_trips(raw, "capital")
#' add_calendar_variables(trips)
#' @export
add_calendar_variables <- function(trips) {
  if (
    !is.data.frame(trips) ||
      !all(c("system", "started_at") %in% names(trips))
  ) {
    stop(
      "`trips` must contain `system` and `started_at` columns.",
      call. = FALSE
    )
  }
  if (anyNA(trips$started_at)) {
    warning(
      "Trips with missing `started_at` values receive missing calendar fields.",
      call. = FALSE
    )
  }

  trip_date <- as.Date(rep(NA_character_, nrow(trips)))
  hour <- rep(NA_integer_, nrow(trips))
  for (system in unique(trips$system[!is.na(trips$system)])) {
    index <- which(trips$system == system)
    local_time <- lubridate::with_tz(
      trips$started_at[index],
      tzone = .system_timezone(system)
    )
    trip_date[index] <- as.Date(format(local_time, "%Y-%m-%d"))
    hour[index] <- lubridate::hour(local_time)
  }

  years <- lubridate::year(trip_date)
  valid_years <- years[!is.na(years)]
  holidays <- if (length(valid_years)) {
    .federal_holidays(seq(
      min(valid_years) - 1L,
      max(valid_years) + 1L
    ))
  } else {
    tibble::tibble(date = as.Date(character()), holiday_name = character())
  }
  holiday_match <- match(trip_date, holidays$date)
  month <- lubridate::month(trip_date)

  dplyr::mutate(
    trips,
    trip_date = trip_date,
    year = years,
    month = month,
    month_name = month.name[month],
    day = lubridate::day(trip_date),
    weekday = lubridate::wday(trip_date, week_start = 1),
    weekday_name = weekdays(trip_date),
    weekend = weekday %in% c(6L, 7L),
    hour = hour,
    time_of_day = dplyr::case_when(
      hour < 6L ~ "overnight",
      hour < 10L ~ "morning_peak",
      hour < 16L ~ "midday",
      hour < 20L ~ "evening_peak",
      TRUE ~ "evening"
    ),
    season = dplyr::case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Autumn",
      TRUE ~ "Winter"
    ),
    us_federal_holiday = !is.na(holiday_match),
    holiday_name = holidays$holiday_name[holiday_match]
  )
}

#' Add daily weather variables to standardized trips
#'
#' Downloads or reuses daily ASOS weather for each represented system and
#' joins it to trips by system and local trip date.
#'
#' @param trips Standardized trips containing `system` and `started_at`.
#' @param cache_dir Directory for one weather cache per system.
#'
#' @return Trips with daily weather columns appended.
#'
#' @details
#' Default airport stations are DCA for Capital Bikeshare, LGA for Citi Bike,
#' ORD for Divvy, and SFO for Bay Wheels. Weather is a metro-level daily
#' approximation, not station-level or route-level exposure.
#'
#' @export
add_weather_variables <- function(
  trips,
  cache_dir = file.path("data", "weather")
) {
  required <- c("system", "started_at")
  if (!is.data.frame(trips) || !all(required %in% names(trips))) {
    stop(
      "`trips` must contain `system` and `started_at` columns.",
      call. = FALSE
    )
  }

  if (!"trip_date" %in% names(trips)) {
    trips <- add_calendar_variables(trips)
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  systems <- unique(trips$system[!is.na(trips$system)])
  weather <- purrr::map_dfr(seq_along(systems), function(index) {
    system <- systems[[index]]
    system <- .match_system(system)
    metadata <- .system_metadata[.system_metadata$system == system, ]
    dates <- trips$trip_date[trips$system == system]
    dates <- dates[!is.na(dates)]
    if (!length(dates)) {
      return(tibble::tibble())
    }

    daily <- download_weather_data(
      start_date = min(dates),
      end_date = max(dates),
      cache_file = file.path(cache_dir, paste0(system, "-weather.csv")),
      station = metadata$weather_station,
      system = system,
      timezone = metadata$timezone
    )
    if (index < length(systems)) {
      Sys.sleep(1)
    }

    tibble::tibble(
      system = system,
      trip_date = daily$date,
      weather_station = metadata$weather_station,
      weather_temp_f = daily$Temp,
      weather_wind_mph = daily$Wind,
      weather_humidity_pct = daily$Humidity,
      weather_barometer_inhg = daily$Barometer,
      weather_visibility_miles = daily$Visibility,
      weather_precipitation_in = daily$Precipitation,
      weather_conditions = daily$Weather
    )
  })

  dplyr::left_join(trips, weather, by = c("system", "trip_date"))
}

#' Build a standardized multi-city trip dataset
#'
#' Downloads, standardizes, combines, and optionally enriches trips from any
#' subset of the four supported systems.
#'
#' @param systems Character vector of system identifiers.
#' @param start_date First trip date to include.
#' @param end_date Last trip date to include.
#' @param data_dir Parent directory for system-specific downloads.
#' @param overwrite Whether to redownload existing archives.
#' @param n_max Maximum rows to read from each extracted CSV.
#' @param calendar Whether to add calendar variables.
#' @param weather Whether to add daily metro weather variables.
#' @param weather_dir Directory for weather caches.
#' @param output_file Optional combined CSV output path.
#'
#' @return A combined standardized trip-level tibble.
#'
#' @examples
#' \dontrun{
#' trips <- build_multicity_data(
#'   systems = c("capital", "divvy"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-31",
#'   data_dir = "data/multicity",
#'   n_max = 1000,
#'   calendar = TRUE,
#'   weather = TRUE
#' )
#' }
#' @export
build_multicity_data <- function(
  systems = c("capital", "citibike", "divvy", "baywheels"),
  start_date,
  end_date,
  data_dir = file.path("data", "multicity"),
  overwrite = FALSE,
  n_max = Inf,
  calendar = TRUE,
  weather = FALSE,
  weather_dir = file.path(data_dir, "weather"),
  output_file = NULL
) {
  systems <- unique(vapply(systems, .match_system, character(1)))
  start_date <- .as_date(start_date, "start_date")
  end_date <- .as_date(end_date, "end_date")
  if (start_date > end_date) {
    stop("`start_date` must not be later than `end_date`.", call. = FALSE)
  }

  result <- purrr::map_dfr(systems, function(system) {
    destination <- file.path(data_dir, system)
    paths <- download_trip_files(
      start_date = start_date,
      end_date = end_date,
      destination = destination,
      overwrite = overwrite,
      system = system
    )
    trips <- load_trip_data(
      paths,
      system = system,
      start_date = start_date,
      end_date = end_date,
      n_max = n_max
    )
    if (calendar) {
      trips <- add_calendar_variables(trips)
    }
    trips
  })

  if (weather) {
    result <- add_weather_variables(result, cache_dir = weather_dir)
  }
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(result, output_file)
  }
  result
}
