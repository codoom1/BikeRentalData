.read_monthly_trips <- function(path, start_date, end_date) {
  message("Reading ", basename(path))
  # Monthly source schemas contain mixed station-ID formats. Read raw values
  # as text and explicitly convert only the fields used by this package.
  trips <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  start_time_column <- .find_column(
    trips,
    c("started_at", "start_date", "start_time"),
    "trip start time"
  )
  latitude_column <- .find_column(
    trips,
    c("start_lat", "start_latitude", "latitude"),
    "start latitude"
  )
  longitude_column <- .find_column(
    trips,
    c("start_lng", "start_lon", "start_longitude", "longitude"),
    "start longitude"
  )
  membership_column <- .find_column(
    trips,
    c("member_casual", "member_type", "membership", "user_type"),
    "membership type"
  )

  membership <- tolower(trimws(as.character(trips[[membership_column]])))

  tibble::tibble(
    date = as.Date(.parse_trip_datetime(trips[[start_time_column]])),
    latitude = suppressWarnings(as.numeric(trips[[latitude_column]])),
    longitude = suppressWarnings(as.numeric(trips[[longitude_column]])),
    membership = dplyr::case_when(
      membership %in% c("member", "registered", "subscriber") ~ "member",
      membership %in% c("casual", "customer") ~ "casual",
      TRUE ~ membership
    )
  ) |>
    dplyr::filter(
      !is.na(date),
      date >= start_date,
      date <= end_date
    )
}

.aggregate_daily_trips <- function(trips) {
  unknown <- setdiff(unique(trips$membership), c("member", "casual"))
  unknown <- unknown[!is.na(unknown)]
  if (length(unknown) > 0L) {
    warning(
      "Unrecognized membership values were excluded: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  trips |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      registered = sum(membership == "member", na.rm = TRUE),
      casual = sum(membership == "casual", na.rm = TRUE),
      total_rentals = registered + casual,
      latitude = mean(latitude, na.rm = TRUE),
      longitude = mean(longitude, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(date)
}

#' Prepare daily bike-rental analysis data
#'
#' Aggregates monthly trip files, joins weather records by date, and adds
#' calendar fields used by the published analysis.
#'
#' @param trip_directory Directory containing extracted monthly trip CSVs.
#' @param weather A data frame or path to a weather CSV.
#' @param start_date First analysis date.
#' @param end_date Last analysis date.
#' @param holidays Optional holiday dates.
#' @param output_file Optional path at which to save the processed CSV.
#'
#' @return A validated tibble with one row per day.
#' @export
prepare_bike_rentals <- function(
  trip_directory,
  weather,
  start_date,
  end_date,
  holidays = .district_holidays,
  output_file = NULL
) {
  start_date <- .as_date(start_date, "start_date")
  end_date <- .as_date(end_date, "end_date")
  months <- .month_sequence(start_date, end_date)
  expected_paths <- file.path(
    trip_directory,
    paste0(
      format(months, "%Y%m"),
      "-capitalbikeshare-tripdata.csv"
    )
  )

  missing_paths <- expected_paths[!file.exists(expected_paths)]
  if (length(missing_paths) > 0L) {
    stop(
      "Missing ", length(missing_paths), " monthly trip file(s). First: ",
      missing_paths[[1]],
      call. = FALSE
    )
  }

  trips <- purrr::map_dfr(
    expected_paths,
    .read_monthly_trips,
    start_date = start_date,
    end_date = end_date
  )
  daily_trips <- .aggregate_daily_trips(trips)

  if (is.character(weather) && length(weather) == 1L) {
    weather <- readr::read_csv(weather, show_col_types = FALSE)
  }
  if (!is.data.frame(weather)) {
    stop("`weather` must be a data frame or CSV path.", call. = FALSE)
  }

  required_weather <- c(
    "date", "Temp", "Wind", "Humidity", "Barometer", "Visibility", "Weather"
  )
  missing_weather_columns <- setdiff(required_weather, names(weather))
  if (length(missing_weather_columns) > 0L) {
    stop(
      "Weather data are missing columns: ",
      paste(missing_weather_columns, collapse = ", "),
      call. = FALSE
    )
  }

  weather$date <- as.Date(weather$date)
  date_sequence <- seq(start_date, end_date, by = "day")

  result <- daily_trips |>
    dplyr::inner_join(weather, by = "date") |>
    dplyr::filter(date >= start_date, date <= end_date) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      Year = lubridate::year(date),
      Month = lubridate::month(date),
      day = lubridate::day(date),
      Weekday = as.integer(
        !lubridate::wday(date, week_start = 1) %in% c(6, 7)
      ),
      season = dplyr::case_when(
        Month %in% 3:5 ~ "Spring",
        Month %in% 6:8 ~ "Summer",
        Month %in% 9:11 ~ "Autumn",
        TRUE ~ "Winter"
      ),
      holiday = as.integer(date %in% as.Date(holidays)),
      workinday = as.integer(Weekday == 1L & holiday == 0L),
      instant = dplyr::row_number()
    ) |>
    dplyr::select(dplyr::all_of(.required_columns))

  validate_bike_rentals(
    result,
    start_date = start_date,
    end_date = end_date
  )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(result, output_file)
  }

  result
}

#' Download and build bike-rental analysis data
#'
#' Convenience wrapper around [download_trip_files()],
#' [download_weather_data()], and [prepare_bike_rentals()].
#'
#' @param start_date First analysis date.
#' @param end_date Last analysis date.
#' @param raw_dir Directory for monthly trip files.
#' @param weather_cache CSV path used to cache weather records.
#' @param output_file Optional processed CSV output path.
#' @param overwrite Whether to redownload existing trip files.
#'
#' @return A validated daily bike-rental tibble.
#' @export
build_bike_rental_data <- function(
  start_date = "2020-04-01",
  end_date = "2023-05-31",
  raw_dir = file.path("data", "raw"),
  weather_cache = file.path("data", "processed", "weather_data.csv"),
  output_file = file.path("data", "paperbike_data.csv"),
  overwrite = FALSE
) {
  download_trip_files(
    start_date = start_date,
    end_date = end_date,
    destination = raw_dir,
    overwrite = overwrite
  )
  weather <- download_weather_data(
    start_date = start_date,
    end_date = end_date,
    cache_file = weather_cache
  )
  prepare_bike_rentals(
    trip_directory = raw_dir,
    weather = weather,
    start_date = start_date,
    end_date = end_date,
    output_file = output_file
  )
}
