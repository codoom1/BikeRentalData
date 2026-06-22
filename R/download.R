#' Download Capital Bikeshare monthly trip files
#'
#' Downloads official monthly ZIP archives and extracts one CSV per month.
#' Existing extracted files are reused unless `overwrite = TRUE`.
#'
#' @param start_date First date required.
#' @param end_date Last date required.
#' @param destination Directory for extracted monthly CSV files.
#' @param overwrite Whether to replace existing files.
#' @param keep_zip Whether to retain downloaded ZIP archives.
#'
#' @return A character vector containing extracted CSV paths.
#' @export
download_trip_files <- function(
  start_date,
  end_date,
  destination = file.path("data", "raw"),
  overwrite = FALSE,
  keep_zip = FALSE
) {
  months <- .month_sequence(start_date, end_date)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  archive_dir <- file.path(destination, "archives")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  output_paths <- character(length(months))

  for (index in seq_along(months)) {
    month_id <- format(months[[index]], "%Y%m")
    output_path <- file.path(
      destination,
      paste0(month_id, "-capitalbikeshare-tripdata.csv")
    )
    output_paths[[index]] <- output_path

    if (file.exists(output_path) && !overwrite) {
      message("Using existing ", output_path)
      next
    }

    archive_name <- paste0(month_id, "-capitalbikeshare-tripdata.zip")
    archive_url <- paste0(
      "https://capitalbikeshare-data.s3.amazonaws.com/",
      archive_name
    )
    archive_path <- file.path(archive_dir, archive_name)

    message("Downloading ", archive_url)
    utils::download.file(
      archive_url,
      archive_path,
      mode = "wb",
      quiet = TRUE
    )

    archive_contents <- utils::unzip(archive_path, list = TRUE)
    csv_names <- archive_contents$Name[
      grepl("\\.csv$", archive_contents$Name, ignore.case = TRUE)
    ]

    if (length(csv_names) == 0L) {
      stop("No CSV file found in ", archive_name, ".", call. = FALSE)
    }

    temp_dir <- tempfile("bikerentaldata-unzip-")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(archive_path, files = csv_names[[1]], exdir = temp_dir)
    extracted_path <- file.path(temp_dir, csv_names[[1]])

    if (!file.copy(extracted_path, output_path, overwrite = TRUE)) {
      stop("Could not create ", output_path, ".", call. = FALSE)
    }

    if (!keep_zip) {
      unlink(archive_path)
    }
  }

  normalizePath(output_paths, mustWork = TRUE)
}

#' Download daily historical weather observations
#'
#' Retrieves and aggregates Washington National Airport ASOS observations from
#' the Iowa Environmental Mesonet archive. Existing cache records are reused.
#'
#' @param start_date First date required.
#' @param end_date Last date required.
#' @param cache_file Optional CSV cache path.
#' @param station ASOS station identifier. Defaults to Washington National
#'   Airport (`"DCA"`).
#'
#' @return A tibble with one weather record per date.
#' @export
download_weather_data <- function(
  start_date,
  end_date,
  cache_file = file.path("data", "processed", "weather_data.csv"),
  station = "DCA"
) {
  start_date <- .as_date(start_date, "start_date")
  end_date <- .as_date(end_date, "end_date")
  dates <- seq(start_date, end_date, by = "day")

  if (!is.null(cache_file) && file.exists(cache_file)) {
    weather <- readr::read_csv(cache_file, show_col_types = FALSE)
    weather$date <- as.Date(weather$date)
  } else {
    weather <- tibble::tibble(
      date = as.Date(character()),
      Temp = double(),
      Wind = double(),
      Humidity = double(),
      Barometer = double(),
      Visibility = double(),
      Weather = character()
    )
  }

  missing_dates <- setdiff(dates, weather$date)
  if (length(missing_dates) > 0L) {
    new_weather <- .download_iem_weather(
      min(missing_dates),
      max(missing_dates),
      station = station
    ) |>
      dplyr::filter(date %in% missing_dates)

    weather <- dplyr::bind_rows(weather, new_weather) |>
      dplyr::distinct(date, .keep_all = TRUE) |>
      dplyr::arrange(date)
  }

  weather <- dplyr::filter(
    weather,
    date >= start_date,
    date <= end_date
  )

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(weather, cache_file)
  }

  weather
}

.download_iem_weather <- function(start_date, end_date, station) {
  message(
    "Retrieving ASOS weather for ", start_date, " through ", end_date
  )

  endpoint <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py"
  end_exclusive <- as.Date(end_date) + 1
  query <- c(
    paste0("station=", utils::URLencode(station, reserved = TRUE)),
    "data=tmpf",
    "data=relh",
    "data=sknt",
    "data=alti",
    "data=vsby",
    "data=wxcodes",
    "data=skyc1",
    paste0("year1=", format(start_date, "%Y")),
    paste0("month1=", as.integer(format(start_date, "%m"))),
    paste0("day1=", as.integer(format(start_date, "%d"))),
    "hour1=0",
    "minute1=0",
    paste0("year2=", format(end_exclusive, "%Y")),
    paste0("month2=", as.integer(format(end_exclusive, "%m"))),
    paste0("day2=", as.integer(format(end_exclusive, "%d"))),
    "hour2=0",
    "minute2=0",
    paste0(
      "tz=",
      utils::URLencode("America/New_York", reserved = TRUE)
    ),
    "format=onlycomma",
    "missing=empty",
    "trace=empty"
  )
  url <- paste0(endpoint, "?", paste(query, collapse = "&"))
  observations <- readr::read_csv(
    url,
    show_col_types = FALSE,
    na = c("", "M")
  )

  if (nrow(observations) == 0L) {
    stop("The IEM weather request returned no observations.", call. = FALSE)
  }

  sky_labels <- c(
    CLR = "Clear",
    SKC = "Clear",
    FEW = "Few clouds",
    SCT = "Scattered clouds",
    BKN = "Mostly cloudy",
    OVC = "Overcast",
    VV = "Low visibility"
  )

  observations |>
    dplyr::mutate(
      date = as.Date(valid),
      weather_text = dplyr::if_else(
        !is.na(wxcodes) & nzchar(trimws(wxcodes)),
        trimws(wxcodes),
        unname(sky_labels[skyc1]),
        missing = NA_character_
      )
    ) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      Temp = mean(tmpf, na.rm = TRUE),
      Wind = mean(sknt, na.rm = TRUE) * 1.15078,
      Humidity = mean(relh, na.rm = TRUE),
      Barometer = mean(alti, na.rm = TRUE),
      Visibility = mean(vsby, na.rm = TRUE),
      Weather = .daily_mode(weather_text),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(Temp, Wind, Humidity, Barometer, Visibility),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      )
    ) |>
    dplyr::filter(date >= start_date, date <= end_date)
}
