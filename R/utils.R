.required_columns <- c(
  "instant", "date", "registered", "casual", "total_rentals",
  "latitude", "longitude", "Temp", "Wind", "Humidity", "Barometer",
  "Visibility", "Weather", "Year", "Month", "day", "Weekday",
  "season", "holiday", "workinday"
)

.as_date <- function(x, name) {
  value <- as.Date(x)
  if (anyNA(value)) {
    stop("`", name, "` must contain valid dates.", call. = FALSE)
  }
  value
}

.month_sequence <- function(start_date, end_date) {
  start_date <- .as_date(start_date, "start_date")
  end_date <- .as_date(end_date, "end_date")
  if (start_date > end_date) {
    stop("`start_date` must not be later than `end_date`.", call. = FALSE)
  }

  seq(
    lubridate::floor_date(start_date, unit = "month"),
    lubridate::floor_date(end_date, unit = "month"),
    by = "month"
  )
}

.archive_plan <- function(start_date, end_date) {
  start_date <- .as_date(start_date, "start_date")
  end_date <- .as_date(end_date, "end_date")
  if (start_date > end_date) {
    stop("`start_date` must not be later than `end_date`.", call. = FALSE)
  }
  if (start_date < as.Date("2010-09-20")) {
    stop(
      "Capital Bikeshare trip data begin on 2010-09-20.",
      call. = FALSE
    )
  }

  years <- seq.int(
    as.integer(format(start_date, "%Y")),
    as.integer(format(end_date, "%Y"))
  )
  annual_years <- years[years <= 2017L]
  monthly_start <- max(start_date, as.Date("2018-01-01"))
  monthly <- if (monthly_start <= end_date) {
    .month_sequence(monthly_start, end_date)
  } else {
    as.Date(character())
  }

  dplyr::bind_rows(
    if (length(annual_years)) {
      tibble::tibble(
        archive_id = as.character(annual_years),
        format = "annual",
        start_date = as.Date(paste0(annual_years, "-01-01")),
        end_date = as.Date(paste0(annual_years, "-12-31"))
      )
    },
    if (length(monthly)) {
      tibble::tibble(
        archive_id = format(monthly, "%Y%m"),
        format = "monthly",
        start_date = monthly,
        end_date = lubridate::ceiling_date(monthly, "month") - 1
      )
    }
  ) |>
    dplyr::mutate(
      archive_name = paste0(
        archive_id,
        "-capitalbikeshare-tripdata.zip"
      ),
      url = paste0(
        "https://capitalbikeshare-data.s3.amazonaws.com/",
        archive_name
      )
    )
}

.trip_paths_for_range <- function(directory, start_date, end_date) {
  plan <- .archive_plan(start_date, end_date)
  paths <- character()

  for (index in seq_len(nrow(plan))) {
    archive_id <- plan$archive_id[[index]]
    if (plan$format[[index]] == "annual") {
      annual_paths <- list.files(
        directory,
        pattern = paste0(
          "^", archive_id,
          "(Q[1-4])?-capitalbikeshare-tripdata[.]csv$"
        ),
        full.names = TRUE
      )
      paths <- c(paths, sort(annual_paths))
    } else {
      paths <- c(
        paths,
        file.path(
          directory,
          paste0(archive_id, "-capitalbikeshare-tripdata.csv")
        )
      )
    }
  }

  unique(paths)
}

.find_optional_column <- function(data, candidates) {
  normalized_names <- gsub("[^a-z0-9]+", "_", tolower(names(data)))
  normalized_candidates <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(candidates)
  )
  indexes <- match(normalized_candidates, normalized_names, nomatch = 0L)
  indexes <- indexes[indexes > 0L]

  if (length(indexes) == 0L) {
    return(NULL)
  }

  names(data)[indexes[[1]]]
}

.find_column <- function(data, candidates, label) {
  normalized_names <- gsub("[^a-z0-9]+", "_", tolower(names(data)))
  normalized_candidates <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(candidates)
  )
  indexes <- match(normalized_candidates, normalized_names, nomatch = 0L)
  indexes <- indexes[indexes > 0L]

  if (length(indexes) == 0L) {
    stop(
      "Could not find the ", label, " column. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  names(data)[indexes[[1]]]
}

.parse_trip_datetime <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c(
      "ymd HMS", "ymd HM", "mdy HMS", "mdy HM",
      "ymd IMS p", "ymd IM p", "mdy IMS p", "mdy IM p"
    ),
    quiet = TRUE
  )
}

.daily_mode <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    return(NA_character_)
  }

  counts <- table(x)
  names(counts)[which.max(counts)]
}

.extract_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]+", "", as.character(x))))
}

.district_holidays <- as.Date(c(
  "2020-01-01", "2020-01-20", "2020-02-17", "2020-04-08",
  "2020-05-25", "2020-06-19", "2020-07-04", "2020-09-07",
  "2020-10-12", "2020-11-11", "2020-11-26", "2020-12-25",
  "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-16",
  "2021-05-31", "2021-06-18", "2021-07-04", "2021-09-06",
  "2021-10-11", "2021-11-11", "2021-11-25", "2021-12-25",
  "2022-01-01", "2022-01-17", "2022-02-21", "2022-04-15",
  "2022-05-30", "2022-06-19", "2022-07-04", "2022-09-05",
  "2022-10-10", "2022-11-11", "2022-11-24", "2022-12-25",
  "2023-01-01", "2023-01-02", "2023-01-16", "2023-02-20",
  "2023-04-17", "2023-05-29", "2023-06-19", "2023-07-04",
  "2023-09-04", "2023-10-09", "2023-11-10", "2023-11-23",
  "2023-12-25"
))
