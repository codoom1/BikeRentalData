.system_metadata <- tibble::tribble(
  ~system, ~name, ~city, ~archive_endpoint, ~system_data_url,
  "capital", "Capital Bikeshare", "Washington, DC",
  "https://capitalbikeshare-data.s3.amazonaws.com/?list-type=2&max-keys=1000",
  "https://capitalbikeshare.com/system-data",
  "citibike", "Citi Bike", "New York City",
  "https://s3.amazonaws.com/tripdata?list-type=2&max-keys=1000",
  "https://citibikenyc.com/system-data",
  "divvy", "Divvy", "Chicago",
  "https://divvy-tripdata.s3.amazonaws.com/?list-type=2&max-keys=1000",
  "https://divvybikes.com/system-data",
  "baywheels", "Bay Wheels", "San Francisco Bay Area",
  "https://s3.amazonaws.com/baywheels-data?list-type=2&max-keys=1000",
  "https://www.lyft.com/bikes/bay-wheels/system-data"
)

.match_system <- function(system) {
  aliases <- c(
    capital = "capital",
    capitalbikeshare = "capital",
    cabi = "capital",
    citibike = "citibike",
    citi = "citibike",
    divvy = "divvy",
    baywheels = "baywheels",
    bay_wheels = "baywheels",
    fordgo = "baywheels",
    fordgobike = "baywheels"
  )
  key <- gsub("[^a-z0-9_]+", "", tolower(system[[1]]))
  matched <- unname(aliases[key])
  if (is.na(matched)) {
    stop(
      "Unknown system. Choose one of: capital, citibike, divvy, baywheels.",
      call. = FALSE
    )
  }
  matched
}

#' List supported historical bike-share systems
#'
#' Returns the identifiers accepted by the `system` argument in
#' [available_trip_data()], [download_trip_files()], [load_trip_data()], and
#' [standardize_trips()].
#'
#' @return A tibble with system identifiers, names, cities, and official data
#'   pages.
#'
#' @examples
#' available_systems()
#' @export
available_systems <- function() {
  .system_metadata |>
    dplyr::select(system, name, city, system_data_url)
}

.list_archive_keys <- function(system) {
  system <- .match_system(system)
  endpoint <- .system_metadata$archive_endpoint[
    .system_metadata$system == system
  ]
  xml <- paste(readLines(endpoint, warn = FALSE), collapse = "")
  matches <- regmatches(
    xml,
    gregexpr("<Key>[^<]+</Key>", xml)
  )[[1]]
  if (length(matches) == 0L || identical(matches, "")) {
    stop("No archive files were found for ", system, ".", call. = FALSE)
  }
  gsub("</?Key>", "", matches)
}

.archive_url <- function(system, key) {
  switch(
    .match_system(system),
    capital = paste0("https://capitalbikeshare-data.s3.amazonaws.com/", key),
    citibike = paste0("https://s3.amazonaws.com/tripdata/", key),
    divvy = paste0("https://divvy-tripdata.s3.amazonaws.com/", key),
    baywheels = paste0("https://s3.amazonaws.com/baywheels-data/", key)
  )
}

.parse_archive_period <- function(system, keys) {
  system <- .match_system(system)
  result <- vector("list", length(keys))

  for (index in seq_along(keys)) {
    key <- keys[[index]]
    year <- month <- quarter_start <- quarter_end <- NA_integer_
    format <- NA_character_

    if (system == "capital") {
      id <- sub("-capitalbikeshare-tripdata[.]zip$", "", key)
      if (grepl("^[0-9]{4}$", id)) {
        year <- as.integer(id)
        format <- "annual"
      } else if (grepl("^[0-9]{6}$", id)) {
        year <- as.integer(substr(id, 1, 4))
        month <- as.integer(substr(id, 5, 6))
        format <- "monthly"
      }
    } else if (system == "citibike") {
      if (!grepl("^[0-9]{4,6}-citibike-tripdata[.]zip$", key)) next
      id <- sub("-citibike-tripdata[.]zip$", "", key)
      year <- as.integer(substr(id, 1, 4))
      if (nchar(id) == 4L) {
        format <- "annual"
      } else {
        month <- as.integer(substr(id, 5, 6))
        format <- "monthly"
      }
    } else if (system == "baywheels") {
      id <- sub("-(fordgobike|baywhee+ls)-tripdata([.]csv)?[.]zip$", "", key)
      if (grepl("^[0-9]{4}$", id)) {
        year <- as.integer(id)
        format <- "annual"
      } else if (grepl("^[0-9]{6}$", id)) {
        year <- as.integer(substr(id, 1, 4))
        month <- as.integer(substr(id, 5, 6))
        format <- "monthly"
      }
    } else if (system == "divvy") {
      if (grepl("^[0-9]{6}-divvy-tripdata[.]zip$", key)) {
        id <- substr(key, 1, 6)
        year <- as.integer(substr(id, 1, 4))
        month <- as.integer(substr(id, 5, 6))
        format <- "monthly"
      } else {
        year_match <- regmatches(key, regexpr("20[0-9]{2}", key))
        if (!length(year_match) || identical(year_match, "")) next
        year <- as.integer(year_match)
        quarter_matches <- regmatches(
          key,
          gregexpr("Q[1-4]", key)
        )[[1]]
        quarters <- as.integer(sub("Q", "", quarter_matches))
        quarters <- quarters[!is.na(quarters)]
        if (length(quarters)) {
          quarter_start <- min(quarters)
          quarter_end <- max(quarters)
          format <- "quarterly"
        } else {
          format <- "annual"
        }
      }
    }

    if (is.na(format)) next
    start_date <- if (!is.na(month)) {
      as.Date(sprintf("%04d-%02d-01", year, month))
    } else if (!is.na(quarter_start)) {
      as.Date(sprintf("%04d-%02d-01", year, (quarter_start - 1L) * 3L + 1L))
    } else {
      as.Date(sprintf("%04d-01-01", year))
    }
    end_date <- if (!is.na(month)) {
      lubridate::ceiling_date(start_date, "month") - 1
    } else if (!is.na(quarter_end)) {
      lubridate::ceiling_date(
        as.Date(sprintf("%04d-%02d-01", year, quarter_end * 3L)),
        "month"
      ) - 1
    } else {
      as.Date(sprintf("%04d-12-31", year))
    }
    if (system == "capital" && year == 2010L) {
      start_date <- as.Date("2010-09-20")
    }

    result[[index]] <- tibble::tibble(
      system = system,
      archive = key,
      format = format,
      start_date = start_date,
      end_date = end_date,
      url = .archive_url(system, key)
    )
  }

  dplyr::bind_rows(result) |>
    dplyr::arrange(start_date, archive)
}
