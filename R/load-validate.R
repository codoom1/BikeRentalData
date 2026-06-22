#' Load processed bike-rental data
#'
#' Reads a processed CSV, standardizes data types, and validates its structure.
#'
#' @param path Path to a processed bike-rental CSV.
#' @param validate Whether to validate the loaded data.
#'
#' @return A tibble ready for analysis.
#' @export
load_bike_rentals <- function(path, validate = TRUE) {
  if (!file.exists(path)) {
    stop("Data file not found: ", path, call. = FALSE)
  }

  data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  unnamed <- names(data) %in% c("", "...1", "X")
  if (any(unnamed) && !"instant" %in% names(data)) {
    names(data)[which(unnamed)[[1]]] <- "instant"
  }

  if ("date" %in% names(data)) {
    data$date <- as.Date(data$date)
  }

  integer_columns <- intersect(
    c(
      "instant", "registered", "casual", "total_rentals", "Year", "Month",
      "day", "Weekday", "holiday", "workinday"
    ),
    names(data)
  )
  data[integer_columns] <- lapply(data[integer_columns], as.integer)

  if (validate) {
    validate_bike_rentals(data)
  }

  data
}

#' Validate processed bike-rental data
#'
#' Checks required columns, unique and continuous dates, nonnegative rental
#' counts, total-count arithmetic, and calendar ranges.
#'
#' @param data A processed bike-rental data frame.
#' @param start_date Optional expected first date.
#' @param end_date Optional expected last date.
#'
#' @return Invisibly returns `TRUE`; otherwise throws an informative error.
#' @export
validate_bike_rentals <- function(
  data,
  start_date = NULL,
  end_date = NULL
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  missing_columns <- setdiff(.required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  dates <- as.Date(data$date)
  if (anyNA(dates)) {
    stop("`date` contains missing or invalid values.", call. = FALSE)
  }
  if (anyDuplicated(dates)) {
    stop("`date` contains duplicate days.", call. = FALSE)
  }

  expected_dates <- seq(min(dates), max(dates), by = "day")
  if (length(dates) != length(expected_dates) || any(dates != expected_dates)) {
    stop("Dates must be sorted and continuous with one row per day.", call. = FALSE)
  }

  if (!is.null(start_date) && min(dates) != as.Date(start_date)) {
    stop("The first date does not match `start_date`.", call. = FALSE)
  }
  if (!is.null(end_date) && max(dates) != as.Date(end_date)) {
    stop("The last date does not match `end_date`.", call. = FALSE)
  }

  count_columns <- c("registered", "casual", "total_rentals")
  if (any(vapply(data[count_columns], anyNA, logical(1)))) {
    stop("Rental counts contain missing values.", call. = FALSE)
  }
  if (any(as.matrix(data[count_columns]) < 0)) {
    stop("Rental counts must be nonnegative.", call. = FALSE)
  }
  if (any(data$total_rentals != data$registered + data$casual)) {
    stop(
      "`total_rentals` must equal `registered + casual`.",
      call. = FALSE
    )
  }

  if (any(!data$Month %in% 1:12)) {
    stop("`Month` must be between 1 and 12.", call. = FALSE)
  }
  if (any(!data$Weekday %in% 0:1)) {
    stop("`Weekday` must contain only 0 and 1.", call. = FALSE)
  }
  if (any(!data$holiday %in% 0:1) || any(!data$workinday %in% 0:1)) {
    stop("Holiday and working-day indicators must contain only 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Bike-rental data dictionary
#'
#' @return A tibble describing the processed dataset variables.
#' @export
data_dictionary <- function() {
  tibble::tribble(
    ~variable, ~type, ~description,
    "instant", "integer", "Sequential daily record identifier",
    "date", "Date", "Observation date",
    "registered", "integer", "Registered or member rental count",
    "casual", "integer", "Casual rental count",
    "total_rentals", "integer", "Registered plus casual rentals",
    "latitude", "double", "Daily mean trip-start latitude",
    "longitude", "double", "Daily mean trip-start longitude",
    "Temp", "double", "Daily mean temperature in degrees Fahrenheit",
    "Wind", "double", "Daily mean wind speed in miles per hour",
    "Humidity", "double", "Daily mean relative humidity percentage",
    "Barometer", "double", "Daily mean altimeter pressure in inches of mercury",
    "Visibility", "double", "Daily mean visibility in miles",
    "Weather", "character", "Most frequent daily weather description",
    "Year", "integer", "Calendar year",
    "Month", "integer", "Calendar month",
    "day", "integer", "Day of month",
    "Weekday", "integer", "One for Monday-Friday; zero for weekend",
    "season", "character", "Meteorological season",
    "holiday", "integer", "District holiday indicator",
    "workinday", "integer", "One when weekday and not a holiday"
  )
}
