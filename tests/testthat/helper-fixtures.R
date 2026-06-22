make_processed_fixture <- function() {
  dates <- as.Date("2023-01-01") + 0:2
  data.frame(
    instant = 1:3,
    date = dates,
    registered = c(10L, 20L, 30L),
    casual = c(5L, 7L, 9L),
    total_rentals = c(15L, 27L, 39L),
    latitude = rep(38.9, 3),
    longitude = rep(-77.0, 3),
    Temp = rep(50, 3),
    Wind = rep(5, 3),
    Humidity = rep(60, 3),
    Barometer = rep(30, 3),
    Visibility = rep(10, 3),
    Weather = rep("Clear", 3),
    Year = rep(2023L, 3),
    Month = rep(1L, 3),
    day = 1:3,
    Weekday = c(0L, 1L, 1L),
    season = rep("Winter", 3),
    holiday = c(1L, 1L, 0L),
    workinday = c(0L, 0L, 1L)
  )
}
