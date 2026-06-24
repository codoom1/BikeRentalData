# bikerentaldata

`bikerentaldata` downloads and standardizes official historical trip data
from four U.S. bike-share systems:

| System ID | System | Area |
|---|---|---|
| `"capital"` | Capital Bikeshare | Washington, D.C. region |
| `"citibike"` | Citi Bike | New York City |
| `"divvy"` | Divvy | Chicago |
| `"baywheels"` | Bay Wheels | San Francisco Bay Area |

The package returns a common trip-level structure containing bike type,
member/casual rider type, trip duration, stations, and coordinates when those
fields are available in the source data.

## Installation

Install the current GitHub release:

```r
install.packages("pak")
pak::pak("codoom1/BikeRentalData")
```

Load and verify it:

```r
library(bikerentaldata)
packageVersion("bikerentaldata")
```

Update an existing installation:

```r
pak::pak("codoom1/BikeRentalData", upgrade = TRUE)
```

If you are working inside an `renv` project, use `renv::restore()` to install
the version recorded by that project.

## Five-minute quick start

First, see the supported systems:

```r
library(bikerentaldata)

available_systems()
```

Check which archives are available before downloading:

```r
archives <- available_trip_data("divvy")

head(archives)
range(archives$start_date)
range(archives$end_date)
```

Download one month:

```r
paths <- download_trip_files(
  system = "divvy",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  destination = "data/divvy"
)
```

Load and standardize the trips:

```r
trips <- load_trip_data(
  paths,
  system = "divvy"
)

head(trips)
dplyr::count(trips, bike_type, rider_type)
summary(trips$duration_minutes)
```

View definitions for every standardized field:

```r
trip_data_dictionary()
```

## Standardized output

`load_trip_data()` returns:

- system name and city;
- ride ID and bike type;
- start/end timestamps and duration;
- origin/destination station IDs and names;
- start/end coordinates; and
- standardized rider type: `"member"` or `"casual"`.

Legacy source files do not always contain every field. Unavailable bike types,
coordinates, or ride IDs are returned as `NA`.

## Build several cities in one call

```r
multicity <- build_multicity_data(
  systems = c("capital", "citibike", "divvy", "baywheels"),
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  data_dir = "data/multicity",
  calendar = TRUE,
  weather = FALSE
)
```

For a quick trial, limit rows read from each extracted CSV:

```r
multicity_sample <- build_multicity_data(
  systems = c("capital", "divvy"),
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  data_dir = "data/multicity",
  n_max = 1000
)
```

Set `output_file = "data/multicity.csv"` to save the combined result.

## Calendar support

Set `calendar = TRUE` in `build_multicity_data()`, or run:

```r
trips <- add_calendar_variables(trips)
```

Calendar enrichment includes:

- local trip date, year, month, day, and weekday;
- weekend indicator;
- local trip-start hour and time-of-day category;
- meteorological season; and
- observed U.S. federal holiday indicator and name.

Federal holidays are supported across all four systems. City/state holidays,
school calendars, strikes, and special events are not currently included.

## Weather support

Set `weather = TRUE` in `build_multicity_data()`, or run:

```r
trips <- add_weather_variables(
  trips,
  cache_dir = "data/weather"
)
```

The package downloads daily ASOS observations from the Iowa Environmental
Mesonet using these defaults:

| System | Weather station | Time zone |
|---|---|---|
| Capital Bikeshare | DCA | America/New_York |
| Citi Bike | LGA | America/New_York |
| Divvy | ORD | America/Chicago |
| Bay Wheels | SFO | America/Los_Angeles |

Weather enrichment includes daily mean temperature, wind speed, humidity,
altimeter pressure, visibility, reported precipitation, and the most frequent
weather conditions.

These are metro-level airport observations—not exact weather along each
route. Weather is currently daily rather than trip-hour specific.

## Bike infrastructure exposure

If you have a city bike-lane or trail layer as an `sf` line object, add
station-area exposure measures:

```r
install.packages("sf") # only needed for spatial infrastructure exposure

bike_lanes <- download_bike_infrastructure("capital")

trips <- add_bike_infrastructure_exposure(
  trips,
  infrastructure = bike_lanes,
  buffers_m = c(250, 500),
  cache_file = "data/cache/capital_station_exposure.csv"
)
```

For large monthly or yearly studies, the recommended workflow is to cache the
station-level exposure table once, then join it to every trip file:

```r
station_exposure <- build_station_infrastructure_exposure(
  trips,
  infrastructure = bike_lanes,
  buffers_m = c(250, 500),
  cache_file = "data/cache/capital_station_exposure.csv"
)

trips <- add_station_infrastructure_exposure(
  trips,
  station_exposure
)

summarize_station_infrastructure_coverage(
  trips,
  station_exposure
)
```

This avoids recalculating the same station geometry for every month. It also
creates an auditable station-level table for methods sections and
reproducibility.

Supported download shortcuts are:

```r
download_bike_infrastructure("capital")   # DC, Arlington, Alexandria, Montgomery
download_bike_infrastructure("divvy")     # Chicago
download_bike_infrastructure("citibike")  # New York City
download_bike_infrastructure("baywheels") # nine-county San Francisco Bay Area
```

Check the exact source and coverage before analysis:

```r
available_bike_infrastructure_sources()
infrastructure_data_dictionary()
```

### Faster Citi Bike / large-city workflow

Citi Bike is slower because the NYC infrastructure layer has many features.
The first exposure build must do real spatial geometry work. After that, reuse
the station-level cache:

```r
bike_lanes <- download_bike_infrastructure(
  "citibike",
  destination = "data/spatial"
)

station_exposure <- build_station_infrastructure_exposure(
  trips,
  infrastructure = bike_lanes,
  buffers_m = c(250, 500),
  cache_file = "data/cache/citibike_station_exposure_250_500.csv"
)

trips <- add_station_infrastructure_exposure(
  trips,
  station_exposure
)
```

For later monthly files, skip the spatial recalculation and reuse the same
cache:

```r
station_exposure <- readr::read_csv(
  "data/cache/citibike_station_exposure_250_500.csv",
  show_col_types = FALSE
)

trips <- add_station_infrastructure_exposure(
  trips,
  station_exposure
)
```

If you only need origin exposure, use `sides = "start"` to cut the spatial
work roughly in half:

```r
station_exposure <- build_station_infrastructure_exposure(
  trips,
  infrastructure = bike_lanes,
  buffers_m = c(250, 500),
  sides = "start",
  cache_file = "data/cache/citibike_start_exposure_250_500.csv"
)
```

This creates variables such as:

- `start_bikeinfra_250m_m` and `end_bikeinfra_250m_m`;
- `start_protected_bikeinfra_500m_m`;
- `start_trail_500m_m`;
- `start_any_protected_500m`; and
- `start_nearest_bikeinfra_m`.

These are station-area exposure variables. They should be interpreted as bike
infrastructure availability near the start or end station, not proof that a
rider used a specific bike lane or trail.

Capital Bikeshare and Bay Wheels are regional systems. The Capital default now
combines official layers for Washington, DC, Arlington, Alexandria, and
Montgomery County. Prince George's County is not included yet because a clearly
official existing bicycle-infrastructure line layer still needs to be selected.
The Bay Wheels default uses MTC's regional bike-facilities layer, which covers
the nine-county San Francisco Bay Area, including San Francisco, Oakland,
Berkeley, and San José.

## Working with another system

Only the `system` argument and destination need to change:

```r
capital_paths <- download_trip_files(
  system = "capital",
  start_date = "2020-04-01",
  end_date = "2020-04-30",
  destination = "data/capital"
)

capital_trips <- load_trip_data(
  capital_paths,
  system = "capital"
)
```

Valid system identifiers are `"capital"`, `"citibike"`, `"divvy"`, and
`"baywheels"`.

## Download-size warning

Always inspect `available_trip_data()` before downloading. Some historical
archives are large. In particular, Citi Bike's 2013–2023 archives are annual;
its 2024+ archives are monthly.

For a quick trial, request one recent month and use `n_max`:

```r
sample_trips <- load_trip_data(
  paths,
  system = "divvy",
  n_max = 1000
)
```

Downloaded trip files are third-party data and should normally remain outside
version control.

## Capital Bikeshare paper workflow

The package can also rebuild daily Capital Bikeshare analysis data:

```r
bike_data <- build_bike_rental_data(
  start_date = "2020-04-01",
  end_date = "2023-05-31",
  raw_dir = "data/raw",
  weather_cache = "data/processed/weather_data.csv",
  output_file = "data/paperbike_data.csv"
)
```

Fresh builds use Washington National Airport ASOS weather observations from
the Iowa Environmental Mesonet. The published paper dataset used Time and
Date weather records, so fresh weather summaries may differ slightly.

Associated publication:

> Odoom, C., Boateng, A., Fobi Mensah, S., & Maposa, D. (2024). Modeling of
> the daily dynamics in bike rental system using weather and calendar
> conditions: A semi-parametric approach. *Scientific African, 24*, e02211.
> <https://doi.org/10.1016/j.sciaf.2024.e02211>

## Additional information

```r
?bikerentaldata
current_system_info()
summarize_trip_locations("data/capital", system = "capital")
```

Function help is available through commands such as:

```r
?download_trip_files
?load_trip_data
?standardize_trips
```

## Data terms

The package contains processing code, not the complete third-party datasets.
Each operator's data remain governed by its own license or data-use policy.
Review [DATA_LICENSE.md](DATA_LICENSE.md) before downloading, publishing, or
redistributing data.

## For package developers

Clone the repository and install the local source:

```r
install.packages(
  "/path/to/BikeRentalData",
  repos = NULL,
  type = "source"
)
```

Run package tests and checks:

```r
testthat::test_local()
devtools::check()
```
