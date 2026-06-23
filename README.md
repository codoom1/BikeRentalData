# bikerentaldata

`bikerentaldata` is an R package for downloading and standardizing historical
trip data from:

- Capital Bikeshare — Washington, D.C.
- Citi Bike — New York City
- Divvy — Chicago
- Bay Wheels — San Francisco Bay Area

It also prepares the daily Capital Bikeshare dataset used in:

> Odoom, C., Boateng, A., Fobi Mensah, S., & Maposa, D. (2024). Modeling of
> the daily dynamics in bike rental system using weather and calendar
> conditions: A semi-parametric approach. *Scientific African, 24*, e02211.
> <https://doi.org/10.1016/j.sciaf.2024.e02211>

The package contains processing code—not the full third-party dataset.

## Explore available data

```r
library(bikerentaldata)

# Open the package overview in R
?bikerentaldata

# Supported systems
available_systems()

# Official archive coverage for one system
archives <- available_trip_data("citibike")
range(archives$start_date)
range(archives$end_date)

# Current live system footprint
current_system_info()

# Historical station locations represented in downloaded files
summarize_trip_locations("data/raw")
summarize_trip_locations("data/raw", by = "file")
```

## Multi-system trip workflow

```r
paths <- download_trip_files(
  system = "divvy",
  start_date = "2024-01-01",
  end_date = "2024-03-31",
  destination = "data/divvy"
)

trips <- load_trip_data(
  paths,
  system = "divvy"
)

dplyr::count(trips, bike_type, rider_type)
```

The standardized result includes system, city, ride ID, bike type, start/end
times, duration, station identifiers and names, coordinates, and member/casual
rider type. Fields unavailable in legacy source files are returned as `NA`.
Use `trip_data_dictionary()` for full field definitions.

Note that Citi Bike's official 2013–2023 archives are annual and can be large;
its 2024+ archives are monthly. Check `available_trip_data("citibike")` before
downloading.

## Install

```r
install.packages("remotes")
remotes::install_github("codoom1/BikeRentalData")
```

For local development:

```r
install.packages(
  "/path/to/BikeRentalData",
  repos = NULL,
  type = "source"
)
```

## Build analysis-ready data

```r
library(bikerentaldata)

bike_data <- build_bike_rental_data(
  start_date = "2020-04-01",
  end_date = "2023-05-31",
  raw_dir = "data/raw",
  weather_cache = "data/processed/weather_data.csv",
  output_file = "data/paperbike_data.csv"
)
```

This workflow:

1. downloads official annual or monthly Capital Bikeshare ZIP archives;
2. extracts and reads their trip CSV files;
3. retrieves or reuses cached Washington National Airport ASOS observations
   from the Iowa Environmental Mesonet;
4. aggregates registered, casual, and total rentals by date;
5. joins trip and weather records by date;
6. adds season, holiday, weekday, and working-day variables; and
7. validates and optionally saves the result.

The published repository's bundled processed dataset used historical weather
records from Time and Date. Fresh package builds use the stable IEM ASOS
archive and may therefore produce slightly different weather summaries. To
reproduce a specific historical build, pass its cached weather CSV directly
to `prepare_bike_rentals()`.

The lower-level functions `available_systems()`, `available_trip_data()`,
`download_trip_files()`, `load_trip_data()`, `standardize_trips()`,
`download_weather_data()`, `prepare_bike_rentals()`,
`load_bike_rentals()`, `validate_bike_rentals()`,
`current_system_info()`, `summarize_trip_locations()`, and
`data_dictionary()` can be used independently.

Legacy schemas vary by system. The package preserves available fields and
returns `NA` where bike type, coordinates, or ride IDs were not published.

## Data terms

Each operator's data remain governed by its own license or data-use policy.
The package does not grant rights in any operator's trip data or weather data.
Review [DATA_LICENSE.md](DATA_LICENSE.md) before use.
