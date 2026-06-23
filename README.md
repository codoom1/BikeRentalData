# bikerentaldata

`bikerentaldata` is an R package for downloading and preparing the daily
Capital Bikeshare dataset used in:

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

# Official archive coverage: annual files through 2017, monthly thereafter
archives <- available_trip_data()
range(archives$start_date)
range(archives$end_date)

# Current live system footprint
current_system_info()

# Historical station locations represented in downloaded files
summarize_trip_locations("data/raw")
summarize_trip_locations("data/raw", by = "file")
```

The official archive begins on September 20, 2010. Annual or quarterly legacy
files are used through 2017; monthly archives are used from January 2018
onward.

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

The lower-level functions `download_trip_files()`, `download_weather_data()`,
`prepare_bike_rentals()`, `load_bike_rentals()`,
`validate_bike_rentals()`, `available_trip_data()`,
`current_system_info()`, `summarize_trip_locations()`, and
`data_dictionary()` can be used independently.

Legacy files from 2010–2017 do not include station coordinates. The package
preserves their rental counts and station identifiers, but returns `NA` for
daily latitude and longitude. Coordinate-dependent analyses should use newer
files or add an external station-coordinate reference.

## Data terms

Capital Bikeshare data remain governed by the
[Capital Bikeshare Data License Agreement](https://capitalbikeshare.com/data-license-agreement).
The package does not grant rights in Capital Bikeshare or weather data.
Review [DATA_LICENSE.md](DATA_LICENSE.md) before use.
