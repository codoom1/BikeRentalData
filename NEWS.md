# bikerentaldata 0.4.3

- Added `build_station_infrastructure_exposure()` to compute and optionally
  cache reusable station-level infrastructure exposure tables.
- Added `add_station_infrastructure_exposure()` to join cached station-level
  exposure tables back to trip records.
- Updated `add_bike_infrastructure_exposure()` to use the station-level
  workflow internally while preserving the one-step interface.
- Added `summarize_station_infrastructure_coverage()` to audit station
  coordinate and station-exposure coverage.
- Documented planned regional infrastructure coverage for Capital Bikeshare
  and Bay Wheels beyond the current Washington, DC and San Francisco defaults.

# bikerentaldata 0.4.2

- Added `download_bike_infrastructure()` for official open-data bicycle
  infrastructure layers for Capital Bikeshare/DC, Divvy/Chicago, Citi
  Bike/NYC, and Bay Wheels/San Francisco.
- Added `available_bike_infrastructure_sources()` to document source URLs,
  facility-type columns, and current coverage notes.
- `add_bike_infrastructure_exposure()` now reuses facility metadata attached
  to layers downloaded by `download_bike_infrastructure()`.

# bikerentaldata 0.4.1

- Added `add_bike_infrastructure_exposure()` for station-area bike-lane,
  protected-facility, trail, and nearest-facility exposure variables from an
  `sf` line layer.
- Added bike-infrastructure exposure fields to `trip_data_dictionary()`.

# bikerentaldata 0.4.0

- Added `build_multicity_data()` to download and combine multiple systems.
- Added `add_calendar_variables()` with local date/time, weekend, season, and
  observed U.S. federal holiday fields.
- Added `add_weather_variables()` with city-specific daily ASOS weather.
- Added default metro weather stations and time zones to `available_systems()`.

# bikerentaldata 0.3.1

- Rewrote the README around a simple install-and-quick-start workflow.
- Expanded package-level help for supported systems, large downloads, legacy
  fields, and the Capital Bikeshare paper workflow.
- Separated ordinary user installation from package-development instructions.

# bikerentaldata 0.3.0

- Added historical trip support for Citi Bike, Divvy, and Bay Wheels.
- Added `available_systems()`, `load_trip_data()`, and `standardize_trips()`.
- Extended `available_trip_data()` and `download_trip_files()` with a
  `system` argument.
- Added a common multi-city trip schema for bike type, duration, stations,
  coordinates, and member/casual rider type.

# bikerentaldata 0.2.1

- Added package-level help available through `?bikerentaldata`.
- Improved `current_system_info()` printing so all jurisdictions are visible.
- Added usage examples and cross-references to information-function help.

# bikerentaldata 0.2.0

- Added `available_trip_data()` to list official annual and monthly archives.
- Added `current_system_info()` for live station and jurisdiction counts.
- Added `summarize_trip_locations()` for historical local station counts.
- Added support for 2010–2017 annual and quarterly legacy trip files.
- Preserved legacy rental counts when coordinates are unavailable.

# bikerentaldata 0.1.0

- Initial package release.
