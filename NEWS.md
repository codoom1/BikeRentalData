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
