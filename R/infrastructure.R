.require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package `sf` is required for bicycle infrastructure exposure. ",
      "Install it with install.packages(\"sf\").",
      call. = FALSE
    )
  }
}

.bike_infrastructure_sources <- tibble::tribble(
  ~system, ~source_name, ~jurisdiction, ~coverage_area, ~url, ~facility_column,
  ~protected_values, ~trail_values, ~notes,
  "capital",
  "DDOT Bike Lanes Map",
  "Washington, DC",
  "Washington, DC",
  paste0(
    "https://services.arcgis.com/neT9SoYxizqTHZPH/arcgis/rest/services/",
    "Bike_Lanes_Map_(Oct_30_2024)/FeatureServer/0/query?",
    "where=1%3D1&outFields=*&f=geojson"
  ),
  "FACILITY",
  list(c(
    "Protected Bike Lane",
    "Two-way Protected Bike Lane",
    "Protected Contraflow Bike Lane"
  )),
  list(character()),
  paste(
    "Official District Department of Transportation bike-lane layer for",
    "Washington, DC."
  ),
  "capital",
  "Arlington County Bike Route Lines",
  "Arlington, VA",
  "Arlington, VA",
  paste0(
    "https://arlgis.arlingtonva.us/arcgis/rest/services/Open_Data/",
    "od_Bike_Route_Lines/FeatureServer/0/query?",
    "where=1%3D1&outFields=*&f=geojson"
  ),
  "Route_Type",
  list(c("Protected Bike Lane", "Sidepath", "Trail")),
  list(c("Sidepath", "Trail")),
  paste(
    "Official Arlington County bike route layer, including on-street and",
    "off-street bike paths."
  ),
  "capital",
  "Alexandria Bike Lane Routes",
  "Alexandria, VA",
  "Alexandria, VA",
  paste0(
    "https://services2.arcgis.com/ChYV69FhfjwkvRmy/arcgis/rest/services/",
    "Transit_BikeLaneRoutes/FeatureServer/0/query?",
    "where=1%3D1&outFields=*&f=geojson"
  ),
  "BikeLaneTy",
  list(c("Route Off Street", "Protected Bike Lane")),
  list(c("Route Off Street", "Trail", "Shared Use Path")),
  paste(
    "Official City of Alexandria bike-lane route layer, including designated",
    "bike lanes, shared-lane markings, and generalized bike routes."
  ),
  "capital",
  "Montgomery County Existing Bikeways",
  "Montgomery County, MD",
  "Montgomery County, MD",
  paste0(
    "https://montgomeryplans.org/arcgis4/rest/services/Overlays/",
    "MCAtlas_Transportation_Existing/MapServer/16/query?",
    "where=1%3D1&outFields=*&f=geojson"
  ),
  "FACILITY_CLASS",
  list(c("Separated Bikeways", "Sidepath", "Trail")),
  list(c("Sidepath", "Trail", "Shared Use Path")),
  paste(
    "Official Montgomery County Planning existing bikeways layer. Prince",
    "George's County is still not included until a clearly official existing",
    "bike-infrastructure line layer is selected."
  ),
  "divvy",
  "Chicago Data Portal Bike Routes",
  "Chicago",
  "Chicago",
  "https://data.cityofchicago.org/resource/hvv9-38ut.geojson?$limit=50000",
  "displayrou",
  list(c("Protected Bike Lane")),
  list(character()),
  "Divvy serves the Chicago area; this layer covers City of Chicago routes.",
  "citibike",
  "NYC Open Data Bicycle Routes",
  "New York City",
  "New York City",
  "https://data.cityofnewyork.us/resource/mzxg-pwib.geojson?$limit=50000",
  "facilitycl",
  list(c("I")),
  list(c("L")),
  paste(
    "NYC DOT publishes facility classes as codes. Class I is treated as",
    "protected/greenway-style separated infrastructure for exposure summaries."
  ),
  "baywheels",
  "MTC Regional Bike Facilities",
  "San Francisco Bay Area",
  "Nine-county San Francisco Bay Area",
  paste0(
    "https://services3.arcgis.com/i2dkYWmb4wHvYPda/arcgis/rest/services/",
    "regional_bike_facilities/FeatureServer/0/query?",
    "where=1%3D1&outFields=*&f=geojson"
  ),
  "class",
  list(c("1", "4", "Class 1", "Class 4")),
  list(c("1", "Class 1")),
  paste(
    "Official Metropolitan Transportation Commission regional bike facilities",
    "layer. It covers the nine Bay Area counties, including San Francisco,",
    "Oakland, Berkeley, and San Jose, and avoids double-counting overlapping",
    "city layers."
  )
)

#' Available bicycle infrastructure source layers
#'
#' Lists the official open-data layers used by `download_bike_infrastructure()`.
#'
#' @return A tibble with system identifiers, source names, coverage areas,
#'   download URLs, facility-type columns, and coverage notes.
#'
#' @examples
#' available_bike_infrastructure_sources()
#' @export
available_bike_infrastructure_sources <- function() {
  .bike_infrastructure_sources
}

#' Bicycle infrastructure exposure data dictionary
#'
#' Describes the station-area bicycle infrastructure variables created by
#' [build_station_infrastructure_exposure()],
#' [add_station_infrastructure_exposure()], and
#' [add_bike_infrastructure_exposure()].
#'
#' @param buffers_m Numeric buffer distances, in meters.
#' @param sides Station sides to describe: `"start"`, `"end"`, or both.
#'
#' @return A tibble with variable names, units, level, and interpretation.
#'
#' @details
#' These variables measure infrastructure availability near stations. They do
#' not identify the route a rider used and should not be interpreted as
#' evidence that a rider traveled on a specific bike lane or trail.
#'
#' @examples
#' infrastructure_data_dictionary()
#' infrastructure_data_dictionary(buffers_m = c(250, 500, 1000))
#' @export
infrastructure_data_dictionary <- function(
  buffers_m = c(250, 500),
  sides = c("start", "end")
) {
  buffers_m <- sort(unique(as.numeric(buffers_m)))
  if (!length(buffers_m) || anyNA(buffers_m) || any(buffers_m <= 0)) {
    stop("`buffers_m` must contain positive distances in meters.", call. = FALSE)
  }
  sides <- match.arg(sides, c("start", "end"), several.ok = TRUE)

  side_label <- c(
    start = "trip origin/start station",
    end = "trip destination/end station"
  )

  base <- purrr::map_dfr(sides, function(side) {
    tibble::tibble(
      variable = paste0(side, "_nearest_bikeinfra_m"),
      type = "double",
      units = "meters",
      level = "station",
      source = "bike infrastructure layer + station coordinates",
      description = paste(
        "Straight-line distance from the", side_label[[side]],
        "to the nearest bicycle facility in the infrastructure layer."
      ),
      interpretation = paste(
        "Smaller values indicate closer bicycle infrastructure near the",
        side_label[[side]], "not route use."
      )
    )
  })

  buffer_variables <- purrr::map_dfr(sides, function(side) {
    purrr::map_dfr(buffers_m, function(buffer_m) {
      suffix <- paste0(buffer_m, "m")
      tibble::tribble(
        ~variable, ~type, ~units, ~level, ~source, ~description, ~interpretation,
        paste0(side, "_bikeinfra_", suffix, "_m"),
        "double",
        "meters",
        "station",
        "bike infrastructure layer + station coordinates",
        paste(
          "Total clipped length of all bicycle infrastructure within",
          buffer_m, "meters of the", side_label[[side]], "."
        ),
        paste(
          "Higher values indicate more nearby bicycle infrastructure around the",
          side_label[[side]], "not the distance ridden by a user."
        ),
        paste0(side, "_protected_bikeinfra_", suffix, "_m"),
        "double",
        "meters",
        "station",
        "bike infrastructure layer facility classification",
        paste(
          "Total clipped length of protected or separated bicycle",
          "infrastructure within", buffer_m, "meters of the",
          side_label[[side]], "."
        ),
        paste(
          "Higher values indicate more nearby protected/separated bicycle",
          "infrastructure. Classification depends on the official source layer."
        ),
        paste0(side, "_any_protected_", suffix),
        "logical",
        "TRUE/FALSE",
        "station",
        "derived from protected infrastructure length",
        paste(
          "Indicator that protected or separated bicycle infrastructure exists",
          "within", buffer_m, "meters of the", side_label[[side]], "."
        ),
        paste(
          "TRUE means at least one protected/separated facility is nearby; it",
          "does not mean the trip used that facility."
        ),
        paste0(side, "_trail_", suffix, "_m"),
        "double",
        "meters",
        "station",
        "bike infrastructure layer facility classification",
        paste(
          "Total clipped length of trails, greenways, sidepaths, or",
          "shared-use paths within", buffer_m, "meters of the",
          side_label[[side]], "."
        ),
        paste(
          "Higher values indicate more nearby trail/shared-use path exposure.",
          "Availability depends on trail classes present in the source layer."
        )
      )
    })
  })

  dplyr::bind_rows(base, buffer_variables)
}

.safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", tolower(x))
  gsub("(^-+|-+$)", "", x)
}

.with_query_params <- function(url, ...) {
  params <- list(...)
  separator <- if (grepl("[?]", url)) "&" else "?"
  query <- paste(
    paste0(names(params), "=", utils::URLencode(as.character(params))),
    collapse = "&"
  )
  paste0(url, separator, query)
}

.download_arcgis_geojson <- function(url, cache_file, page_size = 1000, quiet = FALSE) {
  features <- list()
  offset <- 0L
  template <- gsub("([?&])resultOffset=[^&]*", "", url)
  template <- gsub("([?&])resultRecordCount=[^&]*", "", template)

  repeat {
    page_url <- .with_query_params(
      template,
      resultOffset = offset,
      resultRecordCount = page_size
    )
    page <- jsonlite::fromJSON(page_url, simplifyVector = FALSE)
    if (!is.null(page$error)) {
      stop(
        "ArcGIS download failed: ",
        page$error$message %||% "unknown error",
        call. = FALSE
      )
    }
    page_features <- page$features %||% list()
    features <- c(features, page_features)
    if (!quiet) {
      message("  downloaded ", length(features), " features")
    }
    if (length(page_features) < page_size) {
      break
    }
    offset <- offset + page_size
  }

  geojson <- list(
    type = "FeatureCollection",
    features = features
  )
  jsonlite::write_json(geojson, cache_file, auto_unbox = TRUE, null = "null")
}

.download_infrastructure_source <- function(source, cache_file, quiet = FALSE) {
  if (grepl("/query[?]", source$url, fixed = FALSE)) {
    .download_arcgis_geojson(source$url, cache_file, quiet = quiet)
  } else {
    result <- tryCatch(
      utils::download.file(
        source$url,
        destfile = cache_file,
        mode = "wb",
        quiet = quiet
      ),
      error = function(error) {
        stop(
          "Could not download bike infrastructure source `",
          source$source_name,
          "`. Source URL: ", source$url, "\n",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
    if (!identical(result, 0L)) {
      stop(
        "Could not download bike infrastructure source `",
        source$source_name,
        "`. Source URL: ", source$url,
        call. = FALSE
      )
    }
  }
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Download official bike infrastructure data
#'
#' Downloads a city bicycle-infrastructure line layer for one supported
#' bike-share system and reads it as an `sf` object.
#'
#' @param system System identifier: `"capital"`, `"divvy"`, `"citibike"`, or
#'   `"baywheels"`.
#' @param destination Directory where the GeoJSON file should be cached.
#' @param overwrite Whether to redownload an existing cached file.
#' @param quiet Whether to suppress download and spatial read messages.
#'
#' @return An `sf` line layer with source metadata attached as attributes.
#'
#' @details
#' These are official city open-data layers, but their coverage differs by
#' system. Capital Bikeshare and Bay Wheels are regional systems; the default
#' layers currently cover Washington, DC and San Francisco respectively, not
#' every jurisdiction served by those systems.
#'
#' Use `available_bike_infrastructure_sources()` to inspect the source URL,
#' facility-type column, and coverage notes before analysis.
#'
#' @examples
#' \dontrun{
#' capital_lanes <- download_bike_infrastructure("capital")
#'
#' trips <- add_bike_infrastructure_exposure(
#'   trips,
#'   infrastructure = capital_lanes
#' )
#' }
#' @export
download_bike_infrastructure <- function(
  system,
  destination = file.path("data", "spatial"),
  overwrite = FALSE,
  quiet = FALSE
) {
  .require_sf()
  system <- .match_system(system)
  sources <- .bike_infrastructure_sources[
    .bike_infrastructure_sources$system == system,
  ]
  if (nrow(sources) == 0L) {
    stop("No bike infrastructure source is configured for `", system, "`.")
  }

  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  layers <- purrr::map(seq_len(nrow(sources)), function(index) {
    source <- sources[index, ]
    cache_file <- file.path(
      destination,
      paste0(
        system,
        "-",
        .safe_filename(source$jurisdiction),
        "-bike-infrastructure.geojson"
      )
    )

    if (!file.exists(cache_file) || overwrite) {
      if (!quiet) {
        message("Downloading ", source$source_name, "...")
      }
      .download_infrastructure_source(source, cache_file, quiet = quiet)
    } else if (!quiet) {
      message("Using cached ", cache_file)
    }

    layer <- suppressWarnings(sf::st_read(cache_file, quiet = quiet))
    facility_column <- .resolve_column_name(layer, source$facility_column)
    facility_class <- if (!is.null(facility_column)) {
      as.character(layer[[facility_column]])
    } else {
      rep(NA_character_, nrow(layer))
    }
    layer$.bikerentaldata_system <- system
    layer$.bikerentaldata_jurisdiction <- source$jurisdiction
    layer$.bikerentaldata_source_name <- source$source_name
    layer$.bikerentaldata_facility_class <- facility_class

    geometry_column <- attr(layer, "sf_column")
    layer[c(
      ".bikerentaldata_system",
      ".bikerentaldata_jurisdiction",
      ".bikerentaldata_source_name",
      ".bikerentaldata_facility_class",
      geometry_column
    )]
  })

  layer <- do.call(rbind, layers)
  attr(layer, "bikerentaldata_system") <- system
  attr(layer, "bikerentaldata_source_name") <-
    paste(sources$source_name, collapse = "; ")
  attr(layer, "bikerentaldata_source_url") <-
    paste(sources$url, collapse = "; ")
  attr(layer, "bikerentaldata_coverage_area") <-
    paste(unique(sources$coverage_area), collapse = "; ")
  attr(layer, "bikerentaldata_facility_column") <-
    ".bikerentaldata_facility_class"
  attr(layer, "bikerentaldata_protected_values") <-
    .as_character_vector(sources$protected_values)
  attr(layer, "bikerentaldata_trail_values") <-
    .as_character_vector(sources$trail_values)
  attr(layer, "bikerentaldata_notes") <- paste(sources$notes, collapse = " ")

  if (!quiet) {
    message("Combined ", nrow(sources), " source layer(s), ", nrow(layer), " features.")
    message("Coverage note: ", attr(layer, "bikerentaldata_notes"))
  }

  layer
}

.local_projected_crs <- function(longitude, latitude) {
  longitude <- longitude[!is.na(longitude)]
  latitude <- latitude[!is.na(latitude)]
  if (!length(longitude) || !length(latitude)) {
    return(3857L)
  }

  zone <- floor((stats::median(longitude) + 180) / 6) + 1
  if (stats::median(latitude) >= 0) {
    32600L + zone
  } else {
    32700L + zone
  }
}

.normalize_facility_value <- function(x) {
  gsub("[^a-z0-9]+", " ", tolower(trimws(as.character(x))))
}

.as_character_vector <- function(x) {
  unique(as.character(unlist(x, use.names = FALSE)))
}

.resolve_column_name <- function(data, column) {
  if (is.null(column) || !length(column) || is.na(column) || !nzchar(column)) {
    return(NULL)
  }
  if (column %in% names(data)) {
    return(column)
  }
  matched <- names(data)[tolower(names(data)) == tolower(column)]
  if (length(matched)) {
    return(matched[[1]])
  }
  NULL
}

.facility_subset <- function(infrastructure, facility_column, values) {
  facility_column <- .resolve_column_name(infrastructure, facility_column)
  if (is.null(facility_column)) {
    return(infrastructure[0, ])
  }
  values <- .as_character_vector(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(infrastructure[0, ])
  }

  column_value <- .normalize_facility_value(infrastructure[[facility_column]])
  lookup <- .normalize_facility_value(values)
  infrastructure[column_value %in% lookup, ]
}

.station_points <- function(trips, side) {
  lat_col <- paste0(side, "_lat")
  lng_col <- paste0(side, "_lng")
  id_col <- paste0(side, "_station_id")
  name_col <- paste0(side, "_station_name")

  keep <- !is.na(trips[[lat_col]]) & !is.na(trips[[lng_col]])
  if (!any(keep)) {
    return(tibble::tibble())
  }

  station_id <- if (id_col %in% names(trips)) {
    as.character(trips[[id_col]])
  } else {
    rep(NA_character_, nrow(trips))
  }
  station_name <- if (name_col %in% names(trips)) {
    as.character(trips[[name_col]])
  } else {
    rep(NA_character_, nrow(trips))
  }

  stations <- tibble::tibble(
    .row_id = seq_len(nrow(trips)),
    .station_id = station_id,
    .station_name = station_name,
    station_id = station_id,
    station_name = station_name,
    latitude = trips[[lat_col]],
    longitude = trips[[lng_col]]
  )[keep, ]
  stations$.station_key <- .make_station_key(
    side,
    stations$.station_id,
    stations$.station_name,
    stations$latitude,
    stations$longitude
  )

  stations |>
    dplyr::group_by(.station_key) |>
    dplyr::summarise(
      .row_id = dplyr::first(.row_id),
      station_id = dplyr::first(station_id),
      station_name = dplyr::first(station_name),
      latitude = stats::median(latitude, na.rm = TRUE),
      longitude = stats::median(longitude, na.rm = TRUE),
      .groups = "drop"
    )
}

.station_row_keys <- function(trips, side) {
  lat_col <- paste0(side, "_lat")
  lng_col <- paste0(side, "_lng")
  id_col <- paste0(side, "_station_id")

  keep <- !is.na(trips[[lat_col]]) & !is.na(trips[[lng_col]])
  station_id <- if (id_col %in% names(trips)) {
    as.character(trips[[id_col]])
  } else {
    rep(NA_character_, nrow(trips))
  }
  name_col <- paste0(side, "_station_name")
  station_name <- if (name_col %in% names(trips)) {
    as.character(trips[[name_col]])
  } else {
    rep(NA_character_, nrow(trips))
  }

  rows <- tibble::tibble(
    .row_id = seq_len(nrow(trips)),
    .station_id = station_id,
    .station_name = station_name,
    latitude = trips[[lat_col]],
    longitude = trips[[lng_col]]
  )[keep, ]
  rows$.station_key <- .make_station_key(
    side,
    rows$.station_id,
    rows$.station_name,
    rows$latitude,
    rows$longitude
  )

  rows[c(".row_id", ".station_key")]
}

.make_station_key <- function(side, station_id, station_name, latitude, longitude) {
  station_id <- trimws(as.character(station_id))
  station_name <- trimws(as.character(station_name))
  has_id <- !is.na(station_id) & nzchar(station_id)
  has_name <- !is.na(station_name) & nzchar(station_name)

  dplyr::case_when(
    has_id ~ paste(side, "id", station_id, sep = "|"),
    has_name ~ paste(side, "name", station_name, sep = "|"),
    TRUE ~ paste(
      side,
      "coord",
      round(latitude, 5),
      round(longitude, 5),
      sep = "|"
    )
  )
}

.line_length_in_buffer <- function(buffer, lines, candidate_index = NULL) {
  if (nrow(lines) == 0L) {
    return(0)
  }
  if (!is.null(candidate_index)) {
    if (!length(candidate_index)) {
      return(0)
    }
    lines <- lines[candidate_index, ]
  }
  clipped <- suppressWarnings(sf::st_intersection(lines, buffer))
  if (nrow(clipped) == 0L) {
    return(0)
  }
  sum(as.numeric(sf::st_length(clipped)), na.rm = TRUE)
}

.nearest_line_distance <- function(point, lines) {
  if (nrow(lines) == 0L) {
    return(NA_real_)
  }
  nearest <- sf::st_nearest_feature(point, lines)
  as.numeric(sf::st_distance(point, lines[nearest, ], by_element = TRUE))
}

.infrastructure_metrics <- function(
  points,
  infrastructure,
  buffers_m,
  protected_infrastructure,
  trail_infrastructure,
  side
) {
  if (nrow(points) == 0L) {
    return(tibble::tibble())
  }

  result <- tibble::as_tibble(sf::st_drop_geometry(points))
  result[[paste0(side, "_nearest_bikeinfra_m")]] <- vapply(
    seq_len(nrow(points)),
    function(index) .nearest_line_distance(points[index, ], infrastructure),
    numeric(1)
  )

  max_buffer_m <- max(buffers_m)
  infrastructure_candidates <- sf::st_is_within_distance(
    points,
    infrastructure,
    dist = max_buffer_m
  )
  protected_candidates <- if (nrow(protected_infrastructure) > 0L) {
    sf::st_is_within_distance(
      points,
      protected_infrastructure,
      dist = max_buffer_m
    )
  } else {
    NULL
  }
  trail_candidates <- if (nrow(trail_infrastructure) > 0L) {
    sf::st_is_within_distance(
      points,
      trail_infrastructure,
      dist = max_buffer_m
    )
  } else {
    NULL
  }

  for (buffer_m in buffers_m) {
    buffer_name <- paste0(buffer_m, "m")
    buffers <- sf::st_buffer(points, dist = buffer_m)

    result[[paste0(side, "_bikeinfra_", buffer_name, "_m")]] <- vapply(
      seq_len(nrow(points)),
      function(index) {
        .line_length_in_buffer(
          buffers[index, ],
          infrastructure,
          infrastructure_candidates[[index]]
        )
      },
      numeric(1)
    )

    if (nrow(protected_infrastructure) > 0L) {
      protected_length <- vapply(
        seq_len(nrow(points)),
        function(index) {
          .line_length_in_buffer(
            buffers[index, ],
            protected_infrastructure,
            protected_candidates[[index]]
          )
        },
        numeric(1)
      )
      result[[paste0(side, "_protected_bikeinfra_", buffer_name, "_m")]] <-
        protected_length
      result[[paste0(side, "_any_protected_", buffer_name)]] <-
        protected_length > 0
    }

    if (nrow(trail_infrastructure) > 0L) {
      result[[paste0(side, "_trail_", buffer_name, "_m")]] <- vapply(
        seq_len(nrow(points)),
        function(index) {
          .line_length_in_buffer(
            buffers[index, ],
            trail_infrastructure,
            trail_candidates[[index]]
          )
        },
        numeric(1)
      )
    }
  }

  result
}

.prepare_infrastructure_layers <- function(
  trips,
  infrastructure,
  sides,
  facility_column,
  protected_values,
  trail_values,
  projected_crs
) {
  .require_sf()
  if (!is.data.frame(trips)) {
    stop("`trips` must be a data frame.", call. = FALSE)
  }
  if (!inherits(infrastructure, "sf")) {
    stop("`infrastructure` must be an sf object.", call. = FALSE)
  }

  required <- as.vector(outer(sides, c("_lat", "_lng"), paste0))
  missing <- setdiff(required, names(trips))
  if (length(missing)) {
    stop(
      "`trips` is missing required coordinate columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(facility_column)) {
    facility_column <- .resolve_column_name(infrastructure, facility_column)
  }
  if (is.null(facility_column)) {
    facility_column <- attr(infrastructure, "bikerentaldata_facility_column")
  }
  facility_column <- .resolve_column_name(infrastructure, facility_column)

  if (is.null(protected_values)) {
    protected_values <- attr(
      infrastructure,
      "bikerentaldata_protected_values"
    )
    if (is.null(protected_values)) {
      protected_values <- c(
        "protected bike lane",
        "protected lane",
        "cycle track",
        "separated bike lane"
      )
    }
  }
  if (is.null(trail_values)) {
    trail_values <- attr(infrastructure, "bikerentaldata_trail_values")
    if (is.null(trail_values)) {
      trail_values <- c(
        "trail",
        "shared-use path",
        "multi-use path",
        "greenway"
      )
    }
  }

  if (is.na(sf::st_crs(infrastructure))) {
    warning(
      "`infrastructure` has no CRS; assuming EPSG:4326 longitude/latitude.",
      call. = FALSE
    )
    infrastructure <- sf::st_set_crs(infrastructure, 4326)
  }
  infrastructure <- sf::st_make_valid(infrastructure)
  infrastructure <- infrastructure[!sf::st_is_empty(infrastructure), ]
  if (nrow(infrastructure) == 0L) {
    stop("`infrastructure` does not contain any non-empty geometries.", call. = FALSE)
  }

  all_longitude <- unlist(trips[paste0(sides, "_lng")], use.names = FALSE)
  all_latitude <- unlist(trips[paste0(sides, "_lat")], use.names = FALSE)
  if (is.null(projected_crs)) {
    projected_crs <- .local_projected_crs(all_longitude, all_latitude)
  }

  infrastructure <- sf::st_transform(infrastructure, projected_crs)
  list(
    infrastructure = infrastructure,
    protected_infrastructure = .facility_subset(
      infrastructure,
      facility_column,
      protected_values
    ),
    trail_infrastructure = .facility_subset(
      infrastructure,
      facility_column,
      trail_values
    ),
    projected_crs = projected_crs
  )
}

.station_metrics_for_side <- function(
  trips,
  side,
  infrastructure_layers,
  buffers_m
) {
  stations <- .station_points(trips, side)
  if (nrow(stations) == 0L) {
    return(tibble::tibble())
  }

  station_points <- sf::st_as_sf(
    stations,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  station_points <- sf::st_transform(
    station_points,
    infrastructure_layers$projected_crs
  )

  metrics <- .infrastructure_metrics(
    station_points,
    infrastructure_layers$infrastructure,
    buffers_m,
    infrastructure_layers$protected_infrastructure,
    infrastructure_layers$trail_infrastructure,
    side
  )

  metric_columns <- setdiff(
    names(metrics),
    c(".station_key", ".row_id", "station_id", "station_name",
      "latitude", "longitude")
  )

  metrics[c(
    ".station_key", "station_id", "station_name", "latitude", "longitude",
    metric_columns
  )]
}

#' Build station-level bicycle infrastructure exposure
#'
#' Computes reusable station-level bike-infrastructure exposure tables from
#' standardized trip records and an `sf` line layer.
#'
#' @param trips Standardized trips containing station IDs/names and coordinates.
#' @param infrastructure An `sf` line layer, usually from
#'   `download_bike_infrastructure()`.
#' @param buffers_m Numeric buffer distances, in meters.
#' @param facility_column Optional infrastructure facility-type column.
#' @param protected_values Values in `facility_column` counted as protected
#'   infrastructure.
#' @param trail_values Values in `facility_column` counted as trails or
#'   shared-use paths.
#' @param sides Which station sides to compute: `"start"`, `"end"`, or both.
#' @param projected_crs Optional projected CRS for distance/length operations.
#' @param cache_file Optional CSV file path where the station exposure table is
#'   written and reused.
#' @param overwrite Whether to recompute an existing `cache_file`.
#'
#' @return A station-level tibble with one row per station side/key.
#'
#' @details
#' This is the recommended workflow for large or repeated monthly/yearly
#' studies. Exposure is station-level, so computing it once and joining it back
#' to many trip files is much faster and easier to audit.
#'
#' @examples
#' \dontrun{
#' lanes <- download_bike_infrastructure("divvy")
#' station_exposure <- build_station_infrastructure_exposure(
#'   trips,
#'   lanes,
#'   cache_file = "data/cache/divvy_station_exposure.csv"
#' )
#' trips <- add_station_infrastructure_exposure(trips, station_exposure)
#' }
#' @export
build_station_infrastructure_exposure <- function(
  trips,
  infrastructure,
  buffers_m = c(250, 500),
  facility_column = NULL,
  protected_values = NULL,
  trail_values = NULL,
  sides = c("start", "end"),
  projected_crs = NULL,
  cache_file = NULL,
  overwrite = FALSE
) {
  if (!is.null(cache_file) && file.exists(cache_file) && !overwrite) {
    return(readr::read_csv(cache_file, show_col_types = FALSE))
  }

  sides <- match.arg(sides, c("start", "end"), several.ok = TRUE)
  buffers_m <- sort(unique(as.numeric(buffers_m)))
  if (!length(buffers_m) || anyNA(buffers_m) || any(buffers_m <= 0)) {
    stop("`buffers_m` must contain positive distances in meters.", call. = FALSE)
  }

  infrastructure_layers <- .prepare_infrastructure_layers(
    trips = trips,
    infrastructure = infrastructure,
    sides = sides,
    facility_column = facility_column,
    protected_values = protected_values,
    trail_values = trail_values,
    projected_crs = projected_crs
  )

  station_exposure <- purrr::map_dfr(sides, function(side) {
    .station_metrics_for_side(
      trips,
      side,
      infrastructure_layers,
      buffers_m
    )
  })

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(station_exposure, cache_file)
  }

  station_exposure
}

#' Join station-level bicycle infrastructure exposure to trips
#'
#' Joins a table from `build_station_infrastructure_exposure()` back to
#' standardized trip records.
#'
#' @param trips Standardized trip records.
#' @param station_exposure Station-level exposure table.
#' @param sides Which station sides to join: `"start"`, `"end"`, or both.
#'
#' @return Trip records with infrastructure exposure columns appended.
#'
#' @examples
#' \dontrun{
#' trips <- add_station_infrastructure_exposure(trips, station_exposure)
#' }
#' @export
add_station_infrastructure_exposure <- function(
  trips,
  station_exposure,
  sides = c("start", "end")
) {
  if (!is.data.frame(trips)) {
    stop("`trips` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(station_exposure)) {
    stop("`station_exposure` must be a data frame.", call. = FALSE)
  }
  if (!".station_key" %in% names(station_exposure)) {
    stop(
      "`station_exposure` must contain a `.station_key` column.",
      call. = FALSE
    )
  }

  sides <- match.arg(sides, c("start", "end"), several.ok = TRUE)
  output <- tibble::as_tibble(trips)
  output$.row_id <- seq_len(nrow(output))

  for (side in sides) {
    row_lookup <- .station_row_keys(output, side)
    side_columns <- grep(
      paste0("^", side, "_"),
      names(station_exposure),
      value = TRUE
    )
    join_table <- station_exposure[c(".station_key", side_columns)]
    join_table <- dplyr::distinct(join_table, .station_key, .keep_all = TRUE)

    output <- dplyr::left_join(output, row_lookup, by = ".row_id")
    output <- dplyr::left_join(output, join_table, by = ".station_key")
    output$.station_key <- NULL
  }

  output$.row_id <- NULL
  output
}

#' Summarize station infrastructure coverage
#'
#' Audits whether trip records have usable station coordinates and, optionally,
#' whether those stations are present in a station-level infrastructure exposure
#' table.
#'
#' @param trips Standardized trip records.
#' @param station_exposure Optional table from
#'   `build_station_infrastructure_exposure()`.
#' @param sides Which station sides to summarize: `"start"`, `"end"`, or both.
#'
#' @return A tibble with station and trip coverage counts by side.
#'
#' @examples
#' \dontrun{
#' summarize_station_infrastructure_coverage(trips, station_exposure)
#' }
#' @export
summarize_station_infrastructure_coverage <- function(
  trips,
  station_exposure = NULL,
  sides = c("start", "end")
) {
  if (!is.data.frame(trips)) {
    stop("`trips` must be a data frame.", call. = FALSE)
  }
  if (!is.null(station_exposure) && !is.data.frame(station_exposure)) {
    stop("`station_exposure` must be a data frame.", call. = FALSE)
  }
  if (
    !is.null(station_exposure) &&
      !".station_key" %in% names(station_exposure)
  ) {
    stop(
      "`station_exposure` must contain a `.station_key` column.",
      call. = FALSE
    )
  }

  sides <- match.arg(sides, c("start", "end"), several.ok = TRUE)
  purrr::map_dfr(sides, function(side) {
    lat_col <- paste0(side, "_lat")
    lng_col <- paste0(side, "_lng")
    if (!all(c(lat_col, lng_col) %in% names(trips))) {
      stop(
        "`trips` is missing required coordinate columns: ",
        paste(setdiff(c(lat_col, lng_col), names(trips)), collapse = ", "),
        call. = FALSE
      )
    }

    row_keys <- .station_row_keys(trips, side)
    station_points <- .station_points(trips, side)
    trip_rows_with_coordinates <- nrow(row_keys)
    total_station_rows <- dplyr::n_distinct(row_keys$.station_key)

    exposure_keys <- if (!is.null(station_exposure)) {
      station_exposure$.station_key[
        station_exposure$.station_key %in% station_points$.station_key
      ]
    } else {
      character()
    }
    stations_with_exposure <- dplyr::n_distinct(exposure_keys)
    trip_rows_with_exposure <- if (!is.null(station_exposure)) {
      sum(row_keys$.station_key %in% station_exposure$.station_key)
    } else {
      NA_integer_
    }

    tibble::tibble(
      side = side,
      trip_rows = nrow(trips),
      trip_rows_with_coordinates = trip_rows_with_coordinates,
      pct_trip_rows_with_coordinates = trip_rows_with_coordinates / nrow(trips) * 100,
      stations_with_coordinates = total_station_rows,
      stations_with_exposure = if (!is.null(station_exposure)) {
        stations_with_exposure
      } else {
        NA_integer_
      },
      pct_stations_with_exposure = if (!is.null(station_exposure) && total_station_rows > 0) {
        stations_with_exposure / total_station_rows * 100
      } else {
        NA_real_
      },
      trip_rows_with_exposure = trip_rows_with_exposure,
      pct_trip_rows_with_exposure = if (!is.null(station_exposure) && nrow(trips) > 0) {
        trip_rows_with_exposure / nrow(trips) * 100
      } else {
        NA_real_
      }
    )
  })
}

#' Add station-area bicycle infrastructure exposure
#'
#' Computes bicycle-infrastructure exposure around trip start and end stations.
#' These variables describe infrastructure near stations; they do not claim
#' that a rider used a specific facility or route.
#'
#' @param trips Standardized trips containing station coordinates such as
#'   `start_lat`, `start_lng`, `end_lat`, and `end_lng`.
#' @param infrastructure An `sf` object with line geometries representing bike
#'   lanes, trails, cycle tracks, or other bicycle facilities.
#' @param buffers_m Numeric buffer distances, in meters.
#' @param facility_column Optional column in `infrastructure` describing the
#'   facility type.
#' @param protected_values Values in `facility_column` that should be counted
#'   as protected or separated bike infrastructure.
#' @param trail_values Values in `facility_column` that should be counted as
#'   trails or shared-use paths.
#' @param sides Which station sides to enrich: `"start"`, `"end"`, or both.
#' @param projected_crs Optional projected CRS used for distance and length
#'   calculations. If `NULL`, a local UTM CRS is chosen from the trip
#'   coordinates.
#' @param cache_file Optional station-level exposure CSV file. If supplied, the
#'   station exposure table is written and reused on later calls.
#' @param overwrite Whether to recompute an existing `cache_file`.
#'
#' @return The input trips with station-area exposure columns appended.
#'
#' @details
#' The function measures line length inside circular station buffers and the
#' straight-line distance from each station to the nearest bicycle facility.
#' Lengths and distances are returned in meters.
#'
#' If `facility_column` is supplied, the function can also create protected
#' bike-infrastructure and trail-specific measures. Matching is exact after
#' lowercasing and removing punctuation, so check the unique values in your
#' source layer before choosing `protected_values` and `trail_values`.
#'
#' @examples
#' \dontrun{
#' bike_lanes <- sf::st_read("data/spatial/dc_bike_lanes.geojson")
#'
#' trips <- add_bike_infrastructure_exposure(
#'   trips,
#'   infrastructure = bike_lanes,
#'   buffers_m = c(250, 500),
#'   facility_column = "facility_type",
#'   protected_values = c("protected bike lane", "cycle track"),
#'   trail_values = c("trail", "shared-use path")
#' )
#' }
#' @export
add_bike_infrastructure_exposure <- function(
  trips,
  infrastructure,
  buffers_m = c(250, 500),
  facility_column = NULL,
  protected_values = NULL,
  trail_values = NULL,
  sides = c("start", "end"),
  projected_crs = NULL,
  cache_file = NULL,
  overwrite = FALSE
) {
  sides <- match.arg(sides, c("start", "end"), several.ok = TRUE)
  station_exposure <- build_station_infrastructure_exposure(
    trips = trips,
    infrastructure = infrastructure,
    buffers_m = buffers_m,
    facility_column = facility_column,
    protected_values = protected_values,
    trail_values = trail_values,
    sides = sides,
    projected_crs = projected_crs,
    cache_file = cache_file,
    overwrite = overwrite
  )
  add_station_infrastructure_exposure(
    trips,
    station_exposure,
    sides = sides
  )
}
