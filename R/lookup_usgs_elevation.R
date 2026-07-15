#' Use USGS API (USGS Elevation Point Query Service) to determine approximate
#' local elevation
#'
#' This is meant to supply an APPROXIMATE elevation, with no guarantees on
#' precision or on the lifetime of the API service used by the function. The
#' lookup uses the JSON response from the service.
#'
#' @param latitude Degrees latitude (positive for north) of the location to look
#'   up.
#' @param longitude Degrees longitude (positive for east) of the location to
#'   look up.
#' @param units A single string specifying the elevation units. Accepts `"m"`,
#'   `"meters"`, `"ft"`, or `"feet"`, case-insensitively.
#' @param timeout A single positive number giving the request timeout in
#'   seconds.
#' @param max_tries A single positive integer giving the maximum number of
#'   request attempts.
#' @returns The numeric elevation in the requested units.
#' @references https://epqs.nationalmap.gov/v1/docs
#' @examplesIf interactive()
#' elevation_m <- lookup_usgs_elevation(
#'   latitude = 39.102075,
#'   longitude = -96.594689
#' )
#' elevation_m
#'
#' elevation_ft <- lookup_usgs_elevation(
#'   latitude = 39.102075,
#'   longitude = -96.594689,
#'   units = "ft"
#' )
#' elevation_ft
#' @export
lookup_usgs_elevation <- function(
  latitude,
  longitude,
  units = "m",
  timeout = 30,
  max_tries = 3
) {
  units <- normalize_epqs_units(units)
  check_coordinate(latitude, -90, 90)
  check_coordinate(longitude, -180, 180)

  request <- httr2::request("https://epqs.nationalmap.gov/v1/json") |>
    httr2::req_url_query(
      x = sprintf("%.10f", longitude),
      y = sprintf("%.10f", latitude),
      wkid = 4326,
      units = units,
      includeDate = "false"
    ) |>
    httr2::req_user_agent(
      "streamMetabolizer (https://github.com/ConnorB/streamMetabolizer)"
    ) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504)
      }
    )

  response <- tryCatch(
    request |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(cnd) {
      .cli_abort(
        "Failed to query the USGS Elevation Point Query Service.",
        parent = cnd
      )
    }
  )

  elevation <- suppressWarnings(as.numeric(response$value))

  if (length(elevation) != 1L || is.na(elevation) || elevation <= -999999) {
    .cli_abort(c(
      "The USGS Elevation Point Query Service returned no elevation for latitude {.val {latitude}} and longitude {.val {longitude}}.",
      i = "The service only covers the United States and its territories."
    ))
  }

  elevation
}

check_coordinate <- function(x, min, max, arg = rlang::caller_arg(x)) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x < min ||
      x > max
  ) {
    .cli_abort(
      "{.arg {arg}} must be a single finite number between {min} and {max}.",
      call = rlang::caller_env()
    )
  }

  invisible(x)
}

normalize_epqs_units <- function(units, arg = rlang::caller_arg(units)) {
  if (!is.character(units) || length(units) != 1L || is.na(units)) {
    .cli_abort(
      "{.arg {arg}} must be a single string.",
      call = rlang::caller_env()
    )
  }

  switch(
    tolower(units),
    m = ,
    meters = "Meters",
    ft = ,
    feet = "Feet",
    .cli_abort(
      "{.arg {arg}} must be one of {.val m}, {.val meters}, {.val ft}, or {.val feet} (case-insensitive), not {.val {units}}.",
      call = rlang::caller_env()
    )
  )
}
