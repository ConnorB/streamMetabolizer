#' Use USGS API (USGS Elevation Point Query Service) to determine approximate
#' local elevation
#'
#' This is meant to supply an APPROXIMATE elevation, with no guarantees on
#' precision or on the lifetime of the API service used by the function. The
#' lookup uses the JSON response from the service.
#'
#' @param latitude degrees latitude (positive for north) of the location to look
#'   up.
#' @param longitude degrees longitude (positive for east) of the location to
#'   look up.
#' @param units character, one of Meters or Feet, specifying the units in which
#'   to return the elevation
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
#'   units = "Feet"
#' )
#' elevation_ft
#' @export
lookup_usgs_elevation <- function(
  latitude,
  longitude,
  units = c("Meters", "Feet")
) {
  units <- match.arg(units)

  api.url <- sprintf(
    paste0(
      "https://epqs.nationalmap.gov/v1/json?",
      "x=%.10f&y=%.10f&wkid=4326&units=%s&includeDate=false"
    ),
    longitude,
    latitude,
    units
  )
  response <- read_epqs_json(api.url)
  elevation <- as.numeric(response$value)

  if (length(elevation) != 1L || is.na(elevation)) {
    stop("the USGS Elevation Point Query Service returned no elevation")
  }

  elevation
}

read_epqs_json <- function(url) {
  jsonlite::fromJSON(url)
}
