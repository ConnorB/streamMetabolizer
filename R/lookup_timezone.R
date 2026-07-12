#' Determine the local time zone from the coordinates
#'
#' Uses the `lutz` package to determine the local timezone name, standard
#' offset, and DST offset of a site from its coordinates.
#'
#' @param latitude degrees latitude (positive for north) of the location to look
#'   up.
#' @param longitude degrees longitude (positive for east) of the location to
#'   look up.
#' @importFrom lutz tz_lookup_coords
#' @export
#' @examples
#' lookup_timezone(41.33, -106.3)
lookup_timezone <- function(latitude, longitude) {
  # check the cache first
  lookup_key <- sprintf("%.10f,%.10f", latitude, longitude)
  tz_info <- pkg.env$tz_lookups[[lookup_key]]

  if (is.null(tz_info)) {
    tz_name <- lutz::tz_lookup_coords(latitude, longitude, method = "accurate")
    if (is.na(tz_name) || tz_name == "") {
      stop("sorry, could not find time zone for specified lat/long")
    }

    # compute standard UTC offset using a non-DST reference date
    ref_time <- if (latitude >= 0) {
      as.POSIXct("2015-01-01 12:00:00", tz = "UTC")
    } else {
      as.POSIXct("2015-07-01 12:00:00", tz = "UTC")
    }
    local <- as.POSIXlt(ref_time, tz = tz_name)
    std_offset <- local$gmtoff / 3600

    tz_info <- list(
      tz = tz_name,
      dst_offset = 0,
      std_offset = std_offset,
      retry = 0
    )
    pkg.env$tz_lookups[[lookup_key]] <- tz_info
  }

  tz_info
}

#' Use Google API to determine local time zone
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been replaced by [lookup_timezone()], which uses
#' the `lutz` package for offline timezone lookup instead of the Google API.
#'
#' @inheritParams lookup_timezone
#' @param timestamp Ignored. Kept for backward compatibility.
#' @keywords internal
lookup_google_timezone <- function(latitude, longitude, timestamp = NULL) {
  lifecycle::deprecate_stop(
    when = "0.13",
    what = "lookup_google_timezone()",
    with = "lookup_timezone()",
    details = paste(
      "This function has been replaced by lookup_timezone(),",
      "which uses the lutz package for offline timezone lookup",
      "instead of the Google API."
    )
  )
}
