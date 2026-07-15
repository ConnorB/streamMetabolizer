#' Convert UTC to local time
#'
#' Convert time from UTC to local time, either standard or with daylight
#' savings. Recommended for post-analysis visualization only; most functions in
#' streamMetabolizer use times in UTC. If you know the timezone code for your
#' local site, use [lubridate::with_tz()] instead.
#'
#' @param date.time POSIXct object the date and time in UTC.
#' @param latitude Numeric, in degrees, either positive and unitted ("degN" or
#'   "degS") or with sign indicating direction (positive = North).
#' @param longitude Numeric, in degrees, either positive and unitted ("degE" or
#'   "degW") or with sign indicating direction (positive = East).
#' @param time.type Character. The type of time zone desired - either standard
#'   time without any daylight savings time or daylight time where daylight
#'   savings is on during the appropriate days.
#' @returns A `POSIXct` vector in the requested local time zone.
#' @importFrom lubridate with_tz
#' @references
#' https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude
#' @examples
#' utc <- as.POSIXct("2024-01-15 18:00:00", tz = "UTC")
#' convert_UTC_to_localtime(utc, latitude = 41.88, longitude = -87.63)
#' @export
convert_UTC_to_localtime <- function(
  date.time,
  latitude,
  longitude,
  time.type = c("standard local", "daylight local")
) {
  # format checking
  time.type <- match.arg(time.type)
  if (class(date.time)[1] != "POSIXct") {
    .cli_abort("{.arg date.time} must be a {.cls POSIXct} vector.")
  }
  if (!(tz(date.time) %in% c("GMT", "Etc/GMT-0", "Etc/GMT+0", "UTC"))) {
    .cli_abort("{.arg date.time} must use the {.val UTC} time zone.")
  }

  # ask the cache and/or Google for the timezone at these coordinates
  tz_info <- lookup_timezone(latitude, longitude)

  # return in specified format
  if (time.type == "daylight local") {
    lubridate::with_tz(date.time, tz_info$tz)
  } else {
    # "POSIX has positive signs west of Greenwich" - https://opensource.apple.com/source/system_cmds/system_cmds-230/zic.tproj/datfiles/etcetera
    std.tz <- sprintf(
      "Etc/GMT%s%d",
      if (tz_info$std_offset > 0) "-" else "+",
      abs(as.numeric(tz_info$std_offset))
    )
    if (std.tz %in% c("Etc/GMT+0", "Etc/GMT-0")) {
      std.tz <- "UTC"
    }
    lubridate::with_tz(date.time, std.tz)
  }
}

#' Convert local time to UTC
#'
#' Convert time from local time (either standard or with daylight savings) to
#' UTC.
#'
#' @param local.time POSIXct date+time of interest, already in local time as
#'   specified by the tz attribute.
#' @returns A `POSIXct` vector in UTC.
#' @importFrom lubridate with_tz
#' @references
#' https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude
#' @examples
#' local <- as.POSIXct("2024-01-15 12:00:00", tz = "America/Chicago")
#' convert_localtime_to_UTC(local)
#' @export
convert_localtime_to_UTC <- function(local.time) {
  return(with_tz(local.time, "UTC"))
}
