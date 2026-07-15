#' Convert a date to a day of year (1-366) with decimal hours
#'
#' Inspired by / copied from LakeMetabolizer date2doy
#'
#' @param date A datetime object as POSIXct or POSIXt.
#' @returns A number expressing the date as days, including fractional days,
#'   since 00:00 on December 31 of the preceding year (that is, January 1 at
#'   00:01 is approximately 1.01).
#' @examples
#' streamMetabolizer:::convert_date_to_doyhr(as.POSIXct("2015-02-03 12:01:00 UTC"))
#' @keywords internal
convert_date_to_doyhr <- function(date) {
  year <- as.POSIXct(
    format(date, "%Y-01-01 00:00:00"),
    tz = lubridate::tz(date)
  )
  out <- as.numeric(date - year, units = "days") + 1 # days_since_dec31
  out
}

#' Convert a decimal day of year to a date
#'
#' @param doyhr Numeric value expressing the date as the number of days, with
#'   decimal hours, since 00:00 of December 31 of the preceding year.
#' @param year Numeric 4-digit year.
#' @param tz The time zone to pass to as.POSIXct().
#' @param origin The origin to pass to as.POSIXct().
#' @param ... Other arguments to pass to as.POSIXct().
#' @returns A datetime object as POSIXct.
#' @examples
#' streamMetabolizer:::convert_doyhr_to_date(34.500695, 2015)
#' @keywords internal
convert_doyhr_to_date <- function(
  doyhr,
  year,
  tz = "UTC",
  origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
  ...
) {
  secs_since_jan1 <- (doyhr - 1) * 24 * 60 * 60
  out <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", year),
    format = "%Y-%m-%d %H:%M:%S",
    tz = tz,
    origin = origin,
    ...
  ) +
    secs_since_jan1
  out
}
