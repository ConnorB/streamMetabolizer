#' Calculates the time of sunrise and sunset
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `calc_sun_rise_set()` is deprecated.
#'
#' Calculates the time of sunrise and sunset based on latitude and date.
#'
#' @param date Vector of dates in `Date` format.
#' @param latitude Single latitude value of site. South should be negative,
#'   North positive
#' @return data.frame of sunrise and sunset (apparent solar time, nominally UTC)
#' @importFrom LakeMetabolizer sun.rise.set
#' @importFrom lubridate force_tz
#' @keywords internal
#' @examples
#' calc_sun_rise_set(latitude=40.75,
#'   date=as.POSIXlt(c('2013-03-31', '2017-07-01')))
#' @seealso [calc_is_daytime]
#' @export
calc_sun_rise_set <- function(date, latitude) {
  lifecycle::deprecate_warn("0.13.0", "calc_sun_rise_set()")

  app.solar.time <- as.POSIXct(
    strftime(date, "%Y-%m-%d 0"),
    format = "%Y-%m-%d %H"
  ) # use local tz, as LakeMetabolizer does
  sun.rise.set <- LakeMetabolizer::sun.rise.set(app.solar.time, latitude)
  # reformat
  data.frame(
    sunrise = lubridate::force_tz(sun.rise.set[, 1], tz = "UTC"),
    sunset = lubridate::force_tz(sun.rise.set[, 2], tz = "UTC")
  )
}
