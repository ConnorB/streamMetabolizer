#' Determine whether datetimes occur during daylight
#'
#' Returns a logical vector indicating whether each datetime occurs during
#' daylight hours.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `calc_is_daytime()` is deprecated.
#'
#' @param datetimes A `POSIXct` or `POSIXlt` vector in solar time. See
#'   [DateTimeClasses()].
#' @param lat A numeric scalar giving the site latitude. Use negative values
#'   south of the equator and positive values north of it.
#'
#' @returns A logical vector with the same length as `datetimes`.
#'
#' @author
#' Luke A. Winslow
#' @seealso
#' [calc_sun_rise_set()]
#' @importFrom LakeMetabolizer is.day
#' @examplesIf interactive()
#' # Warning: this function is deprecated.
#' calc_is_daytime(
#'   datetimes = as.POSIXct(paste("2013-03-31", c("1:00", "11:00"))),
#'   lat = 40.75
#' )
#' @export
calc_is_daytime <- function(datetimes, lat) {
  lifecycle::deprecate_warn("0.13.0", "calc_is_daytime()")

  LakeMetabolizer::is.day(datetimes, lat)
}
