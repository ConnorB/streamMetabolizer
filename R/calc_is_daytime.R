#' Determines if specified datetime is during the daytime
#' Returns T/F indicating whether a datetime occurs during the daytime (sunlight hours)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `calc_is_daytime()` is deprecated.
#'
#' @param datetimes Vector of dates as `POSIXct` or `POSIXlt` (see [DateTimeClasses()]) format, but in SOLAR time
#' @param lat Single latitude value of site. South should be negative, north positive
#'
#' @return a boolean vector of same length as `datetimes`
#'
#' @author
#' Luke A. Winslow
#' @seealso
#' [calc_sun_rise_set]
#' @importFrom LakeMetabolizer is.day
#' @examples
#' \dontrun{
#' # Warning: this function is deprecated.
#' calc_is_daytime(datetimes=as.POSIXct(paste('2013-03-31', c('1:00','11:00'))), lat=40.75)
#' }
#' @export
calc_is_daytime <- function(datetimes, lat) {
  lifecycle::deprecate_warn("0.13.0", "calc_is_daytime()")

  LakeMetabolizer::is.day(datetimes, lat)
}
