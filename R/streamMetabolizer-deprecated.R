#' Deprecated functions in streamMetabolizer
#'
#' These functions are provided for compatibility with older versions of
#' streamMetabolizer only, and may be defunct as soon as the next
#' release.
#'
#' * [calc_DO_deficit()] is superseded by subtracting `DO.obs` from the output
#'   of [calc_DO_sat()].
#' * [calc_DO_at_sat()] is superseded by [calc_DO_sat()].
#' * [calc_is_daytime()] is deprecated.
#' * [calc_sun_rise_set()] is deprecated.
#'
#' These functions are defunct and will error when called:
#'
#' * `lookup_google_timezone()` is superseded by [lookup_timezone()].
#'
#' @name streamMetabolizer-deprecated
NULL
