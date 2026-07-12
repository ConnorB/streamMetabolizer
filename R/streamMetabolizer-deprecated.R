#' Deprecated Functions in package streamMetabolizer
#'
#' These functions are provided for compatibility with older versions of
#' `streamMetabolizer` only, and may be defunct as soon as the next
#' release.
#'
#' \itemize{
#'   \item [calc_DO_deficit()] - instead, subtract `DO.obs` from output of [calc_DO_sat()]
#'   \item `calc_DO_at_sat` - use [calc_DO_sat()] instead
#'   \item [calc_is_daytime()] - if you like and want this function, submit a GitHub issue to keep it
#'   \item [calc_sun_rise_set()] - if you like and want this function, submit a GitHub issue to keep it
#' }
#'
#' These functions are defunct and will error when called:
#'
#' \itemize{
#'   \item `lookup_google_timezone` - use [lookup_timezone()] instead
#' }
#'
#' @name streamMetabolizer-deprecated
NULL
