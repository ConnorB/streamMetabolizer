#' Deprecated: Calculate a vector of dissolved oxygen deficits
#'
#' Deprecated: Submit a GitHub issue if you want calc_DO_deficit() to stick
#' around. Creates a DO.deficit vector for input into metabolism models.
#'
#' @md
#' @param DO.obs a numeric vector of dissolved oxygen concentration
#'   observations, mgO2 L^-1.
#' @param temp.water a numeric vector of water temperature in degrees Celsius.
#' @param pressure.air barometric pressure in millibars.
#' @param salinity.water a numeric vector of salinity in PSU. Defaults to zero. Length must be
#'   one or equal to length of \code{temp.water}.
#' @param ... additional parameters passed to
#'   `LakeMetabolizer::o2.at.sat.base`
#' @return a vector of DO.deficit values
#' @examples
#' \dontrun{
#' # Warning: this function is deprecated.
#' calc_DO_deficit(DO.obs=7, temp.water=25, pressure.air=900, salinity.water=2.43)
#' }
#' @export
calc_DO_deficit <- function(
  DO.obs,
  temp.water,
  pressure.air,
  salinity.water = 0,
  ...
) {
  .Deprecated()
  warning("submit a GitHub issue if you want calc_DO_deficit() to stick around")

  DO.equil <- calc_DO_sat(temp.water, pressure.air, salinity.water, ...)

  # to do: verify incoming units (convert if needed?) and set DO.equil units to mgO2 L^-1
  DO.deficit <- DO.equil - DO.obs

  return(DO.deficit)
}
