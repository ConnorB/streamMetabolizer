#' Estimate velocity from discharge and hydraulic geometry coefficients
#'
#' Uses the relationship U = k × Qᵐ (parameter names and definitions as in
#' Leopold and Maddock, 1953; default values for k and m as in Raymond et al.
#' 2012)
#'
#' @param Q Discharge (m^3 s^-1).
#' @param k A numeric coefficient representing velocity at unit discharge,
#'   usually in m/s; *e* in Raymond et al. (2012).
#' @param m A numeric exponent in the velocity-discharge relationship;
#'   dimensionless and *f* in Raymond et al. (2012).
#' @returns A numeric vector of streamflow velocities in the same units as `k`.
#' @examples
#' Qs <- seq(1,9,2)
#' calc_velocity(Q=Qs)
#' calc_velocity(Q=Qs, k=0.4)
#' @references Raymond, Peter A., Christopher J. Zappa, David Butman, Thomas L.
#'   Bott, Jody Potter, Patrick Mulholland, Andrew E. Laursen, William H.
#'   McDowell, and Denis Newbold. *Scaling the gas transfer velocity and
#'   hydraulic geometry in streams and small rivers*. Limnology & Oceanography:
#'   Fluids & Environments 2 (2012): 41-53.
#'
#'   Leopold, L.B., and Thomas Maddock Jr. *The Hydraulic Geometry of
#'   Stream Channels and Some Physiographic Implications*. Report. Professional
#'   Paper, 1953. USGS Publications Warehouse.
#'   https://pubs.er.usgs.gov/publication/pp252.
#'
#' @export
calc_velocity <- function(Q, k = 0.194, m = 0.285) {
  k * Q^m
}
