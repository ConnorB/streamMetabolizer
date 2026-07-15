#' Estimate depth from discharge and hydraulic geometry coefficients
#'
#' Uses the relationship d = c × Qᶠ (parameter names and definitions as in
#' Leopold and Maddock, 1953; default values for c and f as in Raymond et al.
#' 2012)
#'
#' @param Q Discharge (m^3 s^-1).
#' @param c Coefficient representing depth at unit discharge (usually m).
#' @param f Exponent in depth-discharge relation (unitless).
#' @returns D, stream depth, in the same units as c.
#' @examples
#' Qs <- seq(1,9,2)
#' calc_depth(Q=Qs)
#' calc_depth(Q=Qs, f=0.4)
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
calc_depth <- function(Q, c = 0.409, f = 0.294) {
  c * Q^f
}
