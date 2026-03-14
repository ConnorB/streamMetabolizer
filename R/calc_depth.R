#' Estimate depth from discharge and hydraulic geometry coefficients
#' 
#' Uses the relationship \eqn{d=c*Q^f} (parameter names and definitions as in 
#' Leopold and Maddock, 1953; default values for c and f as in Raymond et al. 
#' 2012)
#' 
#' @param Q discharge (m^3 s^-1)
#' @param c coefficient representing depth at unit discharge (usually m)
#' @param f exponent in depth-discharge relation (unitless)
#' @return d, stream depth, in the same units as c
#' @examples
#' Qs <- seq(1,9,2)
#' calc_depth(Q=Qs)
#' calc_depth(Q=Qs, f=0.4)
#' #' @references Raymond, Peter A., Christopher J. Zappa, David Butman, Thomas L. 
#'   Bott, Jody Potter, Patrick Mulholland, Andrew E. Laursen, William H. 
#'   McDowell, and Denis Newbold. \emph{Scaling the gas transfer velocity and 
#'   hydraulic geometry in streams and small rivers}. Limnology & Oceanography: 
#'   Fluids & Environments 2 (2012): 41-53.
#'   
#'   Leopold, L.B., and Thomas Maddock Jr. \emph{The Hydraulic Geometry of
#'   Stream Channels and Some Physiographic Implications}. Report. Professional
#'   Paper, 1953. USGS Publications Warehouse.
#'   https://pubs.er.usgs.gov/publication/pp252.
#'   
#' @export
calc_depth <- function(Q, c=0.409, f=0.294) {
  c * Q ^ f
}
