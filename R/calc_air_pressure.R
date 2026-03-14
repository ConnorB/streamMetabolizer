#' Calculates the average air pressure for a site
#'
#' Estimates air pressure from air temperature and elevation
#'
#' @param temp.air air temperature in degrees C. Default is 15 degC.
#' @param elevation the site elevation above sea level in m. Default is the
#'   rough mean elevation of the USA at 2500 ft (from
#'   http://www.infoplease.com/ipa/A0001792.html)
#' @param attach.units (deprecated, effectively FALSE in future) logical. Should
#'   the returned vector be a unitted object?
#' @return a numeric vector of barometric pressures in mb, with units attached
#'   if requested.
#' @importFrom lifecycle deprecated is_present
#' @examples
#' calc_air_pressure(15, 762)
#' calc_air_pressure(15, 100)
#' @export
calc_air_pressure <- function(temp.air=15, elevation=762, attach.units=deprecated()) {

  # check units-related arguments
  if (lifecycle::is_present(attach.units)) {
    lifecycle::deprecate_warn("0.12.0", "streamMetabolizer::calc_air_pressure(attach.units)")
  }

  # compute pressure. eqn also at https://en.wikipedia.org/wiki/Barometric_formula
  # Pb=760 mmHg (standard pressure), g0=9.80665 m/s^2, M=0.0289644 kg/mol,
  # Rst=8.31447 J/(mol*K), conversion 1.33322368 mb/mmHg
  760 * exp((-9.80665 * 0.0289644 * elevation) / (8.31447 * (273.15 + temp.air))) * 1.33322368
}
