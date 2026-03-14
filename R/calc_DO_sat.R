#' Calculates the equilibrium saturation concentration of oxygen in water at the
#' supplied conditions
#' 
#' @md
#' @param temp.water a numeric vector of water temperature in degrees Celsius, 
#'   or a \linkS4class{unitted} object of water temperatures.
#' @param pressure.air barometric pressure in millibars, or a 
#'   \linkS4class{unitted} object of barometric pressure.
#' @param salinity.water a numeric vector of salinity in PSU, or a 
#'   \linkS4class{unitted} object of salinity. Defaults to zero.
#' @param model character. One of 'garcia-benson', 'garcia', 'weiss', or
#'   'benson', but 'garcia-benson' is recommended.
#' @param ... additional parameters passed to 
#'   `LakeMetabolizer::o2.at.sat.base`
#' @return a numeric vector of dissolved oxygen equilibrium saturation 
#'   concentrations, in mg/L, with units attached if any of the input vectors 
#'   are unitted.
#'   
#' @importFrom LakeMetabolizer o2.at.sat.base
#' @examples
#' calc_DO_sat(temp=21, press=1000.1, sal=0)
#' @export
calc_DO_sat <- calc_DO_at_sat <- function(temp.water, pressure.air, salinity.water=0, model='garcia-benson', ...){

  if(as.character(sys.call()[[1]]) == 'calc_DO_at_sat') {
    .Deprecated('calc_DO_sat')
  }

  LakeMetabolizer::o2.at.sat.base(temp=temp.water, baro=pressure.air, salinity=salinity.water, model=model, ...)
}
