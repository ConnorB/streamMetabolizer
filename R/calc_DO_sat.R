#' Calculate equilibrium oxygen saturation
#'
#' @param temp.water A numeric vector of water temperature in degrees Celsius.
#' @param pressure.air Barometric pressure in millibars.
#' @param salinity.water A numeric vector of salinity in PSU. Defaults to zero.
#' @param model A string specifying the saturation model. One of
#'   `"garcia-benson"`, `"garcia"`, `"weiss"`, or `"benson"`;
#'   `"garcia-benson"` is recommended.
#' @param ... Additional parameters passed to
#'   `LakeMetabolizer::o2.at.sat.base`.
#' @returns A numeric vector of dissolved oxygen equilibrium saturation
#'   concentrations, in mg/L.
#'
#' @importFrom LakeMetabolizer o2.at.sat.base
#' @examples
#' calc_DO_sat(temp.water = 21, pressure.air = 1000.1, salinity.water = 0)
#' @export
calc_DO_sat <- function(
  temp.water,
  pressure.air,
  salinity.water = 0,
  model = "garcia-benson",
  ...
) {
  LakeMetabolizer::o2.at.sat.base(
    temp = temp.water,
    baro = pressure.air,
    salinity = salinity.water,
    model = model,
    ...
  )
}

#' Calculate equilibrium oxygen saturation (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `calc_DO_at_sat()` was deprecated in streamMetabolizer 0.12.0. Use
#' [calc_DO_sat()] instead.
#'
#' @inheritParams calc_DO_sat
#' @returns A numeric vector of dissolved oxygen equilibrium saturation
#'   concentrations, in mg/L.
#' @keywords internal
#' @examples
#' calc_DO_at_sat(temp.water = 21, pressure.air = 1000.1, salinity.water = 0)
#' @export
calc_DO_at_sat <- function(
  temp.water,
  pressure.air,
  salinity.water = 0,
  model = "garcia-benson",
  ...
) {
  lifecycle::deprecate_warn("0.12.0", "calc_DO_at_sat()", "calc_DO_sat()")

  calc_DO_sat(
    temp.water = temp.water,
    pressure.air = pressure.air,
    salinity.water = salinity.water,
    model = model,
    ...
  )
}
