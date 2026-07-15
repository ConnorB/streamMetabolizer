#' Convert K600 to a gas-specific exchange velocity
#'
#' @param k600 K600 as vector of numbers or single number.
#' @param temperature A numeric vector of water temperatures in degrees Celsius.
#' @param gas Gas for conversion, as string (e.g., 'CO2' or 'O2').
#' @returns Numeric value of gas exchange velocity for gas.
#'
#' @importFrom LakeMetabolizer k600.2.kGAS.base
#' @examples
#' convert_k600_to_kGAS(10, temperature = 20, gas = "O2")
#' @export
convert_k600_to_kGAS <- function(k600, temperature, gas = "O2") {
  # suppressing "In getSchmidt(temperature, gas) : temperature out of range" b/c it's way too common
  out <- suppressWarnings(LakeMetabolizer::k600.2.kGAS.base(
    k600,
    temperature,
    gas
  ))

  # catch extreme values of temperature or kGAS that are so wild they're making NaNs
  bad_nans <- which(!is.na(k600) & !is.na(temperature) & is.nan(out))
  if (length(bad_nans) > 0) {
    .cli_warn(
      "One or more extreme {.arg k600} or {.arg temperature} values produced {.val NaN} in the k600-to-kGAS conversion."
    )
  }

  out
}

#' Convert a gas-specific exchange velocity to K600
#'
#' @param kGAS K of gas as vector of numbers or single number.
#' @param temperature A numeric vector of water temperatures in degrees Celsius.
#' @param gas Gas for conversion, as string (e.g., 'CO2' or 'O2').
#' @returns Numeric value of gas exchange velocity for gas.
#'
#' @importFrom LakeMetabolizer k600.2.kGAS.base
#' @examples
#' convert_kGAS_to_k600(8, temperature = 20, gas = "O2")
#' @export
convert_kGAS_to_k600 <- function(kGAS, temperature, gas = "O2") {
  # suppressing "In getSchmidt(temperature, gas) : temperature out of range" b/c it's way too common
  conversion <- 1 /
    suppressWarnings(LakeMetabolizer::k600.2.kGAS.base(1, temperature, gas))

  # catch extreme values of temperature or kGAS that are so wild they're making NaNs
  bad_nans <- which(!is.na(temperature) & is.nan(conversion))
  if (length(bad_nans) > 0) {
    .cli_warn(
      "One or more extreme {.arg temperature} values produced {.val NaN} in the kGAS-to-k600 conversion."
    )
  }

  kGAS * conversion
}
