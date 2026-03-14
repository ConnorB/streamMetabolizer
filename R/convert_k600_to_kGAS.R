#' Returns the gas exchange velocity for gas of interest w/ no unit conversions
#' 
#' @param k600 k600 as vector of numbers or single number
#' @param temperature Water temperature (deg C) as vector array of numbers or single number
#' @param gas gas for conversion, as string (e.g., 'CO2' or 'O2')
#' @return Numeric value of gas exchange velocity for gas
#' 
#' @importFrom LakeMetabolizer k600.2.kGAS.base
#' @export
convert_k600_to_kGAS = function(k600, temperature, gas="O2") {
  # suppressing "In getSchmidt(temperature, gas) : temperature out of range" b/c it's way too common
  out <- suppressWarnings(LakeMetabolizer::k600.2.kGAS.base(k600, temperature, gas))

  # catch extreme values of temperature or kGAS that are so wild they're making NaNs
  bad_nans <- which(!is.na(k600) & !is.na(temperature) & is.nan(out))
  if(length(bad_nans) > 0) {
    warning('one or more k600 or temperature values are extreme, causing NaNs in the k600-kGAS conversion')
  }

  out
}

#' Returns the gas exchange velocity as k600 for gas of interest w/ no unit conversions
#' 
#' @param kGAS k of gas as vector of numbers or single number
#' @param temperature Water temperature (deg C) as vector array of numbers or single number
#' @param gas gas for conversion, as string (e.g., 'CO2' or 'O2')
#' @return Numeric value of gas exchange velocity for gas
#' 
#' @importFrom LakeMetabolizer k600.2.kGAS.base
#' @export
convert_kGAS_to_k600 = function(kGAS, temperature, gas="O2") {
  # suppressing "In getSchmidt(temperature, gas) : temperature out of range" b/c it's way too common
  conversion <- 1/suppressWarnings(LakeMetabolizer::k600.2.kGAS.base(1, temperature, gas))

  # catch extreme values of temperature or kGAS that are so wild they're making NaNs
  bad_nans <- which(!is.na(temperature) & is.nan(conversion))
  if(length(bad_nans) > 0) {
    warning('one or more temperature values are extreme, causing NaNs in the kGAS-k600 conversion')
  }

  kGAS * conversion
}
