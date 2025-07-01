#' Look for a model file
#' 
#' Looks first in the models folder of the streamMetabolizer package, second 
#' along the relative or absolute file path given by model_name
#' 
#' @param model_name a model file in the 'models' folder of the 
#'   streamMetabolizer package or a relative or absolute file path of a model 
#'   file
#' @return a file path if the file exists or an error otherwise
#' @keywords internal
mm_locate_filename <- function(model_name) {
  package_dir <- system.file("models", package = "streamMetabolizer")
  package_path <- file.path(package_dir, model_name)
  deprecated_path <- file.path(package_dir, "deprecated", model_name)
  other_path <- model_name
  
  # Get version of either backend, if available
  stan_version <- tryCatch({
    if (requireNamespace("cmdstanr", quietly = TRUE) && !is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
      as.numeric_version(cmdstanr::cmdstan_version())
    } else if (requireNamespace("rstan", quietly = TRUE)) {
      as.numeric_version(rstan::stan_version())
    } else {
      NA
    }
  }, error = function(e) NA)
  
  # If engine is detected and version < 2.26.0, use deprecated model if available
  if (!is.na(stan_version) && stan_version < "2.26.0" && file.exists(deprecated_path)) {
    return(deprecated_path)
  }
  
  # Normal fallback paths
  if (file.exists(package_path)) return(package_path)
  if (file.exists(other_path)) return(other_path)
  # Final fallback: warn if engine is unknown
  if (is.na(stan_version)) {
    warning("Neither RStan or CmdStanR Detected")
  }
  stop("Could not locate the model file at ", file.path(package_dir, model_name), " or ", other_path)
}
