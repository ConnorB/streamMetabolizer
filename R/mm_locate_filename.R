#' Look for a model file
#'
#' Looks first in the models folder of the streamMetabolizer package, second
#' along the relative or absolute file path given by model_name
#'
#' @param model_name a model file in the 'models' folder of the
#'   streamMetabolizer package or a relative or absolute file path of a model
#'   file
#' @param stan_engine The Stan interface whose compiler version should be used
#'   when choosing between current and deprecated model syntax. If `NULL`,
#'   CmdStanR is preferred when available, followed by RStan.
#' @return a file path if the file exists or an error otherwise
#' @keywords internal
mm_locate_filename <- function(model_name, stan_engine = NULL) {
  package_dir <- system.file("models", package = "streamMetabolizer")
  package_path <- file.path(package_dir, model_name)
  deprecated_path <- file.path(package_dir, "deprecated", model_name)
  other_path <- model_name

  stan_version <- stan_version_for_engine(stan_engine)

  # If engine is detected and version < 2.26.0, use deprecated model if available
  if (
    !is.na(stan_version) &&
      stan_version < "2.26.0" &&
      file.exists(deprecated_path)
  ) {
    return(deprecated_path)
  }

  # Normal fallback paths
  if (file.exists(package_path)) {
    return(package_path)
  }
  if (file.exists(other_path)) {
    return(other_path)
  }
  # Final fallback: warn if engine is unknown
  if (is.na(stan_version)) {
    .cli_warn(
      "Neither RStan nor a configured CmdStanR installation was detected"
    )
  }
  .cli_abort(
    "Could not locate the model file at ",
    file.path(package_dir, model_name),
    " or ",
    other_path
  )
}

#' Find the Stan compiler version used by an interface
#'
#' @inheritParams mm_locate_filename
#' @return A numeric version, or `NA` when the requested interface is not
#'   available and configured.
#' @keywords internal
stan_version_for_engine <- function(stan_engine = NULL) {
  engines <- if (is.null(stan_engine)) {
    c('cmdstanr', 'rstan')
  } else {
    match.arg(stan_engine, c('rstan', 'cmdstanr'))
  }

  for (engine in engines) {
    version <- tryCatch(
      switch(
        engine,
        cmdstanr = {
          if (!requireNamespace('cmdstanr', quietly = TRUE)) {
            NULL
          } else {
            cmdstanr::cmdstan_version(error_on_NA = FALSE)
          }
        },
        rstan = {
          if (!requireNamespace('rstan', quietly = TRUE)) {
            NULL
          } else {
            rstan::stan_version()
          }
        }
      ),
      error = function(e) NULL
    )
    if (!is.null(version) && length(version) == 1 && !is.na(version)) {
      return(as.numeric_version(version))
    }
  }

  NA
}
