cmdstan_is_available <- function() {
  if (!requireNamespace('cmdstanr', quietly = TRUE)) {
    return(FALSE)
  }
  version <- tryCatch(
    cmdstanr::cmdstan_version(error_on_NA = FALSE),
    error = \(e) NULL
  )
  !is.null(version) && length(version) == 1 && !is.na(version)
}

skip_if_no_cmdstan <- function() {
  testthat::skip_if_not(cmdstan_is_available(), 'CmdStan is not configured')
}

skip_if_no_rstan <- function() {
  testthat::skip_if_not(
    rstan_is_available(),
    'RStan and its compilation dependencies are not installed'
  )
}

skip_if_no_rstan_compilation <- function() {
  testthat::skip_if_not(
    rstan_compilation_is_available(),
    paste(
      'RStan C++ compilation is unavailable or intentionally skipped',
      'on R-devel'
    )
  )
}

rstan_is_available <- function() {
  packages <- c(
    'rstan',
    'StanHeaders',
    'Rcpp',
    'RcppEigen',
    'RcppParallel',
    'BH'
  )
  all(vapply(
    packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  ))
}

rstan_compilation_is_available <- function(
  r_version_string = R.version.string
) {
  rstan_is_available() &&
    !grepl('Under development', r_version_string, fixed = TRUE)
}

stan_engine_for_tests <- function() {
  if (cmdstan_is_available()) {
    return('cmdstanr')
  }
  if (rstan_compilation_is_available()) {
    return('rstan')
  }
  testthat::skip(
    paste(
      'neither CmdStanR nor an RStan configuration suitable for C++',
      'compilation is available'
    )
  )
}
