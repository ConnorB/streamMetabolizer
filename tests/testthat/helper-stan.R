cmdstan_is_available <- function() {
  if (!requireNamespace('cmdstanr', quietly = TRUE)) {
    return(FALSE)
  }
  version <- tryCatch(
    cmdstanr::cmdstan_version(error_on_NA = FALSE),
    error = function(e) NULL
  )
  !is.null(version) && length(version) == 1 && !is.na(version)
}

skip_if_no_cmdstan <- function() {
  testthat::skip_if_not(cmdstan_is_available(), 'CmdStan is not configured')
}

skip_if_no_rstan <- function() {
  testthat::skip_if_not_installed('rstan')
}

stan_engine_for_tests <- function() {
  if (cmdstan_is_available()) {
    return('cmdstanr')
  }
  if (requireNamespace('rstan', quietly = TRUE)) {
    return('rstan')
  }
  testthat::skip('neither CmdStanR nor RStan is available')
}
