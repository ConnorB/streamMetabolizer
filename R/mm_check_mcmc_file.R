#' Use an engine-specific function to check the model syntax
#'
#' @param model_file the file path of the model file to check; the extension
#'   will be used to determine which engine to use for checking.
#' @param stan_engine Character string specifying whether to check with RStan or
#'   CmdStanR.
#' @keywords internal
mm_check_mcmc_file <- function(
  model_file,
  stan_engine = c('rstan', 'cmdstanr')
) {
  stan_engine <- match.arg(stan_engine)
  engine <- mm_parse_name(model_file)$engine
  if (!file.exists(model_file)) {
    model_file <- mm_locate_filename(model_file, stan_engine = stan_engine)
  }
  if (engine != 'stan') {
    .cli_abort('need to add handling for engines other than stan')
  }

  if (stan_engine == 'rstan' && !requireNamespace('rstan', quietly = TRUE)) {
    .cli_abort('the rstan package is required to check Stan MCMC models')
  }
  if (stan_engine == 'cmdstanr') {
    if (!requireNamespace('cmdstanr', quietly = TRUE)) {
      .cli_abort('the cmdstanr package is required to check Stan MCMC models')
    }
    if (is.na(stan_version_for_engine('cmdstanr'))) {
      .cli_abort(
        'CmdStanR is installed, but CmdStan is not configured. ',
        'Install it with cmdstanr::install_cmdstan() and then retry.'
      )
    }
  }
  model_status <- tryCatch(
    {
      if (stan_engine == 'rstan') {
        stanc_result <- rstan::stanc(file = model_file)
        if (!isTRUE(stanc_result$status)) {
          .cli_abort('RStan could not translate the Stan program')
        }
      } else {
        cmdstan_model <- cmdstanr::cmdstan_model(model_file, compile = FALSE)
        cmdstan_model$check_syntax(quiet = TRUE)
      }
      return("correct")
    },
    error = function(e) {
      e$message
    }
  )
  model_status
}

#' Check the syntax of all Bayesian model files in the package
#'
#' @param grep_pattern string on which to filter the names if only some should
#'   be checked. fixed=FALSE.
#' @inheritParams mm_check_mcmc_file
#' @examples
#' \dontrun{
#' # takes a long time, so run only when needed
#' checks <- streamMetabolizer:::mm_check_mcmc_files()
#' saveRDS(checks, file='temp/bayes_model_checks.Rds')
#' checks <- streamMetabolizer:::mm_check_mcmc_files("*ko\\.stan")
#' checks <- streamMetabolizer:::mm_check_mcmc_files("b_np_.*_ko\\.stan")
#' checks <- streamMetabolizer:::mm_check_mcmc_files(
#'   "b_np_.*_ko\\.stan", stan_engine = "cmdstanr"
#' )
#' cat(checks[[7]])
#' }
#' @keywords internal
mm_check_mcmc_files <- function(
  grep_pattern,
  stan_engine = c('rstan', 'cmdstanr')
) {
  stan_engine <- match.arg(stan_engine)
  model_files <- mm_valid_names(type = 'bayes')
  if (!missing(grep_pattern)) {
    model_files <- grep(grep_pattern, model_files, value = TRUE)
  }
  sapply(setNames(model_files, model_files), function(m) {
    model_status <- mm_check_mcmc_file(m, stan_engine = stan_engine)
    status_message <- if (model_status != "correct") {
      "found a problem."
    } else {
      "OK!"
    }
    .cli_inform("checking ", m, "...", status_message)
    model_status
  })
}
