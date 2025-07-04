#' Use an engine-specific function to check the model syntax
#'
#' @param model_file the file path of the model file to check; the extension
#'   will be used to determine which engine to use for checking.
#' @keywords internal
mm_check_mcmc_file <- function(model_file) {
  engine <- mm_parse_name(model_file)$engine
  if(!file.exists(model_file)) model_file <- paste0('inst/models/', model_file)
  if(engine != 'stan') stop('need to add handling for engines other than stan')

  # stan() can't find its own function cpp_object_initializer() unless the
  # namespace is loaded. requireNamespace is somehow not doing this. Thoughts
  # (not solution):
  # https://stat.ethz.ch/pipermail/r-devel/2014-September/069803.html
  if(!requireNamespace("rstan", quietly = TRUE)) {
    stop("the rstan package is required to check Stan MCMC models")
  }
  model_status <- tryCatch({
    rstan::stan_model(file=model_file)
    return("correct")
  }, error=function(e) {
    e$message
  })
  model_status
}

#' Check the syntax of all Bayesian model files in the package
#'
#' @param grep_pattern string on which to filter the names if only some should
#'   be checked. fixed=FALSE.
#' @examples
#' \dontrun{
#' # takes a long time, so run only when needed
#' checks <- streamMetabolizer:::mm_check_mcmc_files()
#' saveRDS(checks, file='temp/bayes_model_checks.Rds')
#' checks <- streamMetabolizer:::mm_check_mcmc_files("*ko\\.stan")
#' checks <- streamMetabolizer:::mm_check_mcmc_files("b_np_.*_ko\\.stan")
#' cat(checks[[7]])
#' }
#' @keywords internal
mm_check_mcmc_files <- function(grep_pattern) {
  model_files <- mm_valid_names(type='bayes')
  if(!missing(grep_pattern)) {
    model_files <- grep(grep_pattern, model_files, value=TRUE)
  }
  sapply(setNames(model_files, model_files), function(m) {
    message("checking ", m, "...", appendLF=FALSE)
    model_status <- mm_check_mcmc_file(m)
    if(model_status != "correct") {
      message("found a problem.")
    } else {
      message("OK!")
    }
    model_status
  })
}
