.cli_abort <- function(
  message,
  ...,
  call = parent.frame(),
  .envir = parent.frame()
) {
  cli::cli_abort(message, ..., call = call, .envir = .envir)
}

.cli_warn <- function(message, ..., .envir = parent.frame()) {
  cli::cli_warn(message, ..., .envir = .envir)
}

.cli_inform <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(message, ..., .envir = .envir)
}
