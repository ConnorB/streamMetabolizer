.cli_message <- function(...) {
  pieces <- lapply(list(...), function(piece) {
    if (inherits(piece, "condition")) {
      conditionMessage(piece)
    } else {
      as.character(piece)
    }
  })

  paste0(unlist(pieces, use.names = FALSE), collapse = "")
}

.cli_abort <- function(..., call. = TRUE, domain = NULL) {
  message <- .cli_message(...)
  caller <- parent.frame()

  cli::cli_abort(
    "{message}",
    call = if (call.) caller else NULL,
    .envir = environment()
  )
}

.cli_warn <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL) {
  message <- .cli_message(...)

  cli::cli_warn("{message}", .envir = environment())
}

.cli_inform <- function(..., appendLF = TRUE, domain = NULL, .class = NULL) {
  message <- .cli_message(...)

  cli::cli_inform(
    "{message}",
    class = .class,
    .envir = environment()
  )
}
