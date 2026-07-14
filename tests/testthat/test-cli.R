test_that("CLI helpers preserve diagnostic text and condition classes", {
  error_condition <- tryCatch(
    .cli_abort("bad {{input}}", call = NULL),
    error = identity
  )
  expect_s3_class(error_condition, "rlang_error")
  expect_equal(conditionMessage(error_condition), "bad {input}")
  expect_null(conditionCall(error_condition))

  warning_condition <- NULL
  withCallingHandlers(
    .cli_warn("careful"),
    warning = function(condition) {
      warning_condition <<- condition
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(warning_condition, "rlang_warning")
  expect_equal(trimws(conditionMessage(warning_condition)), "careful")

  message_condition <- NULL
  withCallingHandlers(
    .cli_inform("hello"),
    message = function(condition) {
      message_condition <<- condition
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(message_condition, "rlang_message")
  expect_equal(conditionMessage(message_condition), "hello")
})

test_that("startup messages remain suppressible", {
  expect_silent(
    suppressPackageStartupMessages(
      .cli_inform("hello", class = "packageStartupMessage")
    )
  )
})

test_that("CLI helpers support semantic messages", {
  input <- "temperature"
  n <- 2

  expect_snapshot(error = TRUE, {
    .cli_abort(
      c(
        "Invalid input",
        "x" = "{.arg {input}} must be numeric",
        "i" = "Found {n} invalid value{?s}"
      ),
      call = NULL
    )
  })
  expect_snapshot(.cli_warn("Column {.var {input}} contains missing values"))
  expect_snapshot(.cli_inform(c("v" = "Processed {n} row{?s}")))
})
