test_that("CLI helpers preserve diagnostic text and condition classes", {
  error_condition <- tryCatch(
    .cli_abort("bad ", "{input}", call. = FALSE),
    error = identity
  )
  expect_s3_class(error_condition, "rlang_error")
  expect_equal(conditionMessage(error_condition), "bad {input}")
  expect_null(conditionCall(error_condition))

  warning_condition <- NULL
  withCallingHandlers(
    .cli_warn(simpleWarning("careful")),
    warning = function(condition) {
      warning_condition <<- condition
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(warning_condition, "rlang_warning")
  expect_equal(conditionMessage(warning_condition), "careful")

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
      .cli_inform("hello", .class = "packageStartupMessage")
    )
  )
})
