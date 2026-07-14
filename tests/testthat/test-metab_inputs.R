test_that("metab_inputs uses cli for text guidance", {
  expect_snapshot(metab_inputs("night", "specs"))
  expect_snapshot(metab_inputs("night", "data_daily"))
  expect_snapshot(metab_inputs("mle", "info"))
  expect_silent(suppressMessages(metab_inputs("mle", "info")))
  expect_invisible(suppressMessages(metab_inputs("mle", "info")))
})

test_that("metab_inputs returns tabular input requirements", {
  requirements <- metab_inputs("mle", "data")

  expect_s3_class(requirements, "tbl_df")
  expect_named(requirements, c("colname", "class", "units", "need"))
  expect_equal(
    requirements$units,
    c("", "mgO2 L^-1", "mgO2 L^-1", "m", "degC", "umol m^-2 s^-1", "m^3 s^-1")
  )
})
