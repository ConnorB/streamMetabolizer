test_that("plot_metab_preds drops entirely missing series", {
  skip_if_not_installed("ggplot2")
  dates <- as.Date(c("2020-01-01", "2020-01-02"))
  predictions <- tibble::tibble(
    date = dates,
    GPP = c(NA_real_, NA_real_),
    GPP.lower = c(NA_real_, NA_real_),
    GPP.upper = c(NA_real_, NA_real_),
    ER = c(-1, -2),
    ER.lower = c(-1.5, -2.5),
    ER.upper = c(-0.5, -1.5)
  )

  plot <- plot_metab_preds(predictions)

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(plot$data$as), "ER")
  expect_equal(plot$data$date, dates)
  expect_equal(plot$data$fit, predictions$ER)
})
