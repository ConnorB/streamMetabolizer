test_that("append_plot_gaps adds one missing row after each date", {
  dates <- as.Date(c("2020-01-01", "2020-01-02"))
  data <- tibble::tibble(
    date = rep(dates, each = 2),
    solar.time = as.POSIXct("2020-01-01", tz = "UTC") +
      c(0, 1, 86400, 86401),
    pure = 1:4,
    mod = 5:8,
    obs = 9:12
  )

  result <- append_plot_gaps(data)

  expect_equal(result$date, rep(dates, each = 3))
  expect_equal(result$solar.time[c(3, 6)], data$solar.time[c(2, 4)])
  expect_equal(
    unname(is.na(unlist(result[c(3, 6), c("pure", "mod", "obs")]))),
    rep(TRUE, 6)
  )
  expect_equal(
    result[-c(3, 6), names(data)],
    data,
    ignore_attr = TRUE
  )
})
