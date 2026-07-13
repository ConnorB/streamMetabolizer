test_that("calc_DO_at_sat() is deprecated", {
  local_edition(3)
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot(. <- calc_DO_at_sat(21, 1013.25))
})

test_that("calc_DO_deficit() is deprecated", {
  local_edition(3)
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot(. <- calc_DO_deficit(7, 25, 900))
})

test_that("calc_is_daytime() is deprecated", {
  local_edition(3)
  withr::local_options(lifecycle_verbosity = "warning")
  datetime <- as.POSIXct("2013-03-31 11:00", tz = "UTC")

  expect_snapshot(. <- calc_is_daytime(datetime, 40.75))
})

test_that("calc_sun_rise_set() is deprecated", {
  local_edition(3)
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot(. <- calc_sun_rise_set(as.Date("2013-03-31"), 40.75))
})
