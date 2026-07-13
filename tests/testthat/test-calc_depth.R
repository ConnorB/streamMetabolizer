test_that("calc_depth checks & returns values & units as expected", {
  # basic numbers, with & without defaults
  expect_type(calc_depth(Q = 7.3), "double")
  expect_equal(calc_depth(Q = 7.3, c = 1, f = 0.5), 7.3^0.5)
  expect_equal(
    calc_depth(
      Q = rep(100, 3),
      c = seq(0.4, 0.6, 0.1),
      f = c(0.25, 0.29, 0.33)
    ),
    c(1.26, 1.9, 2.74),
    tolerance = 0.1
  )
})
