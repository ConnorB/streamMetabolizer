context("calc_DO")

test_that("proper results for calc_DO_sat", {
  expect_equal(calc_DO_sat(temp=21, press=1013.25, sal=0), 8.914559, tol=0.0001, info="with no units")
  expect_is(calc_DO_sat(temp=21, press=1013.25, sal=0), "numeric")
})

