test_that("load_spring_creek returns a tibble with modeling columns", {
  spring <- streamMetabolizer:::load_spring_creek()

  expect_s3_class(spring, "tbl_df")
  expect_named(
    spring,
    c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")
  )
  expect_equal(nrow(spring), 265L)
  expect_s3_class(spring$solar.time, "POSIXct")
})
