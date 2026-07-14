test_that("data_metab handles tibble input from load_french_creek", {
  data <- data_metab(num_days = "1", res = "5")

  expect_s3_class(data, "tbl_df")
  expect_named(
    data,
    c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")
  )
})
