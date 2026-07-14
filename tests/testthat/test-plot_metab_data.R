test_that("plot_metab_data creates the default panels", {
  data <- tibble::tibble(
    solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:2 * 3600,
    DO.obs = c(8, 9, 10),
    DO.sat = c(10, 0, 10),
    depth = c(0.4, 0.5, 0.6),
    temp.water = c(18, 19, 20),
    light = c(0, 100, 200)
  )

  plot <- plot_metab_data(data)

  expect_s3_class(plot, "ggplot")
  expect_setequal(
    levels(plot$data$panel),
    c("DO_mgL", "DO_pctsat", "depth", "temp_water", "light")
  )
  expect_equal(
    plot$data$value[plot$data$variable == "DO.pctsat"],
    c(80, NA, 100)
  )
})

test_that("plot_metab_data respects cols", {
  data <- tibble::tibble(
    solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:1 * 3600,
    DO.obs = c(8, 9),
    temp.water = c(18, 19)
  )

  plot <- plot_metab_data(data, cols = c("DO.obs", "temp.water"))

  expect_setequal(levels(plot$data$variable), c("DO.obs", "temp.water"))
  expect_setequal(levels(plot$data$panel), c("DO_mgL", "temp_water"))
})

test_that("plot_metab_data validates its inputs", {
  data <- tibble::tibble(
    solar.time = as.POSIXct("2024-06-01", tz = "UTC"),
    DO.obs = 8
  )

  expect_snapshot(error = TRUE, plot_metab_data(1))
  expect_snapshot(error = TRUE, plot_metab_data(data, cols = 1))
  expect_snapshot(error = TRUE, plot_metab_data(data, cols = character()))
  expect_snapshot(error = TRUE, plot_metab_data(data, cols = "unknown"))
  expect_snapshot(error = TRUE, plot_metab_data(data, cols = "DO.sat"))

  data$DO.obs <- "8"
  expect_snapshot(error = TRUE, plot_metab_data(data, cols = "DO.obs"))
})
