test_that("lookup_usgs_elevation requests and parses JSON", {
  local_mocked_bindings(
    read_epqs_json = \(url) {
      expect_match(url, "/v1/json\\?")
      expect_match(url, "x=-96\\.5946890000")
      expect_match(url, "y=39\\.1020750000")
      expect_match(url, "units=Meters")
      list(value = 321.5)
    }
  )

  expect_equal(
    lookup_usgs_elevation(39.102075, -96.594689),
    321.5
  )
})

test_that("lookup_usgs_elevation rejects a missing elevation", {
  local_mocked_bindings(read_epqs_json = \(url) list(value = NULL))

  expect_snapshot(error = TRUE, lookup_usgs_elevation(39, -96))
})

test_that("lookup_usgs_elevation works with the live EPQS API", {
  skip_on_cran()
  epqs_available <- tryCatch(
    suppressWarnings({
      jsonlite::fromJSON(paste0(
        "https://epqs.nationalmap.gov/v1/json?",
        "x=-96.594689&y=39.102075&wkid=4326&units=Meters&",
        "includeDate=false"
      ))
      TRUE
    }),
    error = \(e) FALSE
  )
  if (!epqs_available) {
    skip("EPQS is unavailable")
  }

  elevation_m <- lookup_usgs_elevation(
    latitude = 39.102075,
    longitude = -96.594689,
    units = "Meters"
  )
  elevation_ft <- lookup_usgs_elevation(
    latitude = 39.102075,
    longitude = -96.594689,
    units = "Feet"
  )

  expect_type(elevation_m, "double")
  expect_length(elevation_m, 1L)
  expect_equal(elevation_ft, elevation_m * 3.28084, tolerance = 0.01)
})
