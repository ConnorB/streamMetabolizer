test_that("lookup_usgs_elevation requests and parses JSON", {
  httr2::local_mocked_responses(function(req) {
    expect_match(req$url, "/v1/json\\?")
    expect_match(req$url, "x=-96\\.5946890000")
    expect_match(req$url, "y=39\\.1020750000")
    expect_match(req$url, "units=Meters")
    expect_match(req$options$useragent, "streamMetabolizer")

    httr2::response(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      body = charToRaw('{"value":321.5}')
    )
  })

  expect_equal(
    lookup_usgs_elevation(39.102075, -96.594689, timeout = 1),
    321.5
  )
})

test_that("lookup_usgs_elevation rejects a missing elevation", {
  httr2::local_mocked_responses(list(
    httr2::response(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      body = charToRaw('{"value":-1000000}')
    )
  ))

  expect_snapshot(
    error = TRUE,
    lookup_usgs_elevation(39, -96, max_tries = 1)
  )
})

test_that("lookup_usgs_elevation validates coordinates and units", {
  expect_snapshot(error = TRUE, lookup_usgs_elevation(91, -96))
  expect_snapshot(error = TRUE, lookup_usgs_elevation(39, Inf))
  expect_snapshot(error = TRUE, lookup_usgs_elevation(39, -96, units = "yards"))
  expect_snapshot(error = TRUE, lookup_usgs_elevation(39, -96, units = NA))
})

test_that("lookup_usgs_elevation accepts unit aliases", {
  requests <- character()
  httr2::local_mocked_responses(function(req) {
    requests <<- c(requests, req$url)
    httr2::response(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      body = charToRaw('{"value":1}')
    )
  })

  for (units in c("m", "METERS", "ft", "Feet")) {
    lookup_usgs_elevation(39, -96, units = units)
  }

  expect_match(requests[1:2], "units=Meters", all = TRUE)
  expect_match(requests[3:4], "units=Feet", all = TRUE)
})

test_that("lookup_usgs_elevation wraps request failures", {
  httr2::local_mocked_responses(list(httr2::response(status_code = 503)))

  expect_snapshot(
    error = TRUE,
    lookup_usgs_elevation(39, -96, max_tries = 1)
  )
})

test_that("lookup_usgs_elevation works with the live EPQS API", {
  skip_on_cran()
  skip_on_ci()
  elevation_m <- tryCatch(
    lookup_usgs_elevation(
      latitude = 39.102075,
      longitude = -96.594689,
      timeout = 5,
      max_tries = 1
    ),
    error = \(e) skip("EPQS is unavailable")
  )
  elevation_ft <- lookup_usgs_elevation(
    latitude = 39.102075,
    longitude = -96.594689,
    units = "ft",
    timeout = 5,
    max_tries = 1
  )

  expect_type(elevation_m, "double")
  expect_length(elevation_m, 1L)
  expect_equal(elevation_ft, elevation_m * 3.28084, tolerance = 0.01)
})
