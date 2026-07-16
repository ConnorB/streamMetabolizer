test_that("mm_get_timestep groups near-equivalent intervals", {
  datetimes <- as.POSIXct(
    cumsum(c(0, 0, 0.3, 0.6)) * 24 * 60 * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )

  expect_equal(
    mm_get_timestep(datetimes, format = "unique", tol = 0.5),
    c(0, 0.6)
  )
})

test_that("mm_data works", {
  # runs and can be used to select columns
  expect_s3_class(mm_data(), "data.frame")
  expect_equal(nrow(mm_data()), 1)
  expect_equal(mm_data(solar.time), mm_data()["solar.time"], ignore_attr = TRUE)
  expect_equal(
    mm_data(depth, temp.water, solar.time),
    mm_data()[c("depth", "temp.water", "solar.time")],
    ignore_attr = TRUE
  )

  # 'optional' attribute is set sensibly
  expect_null(mm_data(NULL)) # mm_data(NULL) returns NULL; optional='all' semantics handled in mm_validate_data
  expect_equal(attr(mm_data(depth, temp.water, solar.time), 'optional'), 'none')
  expect_equal(
    attr(mm_data(solar.time, DO.obs, optional = 'DO.obs'), 'optional'),
    'DO.obs'
  )
  expect_equal(
    attr(
      mm_data(solar.time, DO.obs, optional = c('DO.obs', 'solar.time')),
      'optional'
    ),
    'all'
  )

  # 'optional' attribute is checked
  expect_snapshot(
    mm_data(solar.time, DO.obs, optional = 'DO.sat'),
    error = TRUE
  )
  expect_snapshot(
    mm_data(solar.time, DO.obs, optional = c('DO.obs', 'all')),
    error = TRUE
  )
})

test_that("mm_data units are available for documentation", {
  expected <- c(
    solar.time = NA_character_,
    DO.obs = "mgO₂ L⁻¹",
    DO.sat = "mgO₂ L⁻¹",
    depth = "m",
    temp.water = "°C",
    light = "µmol m⁻² s⁻¹",
    discharge = "m³ s⁻¹",
    velocity = "m s⁻¹",
    date = NA_character_,
    DO.mod.1 = "mgO₂ L⁻¹",
    err.obs.sigma = "mgO₂ L⁻¹",
    err.obs.phi = NA_character_,
    err.proc.sigma = "gO₂ m⁻² d⁻¹",
    err.proc.phi = NA_character_,
    GPP.daily = "gO₂ m⁻² d⁻¹",
    Pmax = "gO₂ m⁻² d⁻¹",
    alpha = "gO₂ s d⁻¹ µmol⁻¹",
    ER.daily = "gO₂ m⁻² d⁻¹",
    ER20 = "gO₂ m⁻² d⁻¹",
    K600.daily = "d⁻¹",
    K600.daily.lower = "d⁻¹",
    K600.daily.upper = "d⁻¹",
    init.GPP.daily = "gO₂ m⁻² d⁻¹",
    init.Pmax = "gO₂ m⁻² d⁻¹",
    init.alpha = "gO₂ s d⁻¹ µmol⁻¹",
    init.ER.daily = "gO₂ m⁻² d⁻¹",
    init.ER20 = "gO₂ m⁻² d⁻¹",
    init.K600.daily = "d⁻¹",
    discharge.daily = "m³ s⁻¹",
    velocity.daily = "m s⁻¹",
    GPP = "gO₂ m⁻² d⁻¹",
    GPP.lower = "gO₂ m⁻² d⁻¹",
    GPP.upper = "gO₂ m⁻² d⁻¹",
    ER = "gO₂ m⁻² d⁻¹",
    ER.lower = "gO₂ m⁻² d⁻¹",
    ER.upper = "gO₂ m⁻² d⁻¹",
    D = "gO₂ m⁻³ d⁻¹",
    D.lower = "gO₂ m⁻³ d⁻¹",
    D.upper = "gO₂ m⁻³ d⁻¹"
  )

  expect_equal(mm_data_units(), expected)
  expect_equal(names(mm_data_units()), names(mm_data()))
  expect_equal(
    metab_inputs("mle", "data")$units,
    c("", "mgO₂ L⁻¹", "mgO₂ L⁻¹", "m", "°C", "µmol m⁻² s⁻¹", "m³ s⁻¹")
  )
})

test_that("mm_validate_data works", {
  # runs and accepts the defaults without errors
  ignore <- mm_validate_data(
    eval(formals(metab_mle)$data),
    eval(formals(metab_mle)$data_daily),
    'metab_mle'
  )
  ignore <- mm_validate_data(
    eval(formals(metab_night)$data),
    eval(formals(metab_night)$data_daily),
    'metab_night'
  )
  ignore <- mm_validate_data(
    eval(formals(metab_bayes)$data),
    eval(formals(metab_bayes)$data_daily),
    'metab_bayes'
  )
  ignore <- mm_validate_data(
    eval(formals(metab_Kmodel)$data),
    eval(formals(metab_Kmodel)$data_daily),
    'metab_Kmodel'
  )

  # accepts NULL for the fully optional data.frames
  ignore <- mm_validate_data(eval(formals(metab_mle)$data), NULL, 'metab_mle')
  ignore <- mm_validate_data(
    eval(formals(metab_night)$data),
    NULL,
    'metab_night'
  )
  ignore <- mm_validate_data(
    eval(formals(metab_bayes)$data),
    NULL,
    'metab_bayes'
  )
  ignore <- mm_validate_data(
    NULL,
    eval(formals(metab_Kmodel)$data_daily),
    'metab_Kmodel'
  )

  # returns a list
  val_out <- mm_validate_data(
    eval(formals(metab_mle)$data),
    eval(formals(metab_mle)$data_daily),
    'metab_mle'
  )
  expect_type(val_out, 'list')
  expect_equal(names(val_out), c('data', 'data_daily'))
  expect_s3_class(val_out[[1]], 'data.frame')

  # notices missing, extra, badly unitted columns in data; accepts non-unitted data
  ok_data <- eval(formals(metab_mle)$data)
  expect_snapshot(
    mm_validate_data(NULL, NULL, "metab_mle"),
    error = TRUE
  )
  expect_snapshot(
    mm_validate_data(data.frame(), NULL, "metab_mle"),
    error = TRUE
  )
  expect_snapshot(
    mm_validate_data(
      ok_data[names(ok_data) != 'temp.water'],
      NULL,
      "metab_mle"
    ),
    error = TRUE
  )
  expect_snapshot(
    mm_validate_data(
      dplyr::mutate(ok_data, temp.air = 9),
      mm_data('temp.air'),
      "metab_mle"
    ),
    error = TRUE
  )
  # units checking removed (unitted package dependency removed)
  expect_type(mm_validate_data(ok_data, NULL, "metab_mle"), 'list')

  # notices missing, extra, badly unitted columns in data_daily
  ok_data_daily <- eval(formals(metab_mle)$data_daily)
  expect_type(mm_validate_data(ok_data, NULL, "metab_mle"), 'list')
  expect_snapshot(
    mm_validate_data(ok_data, data.frame(), "metab_mle"),
    error = TRUE
  )
  expect_snapshot(
    mm_validate_data(
      ok_data,
      dplyr::mutate(ok_data_daily, temp.air = 9),
      "metab_mle"
    ),
    error = TRUE
  )
  # units checking removed (unitted package dependency removed)
  expect_type(mm_validate_data(ok_data, ok_data_daily, "metab_mle"), 'list')
})

test_that("mm_is_valid_day works", {
  # use a subset of data from Bob
  french <- streamMetabolizer:::load_french_creek()

  good_day <- dplyr::filter(
    french,
    solar.time >= as.POSIXct("2012-08-24 22:30:00", tz = "UTC"),
    solar.time <= as.POSIXct("2012-08-26 06:00:00", tz = "UTC")
  )
  bad_day <- dplyr::mutate(
    good_day,
    DO.obs = replace(DO.obs, 40, NA),
    DO.sat = replace(DO.sat, 51, NA),
    temp.water = replace(temp.water, 20:42, NA)
  )

  # test and pass
  expect_identical(
    mm_is_valid_day(good_day, day_start = -1.5, day_end = 30),
    TRUE
  )

  # test faulty timestep
  dateless_day <- good_day
  dateless_day$solar.time <- replace(
    dateless_day$solar.time,
    2:(nrow(dateless_day) - 1),
    NA
  )
  expect_equal(
    mm_is_valid_day(dateless_day),
    c("no timesteps", "NAs in solar.time")
  )

  # test full_day
  expect_equal(
    mm_is_valid_day(good_day, day_start = -10, day_end = 30),
    "data don't start when expected"
  )
  expect_equal(
    mm_is_valid_day(good_day, day_start = 0, day_end = 30),
    "data don't start when expected"
  )
  expect_equal(
    mm_is_valid_day(good_day, day_start = -1.5, day_end = 25),
    "data don't end when expected"
  )
  expect_equal(
    mm_is_valid_day(good_day, day_start = -1.5, day_end = 35),
    "data don't end when expected"
  )

  # test timestep lengths
  irregular_day <- good_day[-c(3, 20, 99), ]
  expect_equal(
    mm_is_valid_day(irregular_day, day_start = -1.5, day_end = 30),
    "uneven timesteps"
  )

  # test column completeness
  expect_equal(
    mm_is_valid_day(bad_day, day_start = -1.5, day_end = 30),
    c("NAs in DO.obs", "NAs in DO.sat", "NAs in temp.water")
  )

  # test for positive discharge
  good_day$discharge <- seq(1, 3, length.out = nrow(good_day))
  expect_identical(
    mm_is_valid_day(good_day, day_start = -1.5, day_end = 30),
    TRUE
  )
  pretty_good_day <- good_day
  pretty_good_day$discharge <- seq(-1, 1, length.out = nrow(pretty_good_day))
  expect_equal(
    mm_is_valid_day(pretty_good_day, day_start = -1.5, day_end = 30),
    "discharge <= 0"
  )
})

test_that("mm_filter_valid_days works", {
  library(dplyr)

  # catch missorted data
  french <- data_metab(
    '10',
    res = '30',
    flaws = 'missorted',
    day_start = 6,
    day_end = 14
  )
  expect_snapshot(
    mm_filter_valid_days(french, day_start = 10, day_end = 12),
    error = TRUE
  )

  # filter to specified hours
  french <- data_metab('10', res = '30', day_start = 6, day_end = 14)
  french_filt1 <- mm_filter_valid_days(french, day_start = 10, day_end = 12)
  expect_equal(names(french_filt1), c('data', 'data_daily', 'removed'))
  expect_equal(nrow(french_filt1$data), 4 * 10)

  # filter to specified days
  french_daily <- data.frame(
    date = as.Date(sprintf("2012-09-%2d", 15:30)),
    K600 = 7
  )
  french_filt2 <- mm_filter_valid_days(
    french,
    data_daily = french_daily,
    day_start = 10,
    day_end = 12
  )
  expect_equal(nrow(french_filt2$data), 10 * 4)
  expect_equal(nrow(french_filt2$removed), 6)
  expect_equal(
    sort(as.Date(unique(c(
      french_filt2$removed$date,
      french_filt2$data_daily$date
    )))),
    french_daily$date
  )
})

test_that("mm_filter_valid_days preserves tibble inputs", {
  dat <- tibble::as_tibble(data_metab("1", res = "30"))
  dat_daily <- tibble::tibble(date = as.Date("2012-09-18"), value = 1)

  out <- mm_filter_valid_days(
    dat,
    data_daily = dat_daily,
    day_start = 4,
    day_end = 28
  )

  expect_s3_class(out$data, "tbl_df")
  expect_s3_class(out$data_daily, "tbl_df")
})

test_that("mm_filter_dates works", {
  start_time <- Sys.time()
  start_date <- as.Date(start_time)
  udat <- data.frame(
    solar.time = start_time + as.difftime(1:100, units = 'hours'),
    value = 1:100
  )
  ddat <- data.frame(
    date = start_date + as.difftime(1:100, units = 'days'),
    value = 1:100
  )
  # no filter with defaults
  expect_equal(
    streamMetabolizer:::mm_filter_dates(udat),
    udat,
    ignore_attr = "tzone"
  )
  expect_equal(streamMetabolizer:::mm_filter_dates(ddat), ddat)
  # dates are inclusive
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      udat,
      date_start = start_date + as.difftime(1, units = 'days'),
      date_end = start_date + as.difftime(1, units = 'days')
    )),
    24
  )
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      udat,
      date_start = start_date + as.difftime(1, units = 'days'),
      date_end = start_date + as.difftime(2, units = 'days')
    )),
    48
  )
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      udat,
      date_start = start_date + as.difftime(2, units = 'days'),
      date_end = start_date + as.difftime(1, units = 'days')
    )),
    0
  )
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      ddat,
      date_start = start_date + as.difftime(20, units = 'days'),
      date_end = start_date + as.difftime(20, units = 'days')
    )),
    1
  )
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      ddat,
      date_start = start_date + as.difftime(22, units = 'days'),
      date_end = start_date + as.difftime(28, units = 'days')
    )),
    7
  )
  expect_equal(
    nrow(streamMetabolizer:::mm_filter_dates(
      ddat,
      date_start = start_date + as.difftime(22, units = 'days'),
      date_end = start_date + as.difftime(8, units = 'days')
    )),
    0
  )
})
