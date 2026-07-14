test_that('predict_DO works as expected', {
  # empty model
  mm <- metab_model()
  expect_null(predict_DO(mm))
})

test_that('predict_metab works on allmodel types', {
  dat <- data_metab('3', '15')
  expected_cols <- c(
    'date',
    'GPP',
    'GPP.lower',
    'GPP.upper',
    'ER',
    'ER.lower',
    'ER.upper',
    'msgs.fit',
    'warnings',
    'errors'
  )

  # empty model
  mm <- metab_model()
  expect_null(predict_metab(mm))

  # metab_mle
  mm <- metab_mle(data = dat)
  mp <- predict_metab(mm)
  expect_equal(names(mp), expected_cols)
  expect_equal(nrow(mp), 3)

  # metab_night
  mm <- metab_night(data = data_metab('3', day_start = 12, day_end = 36))
  mp <- predict_metab(mm)
  expect_equal(names(mp), expected_cols)
  expect_equal(nrow(mp), 3)

  # metab_bayes
  mm <- metab_bayes(
    specs(
      "b_np_oi_tr_plrckm.stan",
      burnin_steps = 50,
      saved_steps = 50,
      n_cores = 1,
      stan_engine = stan_engine_for_tests()
    ),
    data = dat
  )
  mp <- predict_metab(mm)
  expect_equal(names(mp), expected_cols)
  expect_equal(nrow(mp), 3)

  # metab_sim
  dat_daily <- data.frame(
    date = as.Date(paste0("2012-09-", 18:20)),
    GPP.daily = 2,
    ER.daily = -3,
    K600.daily = 21
  )
  mm <- metab_sim(specs(mm_name('sim')), data = dat, data_daily = dat_daily)
  mp <- predict_metab(mm)
  expect_equal(names(mp), expected_cols)
  expect_equal(nrow(mp), 3)
})

test_that('predict_metab works as expected for bad inputs', {
  # should stop on fitting for missing data and/or fitted parameters
  dat <- data_metab('3', '15', flaws = 'missing end')
  # don't bother predicting on days where we didn't get a model fit
  mm <- metab_mle(data = dat)
  mp <- predict_metab(mm)
  expect_equal(mp$GPP[3], NA_real_)
  expect_equal(mp$ER[3], NA_real_)
  expect_equal(mp$msgs.fit[3], '      E')
  expect_equal(mp$warnings[3], NA_character_)
  expect_equal(get_params(mm)$errors[3], "data don't start when expected")
  # notice bad days for metab_sim, which won't have broken on model fitting
  dat_daily <- data.frame(
    date = as.Date(paste0("2012-09-", 18:20)),
    GPP.daily = 2,
    ER.daily = -3,
    K600.daily = 21
  )
  mm <- metab_sim(specs(mm_name('sim')), data = dat, data_daily = dat_daily)
  mp <- predict_metab(mm, use_saved = FALSE)
  expect_all_true(c(is.na(mp[3, c('GPP', 'ER')])))
  expect_equal(mp$msgs.fit[3], NA)
  expect_equal(mp$errors[3], "data don't start when expected")

  # should NOT stop on fitting if we said not to test
  mm <- metab_mle(specs(mm_name('mle'), day_tests = c()), data = dat)
  mp <- predict_metab(mm)
  expect_all_true(mp$msgs.fit == '       ')
  expect_all_true(mp$warnings == '')
  expect_all_true(mp$errors == '')
  mm <- metab_sim(
    specs(mm_name('sim'), day_tests = c()),
    data = dat,
    data_daily = dat_daily
  )
  mp <- predict_metab(mm)
  expect_equal(mp$GPP[3], get_params(mm)$GPP.daily[3])
  expect_equal(mp$ER[3], get_params(mm)$ER.daily[3])
  expect_all_true(is.na(mp$msgs.fit))
  expect_all_true(mp$warnings == '')
  expect_all_true(mp$errors == '')

  # should give message and force day length to 24 hours for prediction
  dat <- data_metab('3', '30', day_start = 2)
  expect_message(
    mm <- metab_mle(specs(mm_name('mle'), day_start = 2), data = dat),
    "Daily metabolism predictions cover hours"
  )
  mp <- predict_metab(mm)
  expect_all_false(is.na(mp$GPP))
  expect_all_true(mp$msgs.fit == '       ')
  expect_all_true(mp$warnings == '')
  expect_all_true(mp$errors == '')
  expect_snapshot(
    predict_metab(mm, day_start = 20, day_end = 28, use_saved = FALSE),
    error = TRUE
  )
  expect_snapshot(
    predict_metab(mm, day_start = 2, day_end = 28, use_saved = FALSE),
    error = TRUE
  )
  # same for metab_night when requested day is too long
  dat <- data_metab('3', '30', day_start = 7, day_end = 36)
  expect_message(
    mm <- metab(specs(mm_name('night'), day_start = 7), data = dat),
    "Daily metabolism predictions cover hours"
  )
  expect_snapshot(
    predict_metab(mm, day_start = 7, day_end = 36, use_saved = FALSE),
    error = TRUE
  )
  # but should allow <24 hours if needed for nighttime regression
  expect_message(
    mm <- metab(
      specs(mm_name('night'), day_start = 12, day_end = 24),
      data = dat
    ),
    "GPP estimates are zero because predictions cover nighttime only"
  )
  mp <- predict_metab(mm)
  expect_all_false(is.na(mp$ER))
  expect_all_true(mp$msgs.fit == '       ')
  expect_all_true(mp$warnings == '')
  expect_all_true(mp$errors == '')
  # and should notice if we're ignoring the day_start and day_end values
  expect_snapshot(mp <- predict_metab(mm, day_start = 7, day_end = 36))
})
