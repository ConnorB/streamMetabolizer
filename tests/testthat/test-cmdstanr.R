test_that('Stan model lookup rejects unsupported backend versions', {
  testthat::local_mocked_bindings(
    stan_version_for_engine = function(stan_engine) {
      expect_equal(stan_engine, 'cmdstanr')
      as.numeric_version('2.25.0')
    },
    .package = 'streamMetabolizer'
  )

  expect_error(
    mm_locate_filename(
      'b_np_oi_tr_plrckm.stan',
      stan_engine = 'cmdstanr'
    ),
    'Stan version 2.25.0 is not supported'
  )
})

test_that('CmdStan draw selection respects indexed parameters', {
  draws <- array(
    seq_len(24),
    dim = c(4, 2, 3),
    dimnames = list(
      iteration = NULL,
      chain = NULL,
      variable = c('theta[1]', 'theta[2]', 'sigma')
    )
  )

  pooled <- select_cmdstan_draws(draws, 'theta', TRUE)
  second <- select_cmdstan_draws(draws, 'theta', 2)
  scalar <- select_cmdstan_draws(draws, 'sigma', 1)

  expect_true(pooled$indexed)
  expect_equal(pooled$draws, as.vector(draws[,, 1:2, drop = FALSE]))
  expect_equal(second$draws, as.vector(draws[,, 2, drop = FALSE]))
  expect_false(scalar$indexed)
  expect_equal(scalar$draws, as.vector(draws[,, 3, drop = FALSE]))
  expect_error(
    select_cmdstan_draws(draws, 'theta', 3),
    'index does not select a valid element'
  )
})

test_that('CmdStan cache keys include the toolchain and model contents', {
  stan_file <- tempfile(fileext = '.stan')
  cache_root <- tempfile('streamMetabolizer-cache-')
  old_options <- options(streamMetabolizer.cmdstan_cache_dir = cache_root)
  on.exit(options(old_options), add = TRUE)

  writeLines('parameters { real y; } model { y ~ normal(0, 1); }', stan_file)
  first <- cmdstan_cache_dir(
    stan_file,
    '2.39.0',
    cmdstanr_version = '0.9.0',
    platform = 'aarch64-apple-darwin23'
  )
  same <- cmdstan_cache_dir(
    stan_file,
    '2.39.0',
    cmdstanr_version = '0.9.0',
    platform = 'aarch64-apple-darwin23'
  )
  other_cmdstan <- cmdstan_cache_dir(
    stan_file,
    '2.38.0',
    cmdstanr_version = '0.9.0',
    platform = 'aarch64-apple-darwin23'
  )
  other_cmdstanr <- cmdstan_cache_dir(
    stan_file,
    '2.39.0',
    cmdstanr_version = '0.8.1',
    platform = 'aarch64-apple-darwin23'
  )
  other_platform <- cmdstan_cache_dir(
    stan_file,
    '2.39.0',
    cmdstanr_version = '0.9.0',
    platform = 'x86_64-w64-mingw32'
  )
  writeLines('parameters { real y; } model { y ~ normal(1, 1); }', stan_file)
  changed <- cmdstan_cache_dir(
    stan_file,
    '2.39.0',
    cmdstanr_version = '0.9.0',
    platform = 'aarch64-apple-darwin23'
  )

  expect_equal(first, same)
  expect_false(identical(first, other_cmdstan))
  expect_false(identical(first, other_cmdstanr))
  expect_false(identical(first, other_platform))
  expect_false(identical(first, changed))
  expect_true(all(dir.exists(c(
    first,
    other_cmdstan,
    other_cmdstanr,
    other_platform,
    changed
  ))))
})

test_that('CmdStan cache paths reject missing model files', {
  expect_error(
    cmdstan_cache_dir(tempfile(fileext = '.stan'), '2.39.0'),
    'model_path must identify an existing Stan file'
  )
})

test_that('the default CmdStan cache always resolves to a writable directory', {
  stan_file <- tempfile(fileext = '.stan')
  writeLines('parameters { real y; } model { y ~ normal(0, 1); }', stan_file)
  old_options <- options(streamMetabolizer.cmdstan_cache_dir = NULL)
  on.exit(options(old_options), add = TRUE)

  cache_dir <- cmdstan_cache_dir(stan_file, '2.39.0')
  expect_equal(dir.exists(cache_dir), TRUE)
  expect_equal(unname(file.access(cache_dir, mode = 2)), 0)
})

test_that('CmdStan syntax checking is available without C++ compilation', {
  skip_if_no_cmdstan()

  expect_equal(
    mm_check_mcmc_file(
      'b_np_oi_tr_plrckm.stan',
      stan_engine = 'cmdstanr'
    ),
    'correct'
  )
})

test_that('CmdStan summaries honor params_out while retained fits keep draws', {
  skip_if_no_cmdstan()

  stan_file <- tempfile(fileext = '.stan')
  cache_root <- tempfile('streamMetabolizer-cache-')
  old_options <- options(streamMetabolizer.cmdstan_cache_dir = cache_root)
  on.exit(options(old_options), add = TRUE)
  writeLines(
    c(
      'data { int<lower=1> d; int<lower=1> n; }',
      'parameters { real<lower=0> err_obs_iid_sigma; real extra; }',
      'model {',
      '  err_obs_iid_sigma ~ normal(1, 0.1);',
      '  extra ~ normal(0, 1);',
      '}'
    ),
    stan_file
  )

  result <- suppressWarnings(
    runstan_bayes(
      data_list = list(d = 1L, n = 1L),
      model_path = stan_file,
      model_name = 'b_np_oi_tr_plrckm.stan',
      params_out = 'err_obs_iid_sigma',
      split_dates = FALSE,
      keep_mcmc = TRUE,
      n_chains = 2,
      n_cores = 2,
      burnin_steps = 20,
      saved_steps = 20,
      stan_engine = 'cmdstanr'
    )
  )

  expect_s3_class(result$mcmcfit, 'CmdStanMCMC')
  expect_true(any(grepl('^err_obs_iid_sigma_', names(result$overall))))
  expect_false(any(grepl('^extra_', names(result$overall))))
  expect_equal(
    dim(result$mcmcfit$draws(variables = 'extra'))[[3]],
    1
  )

  saved_fit <- tempfile(fileext = '.rds')
  output_files <- result$mcmcfit$output_files()
  saveRDS(result$mcmcfit, saved_fit)
  expect_true(all(file.remove(output_files)))
  restored_fit <- readRDS(saved_fit)
  expect_equal(dim(restored_fit$draws(variables = 'extra'))[[3]], 1)
  expect_s3_class(restored_fit$sampler_diagnostics(), 'draws_array')
})
