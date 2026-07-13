test_that('RStan draw selection handles vectors and arrays', {
  scalar <- seq_len(8)
  vector <- matrix(seq_len(24), nrow = 8, ncol = 3)
  array_draws <- array(seq_len(48), dim = c(8, 2, 3))

  scalar_selected <- select_rstan_draws(scalar, 'sigma', 1)
  vector_pooled <- select_rstan_draws(vector, 'theta', TRUE)
  vector_second <- select_rstan_draws(vector, 'theta', 2)
  array_selected <- select_rstan_draws(array_draws, 'theta', c(2, 5))

  expect_false(scalar_selected$indexed)
  expect_equal(scalar_selected$draws, scalar)
  expect_true(vector_pooled$indexed)
  expect_equal(vector_pooled$draws, as.vector(vector))
  expect_equal(vector_second$draws, vector[, 2])
  expect_equal(
    array_selected$draws,
    as.vector(matrix(array_draws, nrow = 8)[, c(2, 5), drop = FALSE])
  )
  expect_error(
    select_rstan_draws(vector, 'theta', 4),
    'index does not select a valid element'
  )
})

test_that('RStan cache keys include model contents', {
  skip_if_no_rstan()

  stan_file <- tempfile(fileext = '.stan')
  cache_root <- tempfile('streamMetabolizer-cache-')
  old_options <- options(streamMetabolizer.rstan_cache_dir = cache_root)
  on.exit(options(old_options), add = TRUE)

  writeLines('parameters { real y; } model { y ~ normal(0, 1); }', stan_file)
  first <- rstan_cache_file(stan_file)
  same <- rstan_cache_file(stan_file)
  writeLines('parameters { real y; } model { y ~ normal(1, 1); }', stan_file)
  changed <- rstan_cache_file(stan_file)

  expect_equal(first, same)
  expect_false(identical(first, changed))
  expect_true(dir.exists(dirname(first)))
})

test_that('RStan cache keys tolerate missing optional toolchain packages', {
  expect_equal(
    package_version_for_cache('streamMetabolizer-package-that-does-not-exist'),
    'not-installed'
  )
})

test_that('RStan compilation is skipped on R-devel', {
  expect_false(rstan_compilation_is_available(
    'R Under development (unstable) (2026-06-21 r90185)'
  ))
  expect_identical(
    rstan_compilation_is_available('R version 4.6.1'),
    rstan_is_available()
  )
})

test_that('the default RStan cache always resolves to a writable directory', {
  skip_if_no_rstan()

  stan_file <- tempfile(fileext = '.stan')
  writeLines('parameters { real y; } model { y ~ normal(0, 1); }', stan_file)
  old_options <- options(streamMetabolizer.rstan_cache_dir = NULL)
  on.exit(options(old_options), add = TRUE)

  cache_file <- rstan_cache_file(stan_file)
  expect_true(dir.exists(dirname(cache_file)))
  expect_true(file.access(dirname(cache_file), mode = 2) == 0)
})

test_that('RStan syntax checking does not require C++ compilation', {
  skip_if_no_rstan()

  expect_equal(
    mm_check_mcmc_file(
      'b_np_oi_tr_plrckm.stan',
      stan_engine = 'rstan'
    ),
    'correct'
  )
})

test_that('RStan compilation preserves the original compiler error', {
  skip_if_no_rstan()

  stan_file <- tempfile(fileext = '.stan')
  cache_root <- tempfile('streamMetabolizer-cache-')
  old_options <- options(streamMetabolizer.rstan_cache_dir = cache_root)
  on.exit(options(old_options), add = TRUE)
  writeLines('parameters { real y; } model { y ~ normal(0, 1); }', stan_file)
  local_mocked_bindings(
    stan_model = function(...) stop('compiler exploded'),
    .package = 'rstan'
  )

  expect_error(load_rstan_model(stan_file), 'compiler exploded')
})

test_that('RStan caches models and retained fits survive serialization', {
  skip_if_no_rstan_compilation()

  stan_file <- tempfile(fileext = '.stan')
  cache_root <- tempfile('streamMetabolizer-cache-')
  old_options <- options(streamMetabolizer.rstan_cache_dir = cache_root)
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

  cache_file <- rstan_cache_file(stan_file)
  saveRDS('invalid cache entry', cache_file)
  result <- suppressWarnings(
    runstan_bayes(
      data_list = list(d = 1L, n = 1L),
      model_path = stan_file,
      model_name = 'b_np_oi_tr_plrckm.stan',
      params_out = 'err_obs_iid_sigma',
      split_dates = FALSE,
      keep_mcmc = TRUE,
      n_chains = 4,
      n_cores = 2,
      burnin_steps = 100,
      saved_steps = 200,
      stan_engine = 'rstan'
    )
  )

  expect_s4_class(result$mcmcfit, 'stanfit')
  expect_s4_class(readRDS(cache_file), 'stanmodel')
  expect_true(any(grepl('^err_obs_iid_sigma_', names(result$overall))))
  expect_false(any(grepl('^extra_', names(result$overall))))
  expect_false('extra' %in% result$mcmcfit@sim$pars_oi)

  cached <- load_rstan_model(stan_file)
  expect_s4_class(cached$model, 'stanmodel')
  expect_null(cached$compile_log)

  saved_fit <- tempfile(fileext = '.rds')
  saveRDS(result$mcmcfit, saved_fit)
  restored_fit <- readRDS(saved_fit)
  restored_draws <- rstan::extract(
    restored_fit,
    pars = 'err_obs_iid_sigma'
  )$err_obs_iid_sigma
  expect_length(restored_draws, 800)
  expect_true(all(is.finite(restored_draws)))
})
