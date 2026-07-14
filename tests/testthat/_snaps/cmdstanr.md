# Stan model lookup rejects unsupported backend versions

    Code
      mm_locate_filename("b_np_oi_tr_plrckm.stan", stan_engine = "cmdstanr",
        stan_version_fn = stan_version_fn)
    Condition
      Error in `mm_locate_filename()`:
      ! Stan version 2.25.0 is not supported; version 2.26.0 or later is required.

# CmdStan draw selection respects indexed parameters

    Code
      select_cmdstan_draws(draws, "theta", 3)
    Condition
      Error in `select_cmdstan_draws()`:
      ! `index` does not select a valid element of `theta`.

# CmdStan cache paths reject missing model files

    Code
      cmdstan_cache_dir(tempfile(fileext = ".stan"), "2.39.0")
    Condition
      Error in `stan_model_hash()`:
      ! `model_path` must identify an existing Stan file.
