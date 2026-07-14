# RStan draw selection handles vectors and arrays

    Code
      select_rstan_draws(vector, "theta", 4)
    Condition
      Error in `select_rstan_draws()`:
      ! `index` does not select a valid element of `theta`.

# RStan cache paths reject missing model files

    Code
      rstan_cache_file(tempfile(fileext = ".stan"))
    Condition
      Error in `stan_model_hash()`:
      ! `model_path` must identify an existing Stan file.

# RStan compilation preserves the original compiler error

    Code
      suppressMessages(load_rstan_model(stan_file, stan_model_fn = function(...) stop(
        "compiler exploded")))
    Condition
      Error in `stan_model_fn()`:
      ! compiler exploded
