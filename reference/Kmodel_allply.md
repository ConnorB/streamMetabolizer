# Fit a K model

The model will predict daily K estimates from preliminary daily K
estimates. Called from metab_Kmodel().

## Usage

``` r
Kmodel_allply(
  data_daily_all,
  engine,
  weights,
  predictors,
  transforms,
  other_args
)
```

## Arguments

- data_daily_all:

  A data frame containing `K600.daily.obs`, `weight`, and any
  predictors.

- engine:

  The software or function to use in fitting the model. Should be
  specified via `mm_name` rather than here. For `type='bayes'`, always
  `'stan'` indicating the software package to use for the MCMC process
  (see http://mc-stan.org/). For types in `c('mle','night','sim')`
  there's again only one option per model (R functions; these need not
  be named here but will be noted in the suffix of the model name, e.g.,
  `"m_np_oi_tr_plrckm.nlm"` uses
  [`nlm()`](https://rdrr.io/r/stats/nlm.html) for model fitting). For
  type='Kmodel', the name of an interpolation or regression method
  relating K to the predictor(s) of choice. One of
  `c("mean", "lm", "loess")`.

- weights:

  For Kmodel, character vector indicating the type of weighting to use.
  Set to c() for no weights. One of c("1/CI", "K600/CI", c()).

- predictors:

  For Kmodel, character vector of variables (column names in data or
  data_daily) to use in predicting K. Leave blank or set to c() for no
  predictors. Otherwise, one or more of these may be included: c("date",
  "velocity.daily", "discharge.daily").

- transforms:

  For Kmodel, a named character vector of function names (usually
  `"log"` or `NA`) to apply to `K600.daily` and the predictors.
  `K600.daily` should usually be logged. The vector names must match the
  values of `predictors`, although not all elements of `predictors` must
  be included in `transforms`. Recommended transforms include
  `c(K600.daily = "log", date = NA, velocity.daily = "log", discharge.daily = "log")`.

- other_args:

  Other arguments passed to the fitting function given by
  `specs$engine`. `na.rm=TRUE` is already passed to `mean` (which is
  actually implemented as `sum`, anyway).
