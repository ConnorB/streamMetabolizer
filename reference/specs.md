# Generate a coherent list of model specs

Generates an internally consistent list of model specifications that may
be passed to `metab_bayes`, `metab_mle`, etc. via the `specs` argument.
This help file gives the definitive list of all possible model specs,
but only a subset of these are relevant to any given `model_name`. See
the 'Relevant arguments' section below. Irrelevant arguments for the
given `model_name` should not be explicitly passed into this function
(but don't worry - we'll just stop and tell you if you make a mistake).
Relevant arguments for the given `model_name` either have default values
or do not (see Usage). Relevant arguments without a default should
rarely be overridden, because their values will be determined based on
other arguments. Relevant arguments that do have a default can, and
often should, be overridden to tailor the model to your needs.

## Usage

``` r
specs(
  model_name = mm_name(),
  engine,
  day_start = 4,
  day_end = 28,
  day_tests = c("full_day", "even_timesteps", "complete_data", "pos_discharge",
    "pos_depth"),
  required_timestep = NA,
  init.GPP.daily = 8,
  init.Pmax = 10,
  init.alpha = 1e-04,
  init.ER.daily = -10,
  init.ER20 = -10,
  init.K600.daily = 10,
  split_dates,
  keep_mcmcs = TRUE,
  keep_mcmc_data = TRUE,
  GPP_daily_mu = 3.1,
  GPP_daily_lower = -Inf,
  GPP_daily_sigma = 6,
  alpha_meanlog = -4.6,
  alpha_sdlog = 0.5,
  Pmax_mu = 10,
  Pmax_sigma = 7,
  ER_daily_mu = -7.1,
  ER_daily_upper = Inf,
  ER_daily_sigma = 7.1,
  K600_daily_meanlog = log(12),
  K600_daily_meanlog_meanlog = log(12),
  K600_daily_meanlog_sdlog = 1.32,
  lnK600_lnQ_intercept_mu = 2,
  lnK600_lnQ_intercept_sigma = 2.4,
  lnK600_lnQ_slope_mu = 0,
  lnK600_lnQ_slope_sigma = 0.5,
  K600_lnQ_nodes_centers = -3:3,
  K600_lnQ_nodediffs_sdlog = 0.5,
  K600_lnQ_nodes_meanlog = rep(log(12), length(K600_lnQ_nodes_centers)),
  K600_lnQ_nodes_sdlog = rep(1.32, length(K600_lnQ_nodes_centers)),
  K600_daily_sdlog = switch(mm_parse_name(model_name)$pool_K600, none = 1, normal_sdfixed
    = 0.05, NA),
  K600_daily_sigma = switch(mm_parse_name(model_name)$pool_K600, linear_sdfixed = 10,
    binned_sdfixed = 5, NA),
  K600_daily_sdlog_sigma = switch(mm_parse_name(model_name)$pool_K600, normal = 0.05, NA),
  K600_daily_sigma_sigma = switch(mm_parse_name(model_name)$pool_K600, linear = 1.2,
    binned = 0.24, NA),
  err_obs_iid_sigma_scale = 0.03,
  err_proc_iid_sigma_scale = 5,
  err_proc_acor_phi_alpha = 1,
  err_proc_acor_phi_beta = 1,
  err_proc_acor_sigma_scale = 1,
  err_proc_acor_light_alpha_sigma = 5,
  err_mult_GPP_sdlog_sigma = 1,
  params_in,
  params_out,
  n_chains = 4,
  n_cores = 4,
  burnin_steps = 500,
  saved_steps = 500,
  thin_steps = 1,
  stan_engine = "rstan",
  verbose = FALSE,
  weights = c("K600/CI"),
  filters = c(CI.max = NA, discharge.daily.max = NA, velocity.daily.max = NA),
  predictors = c("discharge.daily"),
  transforms = c(K600 = "log", date = NA, velocity.daily = "log", discharge.daily =
    "log"),
  other_args = c(),
  K600_lnQ_cnode_meanlog = log(6),
  K600_lnQ_cnode_sdlog = 1,
  K600_lnQ_nodediffs_meanlog = 0.2,
  lnK600_lnQ_nodes = function(K600_lnQ_nodes_centers, K600_lnQ_cnode_meanlog,
    K600_lnQ_cnode_sdlog, K600_lnQ_nodediffs_meanlog, K600_lnQ_nodediffs_sdlog, ...)
    sim_Kb(K600_lnQ_nodes_centers, K600_lnQ_cnode_meanlog, K600_lnQ_cnode_sdlog,
    K600_lnQ_nodediffs_meanlog, K600_lnQ_nodediffs_sdlog),
  discharge_daily = function(n, ...) rnorm(n, 20, 3),
  DO_mod_1 = NULL,
  K600_daily = function(n, K600_daily_predlog = log(10), ...) {
     pmax(0, rnorm(n,
    K600_daily_predlog, 4))
 },
  GPP_daily = function(n, ...) pmax(0, rnorm(n, 8, 4)),
  Pmax = function(n, ...) pmax(0, rnorm(n, 10, 2)),
  alpha = function(n, ...) pmax(0, rnorm(n, 1e-04, 2e-05)),
  ER_daily = function(n, ...) pmin(0, rnorm(n, -10, 5)),
  ER20 = function(n, ...) pmin(0, rnorm(n, -10, 4)),
  err_obs_sigma = 0.01,
  err_obs_phi = 0,
  err_proc_sigma = 0.2,
  err_proc_phi = 0,
  err_round = NA,
  sim_seed = NA
)
```

## Arguments

- model_name:

  character string identifying the model features. Use
  [`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
  to create a valid name based on desired attributes, or
  [`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
  to see all valid names. Two alternatives to the names given by
  [`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
  are also accepted: (1) a model type as accepted by the `type` argument
  to `mm_name`, which will be used to create the default model name for
  that model type, or (2) a full model file path for custom Bayesian
  models, as long as basename(model_name) can still be parsed correctly
  with
  [`mm_parse_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md)
  and the file exists. In that case the file may be specified either as
  a file path relative to the streamMetabolizer models directory (the
  first assumption; this directory can be found with
  `system.file("models", package="streamMetabolizer")`) or as an
  absolute path or a path relative to the current working directory (the
  second assumption, if the first assumption turns up no files of the
  given name).

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

- day_start:

  start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time. For metabolism models
  working with single days of input data, it is conventional/useful to
  begin the day the evening before, e.g., -1.5, and to end just before
  the next sunrise, e.g., 30. For multiple consecutive days, it may make
  the most sense to start just before sunrise (e.g., 4) and to end 24
  hours later. For nighttime regression, the date assigned to a chunk of
  data should be the date whose evening contains the data. The default
  is therefore 12 to 36 for metab_night, of which the times of darkness
  will be used.

- day_end:

  end time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00. See day_start for recommended start
  and end times.

- day_tests:

  list of tests to conduct to determine whether each date worth of data
  is valid for modeling. The results of these tests will be combined
  with the result of the test implied if `required_timestep` is numeric
  and then will be passed to `model_fun` as the `ply_validity` argument
  to that function.

- required_timestep:

  NA or numeric (length 1). If numeric, the timestep length in days that
  a date must have to pass the validity check (to within a tolerance of
  0.2% of the value of `required_timestep`). The result of this test
  will be combined with the results of the tests listed in `day_tests`
  and reported to `model_fun` as the `ply_validity` argument to that
  function.

- init.GPP.daily:

  the initial value of daily mean GPP (gO2 d^-1 m^-2) to use in the NLM
  fitting process. See the MLE Initial Values section under Details.

- init.Pmax:

  the initial value of Pmax (gO2 d^-1 m^-2) to use in the GPP versus
  light relationship in the NLM fitting process. Pmax is the maximum GPP
  value of the GPP-light curve. See the MLE Initial Values section under
  Details.

- init.alpha:

  the initial value of alpha (gO2 s d^-1 umol^-1, i.e., units of
  GPP/light) to use in the GPP versus light relationship in the NLM
  fitting process. alpha is the initial slope of the GPP-light curve.
  See the MLE Initial Values section under Details.

- init.ER.daily:

  the initial value of daily mean ER (gO2 d^-1 m^-2) to use in the NLM
  fitting process. See the MLE Initial Values section under Details.

- init.ER20:

  the initial value of ER20 (gO2 d^-1 m^-2) to use in the ER versus
  temperature relationship in the NLM fitting process. ER20 is the
  respiration rate at 20 degrees C. See the MLE Initial Values section
  under Details.

- init.K600.daily:

  the initial value of daily mean K600 (d^-1) to use in the NLM fitting
  process. Ignored if K600 is supplied in data_daily, except for those
  dates where K600 is NA. If there are any such dates, K600_init must
  have a numeric (non-NA) value, as this will be used to estimate K600
  for those dates. See the MLE Initial Values section under Details.

- split_dates:

  logical indicating whether the data should be split into daily chunks
  first (TRUE) or processed within one big model (FALSE). If valid days
  differ in their timestep length, split_dates will need to be TRUE;
  otherwise, FALSE is generally more efficient. FALSE is also the only
  appropriate solution for a hierarchical model that pools information
  on error, K600, etc. across days.

- keep_mcmcs:

  TRUE, FALSE, or (for nopool models) a vector of dates (coerced with
  as.Date if character, etc.) indicating whether to keep all of the mcmc
  model objects (TRUE), none of them (FALSE), or specific dates. The
  default is TRUE because these objects often need inspecting.

- keep_mcmc_data:

  FALSE, TRUE, or (for nopool models) a vector of dates (coerced with
  as.Date if character, etc.) indicating whether to keep all of the mcmc
  model objects (TRUE), none of them (FALSE), or specific dates. The
  default is FALSE because these objects can be very large.

- GPP_daily_mu:

  The mean of a dnorm distribution for GPP_daily, the daily rate of
  gross primary production

- GPP_daily_lower:

  The lower bound on every fitted value of GPP_daily, the daily rate of
  gross primary production. Use values other than -Inf with caution,
  recognizing that sometimes the input data are unmodelable and that a
  negative estimate of GPP_daily (when unconstrained) could be your only
  indication.

- GPP_daily_sigma:

  The standard deviation of a dnorm distribution for GPP_daily, the
  daily rate of gross primary production

- alpha_meanlog:

  The mean of a dlnorm (lognormal) distribution for alpha, the daily
  initial slope of the Jassby-Platt saturating curve relating GPP to
  light

- alpha_sdlog:

  The standard deviation parameter of a dlnorm (lognormal) distribution
  for alpha, the daily initial slope of the Jassby-Platt saturating
  curve relating GPP to light.

- Pmax_mu:

  The mean of a dnorm (normal) distribution for Pmax, the daily maximum
  GPP value of a Jassby-Platt saturating curve relating GPP to light.

- Pmax_sigma:

  The standard deviation of a dnorm (normal) distribution for Pmax, the
  daily maximum GPP value of a Jassby-Platt saturating curve relating
  GPP to light.

- ER_daily_mu:

  The mean of a dnorm distribution for ER_daily, the daily rate of
  ecosystem respiration

- ER_daily_upper:

  The upper (less negative) bound on every fitted value of ER_daily, the
  daily rate of ecosystem respiration. Use values other than Inf with
  caution, recognizing that sometimes the input data are unmodelable and
  that a positive estimate of ER_daily (when unconstrained) could be
  your only indication.

- ER_daily_sigma:

  The standard deviation of a dnorm distribution for ER_daily, the daily
  rate of ecosystem respiration

- K600_daily_meanlog:

  Applies when pool_K600 is 'none'. The mean of a dlnorm distribution
  for K600_daily, the daily rate of reaeration

- K600_daily_meanlog_meanlog:

  hyperparameter for pool_K600='normal'. The mean parameter
  (meanlog_meanlog) of a lognormal distribution of meanlog in K ~
  lN(meanlog, sdlog), meanlog ~ lN(meanlog_meanlog, meanlog_sdlog)

- K600_daily_meanlog_sdlog:

  hyperparameter for pool_K600='normal'. The standard deviation
  parameter (meanlog_sdlog) of a lognormal distribution of meanlog in K
  ~ lN(meanlog, sdlog), meanlog ~ lN(meanlog_meanlog, meanlog_sdlog)

- lnK600_lnQ_intercept_mu:

  hyperparameter for pool_K600 == 'linear'. The mean of the prior
  distribution for the intercept parameter in
  `log(K600) ~ lnK600_lnQ_intercept + lnK600_lnQ_slope*log(Q)`

- lnK600_lnQ_intercept_sigma:

  hyperparameter for pool_K600 == 'linear'. The standard deviation of
  the prior distribution for the intercept parameter in
  `log(K600) ~ lnK600_lnQ_intercept + lnK600_lnQ_slope*log(Q)`

- lnK600_lnQ_slope_mu:

  hyperparameter for pool_K600='linear'. The mean of the prior
  distribution for the slope parameter in
  `log(K600) ~ lnK600_lnQ_intercept + lnK600_lnQ_slope*log(Q)`

- lnK600_lnQ_slope_sigma:

  hyperparameter for pool_K600='linear'. The standard deviation of the
  prior distribution for the slope parameter in
  `log(K600) ~ lnK600_lnQ_intercept + lnK600_lnQ_slope*log(Q)`

- K600_lnQ_nodes_centers:

  data configuration argument for pool_K600='binned'. numeric vector
  giving the natural-log-space centers of the discharge bins. See also
  [`calc_bins()`](https://connorb.github.io/streamMetabolizer/reference/calc_bins.md)

- K600_lnQ_nodediffs_sdlog:

  hyperparameter for pool_K600='binned'. The standard deviations of the
  differences in estimated K600 between successive lnQ_nodes (bins),
  where the means of those differences are always zero

- K600_lnQ_nodes_meanlog:

  hyperparameter for pool_K600='binned'. The means of lognormal prior
  distributions for the K600_lnQ_nodes parameters.

- K600_lnQ_nodes_sdlog:

  hyperparameter for pool_K600='binned'. The standard deviations of
  lognormal prior distributions for the K600_lnQ_nodes parameters.

- K600_daily_sdlog:

  The lognormal scale parameter (standard deviation) of a dlnorm
  distribution having meanlog equal to `K600_daily_meanlog` (when
  pool_K600 is 'none') or `K600_daily_predlog` (when pool_K600 is
  'normal_sdfixed') for K600_daily, the daily rate of reaeration as
  corrected for temperature and the diffusivity of oxygen

- K600_daily_sigma:

  The standard deviation of a dnorm distribution having mean equal to
  `exp(K600_daily_predlog)` (applicable when pool_K600 is
  'linear_sdfixed' or 'binned_sdfixed') for K600_daily, the daily rate
  of reaeration as corrected for temperature and the diffusivity of
  oxygen

- K600_daily_sdlog_sigma:

  hyperparameter for pool_K600 in c('normal'). The scale (= sigma)
  parameter of a half-normal distribution of sdlog in K ~ lN(meanlog,
  sdlog), sdlog ~ halfnormal(0, sigma=sdlog_sigma). Visualize the PDF of
  K600_daily_sdlog with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- K600_daily_sigma_sigma:

  hyperparameter for pool_K600 in c('linear','binned'). The scale (=
  sigma) parameter of a half-normal distribution of sigma in K ~
  lN(meanlog, sigma), sigma ~ halfnormal(0, sigma=sigma_sigma).
  Visualize the PDF of K600_daily_sdlog with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_obs_iid_sigma_scale:

  The scale (= sigma) parameter of a half-Cauchy distribution for
  err_obs_iid_sigma, the standard deviation of the observation error.
  Visualize the PDF of err_obs_iid_sigma with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_proc_iid_sigma_scale:

  The scale (= sigma) parameter of a half-Cauchy distribution for
  err_proc_iid_sigma, the standard deviation of the uncorrelated (IID)
  component of process (& sometimes observation) error. Visualize the
  PDF of err_proc_iid_sigma with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_proc_acor_phi_alpha:

  The alpha (= shape1) parameter on a beta distribution for
  err_proc_acor_phi, the autocorrelation coefficient for the
  autocorrelated component of process (& sometimes observation) error.
  Visualize the PDF of err_proc_acor_phi with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_proc_acor_phi_beta:

  The beta (= shape2) parameter on a beta distribution for
  err_proc_acor_phi, the autocorrelation coefficient for the
  autocorrelated component of process (& sometimes observation) error.
  Visualize the PDF of err_proc_acor_phi with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_proc_acor_sigma_scale:

  The scale (= sigma) parameter of a half-Cauchy distribution for
  err_proc_acor_sigma, the standard deviation of the autocorrelated
  component of process (& sometimes observation) error. Visualize the
  PDF of err_proc_acor_sigma with
  [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md).

- err_proc_acor_light_alpha_sigma:

  The scale parameter of a half-normal prior on
  `err_proc_acor_light_alpha`. The fitted alpha is the increase in the
  process-error innovation standard deviation at a timestep receiving
  all of the day's light; actual increases are alpha times the
  timestep's fraction of daily light.

- err_mult_GPP_sdlog_sigma:

  The scale parameter of a half-normal distribution for
  err_mult_GPP_sdlog, the scale parameter of the lognormal distribution
  of err_mult_GPP. err_mult_GPP is multiplied by light and then
  normalized to a daily mean of 1 before being multiplied by GPP_daily
  to estimate GPP_inst. The effect is a special kind of process error
  that is proportional to light (with noise) and is applied to GPP
  rather than to dDO/dt.

- params_in:

  Character vector of hyperparameters to pass from the specs list into
  the data list for the MCMC run. Will be automatically generated during
  the specs() call; need only be revised if you're using a custom model
  that requires different hyperparameters.

- params_out:

  a character vector of parameters whose values in the MCMC runs should
  be recorded and summarized

- n_chains:

  the number of chains to run

- n_cores:

  the number of cores to apply to this run

- burnin_steps:

  the number of steps per chain to run and ignore before starting to
  collect MCMC 'data'

- saved_steps:

  the number of MCMC steps per chain to save

- thin_steps:

  the number of steps to move before saving another step. 1 means save
  all steps.

- stan_engine:

  Character string specifying which Stan R interface to use. Either
  "rstan" or "cmdstanr". Defaults to "rstan" to preserve the original
  behavior of the streamMetabolizer package. CmdStanR requires both the
  R package and a configured CmdStan installation; see
  [`cmdstanr::install_cmdstan()`](https://mc-stan.org/cmdstanr/reference/install_cmdstan.html).

- verbose:

  logical. give status messages?

- weights:

  For Kmodel, character vector indicating the type of weighting to use.
  Set to c() for no weights. One of c("1/CI", "K600/CI", c()).

- filters:

  For Kmodel, named numeric vector of limits to use in filtering
  data_daily. Elements may include
  c("CI.max","discharge.daily.max","velocity.daily.max"). If an element
  is given, the corresponding filter is applied:
  K600.daily.upper-K600.daily.lower \<= CI.max, discharge.daily \<=
  discharge.daily.max, velocity.daily \<= velocity.daily.max

- predictors:

  For Kmodel, character vector of variables (column names in data or
  data_daily) to use in predicting K. Leave blank or set to c() for no
  predictors. Otherwise, one or more of these may be included: c("date",
  "velocity.daily", "discharge.daily").

- transforms:

  For Kmodel, a named character vector of names of functions (probably
  'log' or NA) to apply to K600.daily and/or the predictors. K600.daily
  should probably be logged. The vector names must match the values of
  `predictors`, although not all elements of `predictors` must be
  included in `transforms`. Recommended transforms include
  `c(K600.daily='log', date=NA, velocity.daily="log", discharge.daily="log")`

- other_args:

  Other arguments passed to the fitting function given by
  `specs$engine`. `na.rm=TRUE` is already passed to `mean` (which is
  actually implemented as `sum`, anyway).

- K600_lnQ_cnode_meanlog:

  For a sim model with pool_K600='binned'. The mean of a lognormal
  distribution describing the y=K600 value of the middle (or just past
  middle) node in the piecewise lnK ~ lnQ relationship

- K600_lnQ_cnode_sdlog:

  For a sim model with pool_K600='binned'. The sd of a lognormal
  distribution describing the y=K600 value of the middle (or just past
  middle) node in the piecewise lnK ~ lnQ relationship

- K600_lnQ_nodediffs_meanlog:

  For a sim model with pool_K600='binned'. The average (in log space)
  difference between ln(K) values of successive nodes. A non-zero value
  introduces a trend in K ~ Q.

- lnK600_lnQ_nodes:

  For a sim model with pool_K600='binned'. The values of lnK600 at each
  node. The default value of this spec is a function that computes
  lnK600s based on simulated K~Q relationships.

- discharge_daily:

  Daily values, or a function to generate daily values, of mean daily
  discharge in m^3 s^-1. Fixed values may alternatively be specified as
  discharge.daily in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- DO_mod_1:

  Daily values, or a function to generate daily values, of the first
  DO.mod value on each date. Fixed values may alternatively be specified
  as `DO.mod.1` in the `data_daily` passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).
  Or may be implied by a `DO.obs` column in `data`, from which the first
  values on each date will be extracted by
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- K600_daily:

  Daily values, or a function to generate daily values, of the
  reaeration rate constant K600. Fixed values may alternatively be
  specified as `K600.daily` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- GPP_daily:

  Daily values, or a function to generate daily values, of the
  photosynthesis parameter GPP_daily. Fixed values may alternatively be
  specified as `GPP.daily` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- Pmax:

  Daily values, or a function to generate daily values, of the
  photosynthesis parameter Pmax. Fixed values may alternatively be
  specified as `Pmax` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- alpha:

  Daily values, or a function to generate daily values, of the
  photosynthesis parameter alpha. Fixed values may alternatively be
  specified as `alpha` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- ER_daily:

  Daily values, or a function to generate daily values, of the
  respiration parameter ER_daily. Fixed values may alternatively be
  specified as `ER.daily` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- ER20:

  Daily values, or a function to generate daily values, of the
  respiration parameter ER20. Fixed values may alternatively be
  specified as `ER20` in the data_daily passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

- err_obs_sigma:

  Daily values, or a function to generate daily values, of the sd of
  observation error, or 0 for no observation error. Observation errors
  are those applied to DO.mod after generating the full time series of
  modeled values.

- err_obs_phi:

  Daily values, or a function to generate daily values, of the
  autocorrelation coefficient of the observation errors, or 0 for
  uncorrelated errors.

- err_proc_sigma:

  Daily values, or a function to generate daily values, of the sd of
  process error, or 0 for no process error. Process errors are applied
  at each time step, and therefore propagate into the next timestep.

- err_proc_phi:

  Daily values, or a function to generate daily values, of the
  autocorrelation coefficient of the process errors, or 0 for
  uncorrelated errors.

- err_round:

  A single value indicating whether simulated DO.obs should be rounded
  to simulate the common practice of only reporting a few significant
  figures for DO. Use NA for no effect, or an integer as in the `digits`
  argument to [`round()`](https://rdrr.io/r/base/Round.html) if
  simulated DO.obs should be rounded to the given number of digits
  beyond `.`.

- sim_seed:

  NA to specify that each call to predict_DO should generate new values,
  or an integer, as in the `seed` argument to
  [`set.seed()`](https://rdrr.io/r/base/Random.html), specifying the
  seed to set before every execution of predict_DO and/or predict_metab.

## Value

an internally consistent list of arguments that may be passed to `metab`
as the `specs` argument

## Relevant arguments

- metab_bayes: Always relevant:
  `model_name, engine, split_dates, keep_mcmcs, keep_mcmc_data, day_start, day_end, day_tests, ER_daily_mu, ER_daily_sigma, params_in, params_out, n_chains, n_cores, burnin_steps, saved_steps, thin_steps, verbose`.
  The need for other arguments depends on features of the model
  structure, as from `mm_parse_name(model_name)`:

  - If `GPP_fun=='linlight'` then `GPP_daily_mu, GPP_daily_sigma`, while
    if `GPP_fun=='satlight'` then
    `alpha_meanlog, alpha_sdlog, Pmax_mu, Pmax_sigma`.

  - If `pool_K600=='none'` then `K600_daily_meanlog, K600_daily_sdlog`.

  - If `pool_K600=='normal'` then
    `K600_daily_meanlog_meanlog, K600_daily_meanlog_sdlog, K600_daily_sdlog_sigma`.

  - If `pool_K600=='linear'` then
    `lnK600_lnQ_intercept_mu, lnK600_lnQ_intercept_sigma, lnK600_lnQ_slope_mu, lnK600_lnQ_slope_sigma, K600_daily_sigma_sigma`.

  - If `pool_K600=='binned'` then
    `K600_lnQ_nodes_centers, K600_lnQ_nodediffs_sdlog, K600_lnQ_nodes_meanlog, K600_lnQ_nodes_sdlog, K600_daily_sigma_sigma`.

  - If `err_obs_iid` then `err_obs_iid_sigma_scale`.

  - If `err_proc_acor` then
    `err_proc_acor_phi_alpha, err_proc_acor_phi_beta, err_proc_acor_sigma_scale`,
    and, if `err_proc_acor_light`, `err_proc_acor_light_alpha_sigma`.

  - If `err_proc_iid` then `err_proc_iid_sigma_scale`.

  - If `err_proc_GPP` then `err_mult_GPP_sdlog_sigma`.

- metab_mle:
  `model_name, day_start, day_end, day_tests, init.GPP.daily, init.Pmax, init.alpha, init.ER.daily, init.ER20, init.K600.daily`

- metab_night: `model_name, day_start, day_end, day_tests`

- metab_Kmodel:
  `model_name, engine, day_start, day_end, day_tests, weights, filters, predictors, transforms, other_args`.
  Note that the defaults for `weights`, `predictors`, `filters`, and
  `transforms` are adjusted according to the `engine` implied by
  `model_name`.

- metab_sim:
  `model_name, day_start, day_end, day_tests, err_obs_sigma, err_obs_phi, err_proc_sigma, err_proc_phi, sim_seed`.
  Those arguments whose period-separated name occurs in the default
  data_daily argument to metab(sim) can be specified here as NULL,
  numeric, or a function to be called each time `predict_DO` or
  `predict_metab` is called on the model. If given as a function, an
  argument will be called with any already-evaluated parameters
  (including the contents of data_daily and n, the number of dates)
  passed in as arguments; for example, K600_daily can see n,
  discharge.daily, and GPP_daily can see n, discharge.daily, and
  K600.daily.

## MLE Initial Values

For metab_mle models (maximum likelihood estimation), specification
arguments whose names begin with `init` are applicable. Which arguments
are required depends on the value of model_name and can be determined by
calling `grep('^init.', names(specs(mname)), value=TRUE)` once for your
model name `mname` before supplying any arguments.

## Examples

``` r
specs(mm_name(type='mle', err_obs_iid=FALSE, err_proc_iid=TRUE))
#> Model specifications:
#>   model_name        m_np_pi_tr_plrckm.nlm                                       
#>   day_start         4                                                           
#>   day_end           28                                                          
#>   day_tests         full_day, even_timesteps, complete_data, pos_discharge, p...
#>   required_timestep NA                                                          
#>   init.GPP.daily    8                                                           
#>   init.ER.daily     -10                                                         
#>   init.K600.daily   10                                                          
specs(mm_name(type='bayes', pool_K600='normal'))
#> Model specifications:
#>   model_name                 b_Kn_oipi_tr_plrckm.stan                           
#>   engine                     stan                                               
#>   split_dates                FALSE                                              
#>   keep_mcmcs                 TRUE                                               
#>   keep_mcmc_data             TRUE                                               
#>   day_start                  4                                                  
#>   day_end                    28                                                 
#>   day_tests                  full_day, even_timesteps, complete_data, pos_dis...
#>   required_timestep          NA                                                 
#>   GPP_daily_mu               3.1                                                
#>   GPP_daily_lower            -Inf                                               
#>   GPP_daily_sigma            6                                                  
#>   ER_daily_mu                -7.1                                               
#>   ER_daily_upper             Inf                                                
#>   ER_daily_sigma             7.1                                                
#>   K600_daily_meanlog_meanlog 2.484906649788                                     
#>   K600_daily_meanlog_sdlog   1.32                                               
#>   K600_daily_sdlog_sigma     0.05                                               
#>   err_obs_iid_sigma_scale    0.03                                               
#>   err_proc_iid_sigma_scale   5                                                  
#>   params_in                  GPP_daily_mu, GPP_daily_lower, GPP_daily_sigma, ...
#>   params_out                 GPP, ER, DO_R2, GPP_daily, ER_daily, K600_daily,...
#>   n_chains                   4                                                  
#>   n_cores                    4                                                  
#>   burnin_steps               500                                                
#>   saved_steps                500                                                
#>   thin_steps                 1                                                  
#>   stan_engine                rstan                                              
#>   verbose                    FALSE                                              
```
