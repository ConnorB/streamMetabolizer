# Generate the models in inst/models/bayes

Generate the models in inst/models/bayes

## Usage

``` r
mm_generate_mcmc_file_deprecated(
  type = "bayes",
  pool_K600 = c("none", "normal", "normal_sdzero", "normal_sdfixed", "linear",
    "linear_sdzero", "linear_sdfixed", "binned", "binned_sdzero", "binned_sdfixed"),
  err_obs_iid = c(TRUE, FALSE),
  err_proc_acor = c(FALSE, TRUE),
  err_proc_iid = c(FALSE, TRUE),
  err_proc_GPP = c(FALSE, TRUE),
  ode_method = c("trapezoid", "euler"),
  GPP_fun = c("linlight", "satlight"),
  ER_fun = c("constant"),
  deficit_src = c("DO_mod", "DO_obs"),
  engine = "stan"
)
```

## Arguments

- type:

  A string specifying the model type:

  - `"mle"`: Maximum likelihood estimation; see
    [`metab_mle()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_mle.md).

  - `"bayes"`: Bayesian hierarchical modeling; see
    [`metab_bayes()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_bayes.md).

  - `"night"`: Nighttime regression; see
    [`metab_night()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_night.md).

  - `"Kmodel"`: Regression of daily `K600.daily` estimates against
    discharge, time, or other predictors; see
    [`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_Kmodel.md).

  - `"sim"`: Simulation of `DO.obs` data; see
    [`metab_sim()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_sim.md).

- pool_K600:

  A string specifying how to pool information among days for more
  consistent daily K600 estimates. See *K600 pooling* for details:

  - `"none"`: Do not pool K600.

  - `"normal"`: Use `K₆₀₀ ∼ N(μ, σ)`.

  - `"linear"`: Use `K₆₀₀ ∼ N(B[0] + B[1] × Q, σ)`.

  - `"binned"`: Use `K₆₀₀ ∼ N(B[Q_bin], σ)`, where
    `μ ∼ N(mu_mu, mu_sigma)` and `σ ∼ N(sigma_mu, sigma_sigma)`.

  - `"complete"`: Use complete pooling for `type = "Kmodel"`, generally
    between preceding K estimates and subsequent GPP and ER estimates.

- err_obs_iid:

  A logical. Should IID observation error be included? If not, the model
  will be fit to the differences in successive DO measurements, rather
  than to the DO measurements themselves.

- err_proc_acor:

  A logical. Should autocorrelated process error (with the
  autocorrelation term `phi` fitted) be included? For multi-day Bayesian
  process-error models, the AR likelihood continues across day
  boundaries.

- err_proc_iid:

  A logical. Should IID process error be included?

- err_proc_GPP:

  A logical. Should IID process error in GPP be included? This kind of
  error occurs only during the day and is used to adjust GPP before
  passing it into the dDO/dt equation. `GPP_inst` is the corrected GPP,
  and `GPP_inst_partial` contains the pre-adjustment estimates.

- ode_method:

  A string specifying the method used to solve the ordinary differential
  equation for DO:

  - `"euler"` (formerly `"Euler"`): Use conditions at the start of each
    timestep.

  - `"trapezoid"` (formerly `"pairmeans"`): Use mean conditions across
    each timestep.

  - For `type = "mle"`, `"rk2"` and methods accepted by
    [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html) are
    also available. Many have not been extensively tested with
    streamMetabolizer models.

- GPP_fun:

  A string specifying how gross primary productivity (GPP) varies within
  each day:

  - `"linlight"`: `GPP(t) = GPP.daily * light(t) / mean.light`.
    `GPP.daily` is partitioned among timesteps by their fraction of the
    day's average light.

  - `"satlight"`: `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax)`, where
    `Pmax` is the maximum possible GPP and `alpha` describes the initial
    increase with light.

  - `"satlightq10temp"`: The saturating light model multiplied by
    `1.036 ^ (temp.water(t) - 20)`.

  - `NA`: Do not estimate GPP; applicable only to `type = "Kmodel"`.

- ER_fun:

  A string specifying how ecosystem respiration (ER) varies within each
  day:

  - `"constant"`: `ER(t) = ER.daily`.

  - `"q10temp"`: `ER(t) = ER20 * 1.045 ^ (temp.water(t) - 20)`, where
    `ER20` is ER at 20 degrees C.

  - `NA`: Do not estimate ER; applicable only to `type = "Kmodel"`.

- deficit_src:

  A string specifying the DO estimate used to compute the DO deficit:

  - `"DO_mod"`: Use `DO.sat(t) − DO_mod(t)`, the difference between the
    equilibrium-saturation value and the current best estimate of the
    true DO concentration at that time.

  - `"DO_obs"`: Use `DO.sat(t) − DO.obs(t)`.

  - `"DO_obs_filter"`: Smooth `DO.obs` before nighttime regression;
    applicable only to `type = "night"`.

  - `NA`: Do not estimate DO deficit; applicable only to
    `type = "Kmodel"`.

- engine:

  A string specifying the fitting engine. Valid combinations are `"nlm"`
  for MLE, `"stan"` for Bayesian models, `"lm"` for nighttime
  regression, `"mean"`, `"lm"`, or `"loess"` for K models, and `"rnorm"`
  for simulations.
