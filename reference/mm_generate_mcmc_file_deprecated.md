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

  character. The model type. Options:

  - `mle`: maximum likelihood estimation (see also
    [`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md))

  - `bayes`: bayesian hierarchical models
    [`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md)

  - `night`: nighttime regression (see also
    [`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md))

  - `Kmodel`: regression of *daily* estimates of `K600.daily` versus
    discharge, time, etc., usually for 3-phase estimation of K alone (by
    MLE or nighttime regression), K vs discharge (using this model), and
    then GPP and ER with fixed K (by MLE) (see also
    [`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md))

  - `sim`: simulation of `DO.obs` 'data' for testing other models (see
    also
    [`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md))

- pool_K600:

  character. How should the model pool information among days to get
  more consistent daily estimates for K600? Options (see Details for
  more):

  - `none`: no pooling of K600

  - `normal`: \\K600 ~ N(mu, sigma)\\

  - `linear`: \\K600 ~ N(B\[0\] + B\[1\]\*Q, sigma)\\

  - `binned`: \\K600 ~ N(B\[Q_bin\], sigma)\\ where \\mu ~ N(mu_mu,
    mu_sigma)\\ and \\sigma ~ N(sigma_mu, sigma_sigma)\\

  - `complete`: applicable only for `type='Kmodel'`, which is generally
    used in conjunction with preceding estimates of K (e.g., by
    `type='mle'` or `type='night'`) and subsequent estimates of GPP and
    ER (e.g., by `type='mle'` with daily K600 values specified)

- err_obs_iid:

  logical. Should IID observation error be included? If not, the model
  will be fit to the differences in successive DO measurements, rather
  than to the DO measurements themselves.

- err_proc_acor:

  logical. Should autocorrelated process error (with the autocorrelation
  term phi fitted) be included? For multi-day Bayesian process-error
  models, the AR likelihood continues across day boundaries.

- err_proc_iid:

  logical. Should IID process error be included?

- err_proc_GPP:

  logical. Should IID process error in GPP be included? This kind of
  error occurs only during the day and is used to adjust GPP before
  passing that adjusted GPP into the dDO/dt equation. The GPP_inst
  variable is the corrected GPP, and a new variable, GPP_inst_partial,
  contains the pre-adjustment GPP estimates

- ode_method:

  character. The method to use in solving the ordinary differential
  equation for DO. Options:

  - `euler`, formerly `Euler`: the final change in DO from t=1 to t=2 is
    solely a function of GPP, ER, DO, etc. at t=1

  - `trapezoid`, formerly `pairmeans`: the final change in DO from t=1
    to t=2 is a function of the mean values of GPP, ER, etc. across t=1
    and t=2.

  - for `type='mle'`, options also include `rk2` and any character
    method accepted by
    [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html) in the
    `deSolve` package (`lsoda`, `lsode`, `lsodes`, `lsodar`, `vode`,
    `daspk`, `rk4`, `ode23`, `ode45`, `radau`, `bdf`, `bdf_d`, `adams`,
    `impAdams`, and `impAdams_d`; note that many of these have not been
    well tested in the context of `streamMetabolizer` models)

- GPP_fun:

  character. Function dictating how gross primary productivity (GPP)
  varies within each day. Options:

  - `linlight`: GPP is a linear function of light with an intercept at 0
    and a slope that varies by day.  
    `GPP(t) = GPP.daily * light(t) / mean.light`

    - `GPP.daily`: the daily mean GPP, which is partitioned into
      timestep-specific rates according to the fraction of that day's
      average light that occurs at each timestep (specifically,
      `mean.light` is the mean of the first 24 hours of the date's data
      window)

  - `satlight`: GPP is a saturating function of light.  
    `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax)`

    - `Pmax`: the maximum possible GPP

    - `alpha`: a descriptor of the rate of increase of GPP as a function
      of light

  - `satlightq10temp`: GPP is a saturating function of light and an
    exponential function of temperature.  
    `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax) * 1.036 ^ (temp.water(t) - 20)`

    - `Pmax`: the maximum possible GPP

    - `alpha`: a descriptor of the rate of increase of GPP as a function
      of light

  - `NA`: applicable only to `type='Kmodel'`, for which GPP is not
    estimated

- ER_fun:

  character. Function dictating how ecosystem respiration (ER) varies
  within each day. Options:

  - `constant`: ER is constant over every timestep of the day.  
    `ER(t) = ER.daily`

    - `ER.daily`: the daily mean ER, which is equal to instantaneous ER
      at all times

  - `q10temp`: ER at each timestep is an exponential function of the
    water temperature and a temperature-normalized base rate.  
    `ER(t) = ER20 * 1.045 ^ (temp.water(t) - 20)`

    - `ER20`: the value of ER when `temp.water` is 20 degrees C

  - `NA`: applicable only to `type='Kmodel'`, for which ER is not
    estimated

- deficit_src:

  character. From what DO estimate (observed or modeled) should the DO
  deficit be computed? Options:

  - `DO_mod`: the DO deficit at time t will be \\(DO.sat(t) -
    DO_mod(t))\\, the difference between the equilibrium-saturation
    value and the current best estimate of the true DO concentration at
    that time

  - `DO_obs`: the DO deficit at time t will be \\(DO.sat(t) -
    DO.obs(t))\\, the difference between the equilibrium-saturation
    value and the measured DO concentration at that time

  - `DO_obs_filter`: applicable only to `type='night'`: a smoothing
    filter is applied over the measured DO.obs values before applying
    nighttime regression

  - `NA`: applicable only to `type='Kmodel'`, for which DO deficit is
    not estimated

- engine:

  character. With which function or software should the model fitting be
  done?

  - for `type='mle'`: `nlm` only (the default)

  - for `type='bayes'`: `stan` only (the default), an external software
    package that runs MCMC chains for Bayesian models (see
    http://mc-stan.org)

  - for `type='night'`: `lm` only (the default)

  - for `type='Kmodel'`: `mean`, `lm`, or `loess` enable different types
    of relationships between daily K600 and its predictors (nothing,
    discharge, time, etc.)

  - for `type='sim'`: `rnorm` only (the default)
