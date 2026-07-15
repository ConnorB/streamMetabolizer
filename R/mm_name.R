#' Construct a model name from its features
#'
#' A `model_name` concisely specifies the structure of a metabolism model.
#' From a `model_name`, an appropriate set of model specifications
#' (parameters and runtime options) can be generated with [specs()].
#' From a complete `specs` list, a metabolism model can be run with
#' [metab()].
#'
#' While the usage shows all valid values for each argument, not all
#' argument combinations are valid; the combination will also be checked if
#' `check_validity = TRUE`. For arguments not explicitly specified, defaults
#' depend on the value of `type`: any argument that is not explicitly
#' supplied (besides `type` and `check_validity`) will default to the
#' values indicated by `mm_parse_name(mm_valid_names(type)[1])`.
#'
#' @section K600 pooling:
#' Here are the essential model lines (in Stan language) that distinguish the K
#' pooling options.
#'
#' \tabular{ll}{ **`pool_K600`** \tab **Model code**\cr
#'
#' `none` \tab `K600_daily ~ normal(K600_daily_mu, K600_daily_sigma)`
#' \cr
#'
#' `normal` \tab `K600_daily ~ normal(K600_daily_mu,
#' K600_daily_sigma)`\cr \tab `K600_daily_mu ~ normal(K600_daily_mu_mu,
#' K600_daily_mu_sigma)`\cr \tab `K600_daily_sigma ~
#' gamma(K600_daily_sigma_shape, K600_daily_sigma_rate)`\cr
#'
#' `linear` \tab `K600_daily_pred <- K600_daily_beta[1] +
#' K600_daily_beta[2] * discharge_daily`\cr \tab `K600_daily ~
#' normal(K600_daily_pred, K600_daily_sigma)`\cr \tab `K600_daily_beta ~
#' normal(K600_daily_beta_mu, K600_daily_beta_sigma)`\cr \tab
#' `K600_daily_sigma ~ gamma(K600_daily_sigma_shape,
#' K600_daily_sigma_rate)`\cr
#'
#' `binned` \tab `K600_daily_pred <- K600_daily_beta[Q_bin_daily]`\cr
#' \tab `K600_daily ~ normal(K600_daily_pred, K600_daily_sigma)`\cr \tab
#' `K600_daily_beta ~ normal(K600_daily_beta_mu, K600_daily_beta_sigma)`\cr
#' \tab `K600_daily_sigma ~ gamma(K600_daily_sigma_shape,
#' K600_daily_sigma_rate)`\cr
#'
#' `complete` \tab This option refers to complete pooling via
#' `metab_Kmodel` in conjunction with preceding\cr \tab estimates of K
#' (e.g., by `metab_mle` or `metab_night`) and subsequent estimates of
#' GPP and ER\cr \tab (e.g., by `metab_mle` with daily K600 values
#' specified)\cr }
#'
#' @seealso The converse of this function is [mm_parse_name()].
#'
#' @param type A string specifying the model type:
#'
#'   * `"mle"`: Maximum likelihood estimation; see [metab_mle()].
#'   * `"bayes"`: Bayesian hierarchical modeling; see [metab_bayes()].
#'   * `"night"`: Nighttime regression; see [metab_night()].
#'   * `"Kmodel"`: Regression of daily `K600.daily` estimates against
#'     discharge, time, or other predictors; see [metab_Kmodel()].
#'   * `"sim"`: Simulation of `DO.obs` data; see [metab_sim()].
#' @param pool_K600 A string specifying how to pool information among days for
#'   more consistent daily K600 estimates. See *K600 pooling* for details:
#'
#'   * `"none"`: Do not pool K600.
#'   * `"normal"`: Use `K₆₀₀ ∼ N(μ, σ)`.
#'   * `"linear"`: Use `K₆₀₀ ∼ N(B[0] + B[1] × Q, σ)`.
#'   * `"binned"`: Use `K₆₀₀ ∼ N(B[Q_bin], σ)`, where
#'     `μ ∼ N(mu_mu, mu_sigma)` and `σ ∼ N(sigma_mu, sigma_sigma)`.
#'   * `"complete"`: Use complete pooling for `type = "Kmodel"`, generally
#'     between preceding K estimates and subsequent GPP and ER estimates.
#' @param err_obs_iid A logical. Should IID observation error be included? If
#'   not, the model will be fit to the differences in successive DO
#'   measurements, rather than to the DO measurements themselves.
#' @param err_proc_acor A logical. Should autocorrelated process error (with the
#'   autocorrelation term `phi` fitted) be included? For multi-day Bayesian
#'   process-error models, the AR likelihood continues across day boundaries.
#' @param err_proc_acor_light A logical. Should the innovation standard
#'   deviation of autocorrelated process error increase linearly with the
#'   fraction of each day's light occurring at that timestep? Only available
#'   when `err_proc_acor = TRUE`.
#' @param err_proc_iid A logical. Should IID process error be included?
#' @param err_proc_GPP A logical. Should IID process error in GPP be included?
#'   This kind of error occurs only during the day and is used to adjust GPP
#'   before passing it into the dDO/dt equation. `GPP_inst` is the corrected
#'   GPP, and `GPP_inst_partial` contains the pre-adjustment estimates.
#' @param ode_method A string specifying the method used to solve the ordinary
#'   differential equation for DO:
#'
#'   * `"euler"` (formerly `"Euler"`): Use conditions at the start of each
#'     timestep.
#'   * `"trapezoid"` (formerly `"pairmeans"`): Use mean conditions across each
#'     timestep.
#'   * For `type = "mle"`, `"rk2"` and methods accepted by [deSolve::ode()]
#'     are also available. Many have not been extensively tested with
#'     streamMetabolizer models.
#' @param GPP_fun A string specifying how gross primary productivity (GPP)
#'   varies within each day:
#'
#'   * `"linlight"`: `GPP(t) = GPP.daily * light(t) / mean.light`.
#'     `GPP.daily` is partitioned among timesteps by their fraction of the day's
#'     average light.
#'   * `"satlight"`: `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax)`, where
#'     `Pmax` is the maximum possible GPP and `alpha` describes the initial
#'     increase with light.
#'   * `"satlightq10temp"`: The saturating light model multiplied by
#'     `1.036 ^ (temp.water(t) - 20)`.
#'   * `NA`: Do not estimate GPP; applicable only to `type = "Kmodel"`.
#' @param ER_fun A string specifying how ecosystem respiration (ER) varies
#'   within each day:
#'
#'   * `"constant"`: `ER(t) = ER.daily`.
#'   * `"q10temp"`: `ER(t) = ER20 * 1.045 ^ (temp.water(t) - 20)`, where `ER20`
#'     is ER at 20 degrees C.
#'   * `NA`: Do not estimate ER; applicable only to `type = "Kmodel"`.
#' @param deficit_src A string specifying the DO estimate used to compute the DO
#'   deficit:
#'
#'   * `"DO_mod"`: Use `DO.sat(t) − DO_mod(t)`, the difference between the
#'     equilibrium-saturation value and the current best estimate of the true DO
#'     concentration at that time.
#'   * `"DO_obs"`: Use `DO.sat(t) − DO.obs(t)`.
#'   * `"DO_obs_filter"`: Smooth `DO.obs` before nighttime regression;
#'     applicable only to `type = "night"`.
#'   * `NA`: Do not estimate DO deficit; applicable only to `type = "Kmodel"`.
#' @param engine A string specifying the fitting engine. Valid combinations are
#'   `"nlm"` for MLE, `"stan"` for Bayesian models, `"lm"` for nighttime
#'   regression, `"mean"`, `"lm"`, or `"loess"` for K models, and `"rnorm"`
#'   for simulations.
#' @param check_validity A logical. If `TRUE`, check the resulting name against
#'   `mm_valid_names(type)`.
#' @returns A string containing a valid encoded model name.
#' @import dplyr
#' @export
#' @examples
#' mm_name("mle")
#' mm_name("mle", GPP_fun = "satlight", ER_fun = "q10temp")
#' mm_name("night")
#' mm_name("sim", err_proc_acor = TRUE)
#' mm_name("bayes", pool_K600 = "binned")
#' mm_name("bayes",
#'   err_obs_iid = FALSE,
#'   err_proc_acor = TRUE,
#'   err_proc_acor_light = TRUE,
#'   err_proc_iid = FALSE,
#'   deficit_src = "DO_obs"
#' )
mm_name <- function(
  type = c('mle', 'bayes', 'night', 'Kmodel', 'sim'),
  #pool_GPP='none', pool_ER='none', pool_eoi='alldays', pool_epc='alldays', pool_epi='alldays',
  pool_K600 = c(
    'none',
    'normal',
    'normal_sdzero',
    'normal_sdfixed',
    'linear',
    'linear_sdzero',
    'linear_sdfixed',
    'binned',
    'binned_sdzero',
    'binned_sdfixed',
    'complete'
  ),
  err_obs_iid = c(TRUE, FALSE),
  err_proc_acor = c(FALSE, TRUE),
  err_proc_acor_light = c(FALSE, TRUE),
  err_proc_iid = c(FALSE, TRUE),
  err_proc_GPP = c(FALSE, TRUE),
  ode_method = c(
    'trapezoid',
    'euler',
    'rk2',
    'lsoda',
    'lsode',
    'lsodes',
    'lsodar',
    'vode',
    'daspk',
    'rk4',
    'ode23',
    'ode45',
    'radau',
    'bdf',
    'bdf_d',
    'adams',
    'impAdams',
    'impAdams_d',
    'Euler',
    'pairmeans',
    'NA'
  ),
  GPP_fun = c('linlight', 'satlight', 'satlightq10temp', 'NA'),
  ER_fun = c('constant', 'q10temp', 'NA'),
  deficit_src = c('DO_mod', 'DO_obs', 'DO_obs_filter', 'NA'),
  engine = c('stan', 'nlm', 'lm', 'mean', 'loess', 'rnorm'),
  check_validity = TRUE
) {
  # determine type
  type <- match.arg(type)

  # set type-specific defaults where values weren't specified
  . <- '.dplyr.var'
  if (type != 'Kmodel') {
    relevant_args <- names(formals(mm_name)) |>
      (\(x) x[!x %in% c('type', 'check_validity')])()
  } else {
    # only one argument allowed for Kmodel
    relevant_args <- 'engine'
    # directly specify all the rest
    pool_K600 <- 'complete'
    pool_all <- 'complete'
    err_obs_iid <- FALSE
    err_proc_acor <- FALSE
    err_proc_acor_light <- FALSE
    err_proc_iid <- FALSE
    err_proc_GPP <- FALSE
    ode_method <- 'NA'
    GPP_fun <- 'NA'
    ER_fun <- 'NA'
    deficit_src <- 'NA'
  }
  given_args <- names(match.call()[-1])
  missing_args <- relevant_args[!(relevant_args %in% given_args)]
  if (length(missing_args) > 0) {
    default_args <- mm_parse_name(mm_valid_names(type)[1])
    for (ms in missing_args) {
      assign(ms, default_args[[ms]])
    }
  }

  # check arguments and throw errors as needed. these checks define the names
  # that are possible to create; will be supplemented by call to mm_valid_names
  # to see if a specific arg combo is actually implemented
  if (type != 'Kmodel') {
    pool_K600 <- match.arg(pool_K600)
    pool_all <- if (pool_K600 == 'none') 'none' else 'partial'
    if (!is.logical(err_obs_iid) || length(err_obs_iid) != 1) {
      .cli_abort("{.arg err_obs_iid} must be a logical value of length 1.")
    }
    if (!is.logical(err_proc_acor) || length(err_proc_acor) != 1) {
      .cli_abort("{.arg err_proc_acor} must be a logical value of length 1.")
    }
    if (!is.logical(err_proc_acor_light) || length(err_proc_acor_light) != 1) {
      .cli_abort(
        "{.arg err_proc_acor_light} must be a logical value of length 1."
      )
    }
    if (err_proc_acor_light && !err_proc_acor) {
      .cli_abort(
        "{.arg err_proc_acor_light} requires {.arg err_proc_acor} = {.code TRUE}."
      )
    }
    if (!is.logical(err_proc_iid) || length(err_proc_iid) != 1) {
      .cli_abort("{.arg err_proc_iid} must be a logical value of length 1.")
    }
    if (!is.logical(err_proc_GPP) || length(err_proc_GPP) != 1) {
      .cli_abort("{.arg err_proc_GPP} must be a logical value of length 1.")
    }
    ode_method <- match.arg(ode_method)
    if (ode_method %in% c('Euler', 'pairmeans')) {
      .cli_warn(
        "For {.arg ode_method}, {.val Euler} and {.val pairmeans} are deprecated; use {.val euler} and {.val trapezoid}."
      )
    }
    GPP_fun <- match.arg(GPP_fun)
    ER_fun <- match.arg(ER_fun)
    deficit_src <- match.arg(deficit_src)
  } else {
    if (any(!(given_args %in% c('type', 'engine', 'check_validity')))) {
      .cli_abort(
        "For {.val Kmodel}, only {.arg type}, {.arg engine}, and {.arg check_validity} may be specified."
      )
    }
  }
  engine <- match.arg(engine)
  if (
    !(engine %in%
      list(
        bayes = 'stan',
        mle = 'nlm',
        night = 'lm',
        Kmodel = c('mean', 'lm', 'loess'),
        sim = 'rnorm'
      )[[type]])
  ) {
    .cli_abort(
      "Model type {.val {type}} is incompatible with engine {.val {engine}}."
    )
  }

  # make the name
  mmname <- paste0(
    c(bayes = 'b', mle = 'm', night = 'n', Kmodel = 'K', sim = 's')[[type]],
    '_',
    c(
      none = '',
      normal = 'Kn',
      linear = 'Kl',
      binned = 'Kb',
      complete = 'Kc'
    )[[strsplit(pool_K600, '_')[[1]][[1]]]],
    c(none_or_fitted = '', sdzero = '0', sdfixed = 'x')[[tryCatch(
      strsplit(pool_K600, '_')[[1]][[2]],
      error = function(e) 'none_or_fitted'
    )]],
    c(none = 'np', partial = '', complete = '')[[pool_all]],
    '_',
    if (err_obs_iid) 'oi',
    if (err_proc_acor) 'pc',
    if (err_proc_acor_light) 'lv',
    if (err_proc_iid) 'pi',
    if (err_proc_GPP) 'pp',
    '_',
    c(
      Euler = 'Eu',
      pairmeans = 'pm',
      trapezoid = 'tr',
      rk2 = 'r2',
      lsoda = 'o1',
      lsode = 'o2',
      lsodes = 'o3',
      lsodar = 'o4',
      vode = 'o5',
      daspk = 'o6',
      euler = 'eu',
      rk4 = 'o8',
      ode23 = 'o9',
      ode45 = 'o10',
      radau = 'o11',
      bdf = 'o12',
      bdf_d = 'o13',
      adams = 'o14',
      impAdams = 'o15',
      impAdams_d = 'o16',
      'NA' = ''
    )[[ode_method]],
    '_',
    c(linlight = 'pl', satlight = 'ps', satlightq10temp = 'pq', 'NA' = '')[[
      GPP_fun
    ]],
    c(constant = 'rc', q10temp = 'rq', 'NA' = '')[[ER_fun]],
    c(DO_mod = 'km', DO_obs = 'ko', DO_obs_filter = 'kf', 'NA' = '')[[
      deficit_src
    ]],
    '.',
    engine
  )

  # check validity if requested
  check_validity <- if (!is.logical(check_validity)) {
    .cli_abort("{.arg check_validity} must be a logical value of length 1.")
  } else {
    check_validity[1]
  }
  if (isTRUE(check_validity)) {
    mm_validate_name(mmname)
  }

  # return
  mmname
}
