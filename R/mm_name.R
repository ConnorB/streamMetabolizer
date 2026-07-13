#' Find the name of a model by its features
#'
#' A `model_name` concisely specifies the structure of a metabolism model.
#' From a `model_name`, an appropriate set of model specifications
#' (parameters and runtime options) can be generated with [specs()].
#' From a complete `specs` list, a metabolism model can be run with
#' [metab()].
#'
#' While the `Usage` shows all valid values for each argument, not all
#' argument combinations are valid; the combination will also be checked if
#' `check_validity==TRUE`. For arguments not explicitly specified, defaults
#' depend on the value of `type`: any argument that is not explicitly
#' supplied (besides `type` and `check_validity`) will default to the
#' values indicated by `mm_parse_name(mm_valid_names(type)[1])`.
#'
#' @details
#'
#' \subsection{pool_K600}{
#'
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
#' specified)\cr } }
#'
#' @seealso The converse of this function is [mm_parse_name()].
#'
#' @param type character. The model type. Options: \itemize{ \item `mle`:
#'   maximum likelihood estimation (see also [metab_mle()]) \item
#'   `bayes`: bayesian hierarchical models [metab_bayes()] \item
#'   `night`: nighttime regression (see also [metab_night()])
#'   \item `Kmodel`: regression of *daily* estimates of
#'   `K600.daily` versus discharge, time, etc., usually for 3-phase
#'   estimation of K alone (by MLE or nighttime regression), K vs discharge
#'   (using this model), and then GPP and ER with fixed K (by MLE) (see also
#'   [metab_Kmodel()]) \item `sim`: simulation of `DO.obs`
#'   'data' for testing other models (see also [metab_sim()]) }
#' @param pool_K600 character. How should the model pool information among
#'   days to get more consistent daily estimates for K600? Options (see Details
#'   for more): \itemize{ \item `none`: no pooling of K600 \item
#'   `normal`: \eqn{K600 ~ N(mu, sigma)} \item `linear`: \eqn{K600 ~
#'   N(B[0] + B[1]*Q, sigma)} \item `binned`: \eqn{K600 ~ N(B[Q_bin],
#'   sigma)} where \eqn{mu ~ N(mu_mu, mu_sigma)} and \eqn{sigma ~ N(sigma_mu,
#'   sigma_sigma)} \item `complete`: applicable only for
#'   `type='Kmodel'`, which is generally used in conjunction with preceding
#'   estimates of K (e.g., by `type='mle'` or `type='night'`) and
#'   subsequent estimates of GPP and ER (e.g., by `type='mle'` with daily
#'   K600 values specified) }
#' @param err_obs_iid logical. Should IID observation error be included? If not,
#'   the model will be fit to the differences in successive DO measurements,
#'   rather than to the DO measurements themselves.
#' @param err_proc_acor logical. Should autocorrelated process error (with the
#'   autocorrelation term phi fitted) be included? For multi-day Bayesian
#'   process-error models, the AR likelihood continues across day boundaries.
#' @param err_proc_acor_light logical. Should the innovation standard deviation
#'   of autocorrelated process error increase linearly with the fraction of
#'   each day's light occurring at that timestep? Only available when
#'   `err_proc_acor=TRUE`.
#' @param err_proc_iid logical. Should IID process error be included?
#' @param err_proc_GPP logical. Should IID process error in GPP be included?
#'   This kind of error occurs only during the day and is used to adjust GPP
#'   before passing that adjusted GPP into the dDO/dt equation. The GPP_inst
#'   variable is the corrected GPP, and a new variable, GPP_inst_partial,
#'   contains the pre-adjustment GPP estimates
#' @param ode_method character. The method to use in solving the ordinary
#'   differential equation for DO. Options: \itemize{ \item `euler`,
#'   formerly `Euler`: the final change in DO from t=1 to t=2 is solely a
#'   function of GPP, ER, DO, etc. at t=1 \item `trapezoid`, formerly
#'   `pairmeans`: the final change in DO from t=1 to t=2 is a function of
#'   the mean values of GPP, ER, etc. across t=1 and t=2. \item for
#'   `type='mle'`, options also include `rk2` and any character method
#'   accepted by [deSolve::ode()] in the `deSolve` package
#'   (`lsoda`, `lsode`, `lsodes`, `lsodar`, `vode`,
#'   `daspk`, `rk4`, `ode23`, `ode45`, `radau`,
#'   `bdf`, `bdf_d`, `adams`, `impAdams`, and
#'   `impAdams_d`; note that many of these have not been well tested in the
#'   context of `streamMetabolizer` models) }
#' @param GPP_fun character. Function dictating how gross primary productivity
#'   (GPP) varies within each day. Options: \itemize{ \item `linlight`: GPP
#'   is a linear function of light with an intercept at 0 and a slope that
#'   varies by day. \cr `GPP(t) = GPP.daily * light(t) / mean.light`
#'   \itemize{ \item `GPP.daily`: the daily mean GPP, which is partitioned
#'   into timestep-specific rates according to the fraction of that day's
#'   average light that occurs at each timestep (specifically, `mean.light`
#'   is the mean of the first 24 hours of the date's data window) } \item
#'   `satlight`: GPP is a saturating function of light. \cr `GPP(t) =
#'   Pmax * tanh(alpha * light(t) / Pmax)` \itemize{ \item `Pmax`: the
#'   maximum possible GPP \item `alpha`: a descriptor of the rate of
#'   increase of GPP as a function of light } \item `satlightq10temp`: GPP
#'   is a saturating function of light and an exponential function of
#'   temperature. \cr `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax) *
#'   1.036 ^ (temp.water(t) - 20)` \itemize{ \item `Pmax`: the maximum
#'   possible GPP \item `alpha`: a descriptor of the rate of increase of
#'   GPP as a function of light } \item `NA`: applicable only to
#'   `type='Kmodel'`, for which GPP is not estimated }
#' @param ER_fun character. Function dictating how ecosystem respiration (ER)
#'   varies within each day. Options: \itemize{ \item `constant`: ER is
#'   constant over every timestep of the day. \cr `ER(t) = ER.daily`
#'   \itemize{ \item `ER.daily`: the daily mean ER, which is equal to
#'   instantaneous ER at all times } \item `q10temp`: ER at each timestep
#'   is an exponential function of the water temperature and a
#'   temperature-normalized base rate. \cr `ER(t) = ER20 * 1.045 ^
#'   (temp.water(t) - 20)` \itemize{ \item `ER20`: the value of ER when
#'   `temp.water` is 20 degrees C } \item `NA`: applicable only to
#'   `type='Kmodel'`, for which ER is not estimated }
#' @param deficit_src character. From what DO estimate (observed or modeled)
#'   should the DO deficit be computed? Options: \itemize{ \item `DO_mod`:
#'   the DO deficit at time t will be \eqn{(DO.sat(t) - DO_mod(t))}, the difference
#'   between the equilibrium-saturation value and the current best estimate of
#'   the true DO concentration at that time \item `DO_obs`: the DO deficit
#'   at time t will be \eqn{(DO.sat(t) - DO.obs(t))}, the difference between the
#'   equilibrium-saturation value and the measured DO concentration at that time
#'   \item `DO_obs_filter`: applicable only to `type='night'`: a
#'   smoothing filter is applied over the measured DO.obs values before applying
#'   nighttime regression \item `NA`: applicable only to
#'   `type='Kmodel'`, for which DO deficit is not estimated }
#' @param engine character. With which function or software should the model
#'   fitting be done? \itemize{ \item for `type='mle'`: `nlm` only
#'   (the default) \item for `type='bayes'`: `stan` only (the
#'   default), an external software package that runs MCMC chains for Bayesian
#'   models (see http://mc-stan.org) \item for `type='night'`: `lm`
#'   only (the default) \item for `type='Kmodel'`: `mean`, `lm`,
#'   or `loess` enable different types of relationships between daily K600
#'   and its predictors (nothing, discharge, time, etc.) \item for
#'   `type='sim'`: `rnorm` only (the default) }
#' @param check_validity logical. if TRUE, this function checks the resulting
#'   name against `mm_valid_names(type)`.
#' @import dplyr
#' @export
#' @examples
#' mm_name('mle')
#' mm_name('mle', GPP_fun='satlight', ER_fun='q10temp')
#' mm_name('night')
#' mm_name('sim', err_proc_acor=TRUE)
#' mm_name('bayes', pool_K600='binned')
#' mm_name('bayes', err_obs_iid=FALSE, err_proc_acor=TRUE,
#'   err_proc_acor_light=TRUE, err_proc_iid=FALSE, deficit_src='DO_obs')
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
    relevant_args <- names(formals(mm_name)) %>%
      .[!(. %in% c('type', 'check_validity'))]
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
      .cli_abort("need err_obs_iid to be a logical of length 1")
    }
    if (!is.logical(err_proc_acor) || length(err_proc_acor) != 1) {
      .cli_abort("need err_proc_acor to be a logical of length 1")
    }
    if (!is.logical(err_proc_acor_light) || length(err_proc_acor_light) != 1) {
      .cli_abort("need err_proc_acor_light to be a logical of length 1")
    }
    if (err_proc_acor_light && !err_proc_acor) {
      .cli_abort("err_proc_acor_light requires err_proc_acor=TRUE")
    }
    if (!is.logical(err_proc_iid) || length(err_proc_iid) != 1) {
      .cli_abort("need err_proc_iid to be a logical of length 1")
    }
    if (!is.logical(err_proc_GPP) || length(err_proc_GPP) != 1) {
      .cli_abort("need err_proc_GPP to be a logical of length 1")
    }
    ode_method <- match.arg(ode_method)
    if (ode_method %in% c('Euler', 'pairmeans')) {
      .cli_warn(
        "for ode_method, 'Euler' and 'pairmeans' are deprecated in favor of 'euler' and 'trapezoid'"
      )
    }
    GPP_fun <- match.arg(GPP_fun)
    ER_fun <- match.arg(ER_fun)
    deficit_src <- match.arg(deficit_src)
  } else {
    if (any(!(given_args %in% c('type', 'engine', 'check_validity')))) {
      .cli_abort(
        "for Kmodel, only type, engine, and check_validity may be specified"
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
    .cli_abort("mismatch between type (", type, ") and engine (", engine, ")")
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
    .cli_abort("need check_validity to be a logical of length 1")
  } else {
    check_validity[1]
  }
  if (isTRUE(check_validity)) {
    mm_validate_name(mmname)
  }

  # return
  mmname
}
