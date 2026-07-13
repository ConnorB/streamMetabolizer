# Standalone comparison of Bob Hall"s metab_pi_ar_light_2.stan model with
# streamMetabolizer"s AR(1), light-varying process-error model.
#
# Run from the streamMetabolizer repository root:
#   Rscript development/analysis/compare-bob-ar-light/compare_bob_ar_light.R
#
# Optional environment variables:
#   METAB_PROC_ERR_DIR=/path/to/metab_proc_err
#   BOB_STAN_FILE=/path/to/modernized/metab_pi_ar_light_2.stan
#   COMPARISON_OUTPUT_DIR=/path/to/output
#   CHAINS=4 ITER=2000 WARMUP=1000 SEED=20260711
#   SAVE_FITS=true
#
# This is deliberately a standalone analysis script. It is not sourced by the
# package and is not part of the package test suite.

required_packages <- c("devtools", "ggplot2", "lubridate", "rstan")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Install required packages: ", paste(missing_packages, collapse = ", "))
}

# Load the working tree so this script exercises the new, local model code.
repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".")
} else if (file.exists(file.path("..", "..", "..", "DESCRIPTION"))) {
  normalizePath(file.path("..", "..", ".."))
} else {
  NA_character_
}
if (!is.na(repo_root)) {
  devtools::load_all(repo_root, quiet = TRUE)
} else {
  if (!requireNamespace("streamMetabolizer", quietly = TRUE)) {
    stop("Run from the repository root or install streamMetabolizer")
  }
  library(streamMetabolizer)
}

metab_proc_err_dir <- Sys.getenv(
  "METAB_PROC_ERR_DIR",
  unset = "/Users/connor/GitHub/metab_proc_err"
)
data_file <- file.path(metab_proc_err_dir, "GallatinTestDownstream.csv")
bob_stan_file <- file.path(
  metab_proc_err_dir,
  "metab_pi_ar_light_2_modern.stan"
)
bob_proc_stan_file <- file.path(metab_proc_err_dir, "metab_pi_2_modern.stan")

if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}
if (!file.exists(bob_stan_file)) {
  stop("Bob Stan file not found: ", bob_stan_file)
}
if (!file.exists(bob_proc_stan_file)) {
  stop("Bob process-error Stan file not found: ", bob_proc_stan_file)
}

analysis_dir <- if (!is.na(repo_root)) {
  file.path(repo_root, "development", "analysis", "compare-bob-ar-light")
} else {
  getwd()
}
output_dir <- Sys.getenv(
  "COMPARISON_OUTPUT_DIR",
  unset = file.path(analysis_dir, "comparison_bob_ar_light_output")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

chains <- as.integer(Sys.getenv("CHAINS", unset = "4"))
iter <- as.integer(Sys.getenv("ITER", unset = "2000"))
warmup <- as.integer(Sys.getenv("WARMUP", unset = "1000"))
seed <- as.integer(Sys.getenv("SEED", unset = "20260711"))
save_fits <- tolower(Sys.getenv("SAVE_FITS", unset = "false")) == "true"
if (warmup >= iter) {
  stop("WARMUP must be less than ITER")
}
detected_cores <- parallel::detectCores()
if (is.na(detected_cores)) {
  detected_cores <- chains
}
cores <- min(chains, detected_cores)
options(mc.cores = cores)

# Constants used by Bob's example_data.R.
latitude <- 45.5
longitude <- -111.2
altitude_m <- 5400 / 3.28
standard_pressure_mb <- 1013
mean_depth_m <- 0.5
k600_prior_center <- 30
day_start <- 4
day_end <- 28

bpcalc_atm <- function(bpst, altitude) {
  bpst *
    exp(
      (-9.80665 * 0.0289644 * altitude) /
        (8.31447 * (273.15 + 15))
    )
}

# Bob's temperature correction from K600 to KO2.
Kcor <- function(temp, K600) {
  K600 /
    (600 / (1800.6 - temp * 120.1 + 3.7818 * temp^2 - 0.047608 * temp^3))^-0.5
}

message("Preparing Gallatin data")
gallatin <- utils::read.csv(data_file)
gallatin$time <- lubridate::as_datetime(gallatin$unixtime, tz = "UTC")
gallatin$solar.time <- convert_UTC_to_solartime(
  gallatin$time,
  longitude = longitude,
  time.type = "mean solar"
)
gallatin$light <- calc_light(
  gallatin$solar.time,
  latitude = latitude,
  longitude = longitude,
  max.PAR = 2326
)
gallatin$DO.sat <- calc_DO_sat(
  temp.water = gallatin$temp,
  pressure.air = bpcalc_atm(standard_pressure_mb, altitude_m)
)
gallatin$Kc_bob <- Kcor(gallatin$temp, 1)

# Match the exact window in Bob's example: 30 solar days, each from 04:00 to
# 04:00, with 10-minute observations from 04:05 through 03:55.
window_start <- lubridate::ymd_hms("2024-09-01 04:00:00", tz = "UTC")
window_end <- lubridate::ymd_hms("2024-10-01 04:00:00", tz = "UTC")
gallatin <- gallatin[
  gallatin$solar.time > window_start & gallatin$solar.time < window_end,
]
gallatin <- gallatin[order(gallatin$solar.time), ]
row.names(gallatin) <- NULL

timestep_days <- stats::median(diff(as.numeric(gallatin$solar.time))) /
  (24 * 60 * 60)
ntime <- as.integer(round(1 / timestep_days))
if (nrow(gallatin) %% ntime != 0) {
  stop("The selected data do not contain a whole number of solar days")
}
nday <- nrow(gallatin) / ntime
if (ntime != 144 || nday != 30) {
  stop("Expected 144 observations/day and 30 days; got ", ntime, " and ", nday)
}
gallatin$day <- rep(seq_len(nday), each = ntime)
gallatin$date <- as.Date(
  gallatin$solar.time - lubridate::hours(day_start)
)

bob_data <- list(
  T = nrow(gallatin),
  D = nday,
  y = gallatin$oxy,
  oxysat = gallatin$DO.sat,
  light = gallatin$light,
  z = mean_depth_m,
  Kc = gallatin$Kc_bob,
  day = gallatin$day,
  sumlight = colSums(matrix(gallatin$light, nrow = ntime)),
  ts = timestep_days
)

sm_data <- data.frame(
  solar.time = gallatin$solar.time,
  DO.obs = gallatin$oxy,
  DO.sat = gallatin$DO.sat,
  depth = mean_depth_m,
  temp.water = gallatin$temp,
  light = gallatin$light
)

message("Compiling and fitting Bob model: ", bob_stan_file)
bob_stan_model <- rstan::stan_model(
  file = bob_stan_file,
  model_name = "bob_metab_pi_ar_light_2"
)
bob_fit <- rstan::sampling(
  object = bob_stan_model,
  data = bob_data,
  chains = chains,
  iter = iter,
  warmup = warmup,
  seed = seed,
  cores = cores,
  refresh = max(1, floor((iter - warmup) / 10))
)

new_model_name <- mm_name(
  type = "bayes",
  pool_K600 = "normal",
  err_obs_iid = FALSE,
  err_proc_acor = TRUE,
  err_proc_acor_light = TRUE,
  err_proc_iid = FALSE,
  err_proc_GPP = FALSE,
  ode_method = "euler",
  GPP_fun = "linlight",
  ER_fun = "constant",
  deficit_src = "DO_obs",
  engine = "stan"
)

# Match Bob's shared priors. The baseline process-SD prior intentionally cannot
# be matched through specs(): Bob uses half-normal(0, 1), whereas the package
# model retains streamMetabolizer"s half-Cauchy(0, 1) prior.
new_specs <- specs(
  new_model_name,
  day_start = day_start,
  day_end = day_end,
  keep_mcmcs = TRUE,
  keep_mcmc_data = TRUE,
  GPP_daily_mu = 0,
  GPP_daily_lower = -Inf,
  GPP_daily_sigma = 10,
  ER_daily_mu = -2,
  ER_daily_upper = Inf,
  ER_daily_sigma = 8,
  K600_daily_meanlog_meanlog = log(k600_prior_center),
  K600_daily_meanlog_sdlog = 0.7,
  K600_daily_sdlog_sigma = 0.05,
  err_proc_acor_phi_alpha = 1,
  err_proc_acor_phi_beta = 1,
  err_proc_acor_sigma_scale = 1,
  err_proc_acor_light_alpha_sigma = 5,
  n_chains = chains,
  n_cores = cores,
  burnin_steps = warmup,
  saved_steps = iter - warmup,
  thin_steps = 1,
  stan_engine = "rstan",
  verbose = FALSE
)
new_specs <- revise(
  new_specs,
  params_out = union(new_specs$params_out, "DO_mod")
)

message("Fitting streamMetabolizer model: ", new_model_name)
sm_fit <- metab(
  specs = new_specs,
  data = sm_data,
  info = c(site = "Gallatin downstream", source = "Hall comparison")
)
sm_stan_fit <- get_mcmc(sm_fit)
if (!inherits(sm_stan_fit, "stanfit")) {
  stan_fits <- Filter(function(x) inherits(x, "stanfit"), sm_stan_fit)
  if (length(stan_fits) != 1) {
    stop("Expected exactly one retained streamMetabolizer stanfit object")
  }
  sm_stan_fit <- stan_fits[[1]]
}

# Fit the IID process-error models compared in Bob's
# compare_streamMetabolizer.R. Modernize the obsolete array declaration in
# memory without changing Bob's source file.
bob_proc_stan_code <- paste(
  readLines(bob_proc_stan_file, warn = FALSE),
  collapse = "\n"
)
bob_proc_stan_code <- sub(
  "int <lower=0> day[T];",
  "array[T] int <lower=0> day;",
  bob_proc_stan_code,
  fixed = TRUE
)
bob_proc_stan_code <- gsub("<-", "=", bob_proc_stan_code, fixed = TRUE)

message(
  "Compiling and fitting Bob IID process-error model: ",
  bob_proc_stan_file
)
bob_proc_stan_model <- rstan::stan_model(
  model_code = bob_proc_stan_code,
  model_name = "bob_metab_pi_2"
)
bob_proc_fit <- rstan::sampling(
  object = bob_proc_stan_model,
  data = bob_data,
  chains = chains,
  iter = iter,
  warmup = warmup,
  seed = seed,
  cores = cores,
  refresh = max(1, floor((iter - warmup) / 10))
)

sm_proc_model_name <- mm_name(
  type = "bayes",
  pool_K600 = "normal",
  err_obs_iid = FALSE,
  err_proc_acor = FALSE,
  err_proc_acor_light = FALSE,
  err_proc_iid = TRUE,
  err_proc_GPP = FALSE,
  ode_method = "euler",
  GPP_fun = "linlight",
  ER_fun = "constant",
  deficit_src = "DO_obs",
  engine = "stan"
)
sm_proc_specs <- specs(
  sm_proc_model_name,
  day_start = day_start,
  day_end = day_end,
  keep_mcmcs = TRUE,
  keep_mcmc_data = TRUE,
  GPP_daily_mu = 0,
  GPP_daily_lower = -Inf,
  GPP_daily_sigma = 10,
  ER_daily_mu = -2,
  ER_daily_upper = Inf,
  ER_daily_sigma = 8,
  K600_daily_meanlog_meanlog = log(k600_prior_center),
  K600_daily_meanlog_sdlog = 0.7,
  K600_daily_sdlog_sigma = 0.05,
  err_proc_iid_sigma_scale = 5,
  n_chains = chains,
  n_cores = cores,
  burnin_steps = warmup,
  saved_steps = iter - warmup,
  thin_steps = 1,
  stan_engine = "rstan",
  verbose = FALSE
)
sm_proc_specs <- revise(
  sm_proc_specs,
  params_out = union(sm_proc_specs$params_out, "DO_mod_partial")
)

message(
  "Fitting streamMetabolizer IID process-error model: ",
  sm_proc_model_name
)
sm_proc_fit <- metab(
  specs = sm_proc_specs,
  data = sm_data,
  info = c(site = "Gallatin downstream", source = "Hall process comparison")
)
sm_proc_stan_fit <- get_mcmc(sm_proc_fit)
if (!inherits(sm_proc_stan_fit, "stanfit")) {
  proc_stan_fits <- Filter(
    function(x) inherits(x, "stanfit"),
    sm_proc_stan_fit
  )
  if (length(proc_stan_fits) != 1) {
    stop("Expected exactly one retained process-error stanfit object")
  }
  sm_proc_stan_fit <- proc_stan_fits[[1]]
}

stan_summary <- function(fit, parameter) {
  out <- rstan::summary(
    fit,
    pars = parameter,
    probs = c(0.025, 0.5, 0.975)
  )$summary
  data.frame(
    row = row.names(out),
    mean = out[, "mean"],
    median = out[, "50%"],
    lower = out[, "2.5%"],
    upper = out[, "97.5%"],
    n_eff = out[, "n_eff"],
    Rhat = out[, "Rhat"],
    row.names = NULL,
    check.names = FALSE
  )
}

daily_parameter_comparison <- function(
  bob_fit_object,
  sm_fit_object,
  bob_parameter,
  sm_parameter,
  label
) {
  bob <- stan_summary(bob_fit_object, bob_parameter)
  sm <- stan_summary(sm_fit_object, sm_parameter)
  if (nrow(bob) != nday || nrow(sm) != nday) {
    stop("Unexpected daily parameter length for ", label)
  }
  data.frame(
    date = sort(unique(gallatin$date)),
    parameter = label,
    bob_mean = bob$mean,
    bob_median = bob$median,
    bob_lower = bob$lower,
    bob_upper = bob$upper,
    bob_Rhat = bob$Rhat,
    sm_mean = sm$mean,
    sm_median = sm$median,
    sm_lower = sm$lower,
    sm_upper = sm$upper,
    sm_Rhat = sm$Rhat
  )
}

daily_comparison <- rbind(
  daily_parameter_comparison(bob_fit, sm_stan_fit, "GPP", "GPP_daily", "GPP"),
  daily_parameter_comparison(bob_fit, sm_stan_fit, "ER", "ER_daily", "ER"),
  daily_parameter_comparison(bob_fit, sm_stan_fit, "K", "K600_daily", "K600")
)
utils::write.csv(
  daily_comparison,
  file.path(output_dir, "daily_parameter_comparison.csv"),
  row.names = FALSE
)

proc_daily_comparison <- rbind(
  daily_parameter_comparison(
    bob_proc_fit,
    sm_proc_stan_fit,
    "GPP",
    "GPP_daily",
    "GPP"
  ),
  daily_parameter_comparison(
    bob_proc_fit,
    sm_proc_stan_fit,
    "ER",
    "ER_daily",
    "ER"
  ),
  daily_parameter_comparison(
    bob_proc_fit,
    sm_proc_stan_fit,
    "K",
    "K600_daily",
    "K600"
  )
)
utils::write.csv(
  proc_daily_comparison,
  file.path(output_dir, "proc_daily_parameter_comparison.csv"),
  row.names = FALSE
)

overall_mapping <- data.frame(
  parameter = c("phi", "baseline_sigma", "light_alpha", "K_meanlog", "K_sdlog"),
  bob = c("phi", "sigproc", "alpha", "Kmean", "Ksd"),
  streamMetabolizer = c(
    "err_proc_acor_phi",
    "err_proc_acor_sigma",
    "err_proc_acor_light_alpha",
    "K600_daily_predlog",
    "K600_daily_sdlog"
  )
)
overall_comparison <- do.call(
  rbind,
  lapply(seq_len(nrow(overall_mapping)), function(i) {
    bob <- stan_summary(bob_fit, overall_mapping$bob[i])
    sm <- stan_summary(sm_stan_fit, overall_mapping$streamMetabolizer[i])
    data.frame(
      parameter = overall_mapping$parameter[i],
      model = c("Bob", "streamMetabolizer"),
      rbind(bob[1, -1, drop = FALSE], sm[1, -1, drop = FALSE]),
      row.names = NULL
    )
  })
)
utils::write.csv(
  overall_comparison,
  file.path(output_dir, "overall_parameter_comparison.csv"),
  row.names = FALSE
)

proc_overall_mapping <- data.frame(
  parameter = c("baseline_sigma", "K_meanlog", "K_sdlog"),
  bob = c("sigproc", "Kmean", "Ksd"),
  streamMetabolizer = c(
    "err_proc_iid_sigma",
    "K600_daily_predlog",
    "K600_daily_sdlog"
  )
)
proc_overall_comparison <- do.call(
  rbind,
  lapply(seq_len(nrow(proc_overall_mapping)), function(i) {
    bob <- stan_summary(bob_proc_fit, proc_overall_mapping$bob[i])
    sm <- stan_summary(
      sm_proc_stan_fit,
      proc_overall_mapping$streamMetabolizer[i]
    )
    data.frame(
      parameter = proc_overall_mapping$parameter[i],
      model = c("Bob", "streamMetabolizer"),
      rbind(bob[1, -1, drop = FALSE], sm[1, -1, drop = FALSE]),
      row.names = NULL
    )
  })
)
proc_overall_comparison$note <- ifelse(
  proc_overall_comparison$parameter == "baseline_sigma",
  paste(
    "Bob sigma is a per-timestep DO residual SD;",
    "streamMetabolizer sigma is a process-rate SD"
  ),
  NA_character_
)
utils::write.csv(
  proc_overall_comparison,
  file.path(output_dir, "proc_overall_parameter_comparison.csv"),
  row.names = FALSE
)

posterior_median <- function(fit, parameter, margins) {
  draws <- rstan::extract(fit, pars = parameter, permuted = TRUE)[[parameter]]
  apply(draws, margins, stats::median)
}

# Bob defines eta at all timestamps (with zero residuals at day starts), while
# streamMetabolizer defines 143 within-day transitions for each of 30 days.
bob_eta <- posterior_median(bob_fit, "eta", 2)
bob_eta <- matrix(bob_eta, nrow = ntime, ncol = nday)[-1, , drop = FALSE]
sm_eta <- posterior_median(sm_stan_fit, "err_proc_acor", c(2, 3))

bob_mu <- posterior_median(bob_fit, "mu", 2)
sm_mu <- posterior_median(sm_stan_fit, "DO_mod", c(2, 3))
bob_mu <- matrix(bob_mu, nrow = ntime, ncol = nday)

bob_proc_eta <- posterior_median(bob_proc_fit, "eta", 2)
bob_proc_eta <- matrix(
  bob_proc_eta,
  nrow = ntime,
  ncol = nday
)[-1, , drop = FALSE]
bob_proc_mu <- posterior_median(bob_proc_fit, "mu", 2)
bob_proc_mu <- matrix(bob_proc_mu, nrow = ntime, ncol = nday)
sm_proc_mu <- posterior_median(
  sm_proc_stan_fit,
  "DO_mod_partial",
  c(2, 3)
)
sm_proc_eta <- matrix(gallatin$oxy, nrow = ntime, ncol = nday) - sm_proc_mu
sm_proc_eta <- sm_proc_eta[-1, , drop = FALSE]

residual_comparison <- data.frame(
  date = rep(sort(unique(gallatin$date)), each = ntime - 1),
  timestep = rep(2:ntime, times = nday),
  bob_eta = as.vector(bob_eta),
  sm_eta = as.vector(sm_eta)
)
utils::write.csv(
  residual_comparison,
  file.path(output_dir, "process_residual_comparison.csv"),
  row.names = FALSE
)

proc_residual_comparison <- data.frame(
  date = rep(sort(unique(gallatin$date)), each = ntime - 1),
  timestep = rep(2:ntime, times = nday),
  bob_eta = as.vector(bob_proc_eta),
  sm_eta = as.vector(sm_proc_eta)
)
utils::write.csv(
  proc_residual_comparison,
  file.path(output_dir, "proc_residual_comparison.csv"),
  row.names = FALSE
)

rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}
fit_metrics <- data.frame(
  metric = c(
    "one_step_DO_RMSE",
    "within_day_residual_correlation",
    "within_day_residual_lag1_ACF"
  ),
  bob = c(
    rmse(gallatin$oxy, as.vector(bob_mu)),
    stats::cor(as.vector(bob_eta), as.vector(sm_eta)),
    unname(stats::acf(as.vector(bob_eta), plot = FALSE, lag.max = 1)$acf[2])
  ),
  streamMetabolizer = c(
    rmse(gallatin$oxy, as.vector(sm_mu)),
    stats::cor(as.vector(bob_eta), as.vector(sm_eta)),
    unname(stats::acf(as.vector(sm_eta), plot = FALSE, lag.max = 1)$acf[2])
  )
)
utils::write.csv(
  fit_metrics,
  file.path(output_dir, "fit_metrics.csv"),
  row.names = FALSE
)

proc_fit_metrics <- data.frame(
  metric = c(
    "one_step_DO_RMSE",
    "within_day_residual_correlation",
    "within_day_residual_lag1_ACF"
  ),
  bob = c(
    rmse(gallatin$oxy, as.vector(bob_proc_mu)),
    stats::cor(as.vector(bob_proc_eta), as.vector(sm_proc_eta)),
    unname(
      stats::acf(
        as.vector(bob_proc_eta),
        plot = FALSE,
        lag.max = 1
      )$acf[2]
    )
  ),
  streamMetabolizer = c(
    rmse(gallatin$oxy, as.vector(sm_proc_mu)),
    stats::cor(as.vector(bob_proc_eta), as.vector(sm_proc_eta)),
    unname(
      stats::acf(
        as.vector(sm_proc_eta),
        plot = FALSE,
        lag.max = 1
      )$acf[2]
    )
  )
)
utils::write.csv(
  proc_fit_metrics,
  file.path(output_dir, "proc_fit_metrics.csv"),
  row.names = FALSE
)

sampler_diagnostics <- function(fit, model, parameters) {
  sampler <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  divergences <- sum(vapply(
    sampler,
    function(x) sum(x[, "divergent__"]),
    numeric(1)
  ))
  selected <- rstan::summary(
    fit,
    pars = parameters
  )$summary
  data.frame(
    model = model,
    divergences = divergences,
    max_Rhat = max(selected[, "Rhat"], na.rm = TRUE),
    min_n_eff = min(selected[, "n_eff"], na.rm = TRUE)
  )
}
diagnostics <- rbind(
  sampler_diagnostics(
    bob_fit,
    "Bob",
    c("GPP", "ER", "K", "phi", "sigproc", "alpha")
  ),
  sampler_diagnostics(
    sm_stan_fit,
    "streamMetabolizer",
    c(
      "GPP_daily",
      "ER_daily",
      "K600_daily",
      "err_proc_acor_phi",
      "err_proc_acor_sigma",
      "err_proc_acor_light_alpha"
    )
  )
)
utils::write.csv(
  diagnostics,
  file.path(output_dir, "sampler_diagnostics.csv"),
  row.names = FALSE
)

proc_diagnostics <- rbind(
  sampler_diagnostics(
    bob_proc_fit,
    "Bob",
    c("GPP", "ER", "K", "sigproc")
  ),
  sampler_diagnostics(
    sm_proc_stan_fit,
    "streamMetabolizer",
    c("GPP_daily", "ER_daily", "K600_daily", "err_proc_iid_sigma")
  )
)
utils::write.csv(
  proc_diagnostics,
  file.path(output_dir, "proc_sampler_diagnostics.csv"),
  row.names = FALSE
)

# Recreate the plots in metab_proc_err/compare_streamMetabolizer.R with
# ggplot2, then add the direct residual and AR diagnostics used by this script.
plot_theme <- ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )
model_colors <- c(
  Bob = "#D55E00",
  streamMetabolizer = "#0072B2",
  observed = "#222222"
)

daily_dates <- sort(unique(gallatin$date))
daily_wide <- data.frame(date = daily_dates)
for (parameter in c("GPP", "ER", "K600")) {
  parameter_data <- daily_comparison[
    daily_comparison$parameter == parameter,
  ]
  daily_wide[[paste0("bob_", parameter)]] <- parameter_data$bob_median
  daily_wide[[paste0("sm_", parameter)]] <- parameter_data$sm_median
}

proc_daily_wide <- data.frame(date = daily_dates)
for (parameter in c("GPP", "ER", "K600")) {
  parameter_data <- proc_daily_comparison[
    proc_daily_comparison$parameter == parameter,
  ]
  proc_daily_wide[[paste0("bob_", parameter)]] <- parameter_data$bob_median
  proc_daily_wide[[paste0("sm_", parameter)]] <- parameter_data$sm_median
}

do_plot_data <- rbind(
  data.frame(
    solar.time = gallatin$solar.time,
    DO = gallatin$oxy,
    model = "observed"
  ),
  data.frame(
    solar.time = gallatin$solar.time,
    DO = as.vector(bob_mu),
    model = "Bob"
  ),
  data.frame(
    solar.time = gallatin$solar.time,
    DO = as.vector(sm_mu),
    model = "streamMetabolizer"
  )
)

proc_do_plot_data <- rbind(
  data.frame(
    solar.time = gallatin$solar.time,
    DO = gallatin$oxy,
    model = "observed"
  ),
  data.frame(
    solar.time = gallatin$solar.time,
    DO = as.vector(bob_proc_mu),
    model = "Bob"
  ),
  data.frame(
    solar.time = gallatin$solar.time,
    DO = as.vector(sm_proc_mu),
    model = "streamMetabolizer"
  )
)

# Analog of plot_DO_preds(predict_DO(sm_fit)).
plot_do_predictions <- ggplot2::ggplot(
  do_plot_data,
  ggplot2::aes(x = solar.time, y = DO, color = model)
) +
  ggplot2::geom_line(
    data = do_plot_data[do_plot_data$model != "observed", ],
    linewidth = 0.35
  ) +
  ggplot2::geom_point(
    data = do_plot_data[do_plot_data$model == "observed", ],
    size = 0.25,
    alpha = 0.5
  ) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(
    title = "Dissolved oxygen observations and one-step predictions",
    x = "Solar time",
    y = expression(
      DO ~ (mg ~ L^{
        -1
      })
    ),
    color = NULL
  ) +
  plot_theme

metab_plot_data <- rbind(
  data.frame(
    date = daily_comparison$date,
    parameter = daily_comparison$parameter,
    model = "Bob",
    estimate = daily_comparison$bob_median,
    lower = daily_comparison$bob_lower,
    upper = daily_comparison$bob_upper
  ),
  data.frame(
    date = daily_comparison$date,
    parameter = daily_comparison$parameter,
    model = "streamMetabolizer",
    estimate = daily_comparison$sm_median,
    lower = daily_comparison$sm_lower,
    upper = daily_comparison$sm_upper
  )
)
metab_plot_data <- metab_plot_data[
  metab_plot_data$parameter %in% c("GPP", "ER"),
]

# Analog of plot_metab_preds(predict_metab(sm_fit)).
plot_metabolism_predictions <- ggplot2::ggplot(
  metab_plot_data,
  ggplot2::aes(
    x = date,
    y = estimate,
    ymin = lower,
    ymax = upper,
    color = model,
    fill = model
  )
) +
  ggplot2::geom_ribbon(alpha = 0.12, color = NA) +
  ggplot2::geom_line(linewidth = 0.55) +
  ggplot2::geom_point(size = 0.8) +
  ggplot2::facet_wrap(~parameter, ncol = 1, scales = "free_y") +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::scale_fill_manual(values = model_colors) +
  ggplot2::labs(
    title = "Daily metabolism estimates",
    x = NULL,
    y = expression(
      g ~ O[2] ~ m^{
        -2
      } ~ d^{
        -1
      }
    ),
    color = NULL,
    fill = NULL
  ) +
  plot_theme

model_scatter_plot <- function(data, model, x, y, xlab, ylab, title) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggplot2::geom_point(
      size = 1.7,
      alpha = 0.8,
      color = model_colors[[model]]
    ) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    plot_theme
}

# Analogs of plot(params$GPP.daily, params$ER.daily) and the process-only
# repetition of that plot.
plot_stream_gpp_er <- model_scatter_plot(
  daily_wide,
  "streamMetabolizer",
  "sm_GPP",
  "sm_ER",
  "GPP",
  "ER",
  "streamMetabolizer: daily GPP versus ER"
)

# Analogs of plot(params$K600.daily, params$ER.daily) for the stream model and
# plot(K_est[,1], er_est[,1]) for Bob's model.
plot_stream_k_er <- model_scatter_plot(
  daily_wide,
  "streamMetabolizer",
  "sm_K600",
  "sm_ER",
  expression(
    K[600] ~ (d^{
      -1
    })
  ),
  "ER",
  expression(paste("streamMetabolizer: ", K[600], " versus ER"))
)
plot_bob_k_er <- model_scatter_plot(
  daily_wide,
  "Bob",
  "bob_K600",
  "bob_ER",
  expression(
    K[600] ~ (d^{
      -1
    })
  ),
  "ER",
  expression(paste("Bob: ", K[600], " versus ER"))
)

plot_proc_stream_gpp_er <- model_scatter_plot(
  proc_daily_wide,
  "streamMetabolizer",
  "sm_GPP",
  "sm_ER",
  "GPP",
  "ER",
  "IID process error: streamMetabolizer GPP versus ER"
)
plot_proc_stream_k_er <- model_scatter_plot(
  proc_daily_wide,
  "streamMetabolizer",
  "sm_K600",
  "sm_ER",
  expression(
    K[600] ~ (d^{
      -1
    })
  ),
  "ER",
  expression(paste(
    "IID process error, streamMetabolizer: ",
    K[600],
    " versus ER"
  ))
)
plot_proc_bob_k_er <- model_scatter_plot(
  proc_daily_wide,
  "Bob",
  "bob_K600",
  "bob_ER",
  expression(
    K[600] ~ (d^{
      -1
    })
  ),
  "ER",
  expression(paste("IID process error, Bob: ", K[600], " versus ER"))
)

parameter_comparison_plot <- function(
  comparison_data,
  parameter,
  xlab,
  ylab,
  title
) {
  plot_data <- comparison_data[comparison_data$parameter == parameter, ]
  lim <- range(c(plot_data$bob_median, plot_data$sm_median), finite = TRUE)
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = bob_median, y = sm_median)
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      color = "grey50"
    ) +
    ggplot2::geom_point(
      size = 1.7,
      alpha = 0.8,
      color = model_colors[["streamMetabolizer"]]
    ) +
    ggplot2::coord_equal(xlim = lim, ylim = lim) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    plot_theme
}

# Analogs of the original streamMetabolizer-versus-Bob GPP and K plots.
plot_gpp_comparison <- parameter_comparison_plot(
  daily_comparison,
  "GPP",
  "Bob GPP",
  "streamMetabolizer GPP",
  "Daily GPP comparison"
)
plot_k_comparison <- parameter_comparison_plot(
  daily_comparison,
  "K600",
  expression(paste("Bob ", K[600])),
  expression(paste("streamMetabolizer ", K[600])),
  expression(paste("Daily ", K[600], " comparison"))
)

# Additional direct comparisons retained from the earlier version of this
# script, now also expressed as ggplot objects.
plot_er_comparison <- parameter_comparison_plot(
  daily_comparison,
  "ER",
  "Bob ER",
  "streamMetabolizer ER",
  "Daily ER comparison"
)
plot_proc_gpp_comparison <- parameter_comparison_plot(
  proc_daily_comparison,
  "GPP",
  "Bob GPP",
  "streamMetabolizer GPP",
  "IID process error: daily GPP comparison"
)
plot_proc_k_comparison <- parameter_comparison_plot(
  proc_daily_comparison,
  "K600",
  expression(paste("Bob ", K[600])),
  expression(paste("streamMetabolizer ", K[600])),
  expression(paste("IID process error: daily ", K[600], " comparison"))
)
plot_residual_comparison <- ggplot2::ggplot(
  residual_comparison,
  ggplot2::aes(x = bob_eta, y = sm_eta)
) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    color = "grey50"
  ) +
  ggplot2::geom_point(
    alpha = 0.18,
    size = 0.45,
    color = model_colors[["streamMetabolizer"]]
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = "Within-day process residuals",
    x = "Bob process residual",
    y = "streamMetabolizer process residual"
  ) +
  plot_theme

plot_proc_do_predictions <- ggplot2::ggplot(
  proc_do_plot_data,
  ggplot2::aes(x = solar.time, y = DO, color = model)
) +
  ggplot2::geom_line(
    data = proc_do_plot_data[proc_do_plot_data$model != "observed", ],
    linewidth = 0.35
  ) +
  ggplot2::geom_point(
    data = proc_do_plot_data[proc_do_plot_data$model == "observed", ],
    size = 0.25,
    alpha = 0.5
  ) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(
    title = "IID process error: DO observations and one-step predictions",
    x = "Solar time",
    y = expression(
      DO ~ (mg ~ L^{
        -1
      })
    ),
    color = NULL
  ) +
  plot_theme

plot_proc_residual_comparison <- ggplot2::ggplot(
  proc_residual_comparison,
  ggplot2::aes(x = bob_eta, y = sm_eta)
) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    color = "grey50"
  ) +
  ggplot2::geom_point(
    alpha = 0.18,
    size = 0.45,
    color = model_colors[["streamMetabolizer"]]
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = "IID process error: within-day DO residuals",
    x = "Bob process residual",
    y = "streamMetabolizer process residual"
  ) +
  plot_theme

show_rows <- seq_len(3 * ntime)
plot_first_three_days <- ggplot2::ggplot(
  do_plot_data[do_plot_data$solar.time %in% gallatin$solar.time[show_rows], ],
  ggplot2::aes(x = solar.time, y = DO, color = model)
) +
  ggplot2::geom_line(
    data = do_plot_data[
      do_plot_data$solar.time %in%
        gallatin$solar.time[show_rows] &
        do_plot_data$model != "observed",
    ],
    linewidth = 0.55
  ) +
  ggplot2::geom_point(
    data = do_plot_data[
      do_plot_data$solar.time %in%
        gallatin$solar.time[show_rows] &
        do_plot_data$model == "observed",
    ],
    size = 0.7,
    alpha = 0.7
  ) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(
    title = "First three days: one-step posterior-median predictions",
    x = "Solar time",
    y = expression(
      DO ~ (mg ~ L^{
        -1
      })
    ),
    color = NULL
  ) +
  plot_theme

bob_acf <- stats::acf(as.vector(bob_eta), plot = FALSE)
sm_acf <- stats::acf(as.vector(sm_eta), plot = FALSE)
acf_data <- rbind(
  data.frame(
    lag = as.vector(bob_acf$lag),
    acf = as.vector(bob_acf$acf),
    model = "Bob"
  ),
  data.frame(
    lag = as.vector(sm_acf$lag),
    acf = as.vector(sm_acf$acf),
    model = "streamMetabolizer"
  )
)
plot_residual_acf <- ggplot2::ggplot(
  acf_data,
  ggplot2::aes(x = lag, y = acf, color = model)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey70") +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(
    title = "Within-day residual autocorrelation",
    x = "Lag (10-minute timesteps)",
    y = "ACF",
    color = NULL
  ) +
  plot_theme

comparison_plots <- list(
  "01_do_predictions" = plot_do_predictions,
  "02_metabolism_predictions" = plot_metabolism_predictions,
  "03_stream_gpp_vs_er" = plot_stream_gpp_er,
  "04_stream_k600_vs_er" = plot_stream_k_er,
  "05_bob_k600_vs_er" = plot_bob_k_er,
  "06_gpp_model_comparison" = plot_gpp_comparison,
  "07_k600_model_comparison" = plot_k_comparison,
  "08_er_model_comparison" = plot_er_comparison,
  "09_residual_comparison" = plot_residual_comparison,
  "10_first_three_days" = plot_first_three_days,
  "11_residual_acf" = plot_residual_acf,
  "12_proc_stream_gpp_vs_er" = plot_proc_stream_gpp_er,
  "13_proc_stream_k600_vs_er" = plot_proc_stream_k_er,
  "14_proc_bob_k600_vs_er" = plot_proc_bob_k_er,
  "15_proc_gpp_model_comparison" = plot_proc_gpp_comparison,
  "16_proc_k600_model_comparison" = plot_proc_k_comparison,
  "17_proc_do_predictions" = plot_proc_do_predictions,
  "18_proc_residual_comparison" = plot_proc_residual_comparison
)

for (plot_name in names(comparison_plots)) {
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(plot_name, ".png")),
    plot = comparison_plots[[plot_name]],
    width = 10,
    height = 6,
    dpi = 300
  )
}
grDevices::pdf(
  file.path(output_dir, "comparison_plots.pdf"),
  width = 10,
  height = 6,
  onefile = TRUE
)
invisible(lapply(comparison_plots, print))
grDevices::dev.off()

kc_stream <- convert_k600_to_kGAS(
  k600 = 1,
  temperature = gallatin$temp,
  gas = "O2"
)
data_checks <- data.frame(
  rows = nrow(gallatin),
  days = nday,
  observations_per_day = ntime,
  timestep_days = timestep_days,
  max_abs_KO2_conversion_difference = max(
    abs(gallatin$Kc_bob - kc_stream),
    na.rm = TRUE
  )
)
utils::write.csv(
  data_checks,
  file.path(output_dir, "data_checks.csv"),
  row.names = FALSE
)

notes <- c(
  paste("Bob Stan file:", normalizePath(bob_stan_file)),
  paste("streamMetabolizer model:", new_model_name),
  paste("Bob IID process-error Stan file:", normalizePath(bob_proc_stan_file)),
  paste("streamMetabolizer IID process-error model:", sm_proc_model_name),
  paste("Sampling: chains =", chains, ", iter =", iter, ", warmup =", warmup),
  "",
  "Important comparison details:",
  "1. Both models apply the AR likelihood across day boundaries, where the boundary residual is zero.",
  "2. Bob uses half-normal(0, 1) for baseline process SD.",
  "3. streamMetabolizer uses its existing half-Cauchy(0, 1) baseline process-SD prior.",
  "4. Both use half-normal(0, 5) for the nonnegative light coefficient.",
  "5. Bob indexes Euler process covariates at the incoming observation; streamMetabolizer indexes them at the start of each transition.",
  "6. In the IID comparison, Bob's sigproc is a per-timestep DO residual SD, while streamMetabolizer's err_proc_iid_sigma is a process-rate SD. Compare their induced DO residuals rather than the raw sigma values."
)
writeLines(notes, file.path(output_dir, "comparison_notes.txt"))

if (save_fits) {
  saveRDS(bob_fit, file.path(output_dir, "bob_fit.rds"), compress = FALSE)
  saveRDS(
    sm_fit,
    file.path(output_dir, "streamMetabolizer_fit.rds"),
    compress = FALSE
  )
  saveRDS(
    bob_proc_fit,
    file.path(output_dir, "bob_proc_fit.rds"),
    compress = FALSE
  )
  saveRDS(
    sm_proc_fit,
    file.path(output_dir, "streamMetabolizer_proc_fit.rds"),
    compress = FALSE
  )
}

comparison <- list(
  daily = daily_comparison,
  overall = overall_comparison,
  residuals = residual_comparison,
  metrics = fit_metrics,
  diagnostics = diagnostics,
  data_checks = data_checks,
  plots = comparison_plots,
  bob_fit = bob_fit,
  streamMetabolizer_fit = sm_fit,
  proc = list(
    daily = proc_daily_comparison,
    overall = proc_overall_comparison,
    residuals = proc_residual_comparison,
    metrics = proc_fit_metrics,
    diagnostics = proc_diagnostics,
    bob_fit = bob_proc_fit,
    streamMetabolizer_fit = sm_proc_fit
  )
)

message("Comparison complete. Results written to: ", normalizePath(output_dir))
print(fit_metrics)
print(diagnostics)
print(proc_fit_metrics)
print(proc_diagnostics)
invisible(comparison)
