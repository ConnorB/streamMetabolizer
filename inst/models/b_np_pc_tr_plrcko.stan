// b_np_pc_tr_plrcko.stan

data {
  // Parameters of priors on metabolism
  real GPP_daily_mu;
  real GPP_daily_lower;
  real<lower=0> GPP_daily_sigma;
  real ER_daily_mu;
  real ER_daily_upper;
  real<lower=0> ER_daily_sigma;
  real K600_daily_meanlog;
  real<lower=0> K600_daily_sdlog;

  // Error distributions
  real err_proc_acor_phi_alpha;
  real err_proc_acor_phi_beta;
  real<lower=0> err_proc_acor_sigma_scale;

  // Data dimensions
  int<lower=1> d; // number of dates
  real<lower=0> timestep; // length of each timestep in days
  int<lower=1> n24; // number of observations in first 24 hours per date
  int<lower=1> n; // number of observations per date

  // Daily data
  vector[d] DO_obs_1;

  // Data
  array[n] vector[d] DO_obs;
  array[n] vector[d] DO_sat;
  array[n] vector[d] light_mult_GPP;
  array[n] vector[d] const_mult_ER;
  array[n] vector[d] depth;
  array[n] vector[d] KO2_conv;
}

parameters {
  vector<lower=GPP_daily_lower>[d] GPP_daily;
  vector<upper=ER_daily_upper>[d] ER_daily;
  vector<lower=0>[d] K600_daily;

  real<lower=0, upper=1> err_proc_acor_phi;
  real<lower=0> err_proc_acor_sigma_scaled;
}

transformed parameters {
  real<lower=0> err_proc_acor_sigma;
  array[n] vector[d] GPP_inst;
  array[n] vector[d] ER_inst;
  array[n] vector[d] KO2_inst;
  array[n] vector[d] DO_mod;
  array[n-1] vector[d] err_proc_acor;

  // Rescale error distribution parameters
  err_proc_acor_sigma = err_proc_acor_sigma_scale * err_proc_acor_sigma_scaled;

  // Model DO time series
  // * trapezoid version
  // * no observation error
  // * autocorrelated process error
  // * reaeration depends on DO_obs

  // Calculate individual process rates
  for(i in 1:n) {
    GPP_inst[i] = GPP_daily .* light_mult_GPP[i];
    ER_inst[i] = ER_daily .* const_mult_ER[i];
    KO2_inst[i] = K600_daily .* KO2_conv[i];
  }

  // DO model
  DO_mod[1] = DO_obs_1;
  for(i in 1:(n-1)) {
    DO_mod[i+1] =
      DO_obs[i] + (
        - KO2_inst[i] .* DO_obs[i] - KO2_inst[i+1] .* DO_obs[i+1] +
        (GPP_inst[i] + ER_inst[i]) ./ depth[i] +
        (GPP_inst[i+1] + ER_inst[i+1]) ./ depth[i+1] +
        KO2_inst[i] .* DO_sat[i] + KO2_inst[i+1] .* DO_sat[i+1]
      ) * (timestep / 2.0);
  }
  for(i in 1:(n-1)) {
    err_proc_acor[i] = DO_obs[i+1] - DO_mod[i+1];
  }
}

model {
  // Autocorrelated process error
  // Connect adjacent dates through the zero residual at each day boundary
  if(d > 1) {
    for(j in 2:d) {
      0 ~ normal(err_proc_acor_phi * err_proc_acor[n-1,j-1], err_proc_acor_sigma + 1e-9);
    }
  }
  // Within each date, the first modeled residual follows the zero boundary residual
  err_proc_acor[1] ~ normal(0, err_proc_acor_sigma + 1e-9);
  for(i in 2:(n-1)) {
    err_proc_acor[i] ~ normal(err_proc_acor_phi * err_proc_acor[i-1], err_proc_acor_sigma + 1e-9);
  }
  // Autocorrelation (phi) & SD (sigma) of the process errors
  err_proc_acor_phi ~ beta(err_proc_acor_phi_alpha, err_proc_acor_phi_beta);
  err_proc_acor_sigma_scaled ~ cauchy(0, 1);

  // Daily metabolism priors
  GPP_daily ~ normal(GPP_daily_mu, GPP_daily_sigma);
  ER_daily ~ normal(ER_daily_mu, ER_daily_sigma);
  K600_daily ~ lognormal(K600_daily_meanlog, K600_daily_sdlog);
}
generated quantities {
  vector[d] GPP;
  vector[d] ER;
  vector[n] DO_obs_vec; // temporary
  vector[n] DO_mod_vec; // temporary
  vector[d] DO_R2;

  for(j in 1:d) {
    GPP[j] = sum(GPP_inst[1:n24,j]) / n24;
    ER[j] = sum(ER_inst[1:n24,j]) / n24;

    // Compute R2 for DO observations relative to the modeled, process-error-corrected state (DO_mod)
    for(i in 1:n) {
      DO_mod_vec[i] = DO_mod[i,j];
      DO_obs_vec[i] = DO_obs[i,j];
    }
    DO_R2[j] = 1 - sum((DO_mod_vec - DO_obs_vec) .* (DO_mod_vec - DO_obs_vec)) / sum((DO_obs_vec - mean(DO_obs_vec)) .* (DO_obs_vec - mean(DO_obs_vec)));
  }

}
