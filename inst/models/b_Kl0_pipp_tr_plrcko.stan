// b_Kl0_pipp_tr_plrcko.stan

data {
  // Parameters of priors on metabolism
  real GPP_daily_mu;
  real GPP_daily_lower;
  real<lower=0> GPP_daily_sigma;
  real ER_daily_mu;
  real ER_daily_upper;
  real<lower=0> ER_daily_sigma;
  
  // Parameters of hierarchical priors on K600_daily (linear_sdzero model)
  real lnK600_lnQ_intercept_mu;
  real<lower=0> lnK600_lnQ_intercept_sigma;
  real lnK600_lnQ_slope_mu;
  real<lower=0> lnK600_lnQ_slope_sigma;
  
  // Error distributions
  real<lower=0> err_proc_iid_sigma_scale;
  real<lower=0> err_mult_GPP_sdlog_sigma;
  
  // Data dimensions
  int<lower=1> d; // number of dates
  real<lower=0> timestep; // length of each timestep in days
  int<lower=1> n24; // number of observations in first 24 hours per date
  int<lower=1> n; // number of observations per date
  
  // Daily data
  vector[d] DO_obs_1;
  vector[d] lnQ_daily;
  
  // Data
  vector[d] DO_obs[n];
  vector[d] DO_sat[n];
  vector[d] light_mult_GPP[n];
  vector[d] const_mult_ER[n];
  vector[d] depth[n];
  vector[d] KO2_conv[n];
}

parameters {
  vector<lower=GPP_daily_lower>[d] GPP_daily;
  vector<upper=ER_daily_upper>[d] ER_daily;
  
  real lnK600_lnQ_intercept;
  real lnK600_lnQ_slope;
  
  real<lower=0> err_proc_iid_sigma_scaled;
  real<lower=0> err_mult_GPP_sdlog_scaled;
  vector<lower=0>[d] err_mult_GPP[n];
}

transformed parameters {
  vector[d] K600_daily_predlog;
  vector[d] K600_daily;
  vector[d] DO_mod_partial_sigma[n];
  real<lower=0> err_proc_iid_sigma;
  real<lower=0> err_mult_GPP_sdlog;
  vector[d] GPP_inst[n];
  vector[d] ER_inst[n];
  vector[d] KO2_inst[n];
  vector<lower=0>[d] combo_mult_GPP[n];
  vector<lower=0>[d] mean_combo_mult_GPP;
  vector[d] DO_mod_partial[n];
  
  // Rescale error distribution parameters
  err_proc_iid_sigma = err_proc_iid_sigma_scale * err_proc_iid_sigma_scaled;
  err_mult_GPP_sdlog = err_mult_GPP_sdlog_sigma * err_mult_GPP_sdlog_scaled;
  
  // Hierarchical, linear_sdzero model of K600_daily
  K600_daily_predlog = lnK600_lnQ_intercept + lnK600_lnQ_slope * lnQ_daily;
  K600_daily = exp(K600_daily_predlog);
  
  // Model DO time series
  // * trapezoid version
  // * no observation error
  // * IID process error
  // * reaeration depends on DO_obs
  
  // Calculate individual process rates
  for(i in 1:n) {
    combo_mult_GPP[i] = err_mult_GPP[i] .* light_mult_GPP[i];
  }
  for(j in 1:d) {
    mean_combo_mult_GPP[j] = sum(combo_mult_GPP[,j]) / n;
  }
  for(i in 1:n) {
    GPP_inst[i] = GPP_daily .* combo_mult_GPP[i] ./ mean_combo_mult_GPP;
    ER_inst[i] = ER_daily .* const_mult_ER[i];
    KO2_inst[i] = K600_daily .* KO2_conv[i];
  }
  
  // DO model
  DO_mod_partial[1] = DO_obs_1;
  DO_mod_partial_sigma[1] = err_proc_iid_sigma * timestep ./ depth[1];
  for(i in 1:(n-1)) {
    DO_mod_partial[i+1] =
      DO_obs[i] + (
        - KO2_inst[i] .* DO_obs[i] - KO2_inst[i+1] .* DO_obs[i+1] +
        (GPP_inst[i] + ER_inst[i]) ./ depth[i] +
        (GPP_inst[i+1] + ER_inst[i+1]) ./ depth[i+1] +
        KO2_inst[i] .* DO_sat[i] + KO2_inst[i+1] .* DO_sat[i+1]
      ) * (timestep / 2.0);
    for(j in 1:d) {
      DO_mod_partial_sigma[i+1,j] = err_proc_iid_sigma * 
        sqrt(pow(depth[i,j], -2) + pow(depth[i+1,j], -2)) .*
        (timestep / 2.0);
    }
  }
}

model {
  // Independent, identically distributed process error
  for(i in 1:n) {
    DO_obs[i] ~ normal(DO_mod_partial[i], DO_mod_partial_sigma[i]);
  }
  // SD (sigma) of the IID process errors
  err_proc_iid_sigma_scaled ~ cauchy(0, 1);
  
  // GPP-only independent, identically distributed process error
  for(i in 1:n) {
    err_mult_GPP[i] ~ lognormal(0, err_mult_GPP_sdlog);
  }
  // SD (sigma) of the GPP IID process error multipliers
  err_mult_GPP_sdlog_scaled ~ normal(0, 1);
  
  // Daily metabolism priors
  GPP_daily ~ normal(GPP_daily_mu, GPP_daily_sigma);
  ER_daily ~ normal(ER_daily_mu, ER_daily_sigma);
  // Hierarchical constraints on K600_daily (linear_sdzero model)
  lnK600_lnQ_intercept ~ normal(lnK600_lnQ_intercept_mu, lnK600_lnQ_intercept_sigma);
  lnK600_lnQ_slope ~ normal(lnK600_lnQ_slope_mu, lnK600_lnQ_slope_sigma);
  
}
generated quantities {
  vector[d] err_proc_iid[n-1];
  vector[d] GPP_inst_partial[n];
  vector[d] err_proc_GPP[n];
  vector[d] GPP;
  vector[d] ER;
  
  for(i in 2:n) {
    err_proc_iid[i-1] = (DO_mod_partial[i] - DO_obs[i]) .* (err_proc_iid_sigma ./ DO_mod_partial_sigma[i]);
  }
  for(i in 1:n) {
    GPP_inst_partial[i] = GPP_daily .* light_mult_GPP[i];
    err_proc_GPP[i] = GPP_inst[i] - GPP_inst_partial[i];
  }
  for(j in 1:d) {
    GPP[j] = sum(GPP_inst[1:n24,j]) / n24;
    ER[j] = sum(ER_inst[1:n24,j]) / n24;
  }
  
}
