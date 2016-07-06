// b_Kl_pcpi_pm_plrcko.stan

data {
  // Parameters of priors on metabolism
  real GPP_daily_mu;
  real GPP_daily_sigma;
  real ER_daily_mu;
  real ER_daily_sigma;
  
  // Parameters of hierarchical priors on K600_daily (linear model)
  vector[2] K600_daily_beta_mu;
  vector[2] K600_daily_beta_sigma;
  #real K600_daily_sigma_shape;
  real K600_daily_sigma_rate;
  
  // Error distributions
  real err_proc_acor_phi_shape; # using instead as beta(alpha)
  real err_proc_acor_phi_rate; # using instead as beta(beta)
  #real err_proc_acor_sigma_shape;
  real err_proc_acor_sigma_rate;
  #real err_proc_iid_sigma_shape;
  real err_proc_iid_sigma_rate;
  
  // Data dimensions
  int<lower=1> d; # number of dates
  int<lower=1> n; # number of observations per date
  
  // Daily data
  vector[d] DO_obs_1;
  vector[d] ln_discharge_daily;
  
  // Data
  vector[d] DO_obs[n];
  vector[d] DO_sat[n];
  vector[d] frac_GPP[n];
  vector[d] frac_ER[n];
  vector[d] frac_D[n];
  vector[d] depth[n];
  vector[d] KO2_conv[n];
}

transformed data {
  vector[d] coef_GPP[n-1];
  vector[d] coef_ER[n-1];
  vector[d] coef_K600_full[n-1];
  vector[d] dDO_obs[n-1];
  
  for(i in 1:(n-1)) {
    // Coefficients by pairmeans (e.g., mean(frac_GPP[i:(i+1)]) applies to the DO step from i to i+1)
    coef_GPP[i]  <- (frac_GPP[i] + frac_GPP[i+1])/2.0 ./ ((depth[i] + depth[i+1])/2.0);
    coef_ER[i]   <- (frac_ER[i] + frac_ER[i+1])/2.0 ./ ((depth[i] + depth[i+1])/2.0);
    coef_K600_full[i] <- (KO2_conv[i] + KO2_conv[i+1])/2.0 .* (frac_D[i] + frac_D[i+1])/2.0 .*
      (DO_sat[i] + DO_sat[i+1] - DO_obs[i] - DO_obs[i+1])/2.0;
    // dDO observations
    dDO_obs[i] <- DO_obs[i+1] - DO_obs[i];
  }
}

parameters {
  vector[d] GPP_daily;
  vector[d] ER_daily;
  vector<lower=0>[d] K600_daily;
  
  vector[2] K600_daily_beta;
  real K600_daily_sigma_scaled;
  
  real<lower=0, upper=1> err_proc_acor_phi;
  vector[d] err_proc_acor_inc[n-1];
  
  real err_proc_acor_sigma_scaled;
  real err_proc_iid_sigma_scaled;
}

transformed parameters {
  real K600_daily_sigma;
  real err_proc_acor_sigma;
  real err_proc_iid_sigma;
  vector[d] dDO_mod[n-1];
  vector[d] err_proc_acor[n-1];
  vector[d] K600_daily_pred;
  
  // Hierarchical, linear model of K600_daily
  K600_daily_pred <- exp(K600_daily_beta[1] + K600_daily_beta[2] * ln_discharge_daily);
  
  // Scaled parameters
  K600_daily_sigma <- K600_daily_sigma_rate * exp(K600_daily_sigma_scaled);
  err_proc_acor_sigma <- err_proc_acor_sigma_rate * exp(err_proc_acor_sigma_scaled);
  err_proc_iid_sigma <- err_proc_iid_sigma_rate * exp(err_proc_iid_sigma_scaled);
  
  // Model DO time series
  // * pairmeans version
  // * no observation error
  // * IID and autocorrelated process error
  // * reaeration depends on DO_obs
  
  err_proc_acor[1] <- err_proc_acor_sigma * err_proc_acor_inc[1];
  for(i in 1:(n-2)) {
    err_proc_acor[i+1] <- err_proc_acor_phi * err_proc_acor[i] + err_proc_acor_sigma * err_proc_acor_inc[i+1];
  }
  
  // dDO model
  for(i in 1:(n-1)) {
    dDO_mod[i] <- 
      err_proc_acor[i] +
      GPP_daily  .* coef_GPP[i] +
      ER_daily   .* coef_ER[i] +
      K600_daily .* coef_K600_full[i];
  }
}

model {
  // Independent, identically distributed process error
  for(i in 1:(n-1)) {
    dDO_obs[i] ~ normal(dDO_mod[i], err_proc_iid_sigma);
  }
  // SD (sigma) of the process errors
  err_proc_iid_sigma_scaled ~ normal(0, 1);
  
  // Autocorrelated process error
  for(i in 1:(n-1)) {
    err_proc_acor_inc[i] ~ normal(0, 1);
  }
  // Autocorrelation (phi) & SD (sigma) of the process errors
  err_proc_acor_phi ~ beta(err_proc_acor_phi_shape, err_proc_acor_phi_rate);
  err_proc_acor_sigma_scaled ~ normal(0, 1);
  
  // Daily metabolism priors
  GPP_daily ~ normal(GPP_daily_mu, GPP_daily_sigma);
  ER_daily ~ normal(ER_daily_mu, ER_daily_sigma);
  K600_daily ~ normal(K600_daily_pred, K600_daily_sigma);
  
  // Hierarchical constraints on K600_daily (linear model)
  K600_daily_beta ~ normal(K600_daily_beta_mu, K600_daily_beta_sigma);
  K600_daily_sigma_scaled ~ normal(0, 1);
}
