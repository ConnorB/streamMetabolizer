// b_Kb0_oipi_tr_psrcko.stan

data {
  // Parameters of priors on metabolism
  real alpha_meanlog;
  real<lower=0> alpha_sdlog;
  real<lower=0> Pmax_mu;
  real<lower=0> Pmax_sigma;
  real ER_daily_mu;
  real ER_daily_upper;
  real<lower=0> ER_daily_sigma;
  
  // Parameters of hierarchical priors on K600_daily (binned_sdzero model)
  int <lower=1> b; // number of K600_lnQ_nodes
  real K600_lnQ_nodediffs_sdlog;
  vector[b] K600_lnQ_nodes_meanlog;
  vector[b] K600_lnQ_nodes_sdlog;
  
  // Error distributions
  real<lower=0> err_obs_iid_sigma_scale;
  real<lower=0> err_proc_iid_sigma_scale;
  
  // Data dimensions
  int<lower=1> d; // number of dates
  real<lower=0> timestep; // length of each timestep in days
  int<lower=1> n24; // number of observations in first 24 hours per date
  int<lower=1> n; // number of observations per date
  
  // Daily data
  vector[d] DO_obs_1;
  array[2, d] int<lower=1, upper=b> lnQ_bins;
  array[2] vector<lower=0,upper=1>[d] lnQ_bin_weights;
  
  // Data
  array[n] vector[d] DO_obs;
  array[n] vector[d] DO_sat;
  array[n] vector[d] light;
  array[n] vector[d] const_mult_ER;
  array[n] vector[d] depth;
  array[n] vector[d] KO2_conv;
}

parameters {
  vector[d] alpha_scaled;
  vector[d] Pmax;
  vector<upper=ER_daily_upper>[d] ER_daily;
  
  vector[b] lnK600_lnQ_nodes;
  
  real<lower=0> err_obs_iid_sigma_scaled;
  real<lower=0> err_proc_iid_sigma_scaled;
  array[n] vector[d] DO_mod;
}

transformed parameters {
  vector[d] K600_daily_predlog;
  vector[d] K600_daily;
  real<lower=0> err_obs_iid_sigma;
  array[n] vector[d] DO_mod_partial_sigma;
  real<lower=0> err_proc_iid_sigma;
  vector<lower=0>[d] alpha;
  array[n] vector[d] GPP_inst;
  array[n] vector[d] ER_inst;
  array[n] vector[d] KO2_inst;
  array[n] vector[d] DO_mod_partial;
  
  // Rescale error distribution parameters
  err_obs_iid_sigma = err_obs_iid_sigma_scale * err_obs_iid_sigma_scaled;
  err_proc_iid_sigma = err_proc_iid_sigma_scale * err_proc_iid_sigma_scaled;
  
  // Rescale select daily parameters
  alpha = exp(alpha_meanlog + alpha_sdlog * alpha_scaled);
  
  // Hierarchical, binned_sdzero model of K600_daily
  K600_daily_predlog = lnK600_lnQ_nodes[lnQ_bins[1]] .* lnQ_bin_weights[1] + 
                       lnK600_lnQ_nodes[lnQ_bins[2]] .* lnQ_bin_weights[2];
  K600_daily = exp(K600_daily_predlog);
  
  // Model DO time series
  // * trapezoid version
  // * observation error
  // * IID process error
  // * reaeration depends on DO_obs
  
  // Calculate individual process rates
  for(i in 1:n) {
    GPP_inst[i] = Pmax .* tanh(light[i] .* alpha ./ Pmax);
    ER_inst[i] = ER_daily .* const_mult_ER[i];
    KO2_inst[i] = K600_daily .* KO2_conv[i];
  }
  
  // DO model
  DO_mod_partial[1] = DO_obs_1;
  DO_mod_partial_sigma[1] = err_proc_iid_sigma * timestep ./ depth[1];
  for(i in 1:(n-1)) {
    DO_mod_partial[i+1] =
      DO_mod[i] + (
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
    DO_mod[i] ~ normal(DO_mod_partial[i], DO_mod_partial_sigma[i]);
  }
  // SD (sigma) of the IID process errors
  err_proc_iid_sigma_scaled ~ cauchy(0, 1);
  
  // Independent, identically distributed observation error
  for(i in 1:n) {
    DO_obs[i] ~ normal(DO_mod[i], err_obs_iid_sigma);
  }
  // SD (sigma) of the observation errors
  err_obs_iid_sigma_scaled ~ cauchy(0, 1);
  
  // Daily metabolism priors
  alpha_scaled ~ normal(0, 1);
  Pmax ~ normal(Pmax_mu, Pmax_sigma);
  ER_daily ~ normal(ER_daily_mu, ER_daily_sigma);
  // Hierarchical constraints on K600_daily (binned_sdzero model)
  lnK600_lnQ_nodes ~ normal(K600_lnQ_nodes_meanlog, K600_lnQ_nodes_sdlog);
  for(k in 2:b) {
    lnK600_lnQ_nodes[k] ~ normal(lnK600_lnQ_nodes[k-1], K600_lnQ_nodediffs_sdlog);
  }
  
}
generated quantities {
  array[n] vector[d] err_obs_iid;
  array[n-1] vector[d] err_proc_iid;
  vector[d] GPP;
  vector[d] ER;
  vector[n] DO_obs_vec; // temporary
  vector[n] DO_mod_vec; // temporary
  vector[d] DO_R2;
  
  for(i in 1:n) {
    err_obs_iid[i] = DO_mod[i] - DO_obs[i];
  }
  for(i in 2:n) {
    err_proc_iid[i-1] = (DO_mod_partial[i] - DO_mod[i]) .* (err_proc_iid_sigma ./ DO_mod_partial_sigma[i]);
  }
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
