// b_Kbx_pi_eu_psrckm.stan

data {
  // Parameters of priors on metabolism
  real alpha_meanlog;
  real<lower=0> alpha_sdlog;
  real<lower=0> Pmax_mu;
  real<lower=0> Pmax_sigma;
  real ER_daily_mu;
  real ER_daily_upper;
  real<lower=0> ER_daily_sigma;
  
  // Parameters of hierarchical priors on K600_daily (binned_sdfixed model)
  int <lower=1> b; // number of K600_lnQ_nodes
  real K600_lnQ_nodediffs_sdlog;
  vector[b] K600_lnQ_nodes_meanlog;
  vector[b] K600_lnQ_nodes_sdlog;
  real<lower=0> K600_daily_sigma;
  
  // Error distributions
  real<lower=0> err_proc_iid_sigma_scale;
  
  // Data dimensions
  int<lower=1> d; // number of dates
  real<lower=0> timestep; // length of each timestep in days
  int<lower=1> n24; // number of observations in first 24 hours per date
  int<lower=1> n; // number of observations per date
  
  // Daily data
  vector[d] DO_obs_1;
  int<lower=1,upper=b> lnQ_bins[2,d];
  vector<lower=0,upper=1>[d] lnQ_bin_weights[2];
  
  // Data
  vector[d] DO_obs[n];
  vector[d] DO_sat[n];
  vector[d] light[n];
  vector[d] const_mult_ER[n];
  vector[d] depth[n];
  vector[d] KO2_conv[n];
}

parameters {
  vector[d] alpha_scaled;
  vector[d] Pmax;
  vector<upper=ER_daily_upper>[d] ER_daily;
  vector<lower=0>[d] K600_daily;
  
  vector[b] lnK600_lnQ_nodes;
  
  real<lower=0> err_proc_iid_sigma_scaled;
}

transformed parameters {
  vector[d] K600_daily_predlog;
  vector[d] DO_mod_partial_sigma[n];
  real<lower=0> err_proc_iid_sigma;
  vector<lower=0>[d] alpha;
  vector[d] GPP_inst[n];
  vector[d] ER_inst[n];
  vector[d] KO2_inst[n];
  vector[d] DO_mod_partial[n];
  
  // Rescale error distribution parameters
  err_proc_iid_sigma = err_proc_iid_sigma_scale * err_proc_iid_sigma_scaled;
  
  // Rescale select daily parameters
  alpha = exp(alpha_meanlog + alpha_sdlog * alpha_scaled);
  
  // Hierarchical, binned_sdfixed model of K600_daily
  K600_daily_predlog = lnK600_lnQ_nodes[lnQ_bins[1]] .* lnQ_bin_weights[1] + 
                       lnK600_lnQ_nodes[lnQ_bins[2]] .* lnQ_bin_weights[2];
  
  // Model DO time series
  // * euler version
  // * no observation error
  // * IID process error
  // * reaeration depends on DO_mod
  
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
      DO_obs[i] + (
        (GPP_inst[i] + ER_inst[i]) ./ depth[i] +
        KO2_inst[i] .* (DO_sat[i] - DO_mod_partial[i])
      ) * timestep;
    for(j in 1:d) {
      DO_mod_partial_sigma[i+1,j] = err_proc_iid_sigma * 
        timestep ./ depth[i,j];
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
  
  // Daily metabolism priors
  alpha_scaled ~ normal(0, 1);
  Pmax ~ normal(Pmax_mu, Pmax_sigma);
  ER_daily ~ normal(ER_daily_mu, ER_daily_sigma);
  K600_daily ~ normal(exp(K600_daily_predlog), K600_daily_sigma);
  // Hierarchical constraints on K600_daily (binned_sdfixed model)
  lnK600_lnQ_nodes ~ normal(K600_lnQ_nodes_meanlog, K600_lnQ_nodes_sdlog);
  for(k in 2:b) {
    lnK600_lnQ_nodes[k] ~ normal(lnK600_lnQ_nodes[k-1], K600_lnQ_nodediffs_sdlog);
  }
  
}
generated quantities {
  vector[d] err_proc_iid[n-1];
  vector[d] GPP;
  vector[d] ER;
  
  for(i in 2:n) {
    err_proc_iid[i-1] = (DO_mod_partial[i] - DO_obs[i]) .* (err_proc_iid_sigma ./ DO_mod_partial_sigma[i]);
  }
  for(j in 1:d) {
    GPP[j] = sum(GPP_inst[1:n24,j]) / n24;
    ER[j] = sum(ER_inst[1:n24,j]) / n24;
  }
  
}
