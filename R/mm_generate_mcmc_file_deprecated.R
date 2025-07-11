#' Generate the models in inst/models/bayes
#' 
#' @inheritParams mm_name
#' @keywords internal
mm_generate_mcmc_file_deprecated <- function(
  type='bayes', 
  pool_K600=c('none',
              'normal','normal_sdzero','normal_sdfixed',
              'linear','linear_sdzero','linear_sdfixed',
              'binned','binned_sdzero','binned_sdfixed'),
  err_obs_iid=c(TRUE, FALSE),
  err_proc_acor=c(FALSE, TRUE),
  err_proc_iid=c(FALSE, TRUE),
  err_proc_GPP=c(FALSE, TRUE),
  ode_method=c('trapezoid','euler'),
  GPP_fun=c('linlight', 'satlight'),
  ER_fun=c('constant'), #'q10temp'
  deficit_src=c('DO_mod','DO_obs'),
  engine='stan') {
  
  # handle Euler and pairmeans as deprecated arguments. mm_name runs a similar check & warning
  if(ode_method %in% c('Euler','pairmeans'))
    warning("for ode_method, 'Euler' and 'pairmeans' are deprecated in favor of 'euler' and 'trapezoid'")
  if(ode_method == 'Euler') ode_method <- 'euler'
  if(ode_method == 'pairmeans') ode_method <- 'trapezoid'
  
  # name the model. much argument checking happens here, even with 
  # check_validity=FALSE (which is needed to avoid circularity). reparsing gives
  # us a few extra fields back that we'll want below
  model_name <- mm_name(
    type='bayes',
    pool_K600=pool_K600,
    err_obs_iid=err_obs_iid, err_proc_acor=err_proc_acor, err_proc_iid=err_proc_iid, err_proc_GPP=err_proc_GPP,
    ode_method=ode_method, GPP_fun=GPP_fun, ER_fun=ER_fun, deficit_src=deficit_src, engine=engine,
    check_validity=FALSE)
  features <- mm_parse_name(model_name, expand=TRUE)
  pool_K600_type <- features$pool_K600_type
  pool_K600_sd <- features$pool_K600_sd
  
  #### helper functions ####
  comment <- function(...) { 
    # prefix with the appropriate comment character[s]
    paste0('// ', paste0(...))
  }
  chunk <- function(..., indent=2, newline=TRUE) { 
    # indent a chunk & add a newline
    lines <- c(list(...))
    lines <- lines[!sapply(lines, is.null)]
    lines <- unlist(lines)
    if(newline) lines <- c(lines, '')
    sapply(lines, function(line) {
      paste0(
        paste0(rep(' ',indent),collapse=''), 
        line)
    }, USE.NAMES = FALSE)
  }
  indent <- function(..., indent=2) {
    chunk(..., indent=indent, newline=FALSE)
  }
  f <- function(distrib, ...) { 
    # check that I've named the arguments correctly
    args <- c(list(...))
    switch(
      distrib,
      beta = { if(!all(names(args) == c('alpha','beta'))) stop("expecting beta(alpha,beta)") },
      gamma = { if(!all(names(args) == c('shape','rate'))) stop("expecting gamma(shape,rate)")
        # shape = alpha = k = first argument
        # rate = beta = 1/theta = inverse scale = second argument
      },
      halfcauchy = { if(!all(names(args) == c('scale'))) stop("expecting halfcauchy(scale)")
        distrib <- 'cauchy'
        args <- c(list(location=0), args)
      },
      halfnormal = { if(!all(names(args) == c('sigma'))) stop("expecting halfnormal(sigma)")
        distrib <- 'normal'
        args <- c(list(mu=0), args)
      },
      lognormal = { if(!all(names(args) == c('meanlog','sdlog'))) stop("expecting lognormal(meanlog,sdlog)") 
        # meanlog = mu = first argument
        # sdlog = sigma = second argument
      },
      normal = { if(!all(names(args) == c('mu','sigma'))) stop("expecting normal(mu,sigma)") }, 
      uniform = { if(!all(names(args) == c('min','max'))) stop("expecting uniform(min,max)") },
      stop(paste0("no f function available for ", distrib))
    )
    # create the function call text
    paste0(distrib, '(', paste0(args, collapse=', '), ')')
  }
  fs <- function(distrib, Y) {
    # Y_scaled is from a standard normal distribution, e.g., Y_scaled ~
    # normal(0, 1). this function returns an equation calculating Y, the
    # rescaled values of Y_scaled that follow the distrib distribution. It
    # assumes there exist parameters with suffixes corresponding to the args
    # required by f()
    paste0(
      Y, ' = ',
      switch(
        distrib,
        beta = stop(),
        gamma = stop(),
        halfcauchy = sprintf('%s_scale * %s_scaled', Y, Y), # scaled = cauchy(0,1)
        halfnormal = sprintf('%s_sigma * %s_scaled', Y, Y), # scaled = normal(0,1)
        lognormal = sprintf('exp(%s_meanlog + %s_sdlog * %s_scaled)', Y, Y, Y), # scaled = norm(0,1)
        normal = sprintf('%s_sigma * %s_scaled', Y, Y), # scaled = norm(0,1)
        uniform = sprintf('%s_min + (%s_max - %s_min) * %s_scaled', Y, Y), # scaled = unif(0,1)
        stop(paste0("no fs function available for ", distrib))
      )
    )
  }
  p <- paste0 # partial line: just combine strings into string
  s <- function(...) {
    # line with stop/semicolon: combine strings into string & add semicolon
    p(p(...), ';')
  }
  
  #### <begin model definition> ####
  model_text <- c(
    
    comment(model_name), '',
    
    #### data ####
    c('data {',
      chunk(
        comment('Parameters of priors on metabolism'),
        if(features$GPP_fun == 'linlight') c(
          'real GPP_daily_mu;',
          'real GPP_daily_lower;',
          'real<lower=0> GPP_daily_sigma;'
        ) else c(
          'real alpha_meanlog;',
          'real<lower=0> alpha_sdlog;',
          'real<lower=0> Pmax_mu;',
          'real<lower=0> Pmax_sigma;'
        ),
        
        'real ER_daily_mu;',
        'real ER_daily_upper;',
        'real<lower=0> ER_daily_sigma;',
        
        if(pool_K600_type %in% c('normal','linear','binned')) c(
          p(''),
          comment('Parameters of hierarchical priors on K600_daily (', pool_K600, ' model)')
        ),
        # [non]hierarchical models each do K600_daily_meanlog / K600_daily_predlog 
        # / K600_daily_sdlog / K600_daily_sigma differently
        switch(
          pool_K600_type,
          none=c(
            'real K600_daily_meanlog;'),
          normal=c(
            'real K600_daily_meanlog_meanlog;',
            'real<lower=0> K600_daily_meanlog_sdlog;'),
          linear=c(
            'real lnK600_lnQ_intercept_mu;',
            'real<lower=0> lnK600_lnQ_intercept_sigma;',
            'real lnK600_lnQ_slope_mu;',
            'real<lower=0> lnK600_lnQ_slope_sigma;'),
          binned=c(
            'int <lower=1> b; // number of K600_lnQ_nodes',
            'real K600_lnQ_nodediffs_sdlog;',
            'vector[b] K600_lnQ_nodes_meanlog;',
            'vector[b] K600_lnQ_nodes_sdlog;')
        ),
        switch(
          pool_K600_sd,
          zero=c(),
          fixed=switch(
            pool_K600_type,
            none=, normal='real<lower=0> K600_daily_sdlog;',
            linear=, binned='real<lower=0> K600_daily_sigma;'),
          fitted=switch(
            pool_K600_type,
            normal='real<lower=0> K600_daily_sdlog_sigma;',
            linear=, binned='real<lower=0> K600_daily_sigma_sigma;')
        )
      ),
      
      chunk(
        comment('Error distributions'),
        if(err_obs_iid) c(
          'real<lower=0> err_obs_iid_sigma_scale;'),
        if(err_proc_acor) c(
          'real err_proc_acor_phi_alpha;',
          'real err_proc_acor_phi_beta;',
          'real<lower=0> err_proc_acor_sigma_scale;'),
        if(err_proc_iid) c(
          'real<lower=0> err_proc_iid_sigma_scale;'),
        if(err_proc_GPP) c(
          'real<lower=0> err_mult_GPP_sdlog_sigma;')),
      
      chunk(
        comment('Data dimensions'),
        'int<lower=1> d; // number of dates',
        'real<lower=0> timestep; // length of each timestep in days',
        'int<lower=1> n24; // number of observations in first 24 hours per date',
        'int<lower=1> n; // number of observations per date'),
      
      chunk(
        comment('Daily data'),
        'vector[d] DO_obs_1;',
        switch(
          pool_K600_type,
          linear=c(
            'vector[d] lnQ_daily;'),
          binned=c(
            'int<lower=1,upper=b> lnQ_bins[2,d];',
            'vector<lower=0,upper=1>[d] lnQ_bin_weights[2];')
        )),
      
      # prepare to iterate over n obs for all d at a time:
      # https://groups.google.com/forum/#!topic/stan-users/ZHeFFV4q_gk
      indent(
        comment('Data'),
        'vector[d] DO_obs[n];',
        'vector[d] DO_sat[n];',
        # light_mult_GPP and const_mult_ER are multipliers that reflect light
        # and a constant, respectively, and produce estimates of GPP and ER,
        # respectively. The resulting GPP and ER estimates are in per-day units
        # (not per-timestep units)
        switch(
          features$GPP_fun,
          linlight='vector[d] light_mult_GPP[n];',
          satlight='vector[d] light[n];'
        ),
        'vector[d] const_mult_ER[n];',
        'vector[d] depth[n];',
        'vector[d] KO2_conv[n];'),
      
      '}',''
    ),
    
    #### transformed data ####
    # c('transformed data {', # transformed data = statements evaluated exactly once
    #   indent(
    #     #   chunk(
    #     #     # Coefficient declarations, if any, go here
    #     #   ),
    #     #   
    #     #   indent(
    #     #     p('for(i in 1:n) {'),
    #     #     indent(
    #     #       # Coefficient pre-calculations, if any, go here
    #     #     ),
    #     #     p('}')
    #     #   ),
    #   ),
    #   '}',''
    # ),
    
    #### parameters ####
    c('parameters {',
      indent(
        # daily metabolism rate parameters
        switch(
          features$GPP_fun,
          linlight=c('vector<lower=GPP_daily_lower>[d] GPP_daily;'),
          satlight=c('vector[d] alpha_scaled;',
                     'vector[d] Pmax;') #<lower=0>
        ),
        c('vector<upper=ER_daily_upper>[d] ER_daily;',
          if(pool_K600_sd %in% c('fixed','fitted')) c(
            'vector<lower=0>[d] K600_daily;')
        ),
        
        # K600 pooling parameters
        if(pool_K600_type != 'none') c(
          '',
          switch(
            pool_K600_type,
            normal=c(
              'real K600_daily_predlog;'),
            linear=c(
              'real lnK600_lnQ_intercept;',
              'real lnK600_lnQ_slope;'),
            binned=c(
              'vector[b] lnK600_lnQ_nodes;')
          ),
          if(pool_K600_sd == 'fitted') switch(
            pool_K600_type,
            normal='real<lower=0> K600_daily_sdlog_scaled;',
            linear=, binned='real<lower=0> K600_daily_sigma_scaled;'
          )
        ),
        
        # error distributions
        '',
        if(err_obs_iid) c(
          'real<lower=0> err_obs_iid_sigma_scaled;'),
        if(err_proc_acor) c(
          # need to figure out how to scale phi (which might be 0-1 or very close to 0)
          'real<lower=0, upper=1> err_proc_acor_phi;',
          'real<lower=0> err_proc_acor_sigma_scaled;',
          sprintf('vector[d] err_proc_acor_inc[%s];', switch(ode_method, euler='n-1', trapezoid='n'))),
        if(err_proc_iid) c(
          'real<lower=0> err_proc_iid_sigma_scaled;'),
        if(err_proc_GPP) c(
          'real<lower=0> err_mult_GPP_sdlog_scaled;',
          'vector<lower=0>[d] err_mult_GPP[n];'),
        
        # DO_mod if it's a fitted parameter (oipi models)
        if(err_obs_iid && err_proc_iid) c(
          'vector[d] DO_mod[n];')
        
      ),
      '}',''
    ),
    
    #### transformed parameters ####
    'transformed parameters {', # transformed parameters = statements evaluated once per leapfrog step
    
    # transformed parameter declarations
    chunk(
      # rescaled K600 pooling parameters
      if(pool_K600_type %in% c('linear','binned')) c(
        'vector[d] K600_daily_predlog;'
      ),
      if(pool_K600_sd == 'zero') c(
        'vector[d] K600_daily;'
      ),
      if(pool_K600_sd == 'fitted') switch(
        pool_K600_type,
        normal='real<lower=0> K600_daily_sdlog;',
        linear=, binned='real<lower=0> K600_daily_sigma;'
      ),
      
      # rescaled error distribution parameters
      if(err_obs_iid) c(
        'real<lower=0> err_obs_iid_sigma;'),
      if(err_proc_acor || err_proc_iid) c(
        'vector[d] DO_mod_partial_sigma[n];'
      ),
      if(err_proc_acor) c(
        # 'real<lower=0, upper=1> err_proc_acor_phi;', # currently opting not to scale phi (which might be 0-1 or very close to 0)
        'real<lower=0> err_proc_acor_sigma;'),
      if(err_proc_iid) c(
        'real<lower=0> err_proc_iid_sigma;'),
      if(err_proc_GPP) c(
        'real<lower=0> err_mult_GPP_sdlog;'),
      
      if(features$GPP_fun == 'satlight') c(
        'vector<lower=0>[d] alpha;'
      ),
      
      # instantaneous GPP, ER, and KO2. the nth value isn't used to calculate DO
      # when ode_method=euler, but it's always used to calculate GPP and ER
      c('vector[d] GPP_inst[n];',
        'vector[d] ER_inst[n];',
        'vector[d] KO2_inst[n];'),
      
      # these variables contain the combined, unnormalized GPP-producing
      # multiplier (combo_mult_GPP) and the sum of those multipliers on each day
      # (sum_combo_mult_GPP), from which the final GPP multiplier (mult_GPP)
      # will be implicitly calculated in the GPP_inst equation
      if(err_proc_GPP) c(
        'vector<lower=0>[d] combo_mult_GPP[n];',
        'vector<lower=0>[d] mean_combo_mult_GPP;'),
      
      # instantaneous DO and possibly process error values
      if(err_proc_iid)
        'vector[d] DO_mod_partial[n];'
      else # err_obs_iid and/or err_proc_acor without err_proc_iid
        'vector[d] DO_mod[n];',
      if(err_proc_acor)
        sprintf('vector[d] err_proc_acor[%s];', switch(ode_method, euler='n-1', trapezoid='n'))
    ),
    
    # pooling parameters
    if(pool_K600_sd == 'fitted') chunk(
      comment('Rescale pooling distribution parameter'),
      switch(
        pool_K600_type,
        normal=s(fs('halfnormal', 'K600_daily_sdlog')),
        linear=, binned=s(fs('halfnormal', 'K600_daily_sigma'))
      )
    ),
    
    # error distribution parameters
    chunk(
      comment('Rescale error distribution parameters'),
      if(err_obs_iid) c(
        s(fs('halfcauchy', 'err_obs_iid_sigma'))),
      if(err_proc_acor) c(
        # s(fs('beta', 'err_proc_acor_phi'?)), # currently opting not to scale phi (which might be 0-1 or very close to 0)
        s(fs('halfcauchy', 'err_proc_acor_sigma'))),
      if(err_proc_iid) c(
        s(fs('halfcauchy', 'err_proc_iid_sigma'))),
      if(err_proc_GPP) c(
        s(fs('halfnormal', 'err_mult_GPP_sdlog')))
    ),
    
    # daily parameters
    if(features$GPP_fun == 'satlight') {
      chunk(
        comment('Rescale select daily parameters'),
        c(
          s(fs('lognormal', 'alpha')))
      )
    },
    
    # K600_daily model
    if(pool_K600_type %in% c('linear','binned') || pool_K600_sd == 'zero') chunk(
      comment('Hierarchical, ', pool_K600, ' model of K600_daily'),
      switch(
        pool_K600_type,
        linear=s('K600_daily_predlog = lnK600_lnQ_intercept + lnK600_lnQ_slope * lnQ_daily'),
        binned=s('K600_daily_predlog = lnK600_lnQ_nodes[lnQ_bins[1]] .* lnQ_bin_weights[1] + \n  ',
                 '                     lnK600_lnQ_nodes[lnQ_bins[2]] .* lnQ_bin_weights[2]')
      ),
      if(pool_K600_sd == 'zero') switch(
        pool_K600_type,
        normal=c(
          p('for(j in 1:d) {'),
          indent(
            s('K600_daily[j] = exp(K600_daily_predlog)')
          ),
          p('}')
        ),
        linear=, binned=c(
          s('K600_daily = exp(K600_daily_predlog)')
        )
      )
    ),
    
    # model instantaneous DO, dDO, and/or process error values
    indent(
      comment('Model DO time series'),
      comment('* ', ode_method,' version'),
      comment('* ', if(!err_obs_iid) 'no ', 'observation error'),
      comment('* ', paste0(c(if(err_proc_iid) 'IID', if(err_proc_acor) 'autocorrelated', if(!err_proc_iid && !err_proc_acor) 'no'), collapse=' and '), ' process error'),
      comment('* ', 'reaeration depends on ',deficit_src),
      
      # process error (always looped, vectorized across days)
      if(err_proc_acor) c(
        p(''),
        comment("Calculate autocorrelated process error rates"),
        s('err_proc_acor[1] = err_proc_acor_inc[1]'),
        p(sprintf('for(i in 2:%s) {', switch(ode_method, euler='(n-1)', trapezoid='n'))),
        s('  err_proc_acor[i] = err_proc_acor_phi * err_proc_acor[i-1] + err_proc_acor_inc[i]'),
        p('}')
      ),
      
      # individual processes
      c(
        p(''),
        comment("Calculate individual process rates"),
        
        if(err_proc_GPP) c(
          p('for(i in 1:n) {'),
          indent(
            # X_mult_Y syntax: X = process reflected by multiplier, Y = quantity
            # modified by multiplier.
            s('combo_mult_GPP[i] = err_mult_GPP[i] .* light_mult_GPP[i]')#,
          ),
          p('}'),
          p('for(j in 1:d) {'),
          indent(
            s('mean_combo_mult_GPP[j] = sum(combo_mult_GPP[,j]) / n') #[1:n,j]
          ),
          p('}')
        ),
        
        p('for(i in 1:n) {'),
        indent(
          if(err_proc_GPP) {c(
            # s('pp_mult_GPP[i] = combo_mult_GPP[i] ./ mean_combo_mult_GPP'), # added to next line to save variables
            s('GPP_inst[i] = GPP_daily .* combo_mult_GPP[i] ./ mean_combo_mult_GPP') #[1:d]
          )} else {
            switch(
              features$GPP_fun,
              'linlight'=s('GPP_inst[i] = GPP_daily .* light_mult_GPP[i]'),
              'satlight'=s('GPP_inst[i] = Pmax .* tanh(light[i] .* alpha ./ Pmax)'))
          },
          s('ER_inst[i] = ER_daily .* const_mult_ER[i]'),
          s('KO2_inst[i] = K600_daily .* KO2_conv[i]')
        ),
        p('}')
      ),
      
      # DO model - any model that includes observation error or is a function of
      # the previous moment's DO_mod
      c(
        p(''),
        comment("DO model"),
        if(err_obs_iid && !err_proc_iid) c(
          # applies to oi models. pi models don't have DO_mod, and oipi models 
          # have DO_mod as a parameter rather than a transformed parameter. not
          # sure about models that include pc
          s('DO_mod[1] = DO_obs_1')
        ),
        if(err_proc_iid) c(
          # DO_mod_partial[1] is only strictly needed if(err_proc_iid && 
          # !err_obs_iid && deficit_src=='DO_mod'). oipi_anything, pi_ko, and 
          # pcpi_ko models start with DO_mod_partial[2], and !pi models don't 
          # use DO_mod_partial at all. But if you request it in params_out then 
          # DO_mod_partial[1] must be defined, so we'll define it for all pis
          s('DO_mod_partial[1] = DO_obs_1'),
          s('DO_mod_partial_sigma[1] = err_proc_iid_sigma * timestep ./ depth[1]')
        ),
        p('for(i in 1:(n-1)) {'),
        indent(
          if(err_proc_iid) p(
            'DO_mod_partial[i+1] ='
          ) else p( # err_obs_iid and/or err_proc_acor without err_proc_iid
            'DO_mod[i+1] ='
          ),
          indent(
            switch(
              ode_method,
              'euler' = c(
                p(if(err_obs_iid) 'DO_mod' else 'DO_obs', '[i] + (')
              ),
              'trapezoid' = c(
                switch(
                  deficit_src,
                  'DO_obs' = c(
                    p(if(err_obs_iid) 'DO_mod' else 'DO_obs', '[i] + ('),
                    p('  - KO2_inst[i] .* DO_obs[i] - KO2_inst[i+1] .* DO_obs[i+1] +')),
                  'DO_mod' = c(
                    p(if(err_obs_iid) 'DO_mod' else 'DO_mod_partial', '[i] .*'),
                    p('  (2.0 - KO2_inst[i] * timestep) ./ (2.0 + KO2_inst[i+1] * timestep) + ('))
                )
              )
            ),
            p('  (GPP_inst[i] + ER_inst[i]',
              if(err_proc_acor) ' + err_proc_acor[i]',
              ') ./ depth[i] +'),
            switch(
              ode_method,
              'euler' = c(
                p('  KO2_inst[i] .* (DO_sat[i] - ',
                  if(deficit_src=='DO_mod' && !err_obs_iid) 'DO_mod_partial' else deficit_src,
                  '[i])'),
                s(') * timestep')
              ),
              'trapezoid' = c(
                p('  (GPP_inst[i+1] + ER_inst[i+1]',
                  if(err_proc_acor) ' + err_proc_acor[i+1]',
                  ') ./ depth[i+1] +'),
                p('  KO2_inst[i] .* DO_sat[i] + KO2_inst[i+1] .* DO_sat[i+1]'),
                switch(
                  deficit_src,
                  'DO_obs' = c(
                    s(') * (timestep / 2.0)')),
                  'DO_mod' = c(
                    s(') .* (timestep ./ (2.0 + KO2_inst[i+1] * timestep))'))
                )
              )
            )
          ),
          if(err_proc_iid) c(
            p('for(j in 1:d) {'),
            indent(
              'DO_mod_partial_sigma[i+1,j] = err_proc_iid_sigma * ',
              switch(
                ode_method,
                'euler' = indent(
                  s('timestep ./ depth[i,j]')
                ),
                'trapezoid' = indent(
                  'sqrt(pow(depth[i,j], -2) + pow(depth[i+1,j], -2)) .*',
                  switch(
                    deficit_src,
                    'DO_obs' = s('(timestep / 2.0)'),
                    'DO_mod' = s('(timestep / (2.0 + KO2_inst[i+1,j] * timestep))')
                  )
                )
              )
            ),
            p('}')
          )
        ),
        p('}')
      )
    ),
    '}','',
    
    #### model ####
    'model {',
    
    if(err_proc_iid || err_proc_acor) chunk(
      if(err_proc_iid) c(
        comment('Independent, identically distributed process error'),
        # using 1:n rather than 2:n for all pi or pc models because sometimes this
        # is the only direct constraint on DO_mod[1] (e.g., for oipi_plrcko)
        p('for(i in 1:n) {'),
        indent(
          s(if(!err_obs_iid) 'DO_obs[i]' else 'DO_mod[i]', ' ~ ', 
            f('normal', mu='DO_mod_partial[i]', sigma='DO_mod_partial_sigma[i]')
          )
        ),
        p('}')
      ),
      if(err_proc_acor) c(
        comment('Autocorrelated process error'),
        p('for(i in 1:n) {'),
        indent(
          s('err_proc_acor_inc[i-1] ~ ', f('normal', mu='0', sigma='err_proc_acor_sigma'))
        ),
        p('}')
      ),
      if(err_proc_iid) c(
        comment('SD (sigma) of the IID process errors'),
        s('err_proc_iid_sigma_scaled ~ ', f('halfcauchy', scale='1'))),
      if(err_proc_acor) c(
        comment('Autocorrelation (phi) & SD (sigma) of the process errors'),
        s('err_proc_acor_phi ~ ', f('beta', alpha='err_proc_acor_phi_alpha', beta='err_proc_acor_phi_beta')), # currently opting not to scale phi (which might be 0-1 or very close to 0)
        s('err_proc_acor_sigma_scaled ~ ', f('halfcauchy', scale='1')))
    ),
    
    if(err_proc_GPP) chunk(
      comment('GPP-only independent, identically distributed process error'),
      p('for(i in 1:n) {'),
      indent(
        s('err_mult_GPP[i] ~ ', f('lognormal', meanlog=0, sdlog='err_mult_GPP_sdlog'))),
      p('}'),
      comment('SD (sigma) of the GPP IID process error multipliers'),
      s('err_mult_GPP_sdlog_scaled ~ ', f('normal', mu=0, sigma=1))
    ),
    
    if(err_obs_iid) chunk(
      comment('Independent, identically distributed observation error'),
      if(err_obs_iid && err_proc_iid)
        p('for(i in 1:n) {') # only works for state-space models because these have DO_mod as param, not trans param
      else
        p('for(i in 2:n) {'),
      indent(
        s('DO_obs[i] ~ ', f('normal', mu=p('DO_mod[i]'), sigma='err_obs_iid_sigma'))
      ),
      p('}'),
      comment('SD (sigma) of the observation errors'),
      s('err_obs_iid_sigma_scaled ~ ', f('halfcauchy', scale='1'))),
    
    indent(
      comment('Daily metabolism priors'),
      switch(
        features$GPP_fun,
        linlight = s('GPP_daily ~ ', f('normal', mu='GPP_daily_mu', sigma='GPP_daily_sigma')),
        satlight = c(
          s('alpha_scaled ~ ', f('normal', mu='0', sigma='1')),
          s('Pmax ~ ', f('normal', mu='Pmax_mu', sigma='Pmax_sigma'))
        )
      ),
      s('ER_daily ~ ', f('normal', mu='ER_daily_mu', sigma='ER_daily_sigma')),
      if(pool_K600_sd %in% c('fixed','fitted')) c(
        switch(
          pool_K600_type,
          none=s(
            'K600_daily ~ ', f('lognormal', meanlog='K600_daily_meanlog', sdlog='K600_daily_sdlog')),
          normal=s(
            'K600_daily ~ ', f('lognormal', meanlog='K600_daily_predlog', sdlog='K600_daily_sdlog')),
          linear=,binned=s(
            'K600_daily ~ ', f('normal', mu='exp(K600_daily_predlog)', sigma='K600_daily_sigma'))
        )
      )
    ),
    
    if(pool_K600_type != 'none') chunk(
      comment('Hierarchical constraints on K600_daily (', pool_K600, ' model)'),
      switch(
        pool_K600_type,
        'normal' = c(
          s('K600_daily_predlog ~ ', f('normal', mu='K600_daily_meanlog_meanlog', sigma='K600_daily_meanlog_sdlog'))
        ),
        'linear' = c(
          s('lnK600_lnQ_intercept ~ ', f('normal', mu='lnK600_lnQ_intercept_mu', sigma='lnK600_lnQ_intercept_sigma')),
          s('lnK600_lnQ_slope ~ ', f('normal', mu='lnK600_lnQ_slope_mu', sigma='lnK600_lnQ_slope_sigma'))
        ),
        'binned' = c(
          s('lnK600_lnQ_nodes ~ ', f('normal', mu='K600_lnQ_nodes_meanlog', sigma='K600_lnQ_nodes_sdlog')),
          p('for(k in 2:b) {'),
          s('  lnK600_lnQ_nodes[k] ~ ', f('normal', mu='lnK600_lnQ_nodes[k-1]', sigma='K600_lnQ_nodediffs_sdlog')),
          p('}')
        )
      ),
      if(pool_K600_sd == 'fitted') switch(
        pool_K600_type,
        normal=s('K600_daily_sdlog_scaled ~ ', f('halfnormal', sigma='1')),
        linear=, binned=s('K600_daily_sigma_scaled ~ ', f('halfnormal', sigma='1'))
      )
    ),
    
    '}',
    
    #### generated quantities ####
    'generated quantities {',
    
    chunk(
      if(err_obs_iid) 'vector[d] err_obs_iid[n];',
      if(err_proc_iid) 'vector[d] err_proc_iid[n-1];',
      if(err_proc_GPP) c(
        'vector[d] GPP_inst_partial[n];',
        'vector[d] err_proc_GPP[n];',
        'int n_light_day; // temporary',
        'vector[n] GPP_inst_day; // temporary',
        'vector[n] GPP_inst_diff_day; // temporary',
        'vector[d] GPP_pseudo_R2;'
      ),
      'vector[d] GPP;',
      'vector[d] ER;',
      if(err_obs_iid) c(
        'vector[n] DO_obs_vec; // temporary',
        'vector[n] DO_mod_vec; // temporary'
      ),
      'vector[d] DO_R2;',
      '',
      if(err_obs_iid || err_proc_iid) c(
        if(err_obs_iid) c(
          'for(i in 1:n) {',
          indent(
            s('err_obs_iid[i] = DO_mod[i] - DO_obs[i]')
          ),
          '}'
        ),
        if(err_proc_iid) c(
          # err_proc_iid[t] describes errors in estimates of GPP_inst[t], 
          # ER_inst[t], etc. (and also GPP_inst[t+1], etc. if trapezoid).
          # Process error at time t is reflected in DO_mod_partial[t+1]
          'for(i in 2:n) {',
          indent(
            s('err_proc_iid[i-1] = (DO_mod_partial[i] - ', if(!err_obs_iid) 'DO_obs[i]' else 'DO_mod[i]', 
              ') .* (err_proc_iid_sigma ./ DO_mod_partial_sigma[i])')
          ),
          '}'
        )
      ),
      
      if(err_proc_GPP) c(
        'for(i in 1:n) {',
        indent(
          s('GPP_inst_partial[i] = GPP_daily .* light_mult_GPP[i]'),
          s('err_proc_GPP[i] = GPP_inst[i] - GPP_inst_partial[i]') 
        ),
        '}'
      ),
      
      if(err_proc_GPP) c(
        s('GPP_inst_day = rep_vector(0, n)'),
        s('GPP_inst_diff_day = rep_vector(0, n)')
      ),
      'for(j in 1:d) {',
      indent(
        s('GPP[j] = sum(GPP_inst[1:n24,j]) / n24'),
        s('ER[j] = sum(ER_inst[1:n24,j]) / n24'),
        if(err_obs_iid) c(
          p(''),
          p('// Compute R2 for DO observations relative to the modeled, process-error-corrected state (DO_mod)'),
          p('for(i in 1:n) {'),
          indent(
            s('DO_mod_vec[i] = DO_mod[i,j]'),
            s('DO_obs_vec[i] = DO_obs[i,j]')),
          p('}'),
          s('DO_R2[j] = 1 - ',
            'sum((DO_mod_vec - DO_obs_vec) .* (DO_mod_vec - DO_obs_vec)) / ', # sum((y_hat - y)^2)
            'sum((DO_obs_vec - mean(DO_obs_vec)) .* (DO_obs_vec - mean(DO_obs_vec)))') # sum((y - y_bar)^2)
        ) else c(
          p(''),
          p('// R2 for DO observations is always 1 for process-error-only models'),
          s('DO_R2[j] = 1')
        ),
        # compute GPP_pseudo_R2
        if(err_proc_GPP) c(
          p(''),
          p('// Compute GPP_pseudo_R2 (because model has GPP process error)'),
          s('n_light_day = 0'),
          p('for(i in 1:n) {'),
          indent(
            # only store and use those values for which light_mult_GPP[i] > 0
            p('if(light_mult_GPP[i,j] > 0) {'),
            indent(
              s('n_light_day += 1'),
              s('GPP_inst_day[n_light_day] = GPP_inst[i,j]'),
              s('GPP_inst_diff_day[n_light_day] = GPP_inst[i,j] - GPP_inst_partial[i,j]')
            ),
            p('}')
          ),
          p('}'),
          s('GPP_pseudo_R2[j] = 1 - variance(GPP_inst_diff_day[1:n_light_day]) / variance(GPP_inst_day[1:n_light_day])')
        )
      ),
      '}'
    ),
    
    '}'
  )
  #### <end model definition> ####
  
  writeLines(model_text, con=paste0('inst/models/deprecated/', model_name), sep="\n")
  
}

#' Generate MCMC code files with all of the desired combinations of features
#' 
#' This function gets run on package build and creates every model within the 
#' set of factorial combinations of arguments to mm_generate_mcmc_file, with the
#' exception of the one pair of incompatible arguments (err_obs_iid=F &&
#' deficit_src='DO_mod')
#' 
#' @include mm_name.R
#' @include mm_parse_name.R
#' @include mm_valid_names.R
#' @include mm_validate_name.R
#' @keywords internal
mm_generate_mcmc_files_deprecated <- function() {
  opts <- expand.grid(
    pool_K600=c('none',
                'normal','normal_sdzero','normal_sdfixed',
                'linear','linear_sdzero','linear_sdfixed',
                'binned','binned_sdzero','binned_sdfixed'),
    err_obs_iid=c(TRUE, FALSE),
    err_proc_acor=FALSE,
    err_proc_iid=c(FALSE, TRUE),
    err_proc_GPP=c(FALSE, TRUE),
    ode_method=c('trapezoid','euler'),
    GPP_fun=c('linlight','satlight'),
    ER_fun='constant',
    deficit_src=c('DO_mod','DO_obs'),
    engine='stan',
    stringsAsFactors=FALSE)
  attr(opts, 'out.attrs') <- NULL
  
  # need at least 1 kind of error, and GPP process error doesn't mix with light-saturating GPP
  incompatible <- (!opts$err_obs_iid & !opts$err_proc_acor & !opts$err_proc_iid & !opts$err_proc_GPP)  |
    (opts$err_proc_GPP & (opts$GPP_fun != 'linlight'))
  opts <- opts[!incompatible, ]
  
  for(i in 1:nrow(opts)) {
    do.call(mm_generate_mcmc_file_deprecated, opts[i,])
  }
}
mm_generate_mcmc_files_deprecated()
