## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=80)
library(streamMetabolizer)

## -----------------------------------------------------------------------------
mm_name(type='bayes') # the default Bayesian model
mm_name(type='bayes', pool_K600='normal') # a Bayesian model with simple pooling of K600 to a shared mean
mm_name(type='mle') # the default MLE model
mm_name(type='mle', ode_method='euler') # an MLE model with a simpler ode_method


## -----------------------------------------------------------------------------
mm_parse_name(c('b_np_oipi_tr_plrckm.stan', 'm_np_pi_tr_psrqkm.nlm'))

## -----------------------------------------------------------------------------
valid_names <- mm_valid_names(type=c('bayes','mle','night'))
length(valid_names)
c(valid_names[seq(1,length(valid_names),length.out=20)], '...')

