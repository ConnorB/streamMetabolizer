# Run Stan on a formatted data ply

Run Stan on a formatted data ply

## Usage

``` r
runstan_bayes(
  data_list,
  model_path,
  model_name,
  params_out,
  split_dates,
  keep_mcmc = FALSE,
  n_chains = 4,
  n_cores = 4,
  burnin_steps = 1000,
  saved_steps = 1000,
  thin_steps = 1,
  stan_engine = c("rstan", "cmdstanr"),
  verbose = FALSE,
  ...
)
```

## Arguments

- data_list:

  a formatted list of inputs to the Stan model

- model_path:

  the Stan model file to use, as a full file path

- model_name:

  the coded model name, as from mm_name, giving the model structure

- params_out:

  a character vector of parameters whose values in the MCMC runs should
  be recorded and summarized

- keep_mcmc:

  logical. If TRUE, the Stan output object will be saved. Be careful;
  these can be big, and a run with many models might overwhelm R's
  memory.

- n_chains:

  the number of chains to run

- n_cores:

  the number of cores to apply to this run

- burnin_steps:

  the number of steps per chain to run and ignore before starting to
  collect MCMC 'data'

- saved_steps:

  the number of MCMC steps per chain to save

- thin_steps:

  the number of steps to move before saving another step. 1 means save
  all steps.

- stan_engine:

  Character string specifying which Stan R interface to use. Either
  "rstan" or "cmdstanr". Defaults to "rstan" to preserve the original
  behavior of the streamMetabolizer package. CmdStanR requires both the
  R package and a configured CmdStan installation; see
  [`cmdstanr::install_cmdstan()`](https://mc-stan.org/cmdstanr/reference/install_cmdstan.html).

- verbose:

  logical. give status messages?

- ...:

  ignored arguments

## Details

Compiled Stan models are cached by interface/compiler version, platform,
and model content in the user cache directory. Set the
`streamMetabolizer.rstan_cache_dir` or
`streamMetabolizer.cmdstan_cache_dir` option to use a different
directory.
