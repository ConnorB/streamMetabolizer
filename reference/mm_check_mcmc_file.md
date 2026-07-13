# Use an engine-specific function to check the model syntax

Use an engine-specific function to check the model syntax

## Usage

``` r
mm_check_mcmc_file(model_file, stan_engine = c("rstan", "cmdstanr"))
```

## Arguments

- model_file:

  the file path of the model file to check; the extension will be used
  to determine which engine to use for checking.

- stan_engine:

  Character string specifying whether to check with RStan or CmdStanR.
