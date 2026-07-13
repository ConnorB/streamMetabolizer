# Return the log file(s) from a model run

If a log file was created during a model run, this function can retrieve
it.

## Usage

``` r
get_log(metab_model)

# S3 method for class 'metab_bayes'
get_log(metab_model)
```

## Arguments

- metab_model:

  A Bayesian metabolism model (metab_bayes) from which to return the log
  file, if available

## Value

The MCMC log file(s) lines

## Methods (by class)

- `get_log(metab_bayes)`: If a log file was created during the Bayesian
  MCMC run, metab_bayes() attempted to capture it. Retrieve what was
  captured with this function.
