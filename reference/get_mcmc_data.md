# Extract any MCMC data list(s) that were stored with the model

A function specific to metab_bayes models. Returns data as formatted to
run through the MCMC process or, for nopool models, a list of data
lists. These lists are not saved by default; see `keep_mcmc_data`
argument to
[`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
for options.

## Usage

``` r
get_mcmc_data(metab_model)

# S3 method for class 'metab_bayes'
get_mcmc_data(metab_model)
```

## Arguments

- metab_model:

  A Bayesian metabolism model (metab_bayes) from which to return the
  data list that was passed to the MCMC.

## Value

The MCMC data list.

## Methods (by class)

- `get_mcmc_data(metab_bayes)`: Retrieve any MCMC data list(s) that were
  saved with a metab_bayes model

## Examples

``` r
mm <- metab_model("metab_bayes", mcmc_data = list(n = 10))
get_mcmc_data(mm)
#> $n
#> [1] 10
#> 
```
