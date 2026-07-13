# Make daily metabolism estimates from input parameters using a hierarchical approach.

Called from metab_bayes().

## Usage

``` r
bayes_allply(data_all, data_daily_all, removed, specs)
```

## Arguments

- data_all:

  data.frame of the form
  `mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light)` and
  containing data for just one estimation-day (this may be \>24 hours
  but only yields estimates for one 24-hour period)

- data_daily_all:

  data.frame of daily priors, if appropriate to the given model_path

- removed:

  data.frame of dates that were removed and why

- specs:

  a list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  lists the necessary parameters, describes them in detail, and gives
  default values.

## Value

data.frame of estimates and MCMC model diagnostics
