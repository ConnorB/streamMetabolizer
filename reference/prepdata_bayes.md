# Prepare data for passing to Stan

This function accepts pre-validated data (though more problems may be
discovered here). It prepares the data needed to run a Bayesian MCMC
method to estimate GPP, ER, and K600.

## Usage

``` r
prepdata_bayes(data, data_daily, ply_date = NA, specs)
```

## Arguments

- data:

  A data frame or tibble of input data at the temporal resolution of raw
  observations (unit-value). Columns must have the same names, units,
  and format as the default. The solar.time column must also have a
  timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  A data frame or tibble containing inputs with a daily timestep. See
  the **'Formatting `data_daily`'** section below for a full
  description.

- ply_date:

  The modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- specs:

  A list of model specifications and parameters for a model. Although
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

List of data for input to runstan_bayes.
