# Make daily metabolism estimates from input parameters

Called from metab_bayes().

## Usage

``` r
bayes_1ply(data_ply, data_daily_ply, ply_date, ply_validity, ..., specs)
```

## Arguments

- data_ply:

  A data frame or tibble containing all relevant, validated modeling
  data for a single ply of data. (1 ply ~= 1 date, although the day
  length has been specified by day_start and day_end and may not be
  exactly 24 hours).

- data_daily_ply:

  `NULL` or a data frame or tibble containing inputs with a daily
  timestep.

- ply_date:

  The modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- ply_validity:

  The output of `mm_is_valid_day` as applied to this data_ply for those
  tests specified in `day_tests`. Those tests will have been run before
  this function is called. The result is TRUE if the ply is entirely
  valid, or a character vector containing one or more error messages if
  any tests failed.

- ...:

  Other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

- specs:

  A list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md)
  lists the necessary parameters, describes them in detail, and gives
  default values.

## Value

Data.frame of estimates and MCMC model diagnostics.
