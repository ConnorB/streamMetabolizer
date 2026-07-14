# Make daily metabolism estimates from input parameters

Called from metab_mle().

## Usage

``` r
mle_1ply(
  data_ply,
  data_daily_ply,
  ply_date,
  ply_validity,
  timestep_days,
  ...,
  specs = specs("m_np_oi_tr_km.nlm")
)
```

## Arguments

- data_ply:

  a data frame or tibble containing all relevant, validated modeling
  data for a single ply of data. (1 ply ~= 1 date, although the day
  length has been specified by day_start and day_end and may not be
  exactly 24 hours)

- data_daily_ply:

  `NULL` or a data frame or tibble containing inputs with a daily
  timestep.

- ply_date:

  the modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- ply_validity:

  the output of `mm_is_valid_day` as applied to this data_ply for those
  tests specified in `day_tests`. Those tests will have been run before
  this function is called. The result is TRUE if the ply is entirely
  valid, or a character vector containing one or more error messages if
  any tests failed.

- timestep_days:

  numeric length of the mean timestep for this day, if requested by
  setting `timestep_days` to `TRUE` or a numeric value in the call to
  [`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/reference/mm_model_by_ply.md)

- ...:

  other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

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

data.frame of estimates and
[`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) model diagnostics
