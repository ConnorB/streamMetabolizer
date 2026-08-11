# Predict metabolism from a fitted model

A function in the `metab_model_interface`. Returns estimates of GPP, ER,
and K600.

## Usage

``` r
predict_metab(
  metab_model,
  date_start = NA,
  date_end = NA,
  day_start = get_specs(metab_model)$day_start,
  day_end = min(day_start + 24, get_specs(metab_model)$day_end),
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
)

# S3 method for class 'metab_bayes'
predict_metab(
  metab_model,
  date_start = NA,
  date_end = NA,
  ...,
  attach.units = deprecated()
)

# S3 method for class 'metab_model'
predict_metab(
  metab_model,
  date_start = NA,
  date_end = NA,
  day_start = get_specs(metab_model)$day_start,
  day_end = min(day_start + 24, get_specs(metab_model)$day_end),
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
)
```

## Arguments

- metab_model:

  A metabolism model that implements the `metab_model_interface`.

- date_start:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The first date
  (inclusive) for which to report metabolism predictions. If `NA`, no
  filtering is done.

- date_end:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The last date
  (inclusive) for which to report metabolism predictions. If `NA`, no
  filtering is done.

- day_start:

  Start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, `day_start = -1.5`
  indicates that data describing 2006-06-26 begin at 2006-06-25 22:30,
  or at the first observation time that occurs after that time if
  day_start doesn't fall exactly on an observation time. For daily
  metabolism predictions, `day_end - day_start` should probably equal 24
  so that each day's estimate is representative of a 24-hour period.

- day_end:

  End time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, `day_end = 30` indicates
  that data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00.

- ...:

  Other arguments passed to class-specific implementations of
  `predict_metab()`.

- attach.units:

  Deprecated. A logical indicating whether to attach units to the
  output.

- use_saved:

  A logical. Is it OK to use predictions that were saved with the model?

## Value

A data frame with one row per date and columns that include:

- `GPP`: Gross primary production, which is positive when realistic, in
  gO₂ m⁻² d⁻¹.

- `ER`: Ecosystem respiration, which is negative when realistic, in gO₂
  m⁻² d⁻¹.

- `K600`: The reaeration rate, in d⁻¹.

## Methods (by class)

- `predict_metab(metab_bayes)`: Pulls daily metabolism estimates out of
  the Stan model results; looks for `GPP` or `GPP_daily` and for `ER` or
  `ER_daily` among the `params_out` (see
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)),
  which means you can save just one (or both) of those sets of daily
  parameters when running the Stan model. Saving fewer parameters can
  help models run faster and use less RAM.

- `predict_metab(metab_model)`: This implementation is shared by many
  model types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md)

## Examples

``` r
dat <- data_metab("3", day_start = 12, day_end = 36)
mm <- metab_night(specs(mm_name("night")), data = dat)
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-18     0 NA        NA        -2.12    -2.35    -1.89 "      … ""      
#> 2 2012-09-19     0 NA        NA        -2.93    -3.23    -2.62 "      … ""      
#> 3 2012-09-20     0 NA        NA        -2.13    -2.31    -1.95 "      … ""      
#> # ℹ 1 more variable: errors <chr>
predict_metab(mm, date_start = get_fit(mm)$date[2])
#> # A tibble: 2 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-19     0 NA        NA        -2.93    -3.23    -2.62 "      … ""      
#> 2 2012-09-20     0 NA        NA        -2.13    -2.31    -1.95 "      … ""      
#> # ℹ 1 more variable: errors <chr>
```
