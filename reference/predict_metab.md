# Predict metabolism from a fitted model.

A function in the metab_model_interface. Returns predictions (estimates)
of GPP, ER, and K600.

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

  A metabolism model, implementing the metab_model_interface, to use in
  predicting metabolism

- date_start:

  Date or a class convertible with as.Date. The first date (inclusive)
  for which to report metabolism predictions. If NA, no filtering is
  done.

- date_end:

  Date or a class convertible with as.Date. The last date (inclusive)
  for which to report metabolism predictions. If NA, no filtering is
  done.

- day_start:

  start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time. For daily metabolism
  predictions, day_end - day_start should probably equal 24 so that each
  day's estimate is representative of a 24-hour period.

- day_end:

  end time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00. For daily metabolism predictions,
  day_end - day_start should probably equal 24 so that each day's
  estimate is representative of a 24-hour period.

- ...:

  Other arguments passed to class-specific implementations of
  `predict_metab`

- attach.units:

  (deprecated, effectively FALSE in future) logical. Should units be
  attached to the output?

- use_saved:

  logical. Is it OK to use predictions that were saved with the model?

## Value

A data.frame of daily metabolism estimates. Columns include:

- GPP:

  numeric estimate of Gross Primary Production, positive when realistic,
  \\g O_2 m^{-2} d^{-1}\\

- ER:

  numeric estimate of Ecosystem Respiration, negative when realistic,
  \\g O_2 m^{-2} d^{-1}\\

- K600:

  numeric estimate of the reaeration rate \\d^{-1}\\

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
dat <- data_metab('3', day_start=12, day_end=36)
mm <- metab_night(specs(mm_name('night')), data=dat)
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-18     0 NA        NA        -2.12    -2.35    -1.89 "      … ""      
#> 2 2012-09-19     0 NA        NA        -2.93    -3.23    -2.62 "      … ""      
#> 3 2012-09-20     0 NA        NA        -2.13    -2.31    -1.95 "      … ""      
#> # ℹ 1 more variable: errors <chr>
predict_metab(mm, date_start=get_fit(mm)$date[2])
#> # A tibble: 2 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-19     0 NA        NA        -2.93    -3.23    -2.62 "      … ""      
#> 2 2012-09-20     0 NA        NA        -2.13    -2.31    -1.95 "      … ""      
#> # ℹ 1 more variable: errors <chr>
```
