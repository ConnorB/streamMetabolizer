# Predict DO from a fitted model.

A function in the metab_model_interface. Returns predictions of
dissolved oxygen.

## Usage

``` r
predict_DO(
  metab_model,
  date_start = NA,
  date_end = NA,
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
)

# S3 method for class 'metab_Kmodel'
predict_DO(metab_model, date_start = NA, date_end = NA, ..., use_saved = TRUE)

# S3 method for class 'metab_model'
predict_DO(
  metab_model,
  date_start = NA,
  date_end = NA,
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
)

# S3 method for class 'metab_night'
predict_DO(metab_model, date_start = NA, date_end = NA, ..., use_saved = TRUE)

# S3 method for class 'metab_sim'
predict_DO(metab_model, date_start = NA, date_end = NA, ...)
```

## Arguments

- metab_model:

  A metabolism model, implementing the metab_model_interface, to use in
  predicting metabolism

- date_start:

  Date or a class convertible with as.Date. The first date (inclusive)
  for which to report DO predictions. If NA, no filtering is done.

- date_end:

  Date or a class convertible with as.Date. The last date (inclusive)
  for which to report DO predictions. If NA, no filtering is done.

- ...:

  Other arguments passed to class-specific implementations of
  `predict_DO`

- attach.units:

  (deprecated, effectively FALSE in future) logical. Should units be
  attached to the output?

- use_saved:

  logical. Is it OK to use predictions that were saved with the model?

## Value

A data.frame of dissolved oxygen predictions at the temporal resolution
of the input data

## Methods (by class)

- `predict_DO(metab_Kmodel)`: Throws an error because models of type
  'Kmodel' can't predict DO. `metab_Kmodel` predicts K at daily
  timesteps and usually knows nothing about GPP or ER. So it's not
  possible to predict DO from this model. Try passing the output to
  metab_mle and THEN predicting DO.

- `predict_DO(metab_model)`: This implementation is shared by many model
  types

- `predict_DO(metab_night)`: Generate nighttime dissolved oxygen
  predictions from a nighttime regression model. `metab_night` only fits
  ER and K, and only for the darkness hours, so predictions are only
  generated for those hours.

- `predict_DO(metab_sim)`: Simulate values for DO.obs (with process and
  observation error), DO.mod (with process error only), and DO.pure
  (with no error). The errors are randomly generated on every new call
  to predict_DO.

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
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)

## Examples

``` r
dat <- data_metab('3', day_start=12, day_end=36)
mm <- metab_night(specs(mm_name('night')), data=dat)
preds <- predict_DO(mm, date_start=get_fit(mm)$date[3])
head(preds)
#>            date          solar.time DO.obs   DO.sat depth temp.water light
#> 6429 2012-09-20 2012-09-20 17:55:58   8.07 7.769228  0.16       9.76     0
#> 6430 2012-09-20 2012-09-20 18:00:58   8.06 7.782325  0.16       9.69     0
#> 6431 2012-09-20 2012-09-20 18:05:58   8.00 7.797340  0.16       9.61     0
#> 6432 2012-09-20 2012-09-20 18:10:58   7.94 7.810518  0.16       9.54     0
#> 6433 2012-09-20 2012-09-20 18:15:58   7.91 7.825624  0.16       9.46     0
#> 6434 2012-09-20 2012-09-20 18:20:58   7.87 7.840781  0.16       9.38     0
#>        DO.mod
#> 6429 8.070000
#> 6430 7.998760
#> 6431 7.934597
#> 6432 7.877057
#> 6433 7.825408
#> 6434 7.779299
```
