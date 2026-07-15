# Override generic predict_metab for metab_Kmodel, which can't predict metab

metab_Kmodel predicts K (only) at daily timesteps and usually knows
nothing about GPP or ER. So it's not possible to predict metabolism from
this model. Try get_params() to retrieve the predicted values of
K600.daily.

## Usage

``` r
# S3 method for class 'metab_Kmodel'
predict_metab(
  metab_model,
  date_start = NA,
  date_end = NA,
  day_start = NA,
  day_end = NA,
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
  [`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md).

- attach.units:

  Deprecated. A logical indicating whether to attach units to the
  output.

- use_saved:

  A logical. Is it OK to use predictions that were saved with the model?

## Value

This method always throws an error because K models cannot predict
metabolism.

## Examples

``` r
try(predict_metab(metab_model("metab_Kmodel")))
#> Error in predict_metab(metab_model("metab_Kmodel")) : 
#>   <metab_Kmodel> objects cannot predict metabolism.
#> ℹ Use `get_params()` to retrieve predicted `K600.daily` values.
```
