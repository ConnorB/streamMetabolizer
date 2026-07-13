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
