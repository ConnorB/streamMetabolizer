# Helper to predict_DO.metab_model

Usually assigned to model_fun within mm_model_by_ply, called from there

## Usage

``` r
metab_night_predict_1ply(
  data_ply,
  data_daily_ply,
  day_start,
  day_end,
  ply_date,
  timestep_days,
  ...,
  model_name
)
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

- day_start:

  Start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time.

- day_end:

  End time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00.

- ply_date:

  The modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- timestep_days:

  Numeric length of the mean timestep for this day, if requested by
  setting `timestep_days` to `TRUE` or a numeric value in the call to
  [`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/dev/reference/mm_model_by_ply.md).

- ...:

  Other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

- model_name:

  The coded model name that will determine the GPP_fun, ER_fun,
  deficit_src, and ode_method to use in prediction.

## Value

A data.frame of predictions.
