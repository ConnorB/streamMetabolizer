# Helper to predict_metab.metab_model

Usually assigned to model_fun within mm_model_by_ply, called from there

## Usage

``` r
mm_predict_metab_1ply(
  data_ply,
  data_daily_ply,
  day_start,
  day_end,
  ply_date,
  ply_validity,
  ...,
  model_name
)
```

## Arguments

- data_ply:

  a data.frame containing all relevant, validated modeling data for a
  single ply of data. (1 ply ~= 1 date, although the day length has been
  specified by day_start and day_end and may not be exactly 24 hours)

- data_daily_ply:

  NULL or a data.frame containing inputs with a daily timestep.

- day_start:

  start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time.

- day_end:

  end time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00.

- ply_date:

  the modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- ply_validity:

  the output of `mm_is_valid_day` as applied to this data_ply for those
  tests specified in `day_tests`. Those tests will have been run before
  this function is called. The result is TRUE if the ply is entirely
  valid, or a character vector containing one or more error messages if
  any tests failed.

- ...:

  other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

- model_name:

  the coded model name that will determine the GPP_fun, ER_fun,
  deficit_src, and ode_method to use in prediction

## Value

a data.frame of predictions
