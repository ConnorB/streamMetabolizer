# Split and label data into \>=24-hr days for fitting daily metabolism

Splits up to two data frames or tibbles, `data` and `data_daily`, into
date-specific chunks. These are passed to model_fun. If `day_tests` is
not empty, those validity checks are run and the results are also passed
to model_fun (in `validity`). The results of `model_fun` (which must be
data frames or tibbles) are modified to include the date as the first
column, then row-bound together into a single object containing results
from all days. Tibble inputs and outputs retain their tibble class.

## Usage

``` r
mm_model_by_ply(
  model_fun,
  data,
  data_daily = NULL,
  day_start,
  day_end,
  day_tests = c("full_day", "even_timesteps", "complete_data", "pos_discharge",
    "pos_depth"),
  required_timestep = NA,
  timestep_days = TRUE,
  ...
)
```

## Arguments

- model_fun:

  the function to apply to each data ply. This function should accept
  the arguments `c(data, data_daily, ..., day_start, day_end, ply_date)`
  where `data_daily` is `NULL` when the `data_daily` argument to
  `mm_model_by_ply` is missing or `NULL`

- data:

  required. A data frame or tibble to split into chunks by date, where a
  'date' begins on the hour day_start and ends at the hour day_end. The
  solar.time column must be present.

- data_daily:

  optional. A data frame or tibble containing inputs with a daily
  timestep, each row of which will be passed to the corresponding date
  chunk from `data`. The date column must be present.

- day_start:

  start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time. For metabolism models
  working with single days of input data, it is conventional/useful to
  begin the day the evening before, e.g., -1.5, and to end just before
  the next sunrise, e.g., 30. For multiple consecutive days, it may make
  the most sense to start just before sunrise (e.g., 4) and to end 24
  hours later. For nighttime regression, the date assigned to a chunk of
  data should be the date whose evening contains the data. The default
  is therefore 12 to 36 for metab_night, of which the times of darkness
  will be used.

- day_end:

  end time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00. See day_start for recommended start
  and end times.

- day_tests:

  list of tests to conduct to determine whether each date worth of data
  is valid for modeling. The results of these tests will be combined
  with the result of the test implied if `required_timestep` is numeric
  and then will be passed to `model_fun` as the `ply_validity` argument
  to that function.

- required_timestep:

  NA or numeric (length 1). If numeric, the timestep length in days that
  a date must have to pass the validity check (to within a tolerance of
  0.2% of the value of `required_timestep`). The result of this test
  will be combined with the results of the tests listed in `day_tests`
  and reported to `model_fun` as the `ply_validity` argument to that
  function.

- timestep_days:

  TRUE if you would like the mean timestep length to be calculated for
  each data ply and passed to `model_fun` as the `timestep_days`
  argument to that function. Alternatively, this may be numeric as a
  specifically expected timestep length in days; for example, a 1-hour
  timestep is 1/24 is 0.0416667.

- ...:

  other args to be passed through mm_model_by_ply to model_fun

## Value

A data frame or tibble of model results. The class returned by
`model_fun` is preserved.

## Examples

``` r
dat <- data_metab('10')
mm_model_by_ply(mm_model_by_ply_prototype, data=dat, day_start=2, day_end=28)$date
#>  [1] "2012-09-18" "2012-09-19" "2012-09-20" "2012-09-21" "2012-09-22"
#>  [6] "2012-09-23" "2012-09-24" "2012-09-25" "2012-09-26" "2012-09-27"
mm_model_by_ply(function(...) { data.frame(text='hi')},
  data=dat, day_start=2, day_end=28)
#>          date text
#> 1  2012-09-18   hi
#> 2  2012-09-19   hi
#> 3  2012-09-20   hi
#> 4  2012-09-21   hi
#> 5  2012-09-22   hi
#> 6  2012-09-23   hi
#> 7  2012-09-24   hi
#> 8  2012-09-25   hi
#> 9  2012-09-26   hi
#> 10 2012-09-27   hi
```
