# A prototype for the model_fun argument to mm_model_by_ply

This function does nothing but has the proper form for a model_fun
passed to
[`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/dev/reference/mm_model_by_ply.md)
Other functions to be used as model_fun may call inheritParams to use
the parameter definitions given here.

## Usage

``` r
mm_model_by_ply_prototype(
  data_ply = mm_data(NULL),
  data_daily_ply = mm_data(NULL),
  day_start = NA,
  day_end = NA,
  ply_date = NA,
  ply_validity = NA,
  timestep_days = NA,
  ...
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

- ply_validity:

  The output of `mm_is_valid_day` as applied to this data_ply for those
  tests specified in `day_tests`. Those tests will have been run before
  this function is called. The result is TRUE if the ply is entirely
  valid, or a character vector containing one or more error messages if
  any tests failed.

- timestep_days:

  Numeric length of the mean timestep for this day, if requested by
  setting `timestep_days` to `TRUE` or a numeric value in the call to
  [`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/dev/reference/mm_model_by_ply.md).

- ...:

  Other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

## Value

A one-row tibble summarizing the supplied data ply and arguments.

## Examples

``` r
mm_model_by_ply_prototype()
#> # A tibble: 1 × 9
#>   data_ply_start data_ply_end data_ply_nrow data_daily_ply_date day_start
#>   <lgl>          <lgl>                <dbl> <chr>               <lgl>    
#> 1 NA             NA                       0 ""                  NA       
#> # ℹ 4 more variables: day_end <lgl>, ply_date <lgl>, ply_validity <chr>,
#> #   timestep_days <lgl>
mm_model_by_ply_prototype(extra_arg=7:12)
#> # A tibble: 1 × 10
#>   data_ply_start data_ply_end data_ply_nrow data_daily_ply_date day_start
#>   <lgl>          <lgl>                <dbl> <chr>               <lgl>    
#> 1 NA             NA                       0 ""                  NA       
#> # ℹ 5 more variables: day_end <lgl>, ply_date <lgl>, ply_validity <chr>,
#> #   timestep_days <lgl>, extra_arg <int>
```
