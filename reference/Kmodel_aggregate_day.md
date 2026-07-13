# Aggregate unit values to daily values

For use in predicting K

## Usage

``` r
Kmodel_aggregate_day(
  data_ply,
  ply_validity,
  ...,
  columns = c("discharge", "velocity")
)
```

## Arguments

- data_ply:

  a data.frame containing all relevant, validated modeling data for a
  single ply of data. (1 ply ~= 1 date, although the day length has been
  specified by day_start and day_end and may not be exactly 24 hours)

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

- columns:

  character vector of names of columns to aggregate
