# Validate one day of data, returning a vector of error strings if needed

Provides ability to skip a poorly-formatted day for calculating
metabolism, without breaking the whole loop. Rather than producing
errors, quietly collects problems/errors as a list of strings for the
calling function to handle.

## Usage

``` r
mm_is_valid_day(
  data_ply,
  day_start = 4,
  day_end = 27.99,
  day_tests = c("full_day", "even_timesteps", "complete_data", "pos_discharge",
    "pos_depth"),
  required_timestep = NA,
  ply_date = as.Date(format(data_ply[max(1, nrow(data_ply)/2), "solar.time"],
    "%Y-%m-%d")),
  timestep_days = NA
)
```

## Arguments

- data_ply:

  a data.frame containing all relevant, validated modeling data for a
  single ply of data. (1 ply ~= 1 date, although the day length has been
  specified by day_start and day_end and may not be exactly 24 hours)

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

- day_tests:

  list of tests to conduct to determine whether each date worth of data
  is valid for modeling. `full_day`: Do the data span the full expected
  period (e.g., from 10:30pm on preceding day to 6am on following day)?
  `even_timesteps`: are all of the timesteps within the day the same
  length, to within a tolerance of 0.2% of the timestep length?
  `complete_data`: are all columns of input data available at every
  timestep? `pos_discharge`: is discharge greater than 0 at every
  timestep? `pos_depth`: is depth greater than 0 at every timestep? A
  further test is implied if `required_timestep` is a non-NA numeric.

- required_timestep:

  NA or numeric (length 1). If numeric, the timestep length in days that
  a date must have to pass the validity check (to within a tolerance of
  0.2% of the value of `required_timestep`)

- ply_date:

  the Date this data_ply is intended to match. May be NA

- timestep_days:

  the expected timestep length in fraction of a day; for example, a
  1-hour timestep is 1/24 is 0.0416667. This is calculated within the
  function if timestep_days is NA. May be supplied as an argument to (1)
  pre-calculate the value for efficiency, or (2) require a specific
  timestep.

## Value

character vector of errors if day is invalid, or TRUE if it's valid

## Details

Assumes that the data have already been validated as in
[`mm_validate_data()`](https://connorb.github.io/streamMetabolizer/reference/mm_validate_data.md)

## Examples

``` r
mm_is_valid_day(data_metab('1'))
#> [1] TRUE
mm_is_valid_day(data_metab('1', flaws='missing middle'))
#> [1] "uneven timesteps"
mm_is_valid_day(data_metab('1', flaws='missorted'))
#> [1] "uneven timesteps"
mm_is_valid_day(data_metab('1', flaws='duplicated'))
#> [1] "uneven timesteps"
mm_is_valid_day(data_metab('1', flaws=c('duplicated','missing end')))
#> [1] "data don't end when expected" "uneven timesteps"            
mm_is_valid_day(data_metab('3'))
#> [1] "data don't start when expected" "data don't end when expected"  
```
