# Get a demo dataset for modeling metabolism

Get a formatted data.frame of inputs from which metabolism can be
modeled. These test data were provided by Bob Hall.

## Usage

``` r
data_metab(
  num_days = c("1", "3", "10"),
  res = c("5", "10", "15", "30"),
  flaws = c("missing middle", "missing start", "missing end", "missorted", "duplicated"),
  day_start = 4,
  day_end = 28,
  attach.units = deprecated()
)
```

## Arguments

- num_days:

  The number of days to include in the data. character format because
  only certain numbers of days are permitted (see defaults in Usage for
  the accepted options).

- res:

  Character specifying the desired resolution of the data in minutes
  (character; see defaults in Usage for the accepted options).

- flaws:

  Character specifying one or more flaws to include in the data, or
  empty ([`c()`](https://rdrr.io/r/base/c.html)) for no flaws. default
  is no flaws.

- day_start:

  Start time (inclusive) of a day's data in number of hours from the
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

  End time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00. See day_start for recommended start
  and end times.

- attach.units:

  Deprecated. A logical, default TRUE for backward compatibility. Should
  units be attached to the data.frame?

## Value

A data frame of example metabolism inputs.

## Examples

``` r
head(data_metab())
#> # A tibble: 6 × 6
#>   solar.time          DO.obs DO.sat depth temp.water light
#>   <dttm>               <dbl>  <dbl> <dbl>      <dbl> <dbl>
#> 1 2012-09-18 04:00:58   8.39   9.08  0.16       3.63     0
#> 2 2012-09-18 04:05:58   8.41   9.08  0.16       3.6      0
#> 3 2012-09-18 04:10:58   8.4    9.09  0.16       3.58     0
#> 4 2012-09-18 04:15:58   8.41   9.09  0.16       3.57     0
#> 5 2012-09-18 04:20:58   8.42   9.09  0.16       3.56     0
#> 6 2012-09-18 04:25:58   8.42   9.10  0.16       3.54     0
head(data_metab(res='30'))
#> # A tibble: 6 × 6
#>   solar.time          DO.obs DO.sat depth temp.water light
#>   <dttm>               <dbl>  <dbl> <dbl>      <dbl> <dbl>
#> 1 2012-09-18 04:05:58   8.41   9.08  0.16       3.6     0 
#> 2 2012-09-18 04:35:58   8.42   9.11  0.16       3.51    0 
#> 3 2012-09-18 05:05:58   8.45   9.13  0.16       3.42    0 
#> 4 2012-09-18 05:35:58   8.47   9.15  0.16       3.33    0 
#> 5 2012-09-18 06:05:58   8.55   9.17  0.16       3.26  134.
#> 6 2012-09-18 06:35:58   8.71   9.17  0.16       3.23  361.
```
