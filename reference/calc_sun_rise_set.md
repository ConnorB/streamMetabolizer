# Calculates the time of sunrise and sunset

**\[deprecated\]**

`calc_sun_rise_set()` is deprecated.

Calculates the time of sunrise and sunset based on latitude and date.

## Usage

``` r
calc_sun_rise_set(date, latitude)
```

## Arguments

- date:

  Vector of dates in `Date` format.

- latitude:

  Single latitude value of site. South should be negative, North
  positive

## Value

data.frame of sunrise and sunset (apparent solar time, nominally UTC)

## See also

[calc_is_daytime](https://connorb.github.io/streamMetabolizer/reference/calc_is_daytime.md)

## Examples

``` r
calc_sun_rise_set(latitude=40.75,
  date=as.POSIXlt(c('2013-03-31', '2017-07-01')))
#> Warning: `calc_sun_rise_set()` was deprecated in streamMetabolizer 0.13.0.
#>               sunrise              sunset
#> 1 2013-03-31 05:46:41 2013-03-31 18:13:18
#> 2 2017-07-01 04:33:24 2017-07-01 19:26:35
```
