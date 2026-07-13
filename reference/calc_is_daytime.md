# Determines if specified datetime is during the daytime Returns T/F indicating whether a datetime occurs during the daytime (sunlight hours)

**\[deprecated\]**

`calc_is_daytime()` is deprecated.

## Usage

``` r
calc_is_daytime(datetimes, lat)
```

## Arguments

- datetimes:

  Vector of dates as `POSIXct` or `POSIXlt` (see
  [`DateTimeClasses()`](https://rdrr.io/r/base/DateTimeClasses.html))
  format, but in SOLAR time

- lat:

  Single latitude value of site. South should be negative, north
  positive

## Value

a boolean vector of same length as `datetimes`

## See also

[calc_sun_rise_set](https://connorb.github.io/streamMetabolizer/reference/calc_sun_rise_set.md)

## Author

Luke A. Winslow

## Examples

``` r
if (FALSE) { # \dontrun{
# Warning: this function is deprecated.
calc_is_daytime(datetimes=as.POSIXct(paste('2013-03-31', c('1:00','11:00'))), lat=40.75)
} # }
```
