# Determine whether datetimes occur during daylight

**\[deprecated\]**

`calc_is_daytime()` is deprecated.

## Usage

``` r
calc_is_daytime(datetimes, lat)
```

## Arguments

- datetimes:

  A `POSIXct` or `POSIXlt` vector in solar time. See
  [`DateTimeClasses()`](https://rdrr.io/r/base/DateTimeClasses.html).

- lat:

  A numeric scalar giving the site latitude. Use negative values south
  of the equator and positive values north of it.

## Value

A logical vector with the same length as `datetimes`.

## Details

Returns a logical vector indicating whether each datetime occurs during
daylight hours.

## See also

[`calc_sun_rise_set()`](https://connorb.github.io/streamMetabolizer/reference/calc_sun_rise_set.md)

## Author

Luke A. Winslow

## Examples

``` r
if (FALSE) { # interactive()
# Warning: this function is deprecated.
calc_is_daytime(
  datetimes = as.POSIXct(paste("2013-03-31", c("1:00", "11:00"))),
  lat = 40.75
)
}
```
