# Convert UTC to local time

Convert time from UTC to local time, either standard or with daylight
savings. Recommended for post-analysis visualization only; most
functions in streamMetabolizer use times in UTC. If you know the
timezone code for your local site, use
[`lubridate::with_tz()`](https://lubridate.tidyverse.org/reference/with_tz.html)
instead.

## Usage

``` r
convert_UTC_to_localtime(
  date.time,
  latitude,
  longitude,
  time.type = c("standard local", "daylight local")
)
```

## Arguments

- date.time:

  POSIXct object the date and time in UTC.

- latitude:

  Numeric, in degrees, either positive and unitted ("degN" or "degS") or
  with sign indicating direction (positive = North).

- longitude:

  Numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East).

- time.type:

  Character. The type of time zone desired - either standard time
  without any daylight savings time or daylight time where daylight
  savings is on during the appropriate days.

## Value

A `POSIXct` vector in the requested local time zone.

## References

https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude

## Examples

``` r
utc <- as.POSIXct("2024-01-15 18:00:00", tz = "UTC")
convert_UTC_to_localtime(utc, latitude = 41.88, longitude = -87.63)
#> [1] "2024-01-15 12:00:00 -06"
```
