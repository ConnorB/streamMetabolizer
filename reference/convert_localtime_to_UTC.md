# Convert local time to UTC

Convert time from local time (either standard or with daylight savings)
to UTC.

## Usage

``` r
convert_localtime_to_UTC(local.time)
```

## Arguments

- local.time:

  POSIXct date+time of interest, already in local time as specified by
  the tz attribute.

## Value

A `POSIXct` vector in UTC.

## References

https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude

## Examples

``` r
local <- as.POSIXct("2024-01-15 12:00:00", tz = "America/Chicago")
convert_localtime_to_UTC(local)
#> [1] "2024-01-15 18:00:00 UTC"
```
