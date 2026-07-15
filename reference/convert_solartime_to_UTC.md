# Convert DateTime from local solar time to UTC

Convert DateTime to UTC from local solar time, which may be either
apparent solar (perfect match between noon and solar zenith) or mean
solar (exactly 24 hours between solar noons).

## Usage

``` r
convert_solartime_to_UTC(
  any.solar.time,
  longitude,
  time.type = c("apparent solar", "mean solar")
)
```

## Arguments

- any.solar.time:

  Either apparent or mean solar time (specified by time.type); date-time
  values in POSIXct format. Timezone must be UTC.

- longitude:

  Numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East), describing location
  of the site.

- time.type:

  Character indicating whether any.solar.time values are in apparent or
  mean solar time. "apparent solar", i.e. true solar time, is noon when
  the sun is at its zenith. "mean solar" approximates apparent solar
  time but with noons exactly 24 hours apart.

## Value

A POSIXct object in UTC.

## References

Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
Influence of topographic complexity on solar insolation estimates for
the Colorado River, Grand Canyon, AZ. Ecological Modelling.

## Examples

``` r
solar <- as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
convert_solartime_to_UTC(solar, longitude = -105)
#> [1] "2024-06-01 18:56:31 UTC"
```
