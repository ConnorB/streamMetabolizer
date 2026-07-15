# Convert DateTime from UTC to local solar time

Convert DateTime from UTC to local solar time, which may be either
apparent solar (perfect match between noon and solar zenith) or mean
solar (exactly 24 hours between solar noons).

## Usage

``` r
convert_UTC_to_solartime(
  date.time,
  longitude,
  time.type = c("apparent solar", "mean solar")
)
```

## Arguments

- date.time:

  Date-time values in POSIXct format and UTC timezone.

- longitude:

  Numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East).

- time.type:

  Character. "apparent solar", i.e. true solar time, is noon when the
  sun is at its zenith. "mean solar" approximates apparent solar time
  but with noons exactly 24 hours apart. Elsewhere in this package,
  variables named "solar.time" are mean solar time, whereas
  "app.solar.time" is apparent solar and "any.solar.time" is either.

## Value

A POSIXct object that says it's in tz="UTC" but that's actually in solar
time, with noon being very close to solar noon.

## References

Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
Influence of topographic complexity on solar insolation estimates for
the Colorado River, Grand Canyon, AZ. Ecological Modelling.

## Examples

``` r
utc <- as.POSIXct("2024-06-01 18:00:00", tz = "UTC")
convert_UTC_to_solartime(utc, longitude = -105)
#> [1] "2024-06-01 11:03:26 UTC"
```
