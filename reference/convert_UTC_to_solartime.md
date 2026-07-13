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

  date-time values in POSIXct format and UTC timezone.

- longitude:

  numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East)

- time.type:

  character. "apparent solar", i.e. true solar time, is noon when the
  sun is at its zenith. "mean solar" approximates apparent solar time
  but with noons exactly 24 hours apart. Elsewhere in this package,
  variables named "solar.time" are mean solar time, whereas
  "app.solar.time" is apparent solar and "any.solar.time" is either.

## Value

a POSIXct object that says it's in tz="UTC" but that's actually in solar
time, with noon being very close to solar noon

## References

Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
Influence of topographic complexity on solar insolation estimates for
the Colorado River, Grand Canyon, AZ. Ecological Modelling.
