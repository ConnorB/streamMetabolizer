# Calculate modeled light from solar.time

Calculate photosynthetically active radiation (PAR) for a series of
date-times and site coordinates.

## Usage

``` r
calc_light(
  solar.time,
  latitude,
  longitude,
  max.PAR = 2326,
  attach.units = deprecated()
)
```

## Arguments

- solar.time:

  Mean solar time, as required for input to metabolism models. See
  [`mm_data()`](https://connorb.github.io/streamMetabolizer/dev/reference/mm_data.md)
  and
  [`calc_solar_time()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_solar_time.md).

- latitude:

  Numeric value or vector indicating the site latitude in decimal
  degrees (never radians or deg-min-sec, no matter what `format` is)
  between -90 (South Pole) and 90 (North Pole).

- longitude:

  Numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East), describing location
  of the site.

- max.PAR:

  Numeric or unitted_numeric: the PAR (umol m^-2 s^-1) that each day
  should reach at peak light.

- attach.units:

  Deprecated. A logical. Should the returned vector be a unitted object?

## Value

A numeric vector of modeled PAR values in µmol m⁻² s⁻¹.

## Examples

``` r
solar.time <- lubridate::force_tz(as.POSIXct('2016-09-27 12:00'), 'UTC')
calc_light(solar.time, 40, -120)
#> [1] 1720.981
```
