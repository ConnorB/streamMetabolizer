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

  mean solar time, as required for input to metabolism models. See
  [`mm_data()`](https://connorb.github.io/streamMetabolizer/reference/mm_data.md)
  and
  [`calc_solar_time()`](https://connorb.github.io/streamMetabolizer/reference/calc_solar_time.md).

- latitude:

  numeric value or vector indicating the site latitude in decimal
  degrees (never radians or deg-min-sec, no matter what `format` is)
  between -90 (South Pole) and 90 (North Pole).

- longitude:

  numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East), describing location
  of the site

- max.PAR:

  numeric or unitted_numeric: the PAR (umol m^-2 s^-1) that each day
  should reach at peak light

- attach.units:

  (deprecated, effectively FALSE in future) logical. Should the returned
  vector be a unitted object?

## Examples

``` r
solar.time <- lubridate::force_tz(as.POSIXct('2016-09-27 12:00'), 'UTC')
calc_light(solar.time, 40, -120)
#> [1] 1720.981
```
