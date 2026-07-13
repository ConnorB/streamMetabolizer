# Calculates the average air pressure for a site

Estimates air pressure from air temperature and elevation

## Usage

``` r
calc_air_pressure(temp.air = 15, elevation = 762, attach.units = deprecated())
```

## Arguments

- temp.air:

  air temperature in degrees C. Default is 15 degC.

- elevation:

  the site elevation above sea level in m. Default is the rough mean
  elevation of the USA at 2500 ft (from
  http://www.infoplease.com/ipa/A0001792.html)

- attach.units:

  (deprecated, effectively FALSE in future) logical. Should the returned
  vector be a unitted object?

## Value

a numeric vector of barometric pressures in mb, with units attached if
requested.

## Examples

``` r
calc_air_pressure(15, 762)
#> [1] 925.7246
calc_air_pressure(15, 100)
#> [1] 1001.308
```
