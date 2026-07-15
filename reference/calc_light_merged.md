# Merge modeled and observed PAR into a time series

Merge two time series (one observed, one modeled) of photosynthetically
active radiation (PAR) for a series of date-times. You can also think
about this as a way to smoothly interpolate a time series of
observations.

## Usage

``` r
calc_light_merged(
  PAR.obs = mm_data(solar.time, light),
  solar.time,
  latitude,
  longitude,
  max.PAR = NA,
  max.gap = as.difftime(3, units = "hours"),
  attach.units = deprecated()
)
```

## Arguments

- PAR.obs:

  A two-column data frame or tibble with columns `solar.time` and
  `light`, as in the argument default, containing the full time series
  of observed light (should be at a lower temporal resolution than
  `PAR.mod`).

- solar.time:

  A vector of mean solar times for which the light should be modeled and
  merged with the values in PAR.obs.

- latitude:

  Numeric value or vector indicating the site latitude in decimal
  degrees (never radians or deg-min-sec, no matter what `format` is)
  between -90 (South Pole) and 90 (North Pole).

- longitude:

  Numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East), describing location
  of the site.

- max.PAR:

  The maximum PAR, as in calc_light. if NA, this function does its best
  to guess a max.PAR that will make modeled light pretty similar to
  cloud-free days of observed light.

- max.gap:

  Difftime or NA. If difftime, the maximum gap between a light
  observation and a time point in solar.time, beyond which no value will
  be given for light at that solar.time. If NA, all values will be
  modeled, even if they are many days away from a light observation.

- attach.units:

  Deprecated. A logical. Should the returned vector be a unitted object?

## Value

A data frame with `solar.time` and merged `light` columns.

## Examples

``` r
if (FALSE) { # interactive()
library(dplyr)
library(ggplot2)
timebounds <- as.POSIXct(c('2008-03-12 00:00', '2008-03-12 23:59'), tz='UTC')
coords <- list(lat=32.4, lon=-96.5)
PAR.obs <- tibble::tibble(
  solar.time=seq(timebounds[1], timebounds[2], by=as.difftime(3, units='hours')),
  light=c(0, 0, 85.9, 1160.5, 1539.0, 933.9, 0, 0)
)
PAR.mod <- tibble::tibble(
  solar.time=seq(timebounds[1], timebounds[2], by=as.difftime(0.25, units='hours')),
  light=calc_light(solar.time, latitude=coords$lat, longitude=coords$lon)
)
PAR.merged <- calc_light_merged(PAR.obs, PAR.mod$solar.time,
  latitude=coords$lat, longitude=coords$lon, max.gap=as.difftime(20, units='hours'))
ggplot(bind_rows(mutate(PAR.obs, type='obs'), mutate(PAR.mod, type='mod'),
                 mutate(PAR.merged, type='merged')) |>
       mutate(type=ordered(type, levels=c('obs','mod','merged'))),
  aes(x=solar.time, y=light, color=type)) + geom_line() + geom_point() + theme_bw()
}
```
