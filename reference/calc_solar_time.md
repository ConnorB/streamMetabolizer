# Calculate solar.time from local.time

Calculate the appropriate solar.time column for input to metab(). The
input must be POSIXct clock time and should have the correct timezone
information embedded in the object, whether the tz is UTC, local time
with daylight savings, or local standard time. The output is always mean
solar time (not apparent; see `convert_UTC_to_solartime`).

## Usage

``` r
calc_solar_time(local.time, longitude)
```

## Arguments

- local.time:

  POSIXct date+time of interest, already in local time as specified by
  the tz attribute

- longitude:

  numeric, in degrees, either positive and unitted ("degE" or "degW") or
  with sign indicating direction (positive = East)

## Examples

``` r
local.time <- as.POSIXct('2016-05-27 12:00:00', tz='America/New_York')
solar.time <- calc_solar_time(local.time, longitude=-74)
```
