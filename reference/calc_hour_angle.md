# Calculate hour angle as in http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html.

This is an approximation when hour is in clock time; should actually be
given in solar time

## Usage

``` r
calc_hour_angle(hour, format = c("degrees", "radians"))
```

## Arguments

- hour:

  numeric value or vector. hour since solar midnight as number between 0
  and 23.999

- format:

  The format of both the input and the output. May be "degrees" or
  "radians".

## Value

numeric value or vector, in the units specified by `format`, indicating
the angle corresponding to each value supplied in `hour`.

## Examples

``` r
hourdf <- data.frame(hour=c(0:12,12.5:23.5),
  hragl=streamMetabolizer:::calc_hour_angle(c(0:12,12.5:23.5)))
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(hourdf, aes(x=hour, y=hragl)) +
  geom_hline(yintercept=0, color="gold") + geom_line()
} # }
```
