# Calculate declination angle as in Yard et al. (2005)

Calculate declination angle as in Yard et al. (2005)

## Usage

``` r
calc_declination_angle(jday, format = c("degrees", "radians"))
```

## Arguments

- jday:

  The day of year as a number between 0 (Jan 1) and 364 (365 also OK for
  leap year)

- format:

  The format of both the input and the output. May be "degrees" or
  "radians".

## Value

numeric value or vector, in the units specified by `format`, indicating
the declination angle corresponding to each value supplied in `jday`.

## References

Yard, Michael D., Glenn E. Bennett, Steve N. Mietz, Lewis G. Coggins
Jr., Lawrence E. Stevens, Susan Hueftle, and Dean W. Blinn. *Influence
of Topographic Complexity on Solar Insolation Estimates for the Colorado
River, Grand Canyon, AZ.* Ecological Modelling 183, no. 2-3 (April 25,
2005): 157-72. doi:10.1016/j.ecolmodel.2004.07.027.

## Examples

``` r
decdf <- data.frame(jday=1:366,
  dec=streamMetabolizer:::calc_declination_angle(1:366))
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(decdf, aes(x=jday, y=dec)) + geom_line()
} # }
```
