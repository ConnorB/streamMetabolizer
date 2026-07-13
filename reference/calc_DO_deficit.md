# Calculate a vector of dissolved oxygen deficits

**\[deprecated\]**

`calc_DO_deficit()` is deprecated. Subtract observed dissolved oxygen
from the output of
[`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)
instead.

## Usage

``` r
calc_DO_deficit(DO.obs, temp.water, pressure.air, salinity.water = 0, ...)
```

## Arguments

- DO.obs:

  a numeric vector of dissolved oxygen concentration observations, mgO2
  L^-1.

- temp.water:

  a numeric vector of water temperature in degrees Celsius.

- pressure.air:

  barometric pressure in millibars.

- salinity.water:

  a numeric vector of salinity in PSU. Defaults to zero. Length must be
  one or equal to length of `temp.water`.

- ...:

  additional parameters passed to
  [`LakeMetabolizer::o2.at.sat.base`](https://rdrr.io/pkg/LakeMetabolizer/man/o2.at.sat.html)

## Value

a vector of DO.deficit values

## Examples

``` r
# Old:
calc_DO_deficit(
  DO.obs = 7,
  temp.water = 25,
  pressure.air = 900,
  salinity.water = 2.43
)
#> Warning: `calc_DO_deficit()` was deprecated in streamMetabolizer 0.13.0.
#> ℹ Please use `calc_DO_sat()` instead.
#> [1] 0.2095001
# New:
calc_DO_sat(25, 900, 2.43) - 7
#> [1] 0.2095001
```
