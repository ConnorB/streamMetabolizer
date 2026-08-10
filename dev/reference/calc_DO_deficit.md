# Calculate a vector of dissolved oxygen deficits

**\[deprecated\]**

`calc_DO_deficit()` is deprecated. Subtract observed dissolved oxygen
from the output of
[`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_DO_sat.md)
instead.

## Usage

``` r
calc_DO_deficit(DO.obs, temp.water, pressure.air, salinity.water = 0, ...)
```

## Arguments

- DO.obs:

  A numeric vector of dissolved oxygen concentration observations, mgO2
  L^-1.

- temp.water:

  A numeric vector of water temperature in degrees Celsius.

- pressure.air:

  Barometric pressure in millibars.

- salinity.water:

  A numeric vector of salinity in PSU. Defaults to zero. Its length must
  be one or equal to the length of `temp.water`.

- ...:

  Additional parameters passed to
  [`LakeMetabolizer::o2.at.sat.base`](https://rdrr.io/pkg/LakeMetabolizer/man/o2.at.sat.html).

## Value

A vector of DO.deficit values.

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
