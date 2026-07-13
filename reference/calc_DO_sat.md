# Calculates the equilibrium saturation concentration of oxygen in water at the supplied conditions

**\[deprecated\]**

The `calc_DO_at_sat()` alias is deprecated. Use `calc_DO_sat()` instead.

## Usage

``` r
calc_DO_sat(
  temp.water,
  pressure.air,
  salinity.water = 0,
  model = "garcia-benson",
  ...
)
```

## Arguments

- temp.water:

  a numeric vector of water temperature in degrees Celsius.

- pressure.air:

  barometric pressure in millibars.

- salinity.water:

  a numeric vector of salinity in PSU. Defaults to zero.

- model:

  character. One of 'garcia-benson', 'garcia', 'weiss', or 'benson', but
  'garcia-benson' is recommended.

- ...:

  additional parameters passed to
  [`LakeMetabolizer::o2.at.sat.base`](https://rdrr.io/pkg/LakeMetabolizer/man/o2.at.sat.html)

## Value

a numeric vector of dissolved oxygen equilibrium saturation
concentrations, in mg/L.

## Examples

``` r
calc_DO_sat(temp=21, press=1000.1, sal=0)
#> [1] 8.795956
```
