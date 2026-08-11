# Calculate equilibrium oxygen saturation

Calculate equilibrium oxygen saturation

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

  A numeric vector of water temperature in degrees Celsius.

- pressure.air:

  Barometric pressure in millibars.

- salinity.water:

  A numeric vector of salinity in PSU. Defaults to zero.

- model:

  A string specifying the saturation model. One of `"garcia-benson"`,
  `"garcia"`, `"weiss"`, or `"benson"`; `"garcia-benson"` is
  recommended.

- ...:

  Additional parameters passed to
  [`LakeMetabolizer::o2.at.sat.base`](https://rdrr.io/pkg/LakeMetabolizer/man/o2.at.sat.html).

## Value

A numeric vector of dissolved oxygen equilibrium saturation
concentrations, in mg/L.

## Examples

``` r
calc_DO_sat(temp.water = 21, pressure.air = 1000.1, salinity.water = 0)
#> [1] 8.795956
```
