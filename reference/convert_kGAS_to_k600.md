# Convert a gas-specific exchange velocity to K600

Convert a gas-specific exchange velocity to K600

## Usage

``` r
convert_kGAS_to_k600(kGAS, temperature, gas = "O2")
```

## Arguments

- kGAS:

  K of gas as vector of numbers or single number.

- temperature:

  A numeric vector of water temperatures in degrees Celsius.

- gas:

  Gas for conversion, as string (e.g., 'CO2' or 'O2').

## Value

Numeric value of gas exchange velocity for gas.

## Examples

``` r
convert_kGAS_to_k600(8, temperature = 20, gas = "O2")
#> [1] 7.527372
```
