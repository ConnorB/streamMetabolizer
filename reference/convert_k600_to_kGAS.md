# Convert K600 to a gas-specific exchange velocity

Convert K600 to a gas-specific exchange velocity

## Usage

``` r
convert_k600_to_kGAS(k600, temperature, gas = "O2")
```

## Arguments

- k600:

  K600 as vector of numbers or single number.

- temperature:

  A numeric vector of water temperatures in degrees Celsius.

- gas:

  Gas for conversion, as string (e.g., 'CO2' or 'O2').

## Value

Numeric value of gas exchange velocity for gas.

## Examples

``` r
convert_k600_to_kGAS(10, temperature = 20, gas = "O2")
#> [1] 10.62788
```
