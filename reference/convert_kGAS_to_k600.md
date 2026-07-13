# Returns the gas exchange velocity as k600 for gas of interest w/ no unit conversions

Returns the gas exchange velocity as k600 for gas of interest w/ no unit
conversions

## Usage

``` r
convert_kGAS_to_k600(kGAS, temperature, gas = "O2")
```

## Arguments

- kGAS:

  k of gas as vector of numbers or single number

- temperature:

  Water temperature (deg C) as vector array of numbers or single number

- gas:

  gas for conversion, as string (e.g., 'CO2' or 'O2')

## Value

Numeric value of gas exchange velocity for gas
