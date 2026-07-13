# Returns the gas exchange velocity for gas of interest w/ no unit conversions

Returns the gas exchange velocity for gas of interest w/ no unit
conversions

## Usage

``` r
convert_k600_to_kGAS(k600, temperature, gas = "O2")
```

## Arguments

- k600:

  k600 as vector of numbers or single number

- temperature:

  Water temperature (deg C) as vector array of numbers or single number

- gas:

  gas for conversion, as string (e.g., 'CO2' or 'O2')

## Value

Numeric value of gas exchange velocity for gas
