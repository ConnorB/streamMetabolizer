# Convert from photosynthetically active to shortwave radiation

Convert photosynthetically active radiation (PAR) to shortwave radiation
(SW). Uses a fixed ratio between PAR and SW, ignoring the minor seasonal
changes in this ratio (see Britton and Dodd (1976)).

## Usage

``` r
convert_PAR_to_SW(par, coef = 0.473)
```

## Arguments

- par:

  Vector of photosynthetically active radiation (400-700 nm;
  umol/m^2/sec)

- coef:

  Numerical coefficient to convert PAR (umol/m^2/sec) to SW (W/m^2).
  Defaults to value from Britton and Dodd (1976).

## Value

Numeric vector of shortwave values with units W/m^2

## Examples

``` r
convert_PAR_to_SW(par=400, coef=0.47)
#> [1] 188
convert_PAR_to_SW(1000)
#> [1] 473
```
