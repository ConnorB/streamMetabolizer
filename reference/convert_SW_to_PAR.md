# Convert from shortwave to photosynthetically active radiation

Convert shortwave radiation (SW) to photosynthetically active radiation
(PAR). Uses a fixed ratio between SW and PAR, ignoring the minor
seasonal changes in this ratio (see Britton and Dodd (1976)).

## Usage

``` r
convert_SW_to_PAR(sw, coef = 2.114)
```

## Arguments

- sw:

  Vector of shortwave radiation (W/m^2)

- coef:

  Numerical coefficient to convert SW (W/m^2) to PAR (umol/m^2/sec).
  Defaults to value from Britton and Dodd (1976).

## Value

Numeric vector of PAR values in units umol/m^2/sec

## Examples

``` r
convert_SW_to_PAR(sw=800)
#> [1] 1691.2
convert_SW_to_PAR(sw=800, coef=2.1)
#> [1] 1680
convert_SW_to_PAR(473)
#> [1] 999.922
```
