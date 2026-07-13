# Estimate depth from discharge and hydraulic geometry coefficients

Uses the relationship \\d=c\*Q^f\\ (parameter names and definitions as
in Leopold and Maddock, 1953; default values for c and f as in Raymond
et al. 2012)

## Usage

``` r
calc_depth(Q, c = 0.409, f = 0.294)
```

## Arguments

- Q:

  discharge (m^3 s^-1)

- c:

  coefficient representing depth at unit discharge (usually m)

- f:

  exponent in depth-discharge relation (unitless)

## Value

d, stream depth, in the same units as c

## References

Raymond, Peter A., Christopher J. Zappa, David Butman, Thomas L. Bott,
Jody Potter, Patrick Mulholland, Andrew E. Laursen, William H. McDowell,
and Denis Newbold. *Scaling the gas transfer velocity and hydraulic
geometry in streams and small rivers*. Limnology & Oceanography: Fluids
& Environments 2 (2012): 41-53.

Leopold, L.B., and Thomas Maddock Jr. *The Hydraulic Geometry of Stream
Channels and Some Physiographic Implications*. Report. Professional
Paper, 1953. USGS Publications Warehouse.
https://pubs.er.usgs.gov/publication/pp252.

## Examples

``` r
Qs <- seq(1,9,2)
calc_depth(Q=Qs)
#> [1] 0.4090000 0.5649330 0.6564785 0.7247398 0.7803162
calc_depth(Q=Qs, f=0.4)
#> [1] 0.4090000 0.6347048 0.7785945 0.8907637 0.9849639
```
