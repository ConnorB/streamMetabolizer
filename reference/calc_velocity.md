# Estimate velocity from discharge and hydraulic geometry coefficients

Uses the relationship \\U=k\*Q^m\\ (parameter names and definitions as
in Leopold and Maddock, 1953; default values for k and m as in Raymond
et al. 2012)

## Usage

``` r
calc_velocity(Q, k = 0.194, m = 0.285)
```

## Arguments

- Q:

  discharge (m^3 s^-1)

- k:

  coefficient representing velocity at unit discharge (usually m/s; e in
  Raymond et al.)

- m:

  exponent in velocity-discharge relation (unitless; f in Raymond et
  al.)

## Value

v (= V = U), stream flow velcoity, in the same units as k

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
calc_velocity(Q=Qs)
#> [1] 0.1940000 0.2653269 0.3069080 0.3377961 0.3628782
calc_velocity(Q=Qs, k=0.4)
#> [1] 0.4000000 0.5470658 0.6328000 0.6964869 0.7482024
```
