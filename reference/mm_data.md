# Return the data types that may be used by metab_models using the metab_model_interface.

Produces a data.frame with the column names and data format to be used
by metab_models that comply strictly with the metab_model_interface.
These are the columns that may be included:

- `solar.time` date-time values in mean solar time (see
  [`calc_solar_time()`](https://connorb.github.io/streamMetabolizer/reference/calc_solar_time.md)
  and/or
  [`convert_UTC_to_solartime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_solartime.md)),
  in POSIXct format with a tzone attribute of 'UTC'. May be approximated
  by local, non-daylight-savings clock time (still with nominal UTC
  timezone but with clock noons close to solar noon), but mean solar
  time is better for matching model time windows to the diel cycle of
  light availability. Throughout this package, variables named
  "solar.time" are mean solar time, "app.solar.time" means apparent
  solar time, and "any.solar.time" means either.

- `DO.obs` dissolved oxygen concentration observations, \\mg O_2
  L^{-1}\\

- `DO.sat` dissolved oxygen concentrations if the water were at
  equilibrium saturation \\mg O_2 L^{-1}\\. Calculate using
  [calc_DO_sat](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)

- `depth` stream depth, \\m\\.

- `temp.water` water temperature, \\^\circ\\C

- `light` photosynthetically active radiation, \\\mu mol\\ m^{-2}
  s^{-1}\\

- `date` dates of interest in Date format

- `err.obs.sigma` SD of observation error to use in simulating data

- `err.obs.phi` autocorrelation of observation error to use in
  simulating data

- `err.proc.sigma` SD of process error to use in simulating data

- `err.proc.phi` autocorrelation of process error to use in simulating
  data

- `DO.obs` dissolved oxygen concentration observations, \\mg O_2
  L^{-1}\\

- `GPP` daily estimates of GPP, \\g O_2 m^{-2} d^{-1}\\

- `ER` daily estimates of ER, \\g O_2 m^{-2} d^{-1}\\

- `K600` daily estimates of K600, \\d^{-1}\\

- `GPP.init` daily initial values of GPP, \\g O_2 m^{-2} d^{-1}\\, for
  use in maximum likelihood estimation

- `ER.init` daily initial values of ER, \\g O_2 m^{-2} d^{-1}\\, for use
  in maximum likelihood estimation

- `K600.init` daily initial values of K600, \\d^{-1}\\, for use in
  maximum likelihood estimation

- `discharge.daily` daily mean river discharge, \\m^3 s^{-1}\\

- `velocity.daily` daily mean river flow velocity, \\m s^{-1}\\

## Usage

``` r
mm_data(..., optional = "none")
```

## Arguments

- ...:

  column names to select, as passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

- optional:

  one or more character strings listing the columns, if any, that may be
  excluded. If 'all', the entire data.frame may be omitted. If 'none',
  the entire data.frame must be included as prototyped. If specific
  column names are given, those columns may be omitted entirely or
  passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md)
  as all NAs.

## Value

data data.frame with columns as in the description

## Details

Most models will require a subset of these data columns. Specialized
models may deviate from this format, but this is discouraged.

## Examples

``` r
# all possible columns
mm_data()
#>            solar.time DO.obs DO.sat depth temp.water light discharge velocity
#> 1 2050-03-14 15:10:00   10.1   14.2   0.5       21.8 300.9         9        2
#>         date DO.mod.1 err.obs.sigma err.obs.phi err.proc.sigma err.proc.phi
#> 1 2050-03-14      7.5          0.01           0              5            0
#>   GPP.daily Pmax alpha ER.daily ER20 K600.daily K600.daily.lower
#> 1         5   10 1e-04      -10  -10         10              4.5
#>   K600.daily.upper init.GPP.daily init.Pmax init.alpha init.ER.daily init.ER20
#> 1             15.6              5        10      1e-04           -10       -10
#>   init.K600.daily discharge.daily velocity.daily GPP GPP.lower GPP.upper ER
#> 1              10               9              2   5         4         6 -5
#>   ER.lower ER.upper D D.lower D.upper
#> 1       -6       -4 5       5       5

# columns typical of instantaneous data
mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light)
#>            solar.time DO.obs DO.sat depth temp.water light
#> 1 2050-03-14 15:10:00   10.1   14.2   0.5       21.8 300.9

# columns typical of daily data
mm_data(date, K600.daily, discharge.daily, velocity.daily)
#>         date K600.daily discharge.daily velocity.daily
#> 1 2050-03-14         10               9              2
```
