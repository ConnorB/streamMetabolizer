# Describe model input data types

Produces a data frame containing the column names and formats used by
models that comply with
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).
The available columns are:

- `solar.time`: Datetimes in mean solar time, stored as `POSIXct` with a
  `tzone` attribute of `"UTC"`. See
  [`calc_solar_time()`](https://connorb.github.io/streamMetabolizer/reference/calc_solar_time.md)
  and
  [`convert_UTC_to_solartime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_solartime.md).
  Local standard time may approximate solar time, but mean solar time
  better aligns model windows with the diel light cycle. In this
  package, `solar.time` means mean solar time, `app.solar.time` means
  apparent solar time, and `any.solar.time` means either.

- `DO.obs`: Dissolved oxygen concentration observations, mgO₂ L⁻¹.

- `DO.sat`: Dissolved oxygen concentrations at equilibrium saturation,
  mgO₂ L⁻¹. Calculate with
  [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md).

- `depth`: Stream depth, m.

- `temp.water`: Water temperature, °C.

- `light`: Photosynthetically active radiation, µmol m⁻² s⁻¹.

- `date`: Dates of interest as `Date` values.

- `err.obs.sigma`: Observation-error standard deviation for simulations.

- `err.obs.phi`: Observation-error autocorrelation for simulations.

- `err.proc.sigma`: Process-error standard deviation for simulations.

- `err.proc.phi`: Process-error autocorrelation for simulations.

- `GPP`: Daily GPP estimates, gO₂ m⁻² d⁻¹.

- `ER`: Daily ER estimates, gO₂ m⁻² d⁻¹.

- `K600`: Daily K600 estimates, d⁻¹.

- `GPP.init`: Initial daily GPP values for maximum likelihood
  estimation, gO₂ m⁻² d⁻¹.

- `ER.init`: Initial daily ER values for maximum likelihood estimation,
  gO₂ m⁻² d⁻¹.

- `K600.init`: Initial daily K600 values for maximum likelihood
  estimation, d⁻¹.

- `discharge.daily`: Daily mean river discharge, m³ s⁻¹.

- `velocity.daily`: Daily mean river flow velocity, m s⁻¹.

## Usage

``` r
mm_data(..., optional = "none")
```

## Arguments

- ...:

  Column names to select, as passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

- optional:

  One or more character strings listing the columns, if any, that may be
  excluded. If 'all', the entire data.frame may be omitted. If 'none',
  the entire data.frame must be included as prototyped. If specific
  column names are given, those columns may be omitted entirely or
  passed to
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md)
  as all NAs.

## Value

A data frame containing the columns described above.

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
