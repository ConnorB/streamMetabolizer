# Nighttime regression for K estimation

Fits a model to estimate K from nighttime input data on DO, temperature,
light, etc. The default day start & end are 12 noon on the preceding to
present day; the algorithm then filters the data to just those time
points for which light is very low. Discharge is only used, if at all,
to identify and exclude days with any negative discharge.

## Usage

``` r
metab_night(
  specs = specs(mm_name("night")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge,
    optional = "discharge"),
  data_daily = mm_data(NULL),
  info = NULL
)
```

## Arguments

- specs:

  a list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  lists the necessary parameters, describes them in detail, and gives
  default values.

- data:

  data.frame (not a tbl_df) of input data at the temporal resolution of
  raw observations (unit-value). Columns must have the same names,
  units, and format as the default. The solar.time column must also have
  a timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  data.frame containing inputs with a daily timestep. See the
  **'Formatting `data_daily`'** section below for a full description.

- info:

  any information, in any format, that you would like to store within
  the metab_model object

## Value

A metab_night object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md),
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md),
[`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md),
[`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)

## Author

Alison Appling, Maite Arroita, Bob Hall

## Examples

``` r
dat <- data_metab('3', day_start=12, day_end=35)
mm <- metab_night(data=dat)
predict_metab(mm)
#>         date GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper msgs.fit
#> 1 2012-09-18   0        NA        NA -2.122498 -2.351240 -1.893755         
#> 2 2012-09-19   0        NA        NA -2.927715 -3.233801 -2.621628         
#> 3 2012-09-20   0        NA        NA -2.125522 -2.305859 -1.945185         
#>   warnings errors
#> 1                
#> 2                
#> 3                
if (FALSE) { # \dontrun{
plot_DO_preds(predict_DO(mm))
} # }
```
