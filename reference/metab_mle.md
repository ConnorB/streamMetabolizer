# Fit a metabolism model by maximum likelihood

Uses maximum likelihood to fit a model to estimate GPP and ER from input
data on DO, temperature, light, etc. Discharge is only used, if at all,
to identify and exclude days with any negative discharge.

## Usage

``` r
metab_mle(
  specs = specs(mm_name("mle")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge,
    optional = "discharge"),
  data_daily = mm_data(date, K600.daily, init.GPP.daily, init.Pmax, init.alpha,
    init.ER.daily, init.ER20, init.K600.daily, optional = "all"),
  info = NULL
)
```

## Arguments

- specs:

  A list of model specifications and parameters for a model. Although
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

  A data frame or tibble of input data at the temporal resolution of raw
  observations (unit-value). Columns must have the same names, units,
  and format as the default. The solar.time column must also have a
  timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  A data frame or tibble containing inputs with a daily timestep. See
  the **'Formatting `data_daily`'** section below for a full
  description.

- info:

  Any information, in any format, that you would like to store within
  the metab_model object.

## Value

A metab_mle object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).
The `code` column in `get_fit(mm)` is defined in the Value subsection of
[`?nlm`](https://rdrr.io/r/stats/nlm.html).

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md),
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md),
[`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)

## Examples

``` r
if (FALSE) { # interactive()
dat <- data_metab('3','30')
# PRK
mm <- metab_mle(data=dat)
predict_metab(mm)

# PR with fixed K on two days
dat_daily <- data.frame(
  date = as.Date(c("2012-09-18","2012-09-20")),
  K600.daily = 35
)
metab_mle(data = dat, data_daily = dat_daily)

# PRK with day-specific initial values
dat_daily <- data.frame(
  date = as.Date("2012-09-19"),
  init.GPP.daily = 4,
  init.K600.daily = 60
)
metab_mle(data = dat, data_daily = dat_daily)

# Nonlinear GPP or ER equations
metab_mle(specs(mm_name('mle', GPP_fun = 'satlight')), data = dat)
metab_mle(specs(mm_name('mle', ER_fun = 'q10temp')), data = dat)

plot_DO_preds(predict_DO(mm))
}
```
