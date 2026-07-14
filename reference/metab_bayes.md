# Basic Bayesian metabolism model fitting function

Fits a Bayesian model to estimate GPP and ER from input data on DO,
temperature, light, etc. See
[`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
to choose a Bayesian model and
[`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
for relevant options for the `specs` argument.

## Usage

``` r
metab_bayes(
  specs = specs(mm_name("bayes")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge,
    optional = "discharge"),
  data_daily = mm_data(date, discharge.daily, optional = "all"),
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

  any information, in any format, that you would like to store within
  the metab_model object

## Value

A metab_bayes object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md)
and also
[`get_mcmc()`](https://connorb.github.io/streamMetabolizer/reference/get_mcmc.md).

## Details

As of summer and fall 2016, a new compilation of any Stan model gives
deprecation warnings including
`typedef 'size_type' locally defined but not used [-Wunused-local-typedefs]`,
`typedef 'index_range' locally defined but not used [-Wunused-local-typedefs]`,
`typedef 'index' locally defined but not used [-Wunused-local-typedefs]`,
and
`'void stan::math::set_zero_all_adjoints()' defined but not used [-Wunused-function]`.
THESE ARE OKAY. Subsequent runs of the compiled Stan model will be
quieter, and the model will work.

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md),
[`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md),
[`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)

## Author

Alison Appling, Bob Hall

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- data_metab('3', res='30')
# fast-ish model version, but still too slow to auto-run in examples
mm <- metab_bayes(data=dat,
  specs(mm_name('bayes', err_proc_iid=FALSE),
    n_cores=3, n_chains=3, burnin_steps=300, saved_steps=100))
mm
get_fitting_time(mm)
predict_metab(mm)
plot_DO_preds(predict_DO(mm))

# error and warning messages are printed with the mm object if present
dat <- data_metab('3', res='30', flaws=c('missing middle'))
mm <- metab(specs(mm_name('bayes', err_proc_iid=FALSE),
  n_cores=3, n_chains=3, burnin_steps=300, saved_steps=100, verbose=FALSE),
  data=dat)
predict_metab(mm)

# view the Stan model file as stored on your system
file.edit(get_specs(mm)$model_path)
} # }
```
