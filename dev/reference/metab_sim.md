# Simulate dissolved oxygen data from input data

Takes input data in the form of a sub-daily time series (`data`) of
DO.sat, depth, temperature, and light, and a daily time series
(`data_daily`) of GPP, ER, and K600 values, and turns these into
simulated DO.obs. Either `data` or `data_daily` should specify a
starting DO.obs value for each day; if in `data`, this takes the form of
a DO.obs column with values on at least the first time point of each day
(all other values are ignored), or if in `data_daily`, this takes the
form of a DO.mod.1 column with one starting DO value per day.

## Usage

``` r
metab_sim(
  specs = specs(mm_name("sim")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, optional =
    "DO.obs"),
  data_daily = mm_data(date, discharge.daily, DO.mod.1, K600.daily, GPP.daily, Pmax,
    alpha, ER.daily, ER20, err.obs.sigma, err.obs.phi, err.proc.sigma, err.proc.phi,
    optional = "all"),
  info = NULL
)
```

## Arguments

- specs:

  A list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md)
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

A metab_sim object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_model_interface.md).

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_Kmodel.md),
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_bayes.md),
[`metab_mle()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_mle.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_night.md)

## Examples

``` r
if (FALSE) { # interactive()
## simulations with variation all at sub-daily scale
# prepare input data (DO used only to pick first DO of each day)
dat <- data_metab('3', res = '15')
dat_daily <- data.frame(
  date = as.Date(paste0("2012-09-", 18:20)),
  GPP.daily = 2, ER.daily = -3, K600.daily = 21,
  stringsAsFactors = FALSE
)

# define simulation parameters and run a single model
mm <- metab_sim(
  specs(mm_name('sim'),
    err_obs_sigma = 0.1,
    err_proc_sigma = 2,
    GPP_daily = NULL,
    ER_daily = NULL,
    K600_daily = NULL
  ),
  data = dat,
  data_daily = dat_daily
)

get_params(mm)
predict_metab(mm)
predict_DO(mm)[seq(1, 50, by = 10), ]

# stochastic DO (different each time)
predict_DO(mm)[seq(1, 50, by = 10), ]

# reproducible simulation with seed
mm@specs$sim_seed <- 236
predict_DO(mm)$DO.obs[seq(1, 50, by = 10)]
predict_DO(mm)$DO.obs[seq(1, 50, by = 10)]

# fancy GPP equation
dat_daily <- data.frame(
  date = as.Date(paste0("2012-09-", 18:20)),
  Pmax = 8, alpha = 0.01, ER.daily = -3, K600.daily = 21,
  stringsAsFactors = FALSE
)
mm <- metab_sim(
  specs(mm_name('sim', GPP_fun = 'satlight'),
    err_obs_sigma = 0.1,
    err_proc_sigma = 2,
    Pmax = NULL,
    alpha = NULL,
    ER_daily = NULL,
    K600_daily = NULL
  ),
  data = dat,
  data_daily = dat_daily
)
get_params(mm)
predict_metab(mm)
predict_DO(mm)[seq(1, 50, by = 10), ]

## simulations with variation at both sub-daily and multi-day scales
sp <- specs(mm_name('sim', pool_K600 = 'none'),
  K600_daily = function(n, ...) pmax(0, rnorm(n, 10, 3)))
mm <- metab(sp, dat)
get_params(mm)
predict_metab(mm)

## K~Q model
dat <- data_metab('10', '15')
sp <- specs(mm_name('sim', pool_K600 = 'binned'))
mm <- metab(sp, dat)
pars <- get_params(mm)
attr(pars, 'K600_eqn')

plot_DO_preds(predict_DO(mm))
plot_DO_preds(mm)
library(ggplot2)
}
```
