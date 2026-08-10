# Extract metabolism model parameters

A function in the `metab_model_interface`. Returns estimates of the
parameters describing the rates and/or shapes of GPP, ER, or reaeration.

## Usage

``` r
get_params(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = c("sd", "ci", "none"),
  messages = TRUE,
  fixed = c("none", "columns", "stars"),
  ...,
  attach.units = deprecated()
)

# S3 method for class 'metab_Kmodel'
get_params(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = c("sd", "ci", "none"),
  messages = TRUE,
  fixed = c("none", "columns", "stars"),
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
)

# S3 method for class 'metab_bayes'
get_params(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = "ci",
  messages = TRUE,
  ...,
  attach.units = deprecated()
)

# S3 method for class 'metab_model'
get_params(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = c("sd", "ci", "none"),
  messages = TRUE,
  fixed = c("none", "columns", "stars"),
  ...,
  attach.units = deprecated()
)

# S3 method for class 'metab_sim'
get_params(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = c("sd", "ci", "none"),
  messages = TRUE,
  fixed = c("none", "columns", "stars"),
  ...,
  attach.units = deprecated()
)
```

## Arguments

- metab_model:

  A metabolism model that implements the `metab_model_interface`.

- date_start:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The first date
  (inclusive) for which to report parameters. If `NA`, no filtering is
  done.

- date_end:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The last date
  (inclusive) for which to report parameters. If `NA`, no filtering is
  done.

- uncertainty:

  A string. Should columns for parameter uncertainty be excluded
  (`"none"`), reported as standard deviations (`"sd"`), or reported as
  lower and upper bounds of a 95 percent confidence interval (`"ci"`)?
  When available (e.g., for Bayesian models), if `"ci"` then the central
  value will be the median (50th quantile) and the ranges will be the
  2.5th and 97.5th quantiles. If `"sd"` then the central value will be
  the mean.

- messages:

  A logical. Should warning and error messages from the fitting
  procedure be included in the output?

- fixed:

  A string. Should values pulled from `data_daily` (i.e., fixed rather
  than fitted) be treated identically (`"none"`), paired with logical
  columns ending in `.fixed` (`"columns"`), or converted to character
  and marked with a leading asterisk (`"stars"`)?

- ...:

  Other arguments passed to class-specific implementations of
  `get_params()`.

- attach.units:

  Deprecated. A logical indicating whether to attach units to the
  output.

- use_saved:

  A logical. Is it OK to use predictions that were saved with the model?

## Value

A data frame of the parameters needed to predict GPP, ER, D, and DO,
with one row per date.

## Methods (by class)

- `get_params(metab_Kmodel)`: Make daily re-predictions of K600.daily
  based on the across-days model of K600.daily versus predictors. Only
  returns estimates for K600.daily, not any of the other daily
  parameters

- `get_params(metab_bayes)`: Does a little formatting to convert from
  Stan output to streamMetabolizer parameter names; otherwise the same
  as `get_params.metab_model`

- `get_params(metab_model)`: This implementation is shared by many model
  types

- `get_params(metab_sim)`: Generates new simulated values for daily
  parameters if they were described with evaluatable expressions in
  [`specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md),
  or returns the fixed values for daily parameters if they were set in
  `data_daily`

## See also

[`predict_metab()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_metab.md)
for daily average rates of GPP and ER.

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_param_names.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_metab.md)

## Examples

``` r
dat <- data_metab("3", day_start = 12, day_end = 36)
mm <- metab_night(specs(mm_name("night")), data = dat)
get_params(mm)
#>         date  ER.daily ER.daily.sd K600.daily K600.daily.sd warnings errors
#> 1 2012-09-18 -2.122498  0.11670762   26.17191      1.337894                
#> 2 2012-09-19 -2.927715  0.15616944   34.09664      1.692708                
#> 3 2012-09-20 -2.125522  0.09201041   29.65021      1.184019                
get_params(mm, date_start = get_fit(mm)$date[2])
#>         date  ER.daily ER.daily.sd K600.daily K600.daily.sd warnings errors
#> 1 2012-09-19 -2.927715  0.15616944   34.09664      1.692708                
#> 2 2012-09-20 -2.125522  0.09201041   29.65021      1.184019                
```
