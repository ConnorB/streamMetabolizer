# Extract the amount of time that was required to fit the metabolism model.

A function in the metab_model_interface. Returns the time that was taken
to fit the model; see
[`proc.time()`](https://rdrr.io/r/base/proc.time.html) for details.

## Usage

``` r
get_fitting_time(metab_model)

# S3 method for class 'metab_model'
get_fitting_time(metab_model)
```

## Arguments

- metab_model:

  A metabolism model, implementing the metab_model_interface, for which
  to return the time

## Value

An proc_time object

## Methods (by class)

- `get_fitting_time(metab_model)`: This implementation is shared by many
  model types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)
