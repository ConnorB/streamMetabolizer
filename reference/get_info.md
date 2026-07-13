# Extract the user-supplied metadata about a metabolism model.

A function in the metab_model_interface. Returns any user-supplied
metadata.

## Usage

``` r
get_info(metab_model)

# S3 method for class 'metab_model'
get_info(metab_model)
```

## Arguments

- metab_model:

  A metabolism model, implementing the metab_model_interface, for which
  to return the metadata information.

## Value

The user-supplied metadata in the original format.

## Methods (by class)

- `get_info(metab_model)`: This implementation is shared by many model
  types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)
