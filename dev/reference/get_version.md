# Extract the streamMetabolizer version used to fit a model

A function in the `metab_model_interface`. Returns the version of
streamMetabolizer that was used to fit the model.

## Usage

``` r
get_version(metab_model)

# S3 method for class 'metab_model'
get_version(metab_model)
```

## Arguments

- metab_model:

  A metabolism model that implements the `metab_model_interface`.

## Value

A character representation of the package version.

## Methods (by class)

- `get_version(metab_model)`: This implementation is shared by many
  model types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_specs.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_metab.md)

## Examples

``` r
get_version(metab_model())
#> [1] "0.12.1.9000"
```
