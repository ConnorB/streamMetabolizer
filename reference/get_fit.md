# Extract the internal fitted model

A function in the `metab_model_interface`. Returns the internal model
representation as fitted to the supplied data and arguments.

## Usage

``` r
get_fit(metab_model)

# S3 method for class 'metab_model'
get_fit(metab_model)
```

## Arguments

- metab_model:

  A metabolism model that implements the `metab_model_interface`.

## Value

An internal model representation, which may have any class.

## Methods (by class)

- `get_fit(metab_model)`: This implementation is shared by many model
  types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)

## Examples

``` r
get_fit(metab_model(fit = list(converged = TRUE)))
#> $converged
#> [1] TRUE
#> 
```
