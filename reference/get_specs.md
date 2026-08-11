# Extract model fitting specifications

A function in the `metab_model_interface`. Returns the specifications
that were passed in when fitting the metabolism model.

## Usage

``` r
get_specs(metab_model)

# S3 method for class 'metab_model'
get_specs(metab_model)
```

## Arguments

- metab_model:

  A metabolism model that implements the `metab_model_interface`.

## Value

The list of specifications passed to
[`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md).

## Methods (by class)

- `get_specs(metab_model)`: This implementation is shared by many model
  types

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md),
[`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)

## Examples

``` r
get_specs(metab_model(specs = list(day_start = 4, day_end = 28)))
#> Model specifications:
#>   day_start 4    
#>   day_end   28   
```
