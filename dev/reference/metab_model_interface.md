# Functions implemented by compatible metabolism models

Metabolism models in streamMetabolizer all implement a common set of
core functions. These functions are conceptually packaged as the
`metab_model_interface` defined here.

## Functions in the interface

- `show(metab_model)` displays the model.

- [`get_params()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_params.md)
  returns a data frame of model parameters.

- [`get_param_names()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_param_names.md)
  returns required and optional parameter names.

- [`predict_metab()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_metab.md)
  returns a data frame of metabolism predictions.

- [`predict_DO()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_DO.md)
  returns a data frame of dissolved oxygen predictions.

- [`get_fit()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fit.md)
  returns the internal fitted model.

- [`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fitting_time.md)
  returns the model fitting time.

- [`get_info()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_info.md)
  returns user-supplied metadata.

- [`get_specs()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_specs.md)
  returns model specifications.

- [`get_data()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data.md)
  returns the subdaily fitting data.

- [`get_data_daily()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_data_daily.md)
  returns the daily fitting data.

- [`get_version()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_version.md)
  returns the package version used to fit the model.

## Examples

``` r
methods(class = "metab_model")
#>  [1] get_data         get_data_daily   get_fit          get_fitting_time
#>  [5] get_info         get_param_names  get_params       get_specs       
#>  [9] get_version      predict_DO       predict_metab    show            
#> see '?methods' for accessing help and source code
```
