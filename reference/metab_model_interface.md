# Functions implemented by any `streamMetabolizer`-compatible metabolism model.

Metabolism models in the `streamMetabolizer` package all implement a
common set of core functions. These functions are conceptually packaged
as the `metab_model_interface` defined here.

## Format

A collection of functions which any metabolism model in
`streamMetabolizer` should implement.

## Functions in the interface

- `[show](metab_model) { display(metab_model) }`

- `[get_params](metab_model, ...) { return(data.frame) }`

- `[get_param_names](metab_model, ...) { return(list) }`

- `[predict_metab](metab_model, ...) { return(data.frame) }`

- `[predict_DO](metab_model, ...) { return(data.frame) }`

- `[get_fit](metab_model) { return(fitted.model) }`

- `[get_fitting_time](metab_model) { return(proc_time) }`

- `[get_info](metab_model) { return(info) }`

- `[get_specs](metab_model) { return(specs.list) }`

- `[get_data](metab_model) { return(data.frame) }`

- `[get_data_daily](metab_model) { return(data.frame) }`

- `[get_version](metab_model) { return(version.string) }`

## Examples

``` r
methods(class="metab_model")
#>  [1] get_data         get_data_daily   get_fit          get_fitting_time
#>  [5] get_info         get_param_names  get_params       get_specs       
#>  [9] get_version      predict_DO       predict_metab    show            
#> see '?methods' for accessing help and source code
```
