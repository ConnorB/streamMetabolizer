# Extract daily metabolism parameter names

A function in the `metab_model_interface`. Returns vectors of the
required and optional daily metabolism parameters for the model.

## Usage

``` r
get_param_names(metab_model, ...)

# S3 method for class 'character'
get_param_names(metab_model, ...)

# S3 method for class 'metab_model'
get_param_names(metab_model, ...)
```

## Arguments

- metab_model:

  A metabolism model object or model name for which to return the list
  of required and optional metabolism parameters.

- ...:

  Reserved for future arguments.

## Value

A list of two vectors containing the names of required and optional
daily metabolism parameters, respectively.

## Methods (by class)

- `get_param_names(character)`: This implementation is shared by many
  model types

- `get_param_names(metab_model)`: Lets you pass in a model object rather
  than a character string

## See also

Other metab_model_interface:
[`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md),
[`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md),
[`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md),
[`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md),
[`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md),
[`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md),
[`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md),
[`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md),
[`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md),
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)

## Examples

``` r

# pass in a character string:
get_param_names(mm_name('mle', GPP_fun='satlight'))
#> $required
#> [1] "Pmax"       "alpha"      "ER.daily"   "K600.daily"
#> 
#> $optional
#> [1] "DO.mod.1"
#> 
get_param_names(mm_name('bayes'))
#> $required
#> [1] "GPP.daily"  "ER.daily"   "K600.daily"
#> 
#> $optional
#> [1] "DO.mod.1"
#> 
get_param_names(mm_name('Kmodel'))
#> $required
#> [1] "K600.daily"
#> 
#> $optional
#> NULL
#> 
get_param_names(mm_name('night'))
#> $required
#> [1] "ER.daily"   "K600.daily"
#> 
#> $optional
#> [1] "DO.mod.1"
#> 
get_param_names(mm_name('sim'))
#> $required
#> [1] "K600.daily"     "GPP.daily"      "ER.daily"       "err.obs.sigma" 
#> [5] "err.obs.phi"    "err.proc.sigma" "err.proc.phi"  
#> 
#> $optional
#> [1] "discharge.daily" "DO.mod.1"       
#> 

# or pass in a metab_model object:
model <- metab_model(specs = list(model_name = mm_name('night')))
get_param_names(model)
#> $required
#> [1] "ER.daily"   "K600.daily"
#> 
#> $optional
#> [1] "DO.mod.1"
#> 
```
