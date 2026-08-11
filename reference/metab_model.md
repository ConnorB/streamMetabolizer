# Create a metabolism model object

Generates a new model of class metab_model
([`metab_model-class()`](https://connorb.github.io/streamMetabolizer/reference/metab_model-class.md)).

## Usage

``` r
metab_model(
  model_class = "metab_model",
  info = "user metadata goes here",
  metab_daily = NULL,
  fit = "generic metab_model class; no actual fit",
  fitting_time = system.time({
 }),
  specs = list(),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light),
  data_daily = mm_data(date, optional = "all"),
  pkg_version = as.character(packageVersion("streamMetabolizer")),
  ...
)
```

## Arguments

- model_class:

  Character name of a class inheriting from metab_model - the type of
  object to create.

- info:

  User-supplied metadata of any form.

- metab_daily:

  A data.frame of daily metabolism estimates produced from the fit.

- fit:

  An internal representation of a fitted model.

- fitting_time:

  A proc_time object giving the time taken to fit the model.

- specs:

  A list of model specifications that were supplied to the fitting
  function.

- data:

  The data that were used to fit the model.

- data_daily:

  The data_daily that were used to fit the model. May be NULL.

- pkg_version:

  A string indicating the package version used to create this
  metab_model object. The default should almost always be appropriate.

- ...:

  Other arguments passed to new() for this particular model_class.

## Value

A metab_model object.

## Examples

``` r
metab_model()
#> metab_model 
#>   User-supplied metadata:
#> [1] "user metadata goes here"
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   
#> Fitting time: 0 secs elapsed
metab_model("metab_mle", fit=1:5, specs=list(length=5))
#> metab_model of type metab_mle 
#>   User-supplied metadata:
#> [1] "user metadata goes here"
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   length 5    
#> Fitting time: 0 secs elapsed
```
