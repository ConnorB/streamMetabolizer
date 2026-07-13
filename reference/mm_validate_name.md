# Check the validity of a model name

Check the syntactic & scientific validity of a model name. Returns the
model name if it's valid, otherwise gives an error

## Usage

``` r
mm_validate_name(model_name)
```

## Arguments

- model_name:

  character string identifying the model features. Use
  [`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
  to create a valid name based on desired attributes, or
  [`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
  to see all valid names. Two alternatives to the names given by
  [`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
  are also accepted: (1) a model type as accepted by the `type` argument
  to `mm_name`, which will be used to create the default model name for
  that model type, or (2) a full model file path for custom Bayesian
  models, as long as basename(model_name) can still be parsed correctly
  with
  [`mm_parse_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md)
  and the file exists. In that case the file may be specified either as
  a file path relative to the streamMetabolizer models directory (the
  first assumption; this directory can be found with
  `system.file("models", package="streamMetabolizer")`) or as an
  absolute path or a path relative to the current working directory (the
  second assumption, if the first assumption turns up no files of the
  given name).

## Examples

``` r
mm_validate_name("b_np_oipi_tr_plrckm.stan")
#> [1] "b_np_oipi_tr_plrckm.stan"
if (FALSE) { # \dontrun{
mm_validate_name("b_np_oipn") # throws error
} # }
```
