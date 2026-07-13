# Load or compile an RStan model using a persistent cache

Load or compile an RStan model using a persistent cache

## Usage

``` r
load_rstan_model(model_path, verbose = FALSE)
```

## Arguments

- model_path:

  Path to a Stan program.

- verbose:

  logical. give status messages?

## Value

A list containing the `stanmodel`, compilation time, compilation output,
and cache file.
