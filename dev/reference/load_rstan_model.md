# Load or compile an RStan model using a persistent cache

Load or compile an RStan model using a persistent cache

## Usage

``` r
load_rstan_model(model_path, verbose = FALSE, stan_model_fn = rstan_stan_model)
```

## Arguments

- model_path:

  Path to a Stan program.

- verbose:

  Logical. give status messages?

- stan_model_fn:

  Internal function used to compile the Stan model.

## Value

A list containing the `stanmodel`, compilation time, compilation output,
and cache file.
