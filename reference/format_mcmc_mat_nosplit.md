# Format MCMC output into a list of data.frames

For multi-day or unsplit models. Formats output into a list of
data.frames, one per unique number of nodes sharing a variable name

## Usage

``` r
format_mcmc_mat_nosplit(
  mcmc_mat,
  data_list_d,
  data_list_n,
  model_name,
  keep_mcmc,
  runmcmc_out
)
```

## Arguments

- mcmc_mat:

  matrix as extracted from Stan
