# Format MCMC output into a one-row data.frame

For split_dates models. Formats output into a one-row data.frame for
row-binding with other such data.frames

## Usage

``` r
format_mcmc_mat_split(
  mcmc_mat,
  names_params,
  names_stats,
  keep_mcmc,
  runmcmc_out
)
```

## Arguments

- mcmc_mat:

  matrix as extracted from Stan

- names_params:

  character vector of the names of the parameters

- names_stats:

  character vector of the names of the statistics
