# Find the Stan compiler version used by an interface

Find the Stan compiler version used by an interface

## Usage

``` r
stan_version_for_engine(stan_engine = NULL)
```

## Arguments

- stan_engine:

  The Stan interface whose compiler version should be validated. If
  `NULL`, CmdStanR is preferred when available, followed by RStan.

## Value

A numeric version, or `NA` when the requested interface is not available
and configured.
