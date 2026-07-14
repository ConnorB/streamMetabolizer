# Look for a model file

Looks first in the models folder of the streamMetabolizer package,
second along the relative or absolute file path given by model_name

## Usage

``` r
mm_locate_filename(
  model_name,
  stan_engine = NULL,
  stan_version_fn = stan_version_for_engine
)
```

## Arguments

- model_name:

  a model file in the 'models' folder of the streamMetabolizer package
  or a relative or absolute file path of a model file

- stan_engine:

  The Stan interface whose compiler version should be validated. If
  `NULL`, CmdStanR is preferred when available, followed by RStan.

- stan_version_fn:

  Internal function used to determine the Stan version.

## Value

a file path if the file exists or an error otherwise
