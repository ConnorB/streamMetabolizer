# Get a parameter from data_daily, specs, or both

Used in get_params.metab_sim. Looks in both data_daily and specs for a
daily paramter, e.g., 'K600.daily'. If it's present in just one place,
those values will be used. If it's present in neither, an error or NULLs
will be returned depending on whether `required=TRUE`.

## Usage

``` r
sim_get_par(par.name, specs, data_daily, eval_env, required = TRUE)
```

## Arguments

- par.name:

  The parameter name. Should be period.separated if that's how
  data_daily is. Periods will be converted to underscores when searching
  specs for the parameter.

- specs:

  A specifications list from which parameter values/functions will be
  drawn.

- data_daily:

  A data.frame of daily values from which parameter values will be
  drawn.

- eval_env:

  An environment containing any parameters that have already been
  finalized, plus the variable `n` containing the number of daily values
  required.

- required:

  Logical. If true and the parameter is unavailable, an error will be
  thrown.

## Value

List containing up to three vectors (or NULLs) named `specs`,
`data_daily`, and `combo` according to the source of the numbers in each
vector.
