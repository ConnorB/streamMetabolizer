# Select scalar or indexed CmdStan draws for a distribution plot

Select scalar or indexed CmdStan draws for a distribution plot

## Usage

``` r
select_cmdstan_draws(draws_array, parname, index = TRUE)
```

## Arguments

- draws_array:

  A CmdStanR `draws_array`.

- parname:

  The base parameter name.

- index:

  integer or logical. Applicable only if plotting posteriors, and useful
  only if the parname is for a parameter having multiple (e.g., daily)
  instances. In this case, the index selects the instance and
  corresponds to the row number in the data.frame element of
  `get_fit(metab_model)` that contains the parameter, e.g.
  `get_fit(metab_model)$daily` for `'GPP_daily'`. The default, TRUE,
  selects and pools all instances of the parameter.

## Value

A list containing a numeric vector of draws and a logical indicating
whether the parameter is indexed.
