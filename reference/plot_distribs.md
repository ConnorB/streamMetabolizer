# Plot the prior/posterior distributions of a parameter

Plot prior and posterior distributions implied by a `specs` list or
fitted metabolism model.

## Usage

``` r
plot_distribs(
  dist_data,
  parname = c("GPP_daily", "alpha", "Pmax", "ER_daily", "K600_daily",
    "K600_daily_meanlog", "lnK600_lnQ_intercept", "lnK600_lnQ_slope", "K600_lnQ_nodes",
    "K600_daily_sdlog", "K600_daily_sigma", "err_obs_iid_sigma", "err_proc_acor_phi",
    "err_proc_acor_sigma", "err_proc_iid_sigma", "err_mult_GPP_sdlog"),
  index = TRUE,
  style = c("dygraphs", "ggplot2")
)
```

## Arguments

- dist_data:

  Either a specs list (for priors only) or a metab_model object (for
  both priors and posteriors).

- parname:

  Character. the name of the parameter whose distribution(s) you wish to
  plot.

- index:

  Integer or logical. Applicable only if plotting posteriors, and useful
  only if the parname is for a parameter having multiple (e.g., daily)
  instances. In this case, the index selects the instance and
  corresponds to the row number in the data.frame element of
  `get_fit(metab_model)` that contains the parameter, e.g.
  `get_fit(metab_model)$daily` for `'GPP_daily'`. The default, TRUE,
  selects and pools all instances of the parameter.

- style:

  Character indicating which graphics package to use.

## Value

A ggplot object when `style = "ggplot2"` or a dygraph object when
`style = "dygraphs"`.

## Examples

``` r
if (FALSE) { # interactive()
# priors only
plot_distribs(specs('bayes', K600_daily_mu=30), 'K600_daily')

# posteriors, too
mm <- metab(specs(mm_name('bayes')), data=data_metab('1', res='30'))
plot_distribs(mm, 'GPP_daily', 1)

# with modifications
plot_distribs(mm, 'err_proc_iid_sigma') |>
  dygraphs::dyRangeSelector(dateWindow=c(-0.1,1.3)) |>
  dygraphs::dyAxis(name='y', valueRange=c(0,15))
}
```
