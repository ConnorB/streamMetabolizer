# Bayesian Models

This page provides details on how to get the best results from a
Bayesian metabolism model. See
[Quickstart](https://connorb.github.io/streamMetabolizer/dev/articles/get_started.md)
for an example of preparing and fitting a Bayesian model.

## Why Bayesian models?

Bayesian models are slower than the alternatives, but they offer several
advantages: \* They can be state-space models, including both
observation error and process error. \* They can be hierarchically
structured, allowing you to use information from many days of data to
inform estimates on each individual day. \* They produce more accurate
and nuanced uncertainty estimates.

## Configuring Bayesian models

The structure and specifications of Bayesian models require special
attention. For more information, see the help file at
[`?specs`](https://connorb.github.io/streamMetabolizer/dev/reference/specs.md),
especially the `Relevant arguments` section and the parameter
definitions. Also see the vignette called ‘fit_fix_k’.

## Inspecting Bayesian models

### Stan MCMC information

After fitting a `streamMetabolizer` model, retrieve the fitted Stan
object with `get_mcmc(mm)`, where `mm` is your fitted metabolism model.
With `stan_engine = "rstan"`, this is an RStan `stanfit` object. Useful
first checks include
[`rstan::traceplot()`](https://mc-stan.org/rstan/reference/stanfit-method-traceplot.html),
`rstan::summary()`,
[`rstan::stan_diag()`](https://mc-stan.org/rstan/reference/stan_plot_diagnostics.html),
and
[`rstan::get_sampler_params()`](https://mc-stan.org/rstan/reference/stanfit-class.html);
`inc_warmup = TRUE` is often useful when diagnosing a trace plot. See
[`?rstan::stanfit`](https://mc-stan.org/rstan/reference/stanfit-class.html)
for the full interface.

With `stan_engine = "cmdstanr"`, `get_mcmc(mm)` returns a `CmdStanMCMC`
object; use its `$draws()`, `$summary()`, and `$diagnostic_summary()`
methods.

### Model warnings and errors

Unlike other model types in streamMetabolizer, Bayesian models sometimes
have overall warnings and errors not specific to any one day. If there
are any, you will see a note in the ‘warnings’ or ‘errors’ columns of
the model printout, and you can see the full message\[s\] as elements in
the list returned by
[`get_fit()`](https://connorb.github.io/streamMetabolizer/dev/reference/get_fit.md).
There’s also a \[smaller\] possibility of errors or warnings directly
associated with metabolism prediction rather than fitting, inspectable
in the ‘warnings’ and ‘errors’ columns of the data.frame returned by
[`predict_metab()`](https://connorb.github.io/streamMetabolizer/dev/reference/predict_metab.md).

``` r

# here's where you'd find fitting messages:
select(get_params(mm), warnings, errors)
get_fit(mm)$warnings
get_fit(mm)$errors

# and prediction messages
select(predict_metab(mm), warnings, errors)
```
