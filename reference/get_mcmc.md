# Extract any MCMC model objects that were stored with the model

A function specific to metab_bayes models. Returns the fitted object
from the selected Stan interface: a `stanfit`
([rstan::stanfit](https://mc-stan.org/rstan/reference/stanfit-class.html))
for RStan or a `CmdStanMCMC`
([cmdstanr::CmdStanMCMC](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html))
for CmdStanR. The object is saved in the metab_model by default because
you should almost always inspect it; see the `keep_mcmcs` argument to
[`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
for options for saving space. See the interface documentation for
available summaries, diagnostics, draws, and plotting methods.

## Usage

``` r
get_mcmc(metab_model)

# S3 method for class 'metab_bayes'
get_mcmc(metab_model)
```

## Arguments

- metab_model:

  A Bayesian metabolism model (metab_bayes) from which to return the
  MCMC model object(s).

## Value

The MCMC model object(s).

## Methods (by class)

- `get_mcmc(metab_bayes)`: Get the Bayesian MCMC model object

## Examples

``` r
mm <- metab_model("metab_bayes", mcmc = list(chain = 1))
get_mcmc(mm)
#> $chain
#> [1] 1
#> 
```
