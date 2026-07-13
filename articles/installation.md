# Installation

## Installing streamMetabolizer

To install the `streamMetabolizer` package, use the `remotes` package
(running `install.packages('remotes')` first if needed). To use
`remotes::install_github()` it is convenient to set a [GitHub Personal
Access Token
(PAT)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).
There are [several
methods](https://usethis.r-lib.org/articles/git-credentials.html) for
setting your PATs within R; the simplest is to call
\`Sys.setenv(GITHUB_PAT=“yyyy”), replacing yyyy with the PAT you
established on the GitHub website.

You can install the most cutting edge version of streamMetabolizer with
this command:

``` r

remotes::install_github(
  "USGS-R/streamMetabolizer", # soon to be "DOI-USGS/streamMetabolizer"
  build_vignettes = TRUE)
```

### Software dependencies for Bayesian models

Bayesian models require a Stan backend, provided by either the `rstan`
or `cmdstanr` packages. Installation of these packages is rarely as
simple as a call to
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html).
See the [RStan installation
guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) or
the [CmdStanR installation
guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) for
up-to-date instructions, which differ by operating system. Select the
backend with `specs(..., stan_engine = "rstan")` or
`specs(..., stan_engine = "cmdstanr")`.

`streamMetabolizer` caches compiled models outside the installed package
so they can be reused when the package library is read-only. Advanced
users can override the cache locations with the
`streamMetabolizer.rstan_cache_dir` and
`streamMetabolizer.cmdstan_cache_dir` options.
