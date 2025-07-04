---
title: "Installation"
author: "Alison Appling"
date: "2025-06-30"
output:
  # rmarkdown::github_document
  rmarkdown::html_vignette:
      keep_md: true
vignette: >
  %\VignetteIndexEntry{Installation}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Installing streamMetabolizer

To install the `streamMetabolizer` package, use the `remotes` package (running `install.packages('remotes')` first if needed). To use `remotes::install_github()` it is convenient to set a [GitHub Personal Access Token (PAT)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens). There are [several methods](https://usethis.r-lib.org/articles/git-credentials.html) for setting your PATs within R; the simplest is to call `Sys.setenv(GITHUB_PAT="yyyy"),
replacing yyyy with the PAT you established on the GitHub website.

You may first need to install the `unitted` dependency:

``` r
remotes::install_github('appling/unitted')
```

You can then install the most cutting edge version of streamMetabolizer with this command:

``` r
remotes::install_github(
  "USGS-R/streamMetabolizer", # soon to be "DOI-USGS/streamMetabolizer"
  build_vignettes = TRUE)
```

## Software dependencies for Bayesian models

Bayesian models require a Stan backend, provided by either the `rstan` or `cmdstanr` packages. Installation of these packages is rarely as simple as a call to `install.packages()`. See the [rstan wiki](https://github.com/stan-dev/rstan/wiki) or the [cmdstanr installation guide](https://mc-stan.org/cmdstanr/articles/quick-start.html) for up-to-date instructions, which differ by operating system.
