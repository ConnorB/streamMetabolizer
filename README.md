# streamMetabolizer: Models for Estimating Aquatic Photosynthesis and Respiration
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![USGS Status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)
<!-- badges: end -->

This is a fork of the original [`streamMetabolizer`](https://github.com/DOI-USGS/streamMetabolizer)

The `streamMetabolizer` R package uses inverse modeling to estimate aquatic
photosynthesis and respiration (collectively, metabolism) from time series
data on dissolved oxygen, water temperature, depth, and light. The package
assists with data preparation, handles data gaps during modeling, and
provides tabular and graphical reports of model outputs. Several
time-honored methods are implemented along with many promising new variants
that produce more accurate and precise metabolism estimates.

This package has been described, with special focus on the Bayesian model options, by
[Appling et al. 2018a](https://doi.org/10.1002/2017JG004140). An application to 356
streams across the U.S. is described in [Appling et al. 2018b](https://doi.org/10.1038/sdata.2018.292).

> Appling, A. P., Hall, R. O., Yackulic, C. B., & Arroita, M. (2018a). Overcoming equifinality: Leveraging long time series for stream metabolism estimation. Journal of Geophysical Research: Biogeosciences, 123(2), 624–645. https://doi.org/10.1002/2017JG004140

> Appling, A. P., Read, J. S., Winslow, L. A., Arroita, M., Bernhardt, E. S., Griffiths, N. A., Hall, R. O., Harvey, J. W., Heffernan, J. B., Stanley, E. H., Stets, E. G., & Yackulic, C. B. (2018b). The metabolic regimes of 356 rivers in the United States. Scientific Data, 5(1), 180292. https://doi.org/10.1038/sdata.2018.292

To see the recommended citation for this package, please run `citation('streamMetabolizer')` at the R prompt.
``` r
citation('streamMetabolizer')
```

## Installation

To install the `streamMetabolizer` package, use the `remotes` package (running `install.packages('remotes')` first if needed). To use `remotes::install_github()` it is convenient to set a [GitHub Personal Access Token (PAT)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens). There are [several methods](https://usethis.r-lib.org/articles/git-credentials.html) for setting your PATs within R; the simplest is to call `Sys.setenv(GITHUB_PAT="yyyy")`,
replacing yyyy with the PAT you established on the GitHub website.

You may first need to install the `unitted` dependency:
``` r
remotes::install_github('appling/unitted')
```

You can then install the most cutting edge version of `streamMetabolizer` with this command:
``` r
remotes::install_github("ConnorB/streamMetabolizer", 
                        build_vignettes = TRUE)
```

### Software dependencies for Bayesian models

Bayesian models require a Stan backend. You may use either the `rstan` or `cmdstanr` packages, and installation of these packages is rarely as simple as a call to `install.packages()`. Start at the [rstan wiki page](https://github.com/stan-dev/rstan/wiki) or the [cmdstanr installation guide](https://mc-stan.org/cmdstanr/articles/quick-start.html) for the most up-to-date instructions.


## Getting started

After installing and loading `streamMetabolizer`, run `vignette()` in R to see tutorials on getting started and customizing your metabolism models.
``` r 
vignette(package='streamMetabolizer')
## displays a list of available vignettes

vignette('get_started', package='streamMetabolizer')
## displays an html or pdf rendering of the 'get_started' vignette
```

## Contributing

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See [CODE_OF_CONDUCT.md](https://github.com/ConnorB/streamMetabolizer/blob/main/CODE_OF_CONDUCT.md) for more information.

For technical details on how to contribute, see [CONTRIBUTING.md](https://github.com/ConnorB/streamMetabolizer/blob/main/CONTRIBUTING.md)


### Development History

`streamMetabolizer` was developed 2015-2018 with support from the USGS Powell Center (through a working group on Continental Patterns of Stream Metabolism), the USGS National Water Quality Program, and the USGS Office of Water Information.


## Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
