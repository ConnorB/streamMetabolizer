# Generate MCMC code files with all of the desired combinations of features

This function gets run on package build and creates every model within
the set of factorial combinations of arguments to mm_generate_mcmc_file,
with the exception of the one pair of incompatible arguments
(err_obs_iid=F && deficit_src='DO_mod')

## Usage

``` r
mm_generate_mcmc_files()
```
