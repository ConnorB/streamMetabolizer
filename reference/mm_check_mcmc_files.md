# Check the syntax of all Bayesian model files in the package

Check the syntax of all Bayesian model files in the package

## Usage

``` r
mm_check_mcmc_files(grep_pattern, stan_engine = c("rstan", "cmdstanr"))
```

## Arguments

- grep_pattern:

  String on which to filter the names if only some should be checked.
  fixed=FALSE.

- stan_engine:

  Character string specifying whether to check with RStan or CmdStanR.

## Examples

``` r
if (FALSE) { # interactive()
# takes a long time, so run only when needed
checks <- streamMetabolizer:::mm_check_mcmc_files()
saveRDS(checks, file='temp/bayes_model_checks.Rds')
checks <- streamMetabolizer:::mm_check_mcmc_files("*ko\\.stan")
checks <- streamMetabolizer:::mm_check_mcmc_files("b_np_.*_ko\\.stan")
checks <- streamMetabolizer:::mm_check_mcmc_files(
  "b_np_.*_ko\\.stan", stan_engine = "cmdstanr"
)
cat(checks[[7]])
}
```
