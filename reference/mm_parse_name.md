# Parse a model name into its features

Returns a data frame with one column per model structure detail and one
row per `model_name`. See
[`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
for a description of each column.

## Usage

``` r
mm_parse_name(model_name, expand = FALSE)
```

## Arguments

- model_name:

  Character: the model name.

- expand:

  Logical: should additional columns such as model_name and
  pool_K600_type be added? If expand=TRUE then the result cannot be
  passed directly back into mm_name, but the additional columns may be
  helpful for interpreting the model structure.

## Value

A data frame with one row per model name and one column per model
feature.

## Details

Custom model files (for MCMC) may have additional characters after an
underscore at the end of the name and before the prefix. For example,
'b_np_pcpi_eu_ko.stan' and 'b_np_pcpi_eu_ko_v2.stan' are parsed the
same; the \_v2 is ignored by this function.

## See also

The converse of this function is
[`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md).

## Examples

``` r
mm_parse_name(c(mm_name('mle'), mm_name('night'), mm_name('bayes')))
#>    type pool_K600 err_obs_iid err_proc_acor err_proc_acor_light err_proc_iid
#> 1   mle      none        TRUE         FALSE               FALSE        FALSE
#> 2 night      none       FALSE         FALSE               FALSE         TRUE
#> 3 bayes      none        TRUE         FALSE               FALSE         TRUE
#>   err_proc_GPP ode_method  GPP_fun   ER_fun   deficit_src engine
#> 1        FALSE  trapezoid linlight constant        DO_mod    nlm
#> 2        FALSE      euler       NA constant DO_obs_filter     lm
#> 3        FALSE  trapezoid linlight constant        DO_mod   stan
mm_parse_name(c(mm_name('mle'), mm_name('night'), mm_name('bayes')), expand=TRUE)
#>                 model_name  type pool_K600 pool_K600_type pool_K600_sd
#> 1    m_np_oi_tr_plrckm.nlm   mle      none           none        fixed
#> 2       n_np_pi_eu_rckf.lm night      none           none        fixed
#> 3 b_np_oipi_tr_plrckm.stan bayes      none           none        fixed
#>   err_obs_iid err_proc_acor err_proc_acor_light err_proc_iid err_proc_GPP
#> 1        TRUE         FALSE               FALSE        FALSE        FALSE
#> 2       FALSE         FALSE               FALSE         TRUE        FALSE
#> 3        TRUE         FALSE               FALSE         TRUE        FALSE
#>   ode_method  GPP_fun   ER_fun   deficit_src engine
#> 1  trapezoid linlight constant        DO_mod    nlm
#> 2      euler       NA constant DO_obs_filter     lm
#> 3  trapezoid linlight constant        DO_mod   stan
```
