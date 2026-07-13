# Describe the requirements for an argument to metab()

Describe the requirements for an argument to metab()

## Usage

``` r
metab_inputs(
  type = c("bayes", "mle", "night", "Kmodel", "sim"),
  input = c("specs", "data", "data_daily", "info")
)
```

## Arguments

- type:

  the type of model you want to fit

- input:

  the name of an argument to pass into metab()

## Examples

``` r
metab_inputs('night','specs')
#> [1] "specs(mm_name('night')) # see ?mm_name, ?mm_specs for more options"
metab_inputs('bayes','data')
#>      colname          class units     need
#> 1 solar.time POSIXct,POSIXt  <NA> required
#> 2     DO.obs        numeric  <NA> required
#> 3     DO.sat        numeric  <NA> required
#> 4      depth        numeric  <NA> required
#> 5 temp.water        numeric  <NA> required
#> 6      light        numeric  <NA> required
#> 7  discharge        numeric  <NA> optional
metab_inputs('Kmodel','data_daily')
#>            colname   class units     need
#> 1             date    Date  <NA> required
#> 2       K600.daily numeric  <NA> required
#> 3 K600.daily.lower numeric  <NA> optional
#> 4 K600.daily.upper numeric  <NA> optional
#> 5  discharge.daily numeric  <NA> optional
#> 6   velocity.daily numeric  <NA> optional
metab_inputs('mle','info')
#> [1] "info may be NULL, a list, or any other data you want to attach to the output of metab()"
```
