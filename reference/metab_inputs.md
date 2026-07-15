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

## Value

For `data` and `data_daily`, a tibble describing the required columns
when the selected model accepts that input. Otherwise, returns `NULL`
invisibly after displaying guidance with
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).

## Examples

``` r
metab_inputs('night','specs')
#> ℹ Use `specs(mm_name('night'))`.
#> • See `mm_name()` (`?streamMetabolizer::mm_name()`) and `specs()`
#>   (`?streamMetabolizer::specs()`) for more options.
metab_inputs('bayes','data')
#> # A tibble: 7 × 4
#>   colname    class          units            need    
#>   <chr>      <chr>          <chr>            <chr>   
#> 1 solar.time POSIXct,POSIXt ""               required
#> 2 DO.obs     numeric        "mgO_2 L^-1"     required
#> 3 DO.sat     numeric        "mgO_2 L^-1"     required
#> 4 depth      numeric        "m"              required
#> 5 temp.water numeric        "°C"             required
#> 6 light      numeric        "umol m^-2 s^-1" required
#> 7 discharge  numeric        "m^3 s^-1"       optional
metab_inputs('Kmodel','data_daily')
#> # A tibble: 6 × 4
#>   colname          class   units      need    
#>   <chr>            <chr>   <chr>      <chr>   
#> 1 date             Date    ""         required
#> 2 K600.daily       numeric "d^-1"     required
#> 3 K600.daily.lower numeric "d^-1"     optional
#> 4 K600.daily.upper numeric "d^-1"     optional
#> 5 discharge.daily  numeric "m^3 s^-1" optional
#> 6 velocity.daily   numeric "m s^-1"   optional
metab_inputs('mle','info')
#> ℹ `info` is optional metadata stored in the returned `metab_model()`
#>   (`?streamMetabolizer::metab_model()`).
#> • Use `NULL` (the default) or any R object, then retrieve it with `get_info()`
#>   (`?streamMetabolizer::get_info()`).
```
