# Print metab logs

Print metab model compilation and/or fitting logs

## Usage

``` r
# S3 method for class 'logs_metab'
print(x, ...)
```

## Arguments

- x:

  An object to print.

- ...:

  Ignored; included only for compatibility with
  [`base::print`](https://rdrr.io/r/base/print.html).

## Value

`x`, invisibly.

## Examples

``` r
print(structure("Sampling complete", class = "logs_metab"))
#> Sampling complete
```
