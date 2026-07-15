# Convert standard deviation columns to confidence intervals

Convert data with var and var.sd columns into data with var, var.lower,
and var.upper columns

## Usage

``` r
mm_sd_to_ci(data, alpha = 0.05)
```

## Arguments

- data:

  A data.frame with 1+ pairs of columns named var and var.sd (where var
  can be anything).

- alpha:

  The desired significance level described by the confidence interval.
