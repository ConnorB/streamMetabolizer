# Compress warnings and errors in to a single column

Compress two columns of warning and error messages into one short-hand
column

## Usage

``` r
compress_msgs(
  ddat,
  colname = "messages",
  warnings.overall = c(),
  errors.overall = c()
)
```

## Arguments

- ddat:

  a data.frame including warnings and errors columns

- colname:

  the name of the column where the summary should be placed

- warnings.overall:

  any general warnings (for the whole model) to be included in the
  summary

- errors.overall:

  any general errors (for the whole model) to be included in the summary
