# Generate outputs using Bob's code for comparison

Bob's code includes MLE and nighttime regression models. This function
generates the output from those models, keeping the code as much intact
as possible. The exception is that we're using solar.time rather than
local.time, for consistency with streamMetabolizer's recommendations

## Usage

``` r
load_french_creek_std_mle(
  french,
  K = 35,
  estimate = c("PRK", "K", "PR"),
  start = c(dates = "08/23/12", times = "22:00:00"),
  end = c(dates = "08/25/12", times = "06:00:00"),
  plot = FALSE
)
```

## Arguments

- french:

  The French Creek dataset.

- K:

  Optional. If specified, a number for the K600 to assume (units of
  1/d).

- estimate:

  Character indicating the type of model to fit.

- start:

  A character vector specifying the time at which the 'day' (the time
  period to use in producing an estimate for a single date) begins. The
  vector should have 2 elements, dates and times, to pass to chron().

- end:

  A character vector specifying the time at which the 'day' ends. The
  vector should have 2 elements, dates and times, to pass to chron().

- plot:

  Logical - should plots be produced?

## Details

This function requires the `chron` package, which is only suggested
rather than required for the `streamMetabolizer` package. If you wish to
run this function, ensure that `chron` is installed or install it with
`install.packages('chron')`.
