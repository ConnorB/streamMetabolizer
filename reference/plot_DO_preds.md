# Plot dissolved oxygen predictions

Plots modeled values as lines, observed values as points

## Usage

``` r
plot_DO_preds(
  DO_preds,
  y_var = c("conc", "pctsat", "ddodt"),
  style = c("ggplot2", "dygraphs"),
  y_lim = list(conc = c(NA, NA), pctsat = c(NA, NA), ddodt = c(NA, NA)),
  date_start = NA,
  date_end = NA,
  use_saved = TRUE
)
```

## Arguments

- DO_preds:

  A data.frame of predictions such as that returned by predict_DO().

- y_var:

  Character. Should the plot display predicted & observed values in
  concentration (conc) or as percent of saturation (pctsat)? The default
  is to plot both.

- style:

  Character indicating which graphics package to use.

- y_lim:

  List of named vectors, each of which has length 2 and is numeric and
  has a name in the possible values of y_var. NA within a vector
  indicates that the data range should be used. for ggplot2, y_lim is
  only used to exclude values outside that range and is ignored if the
  data span a narrower range.

- date_start:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The first date
  (inclusive) for which to report DO predictions. If `NA`, no filtering
  is done.

- date_end:

  A `Date` or an object coercible with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). The last date
  (inclusive) for which to report DO predictions. If `NA`, no filtering
  is done.

- use_saved:

  A logical. Is it OK to use predictions that were saved with the model?

## Value

A ggplot object when `style = "ggplot2"` or a dygraph object when
`style = "dygraphs"`.

## Examples

``` r
if (FALSE) { # interactive()
mm <- metab_night(specs(mm_name('night')), data=data_metab('3', day_start=12, day_end=36))
plot_DO_preds(mm)
plot_DO_preds(mm, date_start='2012-09-19', date_end='2012-09-19')
plot_DO_preds(mm, style='dygraphs', y_var='pctsat')
}
```
