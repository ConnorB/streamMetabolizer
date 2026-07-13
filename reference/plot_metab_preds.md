# Plot predictions produced with predict_DO

Plots modeled values as lines, observed values as points

## Usage

``` r
plot_metab_preds(
  metab_preds,
  y_var = c("GPP", "ER"),
  style = c("ggplot2"),
  y_lim = list(GPP = c(NA, NA), ER = c(NA, NA))
)
```

## Arguments

- metab_preds:

  a data.frame of predictions such as that returned by predict_metab()

- y_var:

  character. Should the plot display predicted values of GPP and/or ER?
  The default is to plot both.

- style:

  character indicating which graphics package to use

- y_lim:

  list of named vectors, each of which has length 2 and is numeric and
  has a name in the possible values of y_var. NA within a vector
  indicates that the data range should be used. for ggplot2, y_lim is
  only used to exclude values outside that range and is ignored if the
  data span a narrower range

## Examples

``` r
if (FALSE) { # \dontrun{
mm <- metab_night(specs(mm_name('night')), data=data_metab('10', day_start=12, day_end=36))
plot_metab_preds(mm)
} # }
```
