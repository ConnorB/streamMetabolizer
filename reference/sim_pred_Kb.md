# Predict ln(K600) as Kb-style function of discharge

Uses linear interpolation among "nodes" (lnQ, lnK points) to predict
daily values of the natural log of K600, based on the lnK ~ lnQ
relationship specified by `K600_lnQ_nodes_centers` and
`lnK600_lnQ_nodes`

## Usage

``` r
sim_pred_Kb(K600_lnQ_nodes_centers, lnK600_lnQ_nodes, lnQ.daily)
```

## Arguments

- K600_lnQ_nodes_centers:

  data configuration argument for pool_K600='binned'. numeric vector
  giving the natural-log-space centers of the discharge bins. See also
  [`calc_bins()`](https://connorb.github.io/streamMetabolizer/reference/calc_bins.md)

- lnK600_lnQ_nodes:

  For a sim model with pool_K600='binned'. The values of lnK600 at each
  node. The default value of this spec is a function that computes
  lnK600s based on simulated K~Q relationships.

- lnQ.daily:

  vector of daily values of the natural log of discharge, e.g.,
  `log(data_daily$discharge.daily)`
