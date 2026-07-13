# Simulate a lnK ~ lnQ relationship in the Kb format

Uses linear interpolation among "nodes" (lnQ, lnK points) to describe
the lnK ~ lnQ relationship

## Usage

``` r
sim_Kb(
  K600_lnQ_nodes_centers,
  K600_lnQ_cnode_meanlog,
  K600_lnQ_cnode_sdlog,
  K600_lnQ_nodediffs_meanlog,
  K600_lnQ_nodediffs_sdlog
)
```

## Arguments

- K600_lnQ_nodes_centers:

  data configuration argument for pool_K600='binned'. numeric vector
  giving the natural-log-space centers of the discharge bins. See also
  [`calc_bins()`](https://connorb.github.io/streamMetabolizer/reference/calc_bins.md)

- K600_lnQ_cnode_meanlog:

  For a sim model with pool_K600='binned'. The mean of a lognormal
  distribution describing the y=K600 value of the middle (or just past
  middle) node in the piecewise lnK ~ lnQ relationship

- K600_lnQ_cnode_sdlog:

  For a sim model with pool_K600='binned'. The sd of a lognormal
  distribution describing the y=K600 value of the middle (or just past
  middle) node in the piecewise lnK ~ lnQ relationship

- K600_lnQ_nodediffs_meanlog:

  For a sim model with pool_K600='binned'. The average (in log space)
  difference between ln(K) values of successive nodes. A non-zero value
  introduces a trend in K ~ Q.

- K600_lnQ_nodediffs_sdlog:

  hyperparameter for pool_K600='binned'. The standard deviations of the
  differences in estimated K600 between successive lnQ_nodes (bins),
  where the means of those differences are always zero
