# Simulate dissolved oxygen data from input data

Takes input data in the form of a sub-daily time series (`data`) of
DO.sat, depth, temperature, and light, and a daily time series
(`data_daily`) of GPP, ER, and K600 values, and turns these into
simulated DO.obs. Either `data` or `data_daily` should specify a
starting DO.obs value for each day; if in `data`, this takes the form of
a DO.obs column with values on at least the first time point of each day
(all other values are ignored), or if in `data_daily`, this takes the
form of a DO.mod.1 column with one starting DO value per day.

## Usage

``` r
metab_sim(
  specs = specs(mm_name("sim")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, optional =
    "DO.obs"),
  data_daily = mm_data(date, discharge.daily, DO.mod.1, K600.daily, GPP.daily, Pmax,
    alpha, ER.daily, ER20, err.obs.sigma, err.obs.phi, err.proc.sigma, err.proc.phi,
    optional = "all"),
  info = NULL
)
```

## Arguments

- specs:

  a list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  lists the necessary parameters, describes them in detail, and gives
  default values.

- data:

  A data frame or tibble of input data at the temporal resolution of raw
  observations (unit-value). Columns must have the same names, units,
  and format as the default. The solar.time column must also have a
  timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  A data frame or tibble containing inputs with a daily timestep. See
  the **'Formatting `data_daily`'** section below for a full
  description.

- info:

  any information, in any format, that you would like to store within
  the metab_model object

## Value

A metab_sim object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md),
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md),
[`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md)

## Examples

``` r
## simulations with variation all at sub-daily scale
# prepare input data (DO used only to pick first DO of each day)
dat <- data_metab('3', res='15')
dat_daily <- data.frame(date=as.Date(paste0("2012-09-", 18:20)),
  GPP.daily=2, ER.daily=-3, K600.daily=21, stringsAsFactors=FALSE)

# define simulation parameters
mm <- metab_sim(
  specs(mm_name('sim'), err_obs_sigma=0.1, err_proc_sigma=2,
    GPP_daily=NULL, ER_daily=NULL, K600_daily=NULL),
  data=dat, data_daily=dat_daily)
# actual simulation happens during prediction - different each time
get_params(mm)
#>         date K600.daily GPP.daily ER.daily err.obs.sigma err.obs.phi
#> 1 2012-09-18         21         2       -3           0.1           0
#> 2 2012-09-19         21         2       -3           0.1           0
#> 3 2012-09-20         21         2       -3           0.1           0
#>   err.proc.sigma err.proc.phi discharge.daily
#> 1              2            0        19.13472
#> 2              2            0        22.90962
#> 3              2            0        20.21814
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl> <lgl>    <lgl>    <lgl>    <chr>   
#> 1 2012-09-18     2 NA        NA           -3 NA       NA       NA       ""      
#> 2 2012-09-19     2 NA        NA           -3 NA       NA       NA       ""      
#> 3 2012-09-20     2 NA        NA           -3 NA       NA       NA       ""      
#> # ℹ 1 more variable: errors <chr>
predict_DO(mm)[seq(1,50,by=10),]
#> # A tibble: 5 × 9
#>   date       solar.time          DO.sat depth temp.water light DO.pure DO.mod
#>   <date>     <dttm>               <dbl> <dbl>      <dbl> <dbl>   <dbl>  <dbl>
#> 1 2012-09-18 2012-09-18 04:05:58   9.08  0.16       3.6     0     8.41   8.41
#> 2 2012-09-18 2012-09-18 06:35:58   9.17  0.16       3.23  361.    8.07   8.12
#> 3 2012-09-18 2012-09-18 09:05:58   8.64  0.16       5.51 1339.    8.87   9.09
#> 4 2012-09-18 2012-09-18 11:35:58   7.78  0.16       9.68 1779.    9.11   9.37
#> 5 2012-09-18 2012-09-18 14:05:58   7.31  0.16      12.4  1499.    8.49   8.65
#> # ℹ 1 more variable: DO.obs <dbl>
predict_DO(mm)[seq(1,50,by=10),]
#> # A tibble: 5 × 9
#>   date       solar.time          DO.sat depth temp.water light DO.pure DO.mod
#>   <date>     <dttm>               <dbl> <dbl>      <dbl> <dbl>   <dbl>  <dbl>
#> 1 2012-09-18 2012-09-18 04:05:58   9.08  0.16       3.6     0     8.41   8.41
#> 2 2012-09-18 2012-09-18 06:35:58   9.17  0.16       3.23  361.    8.07   7.99
#> 3 2012-09-18 2012-09-18 09:05:58   8.64  0.16       5.51 1339.    8.87   8.41
#> 4 2012-09-18 2012-09-18 11:35:58   7.78  0.16       9.68 1779.    9.11   8.81
#> 5 2012-09-18 2012-09-18 14:05:58   7.31  0.16      12.4  1499.    8.49   8.49
#> # ℹ 1 more variable: DO.obs <dbl>

# or same each time if seed is set
mm@specs$sim_seed <- 236
predict_DO(mm)$DO.obs[seq(1,50,by=10)]
#> [1] 8.514605 8.226884 9.242439 9.682286 8.660459
predict_DO(mm)$DO.obs[seq(1,50,by=10)]
#> [1] 8.514605 8.226884 9.242439 9.682286 8.660459

# fancy GPP equation
dat_daily <- data.frame(date=as.Date(paste0("2012-09-", 18:20)),
  Pmax=8, alpha=0.01, ER.daily=-3, K600.daily=21, stringsAsFactors=FALSE)
mm <- metab_sim(
  specs(mm_name('sim', GPP_fun='satlight'), err_obs_sigma=0.1, err_proc_sigma=2,
    Pmax=NULL, alpha=NULL, ER_daily=NULL, K600_daily=NULL),
  data=dat, data_daily=dat_daily)
get_params(mm)
#>         date K600.daily Pmax alpha ER.daily err.obs.sigma err.obs.phi
#> 1 2012-09-18         21    8  0.01       -3           0.1           0
#> 2 2012-09-19         21    8  0.01       -3           0.1           0
#> 3 2012-09-20         21    8  0.01       -3           0.1           0
#>   err.proc.sigma err.proc.phi discharge.daily
#> 1              2            0        19.36654
#> 2              2            0        19.95024
#> 3              2            0        21.22849
predict_metab(mm) # metab estimates are for data without errors
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl> <lgl>     <lgl>     <dbl> <lgl>    <lgl>    <lgl>    <chr>   
#> 1 2012-09-18  3.18 NA        NA           -3 NA       NA       NA       ""      
#> 2 2012-09-19  3.17 NA        NA           -3 NA       NA       NA       ""      
#> 3 2012-09-20  3.15 NA        NA           -3 NA       NA       NA       ""      
#> # ℹ 1 more variable: errors <chr>
predict_DO(mm)[seq(1,50,by=10),]
#> # A tibble: 5 × 9
#>   date       solar.time          DO.sat depth temp.water light DO.pure DO.mod
#>   <date>     <dttm>               <dbl> <dbl>      <dbl> <dbl>   <dbl>  <dbl>
#> 1 2012-09-18 2012-09-18 04:05:58   9.08  0.16       3.6     0     8.41   8.41
#> 2 2012-09-18 2012-09-18 06:35:58   9.17  0.16       3.23  361.    8.27   8.21
#> 3 2012-09-18 2012-09-18 09:05:58   8.64  0.16       5.51 1339.    9.94  10.2 
#> 4 2012-09-18 2012-09-18 11:35:58   7.78  0.16       9.68 1779.    9.91  10.1 
#> 5 2012-09-18 2012-09-18 14:05:58   7.31  0.16      12.4  1499.    9.20   9.39
#> # ℹ 1 more variable: DO.obs <dbl>

## simulations with variation at both sub-daily and multi-day scales
sp <- specs(mm_name('sim', pool_K600='none'),
  K600_daily = function(n, ...) pmax(0, rnorm(n, 10, 3))) # n is available within sim models
mm <- metab(sp, dat)
get_params(mm)
#> # A tibble: 3 × 9
#>   date       K600.daily GPP.daily ER.daily err.obs.sigma err.obs.phi
#>   <date>          <dbl>     <dbl>    <dbl>         <dbl>       <dbl>
#> 1 2012-09-18      11.0      0.872    -7.14          0.01           0
#> 2 2012-09-19       8.87     6.16     -6.99          0.01           0
#> 3 2012-09-20       9.81     6.69      0             0.01           0
#> # ℹ 3 more variables: err.proc.sigma <dbl>, err.proc.phi <dbl>,
#> #   discharge.daily <dbl>
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper     ER ER.lower ER.upper msgs.fit
#>   <date>     <dbl> <lgl>     <lgl>      <dbl> <lgl>    <lgl>    <lgl>   
#> 1 2012-09-18  5.76 NA        NA         -1.80 NA       NA       NA      
#> 2 2012-09-19  5.48 NA        NA        -10.9  NA       NA       NA      
#> 3 2012-09-20 17.9  NA        NA         -4.70 NA       NA       NA      
#> # ℹ 2 more variables: warnings <chr>, errors <chr>

## K~Q model
dat <- data_metab('10','15')
sp <- specs(mm_name('sim', pool_K600='binned'))
mm <- metab(sp, dat)
pars <- get_params(mm)
attr(pars, 'K600_eqn')
#> $K600_lnQ_nodes_centers
#> [1] 2.7 2.9 3.1 3.3
#> 
#> $K600_lnQ_cnode_meanlog
#>  [1] 1.791759 1.791759 1.791759 1.791759 1.791759 1.791759 1.791759 1.791759
#>  [9] 1.791759 1.791759
#> 
#> $K600_lnQ_cnode_sdlog
#>  [1] 1 1 1 1 1 1 1 1 1 1
#> 
#> $K600_lnQ_nodediffs_meanlog
#>  [1] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
#> 
#> $K600_lnQ_nodediffs_sdlog
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#> 
#> $lnK600_lnQ_nodes
#> [1] 1.8055839 1.7903077 1.2187666 0.6656684
#> 
#> $K600_daily_predlog
#>  [1] 1.2645783 1.6185911 0.9744718 1.7094699 1.3963858 1.7913175 0.9430280
#>  [8] 1.4406586 0.9215569 1.4184119
#> 

if (FALSE) { # \dontrun{
plot_DO_preds(predict_DO(mm))
plot_DO_preds(mm)
library(ggplot2)
} # }
```
