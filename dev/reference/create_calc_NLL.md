# Create a function to compute the negative log likelihood of a set of metabolism parameter values

Produces a function that can be given to nlm(). K600.daily may be passed
to that function within the p vector (for fitting K600.daily) or as an
additional argument (for fixing it).

## Usage

``` r
create_calc_NLL(
  calc_DO,
  par.names = environment(environment(calc_DO)$calc_dDOdt)$metab.needs,
  err_obs_iid = FALSE,
  err_proc_iid = FALSE
)
```

## Arguments

- calc_DO:

  A function as from `create_calc_DO`.

- par.names:

  Vector of names of parameters that can be expected in calls to the
  function created by this one (the calc_NLL function).

- err_obs_iid:

  A logical. Should IID observation error be included? If not, the model
  will be fit to the differences in successive DO measurements, rather
  than to the DO measurements themselves.

- err_proc_iid:

  A logical. Should IID process error be included?

## Value

A function that will return a negative log likelihood of the data given
a set of metab.pars. metab.pars is the first argument of the returned
function; its names are defined in `par.names`.

## Examples

``` r
data <- data_metab('1','30')[seq(1,48,by=2),]
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt)
NLL <- create_calc_NLL(DO, err_obs_iid=TRUE)
NLL(metab.pars=c(GPP.daily=2, ER.daily=-2, K600.daily=25))
#> [1] 1.60391
NLL(metab.pars=c(GPP.daily=2, ER.daily=-2, K600.daily=25), DO.mod.1=8)
#> [1] 3.86777
NLL(metab.pars=c(GPP.daily=4, ER.daily=-7, K600.daily=15))
#> [1] 52.37003
NLL2 <- create_calc_NLL(DO, par.names=c('GPP.daily','ER.daily'), err_obs_iid=TRUE)
NLL2(metab.pars=c(GPP.daily=2, ER.daily=-2), K600.daily=25)
#> [1] 1.60391
NLL3 <- create_calc_NLL(DO,
  par.names=c('GPP.daily','ER.daily','K600.daily','DO.mod.1'), err_obs_iid=TRUE)
NLL3(metab.pars=c(GPP.daily=2, ER.daily=-2, K600.daily=25, DO.mod.1=9))
#> [1] 3.551421
nlm(NLL, p=c(GPP.daily=2, ER.daily=-2, K600.daily=25))
#> $minimum
#> [1] -8.833364
#> 
#> $estimate
#> [1]  2.829716 -2.121566 31.265547
#> 
#> $gradient
#> [1] -1.060271e-06 -6.338252e-07  3.692985e-08
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 21
#> 
nlm(NLL2, p=c(GPP.daily=2, ER.daily=-2), K600.daily=31.265)
#> $minimum
#> [1] -8.833364
#> 
#> $estimate
#> [1]  2.829662 -2.121524
#> 
#> $gradient
#> [1] -6.277629e-10  1.465279e-07
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 9
#> 
nlm(NLL3, p=c(GPP.daily=2, ER.daily=-2, K600.daily=25, DO.mod.1=9))
#> $minimum
#> [1] -8.837646
#> 
#> $estimate
#> [1]  2.824270 -2.118702 31.201832  8.424478
#> 
#> $gradient
#> [1] -3.422808e-06  2.424703e-06  5.524031e-07  9.581324e-07
#> 
#> $code
#> [1] 2
#> 
#> $iterations
#> [1] 22
#> 
```
