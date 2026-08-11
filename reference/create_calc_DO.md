# Create a function to compute the numerical integration of a dDOdt function

Create a function to compute the numerical integration of a dDOdt
function

## Usage

``` r
create_calc_DO(
  calc_dDOdt,
  ode_method = environment(calc_dDOdt)$ode_method,
  err.obs = 0
)
```

## Arguments

- calc_dDOdt:

  A function as from `create_calc_dDOdt`.

- ode_method:

  A string specifying the method used to solve the ordinary differential
  equation for DO:

  - `"euler"` (formerly `"Euler"`): Use conditions at the start of each
    timestep.

  - `"trapezoid"` (formerly `"pairmeans"`): Use mean conditions across
    each timestep.

  - For `type = "mle"`, `"rk2"` and methods accepted by
    [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html) are
    also available. Many have not been extensively tested with
    streamMetabolizer models.

- err.obs:

  Optional numerical vector of length nrow(data) in units of gO2 m^3.
  Appropriate for simulation, when this vector of observation errors
  will be added to the calculated DO values to simulate observation
  error. But usually (for MLE or prediction from a fitted
  MLE/Bayesian/nighttime regression model) `err.obs` should be missing
  or 0.

## Value

A function that will return a negative log likelihood of the data given
a set of metab.pars.

## Examples

``` r
if (FALSE) { # interactive()
# prepare data for examples
data <- data_metab('3','30')[97:144,][seq(1,48,by=2),]
# preds.init <- list(GPP.daily=2.82,ER.daily=-2.12,K600.daily=31.27)
preds.init <- as.list(dplyr::select(
  predict_metab(metab(specs(mm_name('mle', ode_method='trapezoid')), data=data)),
  GPP.daily=GPP, ER.daily=ER, K600.daily=K600))
DOtime <- data$solar.time
dDOtime <- data$solar.time[-nrow(data)] + (data$solar.time[2] - data$solar.time[1])/2

# integration of dDOdt by euler, trapezoid, rk2, rk4, and lsoda methods
plot(x=DOtime, y=data$DO.obs, pch=3, cex=0.6)
# euler
dDOdt <- create_calc_dDOdt(data, ode_method='euler', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='euler')
DO.mod.euler <- DO(metab.pars=preds.init)
lines(x=DOtime, y=DO.mod.euler, type='l', col='chartreuse3')
# trapezoid=pairmeans
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='trapezoid')
DO.mod.trap <- DO(metab.pars=preds.init)
lines(x=DOtime, y=DO.mod.trap, type='l', col='gold')
# lsoda
dDOdt <- create_calc_dDOdt(data, ode_method='lsoda', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='lsoda')
DO.mod <- DO(metab.pars=preds.init)
lines(x=DOtime, y=DO.mod, type='l', col='navy')
# rk2
dDOdt <- create_calc_dDOdt(data, ode_method='rk2', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='rk2')
DO.mod <- DO(metab.pars=preds.init)
lines(x=DOtime, y=DO.mod, type='l', col='blue')
# rk4
dDOdt <- create_calc_dDOdt(data, ode_method='rk4', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='rk4')
DO.mod <- DO(metab.pars=preds.init)
lines(x=DOtime, y=DO.mod, type='l', col='magenta')

# with observation and/or process error
plot(x=DOtime, y=data$DO.obs, col='black', pch=3, cex=0.6)
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='trapezoid', err.obs=rnorm(nrow(data), 0, 0))
DO.mod.obs <- DO(preds.init)
lines(x=DOtime, y=DO.mod.obs, col='black', lty=1)
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO <- create_calc_DO(dDOdt, ode_method='trapezoid', err.obs=rnorm(nrow(data), 0, 0.3))
DO.mod.obserr <- DO(preds.init)
lines(x=DOtime, y=DO.mod.obserr, col='blue', lty=2)
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod', err.proc=rnorm(nrow(data), 0, 2))
DO <- create_calc_DO(dDOdt, ode_method='trapezoid', err.obs=rnorm(nrow(data), 0, 0.1))
DO.mod.operr <- DO(preds.init)
lines(x=DOtime, y=DO.mod.operr, col='red', lty=2)
}
```
