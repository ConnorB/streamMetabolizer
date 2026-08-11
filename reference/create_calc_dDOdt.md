# Create a function that calculates a one-day DO time series

Creates a closure that bundles data and helper functions into a single
function that returns dDOdt in gO2 m^-3 timestep^-1 for any given time
t.

## Usage

``` r
create_calc_dDOdt(data, ode_method, GPP_fun, ER_fun, deficit_src, err.proc = 0)
```

## Arguments

- data:

  Data.frame as in
  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md),
  except that data must contain exactly one date worth of inputs (~24
  hours according to `[specs]$day_start` and `[specs]$day_end`).

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

- GPP_fun:

  A string specifying how gross primary productivity (GPP) varies within
  each day:

  - `"linlight"`: `GPP(t) = GPP.daily * light(t) / mean.light`.
    `GPP.daily` is partitioned among timesteps by their fraction of the
    day's average light.

  - `"satlight"`: `GPP(t) = Pmax * tanh(alpha * light(t) / Pmax)`, where
    `Pmax` is the maximum possible GPP and `alpha` describes the initial
    increase with light.

  - `"satlightq10temp"`: The saturating light model multiplied by
    `1.036 ^ (temp.water(t) - 20)`.

  - `NA`: Do not estimate GPP; applicable only to `type = "Kmodel"`.

- ER_fun:

  A string specifying how ecosystem respiration (ER) varies within each
  day:

  - `"constant"`: `ER(t) = ER.daily`.

  - `"q10temp"`: `ER(t) = ER20 * 1.045 ^ (temp.water(t) - 20)`, where
    `ER20` is ER at 20 degrees C.

  - `NA`: Do not estimate ER; applicable only to `type = "Kmodel"`.

- deficit_src:

  A string specifying the DO estimate used to compute the DO deficit:

  - `"DO_mod"`: Use `DO.sat(t) − DO_mod(t)`, the difference between the
    equilibrium-saturation value and the current best estimate of the
    true DO concentration at that time.

  - `"DO_obs"`: Use `DO.sat(t) − DO.obs(t)`.

  - `"DO_obs_filter"`: Smooth `DO.obs` before nighttime regression;
    applicable only to `type = "night"`.

  - `NA`: Do not estimate DO deficit; applicable only to
    `type = "Kmodel"`.

- err.proc:

  Optional numerical vector of length nrow(data). Process error in units
  of gO2 m^-2 d^-1 (THIS MAY DIFFER FROM WHAT YOU'RE USED TO!).
  Appropriate for simulation, when this vector of process errors will be
  added to the calculated values of GPP and ER (then divided by depth
  and multiplied by timestep duration) to simulate process error. But
  usually (for MLE or prediction from a fitted MLE/Bayesian/nighttime
  regression model) `err.proc` should be missing or 0.

## Value

A function that accepts args `t` (the time in 0:(n-1) where n is the
number of timesteps), `DO.mod.t` (the value of DO.mod at time t in gO2
m^-3), and `metab` (a list of metabolism parameters; to see which
parameters should be included in this list, create `dDOdt` with this
function and then call `environment(dDOdt)$metab.needs`).

## ode_method 'trapezoid'

'pairmeans' and 'trapezoid' are identical. They are the analytical
solution to a trapezoid rule with this starting point:

     DO.mod[t+1] =
      DO.mod[t] +
       (((GPP[t]+GPP[t+1])/2) / (depth[t]+depth[t+1])/2
       + ((ER[t]+ER[t+1])/2) / (depth[t]+depth[t+1])/2
       + (k.O2[t](DO.sat[t] - DO.mod[t])
          + k.O2[t+1](DO.sat[t+1] - DO.mod[t+1]))/2
       + ((err.proc[t]+err.proc[t+1])/2) / (depth[t]+depth[t+1])/2
       ) * timestep

and this solution:

    DO.mod[t+1] - DO.mod[t] =
     (- DO.mod[t] * (k.O2[t]+k.O2[t+1])/2
       + (GPP[t]+GPP[t+1] +
          ER[t]+ER[t+1] +
          err.proc[t]+err.proc[t+1]
         ) / (depth[t]+depth[t+1])
       + (k.O2[t]*DO.sat[t] + k.O2[t+1]*DO.sat[t+1])/2
     ) * timestep / (1 + timestep*k.O2[t+1]/2)

where we're treating err.proc as a rate in gO2/m2/d, just like GPP & ER,
and err.proc=0 for model fitting.

## Examples

``` r
if (FALSE) { # interactive()
data <- data_metab('1','30')[seq(1,48,by=2),]
dDOdt.obs <- diff(data$DO.obs)
preds.init <- as.list(dplyr::select(
  predict_metab(metab(specs(mm_name('mle', ode_method='euler')), data=data)),
  GPP.daily=GPP, ER.daily=ER, K600.daily=K600))
DOtime <- data$solar.time
dDOtime <- data$solar.time[-nrow(data)] + (data$solar.time[2] - data$solar.time[1])/2

# args to create_calc_dDOdt determine which values are needed in metab.pars
dDOdt <- create_calc_dDOdt(data, ode_method='trapezoid', GPP_fun='satlight',
  ER_fun='q10temp', deficit_src='DO_mod')
names(formals(dDOdt)) # always the same: args to pass to dDOdt()
environment(dDOdt)$metab.needs # get the names to be included in metab.pars
dDOdt(t=23, state=c(DO.mod=data$DO.obs[1]),
  metab.pars=list(Pmax=0.2, alpha=0.01, ER20=-0.05, K600.daily=3))$dDOdt

# different required args; try in a timeseries
dDOdt <- create_calc_dDOdt(data, ode_method='euler', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
environment(dDOdt)$metab.needs # get the names to be included in metab
# approximate dDOdt and DO using DO.obs for DO deficits & Eulerian integration
DO.mod.m <- data$DO.obs[1]
dDOdt.mod.m <- NA
for(t in 1:23) {
 dDOdt.mod.m[t] <- dDOdt(t=t, state=c(DO.mod=DO.mod.m[t]),
    metab.pars=list(GPP.daily=2, ER.daily=-1.4, K600.daily=21))$dDOdt
 DO.mod.m[t+1] <- DO.mod.m[t] + dDOdt.mod.m[t]
}
par(mfrow=c(2,1), mar=c(3,3,1,1)+0.1)
plot(x=DOtime, y=data$DO.obs)
lines(x=DOtime, y=DO.mod.m, type='l', col='purple')
plot(x=dDOtime, y=dDOdt.obs)
lines(x=dDOtime, y=dDOdt.mod.m, type='l', col='blue')
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)

# compute & plot a full timeseries with ode() integration
dDOdt <- create_calc_dDOdt(data, ode_method='euler', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
DO.mod.o <- deSolve::ode(
  y=c(DO.mod=data$DO.obs[1]),
  parms=list(GPP.daily=2, ER.daily=-1.4, K600.daily=21),
  times=1:nrow(data), func=dDOdt, method='euler')[,'DO.mod']
par(mfrow=c(2,1), mar=c(3,3,1,1)+0.1)
plot(x=DOtime, y=data$DO.obs)
lines(x=DOtime, y=DO.mod.m, type='l', col='purple')
lines(x=DOtime, y=DO.mod.o, type='l', col='red', lty=2)
dDOdt.mod.o <- diff(DO.mod.o)
plot(x=dDOtime, y=dDOdt.obs)
lines(x=dDOtime, y=dDOdt.mod.m, type='l', col='blue')
lines(x=dDOtime, y=dDOdt.mod.o, type='l', col='green', lty=2)
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)

# see how values of metab.pars affect the dDOdt predictions
library(dplyr); library(ggplot2); library(tidyr)
dDOdt <- create_calc_dDOdt(data, ode_method='euler', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod')
apply_dDOdt <- function(GPP.daily, ER.daily, K600.daily) {
  DO.mod.m <- data$DO.obs[1]
  dDOdt.mod.m <- NA
  for(t in 1:23) {
   dDOdt.mod.m[t] <- dDOdt(t=t, state=c(DO.mod=DO.mod.m[t]),
    list(GPP.daily=GPP.daily, ER.daily=ER.daily, K600.daily=K600.daily))$dDOdt
   DO.mod.m[t+1] <- DO.mod.m[t] + dDOdt.mod.m[t]
  }
  dDOdt.mod.m
}
dDO.preds <- tibble::tibble(
  solar.time = dDOtime,
  dDO.preds.base = apply_dDOdt(3, -5, 15),
  dDO.preds.dblGPP = apply_dDOdt(6, -5, 15),
  dDO.preds.dblER = apply_dDOdt(3, -10, 15),
  dDO.preds.dblK = apply_dDOdt(3, -5, 30))
dDO.preds |>
  pivot_longer(starts_with('dDO.preds'), names_to='dDO.series',
    values_to='dDO.dt') |>
  ggplot(aes(x=solar.time, y=dDO.dt, color=dDO.series)) + geom_line() + theme_bw()

# try simulating process eror
data <- data_metab('1','30')[seq(1,48,by=2),]
plot(x=data$solar.time, y=data$DO.obs)
dDOdt.noerr <- create_calc_dDOdt(data, ode_method='rk4', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod', err.proc=rep(0, nrow(data)))
DO.mod.noerr <- deSolve::ode(
  y=c(DO.mod=data$DO.obs[1]),
  parms=list(GPP.daily=2, ER.daily=-1.4, K600.daily=21),
  times=1:nrow(data), func=dDOdt.noerr, method='rk4')[,'DO.mod']
lines(x=data$solar.time, y=DO.mod.noerr, type='l', col='purple')
# with error
dDOdt.err <- create_calc_dDOdt(data, ode_method='rk4', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod', err.proc=rep(+0.4, nrow(data)))
DO.mod.err <- deSolve::ode(
  y=c(DO.mod=data$DO.obs[1]),
  parms=list(GPP.daily=2, ER.daily=-1.4, K600.daily=21),
  times=1:nrow(data), func=dDOdt.err, method='rk4')[,'DO.mod']
lines(x=data$solar.time, y=DO.mod.err, type='l', col='red', lty=2)
# with same error each timestep is same as with reduced ER
dDOdt.noerr2 <- create_calc_dDOdt(data, ode_method='rk4', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod', err.proc=rep(0, nrow(data)))
DO.mod.noerr2 <- deSolve::ode(
  y=c(DO.mod=data$DO.obs[1]),
  parms=list(GPP.daily=2, ER.daily=-1.4+0.4, K600.daily=21),
  times=1:nrow(data), func=dDOdt.noerr2, method='rk4')[,'DO.mod']
lines(x=data$solar.time, y=DO.mod.noerr2, type='l', col='green', lty=3)
# with different timestep, same error value should mean very similar curve
data <- data_metab('1','30')
dDOdt.err2 <- create_calc_dDOdt(data, ode_method='rk4', GPP_fun='linlight',
  ER_fun='constant', deficit_src='DO_mod', err.proc=rep(0.4, nrow(data)))
DO.mod.err2 <- deSolve::ode(
  y=c(DO.mod=data$DO.obs[1]),
  parms=list(GPP.daily=2, ER.daily=-1.4, K600.daily=21),
  times=1:nrow(data), func=dDOdt.err2, method='rk4')[,'DO.mod']
lines(x=data$solar.time, y=DO.mod.err2, type='l', col='black', lty=2)
}
```
