# Combine a time series of K estimates to predict consistent values

Takes daily estimates of K, usually from nighttime regression, and
regresses against predictors such as discharge.daily. Returns a
metab_Kmodel object that only predicts daily K, nothing else.

## Usage

``` r
metab_Kmodel(
  specs = specs(mm_name("Kmodel")),
  data = mm_data(solar.time, discharge, velocity, optional = c("all")),
  data_daily = mm_data(date, K600.daily, K600.daily.lower, K600.daily.upper,
    discharge.daily, velocity.daily, optional = c("K600.daily.lower", "K600.daily.upper",
    "discharge.daily", "velocity.daily")),
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

  data.frame (not a tbl_df) of input data at the temporal resolution of
  raw observations (unit-value). Columns must have the same names,
  units, and format as the default. The solar.time column must also have
  a timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  data.frame containing inputs with a daily timestep. See the
  **'Formatting `data_daily`'** section below for a full description.

- info:

  any information, in any format, that you would like to store within
  the metab_model object

## Value

A metab_Kmodel object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).

## Details

Possible approaches:

- mean:

  Predict K as the mean of all K values

- weighted mean:

  Predict K as the mean of all K values, weighted by the inverse of the
  confidence intervals in the input K values

- KvQ:

  Regress K versus Q, tending toward overall mean in ranges of Q with
  sparse data

- weighted KvQ:

  Regress K versus Q, tending toward overall mean in ranges of Q with
  sparse data, weighting high-confidence K values more heavily

- T smoother:

  Predict K using a loess or spline smoother over time

- Q smoother:

  Predict K using a loess or spline smoother over discharge.daily

- TQ smoother:

  Predict K using a loess or spline smoother over both time and
  discharge.daily

## See also

Other metab_model:
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md),
[`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md),
[`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)

## Author

Alison Appling

## Examples

``` r
library(dplyr)
# create example data
set.seed(24842)
example_Ks <- data.frame(date=seq(as.Date("2012-08-15"),as.Date("2012-09-15"),
  as.difftime(1,units='days')), discharge.daily=exp(rnorm(32,2,1)), K600.daily=rnorm(32,30,4)) %>%
  mutate(K600.daily.lower=K600.daily-5, K600.daily.upper=K600.daily+6)

# mean
mm <- metab_Kmodel(
  specs(mm_name('Kmodel', engine='mean')),
  data_daily=example_Ks) # two warnings expected for engine='mean'
get_params(mm)
#>          date K600.daily K600.daily.sd         warnings errors
#> 1  2012-08-15   29.41813            NA overall warnings       
#> 2  2012-08-16   29.41813            NA overall warnings       
#> 3  2012-08-17   29.41813            NA overall warnings       
#> 4  2012-08-18   29.41813            NA overall warnings       
#> 5  2012-08-19   29.41813            NA overall warnings       
#> 6  2012-08-20   29.41813            NA overall warnings       
#> 7  2012-08-21   29.41813            NA overall warnings       
#> 8  2012-08-22   29.41813            NA overall warnings       
#> 9  2012-08-23   29.41813            NA overall warnings       
#> 10 2012-08-24   29.41813            NA overall warnings       
#> 11 2012-08-25   29.41813            NA overall warnings       
#> 12 2012-08-26   29.41813            NA overall warnings       
#> 13 2012-08-27   29.41813            NA overall warnings       
#> 14 2012-08-28   29.41813            NA overall warnings       
#> 15 2012-08-29   29.41813            NA overall warnings       
#> 16 2012-08-30   29.41813            NA overall warnings       
#> 17 2012-08-31   29.41813            NA overall warnings       
#> 18 2012-09-01   29.41813            NA overall warnings       
#> 19 2012-09-02   29.41813            NA overall warnings       
#> 20 2012-09-03   29.41813            NA overall warnings       
#> 21 2012-09-04   29.41813            NA overall warnings       
#> 22 2012-09-05   29.41813            NA overall warnings       
#> 23 2012-09-06   29.41813            NA overall warnings       
#> 24 2012-09-07   29.41813            NA overall warnings       
#> 25 2012-09-08   29.41813            NA overall warnings       
#> 26 2012-09-09   29.41813            NA overall warnings       
#> 27 2012-09-10   29.41813            NA overall warnings       
#> 28 2012-09-11   29.41813            NA overall warnings       
#> 29 2012-09-12   29.41813            NA overall warnings       
#> 30 2012-09-13   29.41813            NA overall warnings       
#> 31 2012-09-14   29.41813            NA overall warnings       
#> 32 2012-09-15   29.41813            NA overall warnings       
if (FALSE) { # \dontrun{
plot(get_params(mm)$date, get_params(mm)$K600.daily)
} # }

# linear model
mm <- metab_Kmodel(
  specs(mm_name('Kmodel', engine='lm'), predictors='discharge.daily'),
  data_daily=example_Ks)
get_params(mm)
#>          date K600.daily K600.daily.sd warnings errors
#> 1  2012-08-15   29.48773            NA                
#> 2  2012-08-16   29.45253            NA                
#> 3  2012-08-17   30.52907            NA                
#> 4  2012-08-18   29.36423            NA                
#> 5  2012-08-19   26.97996            NA                
#> 6  2012-08-20   29.85889            NA                
#> 7  2012-08-21   29.41888            NA                
#> 8  2012-08-22   29.21206            NA                
#> 9  2012-08-23   28.90789            NA                
#> 10 2012-08-24   28.55220            NA                
#> 11 2012-08-25   30.51001            NA                
#> 12 2012-08-26   28.45314            NA                
#> 13 2012-08-27   30.15313            NA                
#> 14 2012-08-28   29.14110            NA                
#> 15 2012-08-29   28.96144            NA                
#> 16 2012-08-30   29.49264            NA                
#> 17 2012-08-31   28.70135            NA                
#> 18 2012-09-01   30.78642            NA                
#> 19 2012-09-02   29.42760            NA                
#> 20 2012-09-03   28.33825            NA                
#> 21 2012-09-04   29.61804            NA                
#> 22 2012-09-05   29.14045            NA                
#> 23 2012-09-06   29.01319            NA                
#> 24 2012-09-07   29.95855            NA                
#> 25 2012-09-08   29.99182            NA                
#> 26 2012-09-09   28.54950            NA                
#> 27 2012-09-10   29.76544            NA                
#> 28 2012-09-11   29.46747            NA                
#> 29 2012-09-12   29.70034            NA                
#> 30 2012-09-13   28.51264            NA                
#> 31 2012-09-14   30.65190            NA                
#> 32 2012-09-15   30.86205            NA                
if (FALSE) { # \dontrun{
plot(get_data_daily(mm)$discharge.daily, get_params(mm)$K600.daily)
} # }

# loess
mm <- metab_Kmodel(    ### breaks ###
  specs(mm_name('Kmodel', engine='loess'), predictors='date', other_args=list(span=0.4)),
  data_daily=example_Ks)
get_params(mm)
#>          date K600.daily K600.daily.sd warnings errors
#> 1  2012-08-15   26.93642      1.136389                
#> 2  2012-08-16   28.13190      1.087341                
#> 3  2012-08-17   28.79526      1.069291                
#> 4  2012-08-18   29.09960      1.071040                
#> 5  2012-08-19   28.84944      1.075185                
#> 6  2012-08-20   28.29487      1.079092                
#> 7  2012-08-21   27.62516      1.075342                
#> 8  2012-08-22   27.12816      1.079242                
#> 9  2012-08-23   27.21836      1.074020                
#> 10 2012-08-24   27.67841      1.078880                
#> 11 2012-08-25   28.83481      1.073739                
#> 12 2012-08-26   30.20604      1.076872                
#> 13 2012-08-27   31.72782      1.070802                
#> 14 2012-08-28   33.11495      1.073499                
#> 15 2012-08-29   33.64264      1.068786                
#> 16 2012-08-30   33.69489      1.073006                
#> 17 2012-08-31   32.56211      1.070038                
#> 18 2012-09-01   31.32665      1.074744                
#> 19 2012-09-02   31.20208      1.070357                
#> 20 2012-09-03   31.10106      1.074969                
#> 21 2012-09-04   30.84886      1.071741                
#> 22 2012-09-05   30.37960      1.076465                
#> 23 2012-09-06   28.82896      1.072684                
#> 24 2012-09-07   27.27600      1.079072                
#> 25 2012-09-08   26.11465      1.076778                
#> 26 2012-09-09   25.49586      1.082287                
#> 27 2012-09-10   26.40117      1.076760                
#> 28 2012-09-11   27.50791      1.077389                
#> 29 2012-09-12   28.11343      1.072039                
#> 30 2012-09-13   28.71594      1.069151                
#> 31 2012-09-14   29.09925      1.084802                
#> 32 2012-09-15   29.31613      1.132190                
if (FALSE) { # \dontrun{
plot(get_params(mm)$date, get_params(mm)$K600.daily)
} # }

## 3-phase workflow (sort of like complete pooling) for estimating K within
## days, then K across days, then GPP and ER within days

# 1. data and specifications for both of the MLE models
dat <- data_metab('10','15')
mle_specs <- specs(mm_name('mle'))

# fit a first-round MLE and extract the K estimates
mm1 <- metab_mle(mle_specs, data=dat)
K600_mm1 <- get_params(mm1, uncertainty='ci') %>%
  select(date, K600.daily, K600.daily.lower, K600.daily.upper)

# smooth the K600s
mm2 <- metab_Kmodel(specs(mm_name('Kmodel', engine='mean'),
  day_start=-1, day_end=23), data_daily=K600_mm1)
K600_mm2 <- get_params(mm2) %>% select(date, K600.daily)

# refit the MLE with fixed K
mm3 <- metab_mle(mle_specs, data=dat, data_daily=K600_mm2)
get_params(mm3, fixed='stars')
#>          date GPP.daily GPP.daily.sd   ER.daily ER.daily.sd K600.daily warnings
#> 1  2012-09-18 2.558979    0.05519044 -1.913915   0.08116143  28.44894*         
#> 2  2012-09-19 2.765987    0.05888406 -2.066356   0.08646030  28.44894*         
#> 3  2012-09-20 2.564581    0.05024763 -1.693336   0.07446124  28.44894*         
#> 4  2012-09-21 2.558030    0.04396162 -1.709250   0.06493983  28.44894*         
#> 5  2012-09-22 2.607292    0.05752125 -1.745543   0.08553130  28.44894*         
#> 6  2012-09-23 2.835975    0.06980850 -2.159617   0.10599571  28.44894*         
#> 7  2012-09-24 2.756249    0.07120201 -2.627882   0.10734383  28.44894*         
#> 8  2012-09-25 1.881598    0.06901491 -1.999497   0.10824960  28.44894*         
#> 9  2012-09-26 2.139146    0.05002992 -1.970511   0.07780874  28.44894*         
#> 10 2012-09-27 1.954997    0.04672300 -1.854895   0.07239294  28.44894*         
#>    errors
#> 1        
#> 2        
#> 3        
#> 4        
#> 5        
#> 6        
#> 7        
#> 8        
#> 9        
#> 10       
predict_metab(mm3)
#>          date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1  2012-09-18 2.558979  2.450808  2.667150 -1.913915 -2.072988 -1.754841
#> 2  2012-09-19 2.765987  2.650577  2.881398 -2.066356 -2.235815 -1.896897
#> 3  2012-09-20 2.564581  2.466098  2.663065 -1.693336 -1.839278 -1.547395
#> 4  2012-09-21 2.558030  2.471867  2.644193 -1.709250 -1.836530 -1.581971
#> 5  2012-09-22 2.607292  2.494553  2.720032 -1.745543 -1.913181 -1.577905
#> 6  2012-09-23 2.835975  2.699153  2.972797 -2.159617 -2.367365 -1.951869
#> 7  2012-09-24 2.756249  2.616695  2.895802 -2.627882 -2.838272 -2.417492
#> 8  2012-09-25 1.881598  1.746331  2.016865 -1.999497 -2.211662 -1.787331
#> 9  2012-09-26 2.139146  2.041090  2.237203 -1.970511 -2.123014 -1.818009
#> 10 2012-09-27 1.954997  1.863422  2.046572 -1.854895 -1.996783 -1.713008
#>    msgs.fit warnings errors
#> 1                          
#> 2                          
#> 3                          
#> 4                          
#> 5                          
#> 6                          
#> 7                          
#> 8                          
#> 9                          
#> 10                         
if (FALSE) { # \dontrun{
plot_metab_preds(mm1)
plot_metab_preds(mm3)
} # }
```
