# Maximum likelihood metabolism model fitting function

Uses maximum likelihood to fit a model to estimate GPP and ER from input
data on DO, temperature, light, etc. Discharge is only used, if at all,
to identify and exclude days with any negative discharge.

## Usage

``` r
metab_mle(
  specs = specs(mm_name("mle")),
  data = mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge,
    optional = "discharge"),
  data_daily = mm_data(date, K600.daily, init.GPP.daily, init.Pmax, init.alpha,
    init.ER.daily, init.ER20, init.K600.daily, optional = "all"),
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

A metab_mle object containing the fitted model. This object can be
inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).
The `code` column in `get_fit(mm)` is defined in the Value subsection of
[`?nlm`](https://rdrr.io/r/stats/nlm.html).

## See also

Other metab_model:
[`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md),
[`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md),
[`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md),
[`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)

## Examples

``` r
dat <- data_metab('3','30')
# PRK
mm <- metab_mle(data=dat)
predict_metab(mm)
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 2.816207  1.894260  3.738153 -2.116196 -2.867236 -1.365155
#> 2 2012-09-19 3.287146  2.265982  4.308309 -2.479248 -3.322547 -1.635950
#> 3 2012-09-20 2.599133  1.935957  3.262309 -1.718192 -2.224222 -1.212162
#>   msgs.fit warnings errors
#> 1                         
#> 2                         
#> 3                         

# PR with fixed K on two days
dat_daily <- data.frame(date=as.Date(c("2012-09-18","2012-09-20")), K600.daily=35)
metab_mle(data=dat, data_daily=dat_daily)
#> metab_model of type metab_mle 
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   model_name        m_np_oi_tr_plrckm.nlm                                       
#>   day_start         4                                                           
#>   day_end           28                                                          
#>   day_tests         full_day, even_timesteps, complete_data, pos_discharge, p...
#>   required_timestep NA                                                          
#>   init.GPP.daily    8                                                           
#>   init.ER.daily     -10                                                         
#>   init.K600.daily   10                                                          
#> Fitting time: 0.164 secs elapsed
#> Parameters (3 dates):
#>         date GPP.daily GPP.daily.lower GPP.daily.upper   ER.daily
#> 1 2012-09-18 3.202406         3.018387        3.386425 -2.419569 
#> 2 2012-09-19 3.287146         2.265982        4.308309 -2.479248 
#> 3 2012-09-20 3.214161         3.042628        3.385693 -2.158125 
#>   ER.daily.lower ER.daily.upper K600.daily K600.daily.lower K600.daily.upper
#> 1      -2.691317      -2.147821 35.00000NA               NA               NA
#> 2      -3.322547      -1.635950  33.31607          23.82513         42.80701
#> 3      -2.413318      -1.902933 35.00000NA               NA               NA
#>   msgs.fit
#> 1         
#> 2      W  
#> 3         
#> Fitting warnings:
#>   1 date: data_daily$K600.daily==NA so fitting by MLE
#> Predictions (3 dates):
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 3.202406  3.018387  3.386425 -2.419569 -2.691317 -2.147821
#> 2 2012-09-19 3.287146  2.265982  4.308309 -2.479248 -3.322547 -1.635950
#> 3 2012-09-20 3.214161  3.042628  3.385693 -2.158125 -2.413318 -1.902933
#>   msgs.fit msgs.pred
#> 1                   
#> 2      W            
#> 3                   

# PRK with day-specific inits on some days
dat_daily <- data.frame(date=as.Date("2012-09-19"),
  init.GPP.daily=4, init.K600.daily=60)
metab_mle(data=dat, data_daily=dat_daily)
#> metab_model of type metab_mle 
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   model_name        m_np_oi_tr_plrckm.nlm                                       
#>   day_start         4                                                           
#>   day_end           28                                                          
#>   day_tests         full_day, even_timesteps, complete_data, pos_discharge, p...
#>   required_timestep NA                                                          
#>   init.GPP.daily    8                                                           
#>   init.ER.daily     -10                                                         
#>   init.K600.daily   10                                                          
#> Fitting time: 0.247 secs elapsed
#> Parameters (3 dates):
#>         date GPP.daily GPP.daily.lower GPP.daily.upper   ER.daily
#> 1 2012-09-18 2.816207         1.894260        3.738153 -2.116196 
#> 2 2012-09-19 3.287148         2.269303        4.304993 -2.479250 
#> 3 2012-09-20 2.599133         1.935957        3.262309 -1.718192 
#>   ER.daily.lower ER.daily.upper K600.daily K600.daily.lower K600.daily.upper
#> 1      -2.867236      -1.365155  31.06500          21.81770         40.31231
#> 2      -3.319998      -1.638502  33.31609          23.85389         42.77829
#> 3      -2.224222      -1.212162  28.73156          22.12035         35.34277
#>   msgs.fit
#> 1      W  
#> 2         
#> 3      W  
#> Fitting warnings:
#>   2 dates: data_daily$init.GPP.daily==NA so using specs
#>   2 dates: data_daily$init.K600.daily==NA so using specs
#> Predictions (3 dates):
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 2.816207  1.894260  3.738153 -2.116196 -2.867236 -1.365155
#> 2 2012-09-19 3.287148  2.269303  4.304993 -2.479250 -3.319998 -1.638502
#> 3 2012-09-20 2.599133  1.935957  3.262309 -1.718192 -2.224222 -1.212162
#>   msgs.fit msgs.pred
#> 1      W            
#> 2                   
#> 3      W            

# Nonlinear GPP or ER equations
metab_mle(specs(mm_name('mle', GPP_fun='satlight')), data=dat)
#> metab_model of type metab_mle 
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   model_name        m_np_oi_tr_psrckm.nlm                                       
#>   day_start         4                                                           
#>   day_end           28                                                          
#>   day_tests         full_day, even_timesteps, complete_data, pos_discharge, p...
#>   required_timestep NA                                                          
#>   init.Pmax         10                                                          
#>   init.alpha        1e-04                                                       
#>   init.ER.daily     -10                                                         
#>   init.K600.daily   10                                                          
#> Fitting time: 0.912 secs elapsed
#> Parameters (3 dates):
#>         date      Pmax Pmax.lower Pmax.upper        alpha alpha.lower
#> 1 2012-09-18 6.109051    5.645596   6.572507 0.008351592  0.007714564
#> 2 2012-09-19 7.286768    6.537958   8.035577 0.009177225  0.008270835
#> 3 2012-09-20 6.269003    5.586875   6.951132 0.007380747  0.006604585
#>   alpha.upper   ER.daily ER.daily.lower ER.daily.upper K600.daily
#> 1 0.008988621 -1.958640       -2.091695      -1.825586  24.80407 
#> 2 0.010083616 -2.260354       -2.468651      -2.052058  26.65527 
#> 3 0.008156909 -1.685959       -1.851738      -1.520180  24.46159 
#>   K600.daily.lower K600.daily.upper msgs.fit
#> 1         23.32555         26.28260         
#> 2         24.52059         28.78995         
#> 3         22.48988         26.43329         
#> Predictions (3 dates):
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 2.495259        NA        NA -1.958640 -2.091695 -1.825586
#> 2 2012-09-19 2.892573        NA        NA -2.260354 -2.468651 -2.052058
#> 3 2012-09-20 2.422478        NA        NA -1.685959 -1.851738 -1.520180
#>   msgs.fit msgs.pred
#> 1                   
#> 2                   
#> 3                   
metab_mle(specs(mm_name('mle', ER_fun='q10temp')), data=dat)
#> metab_model of type metab_mle 
#> streamMetabolizer version 0.12.1.9000 
#> Specifications:
#>   model_name        m_np_oi_tr_plrqkm.nlm                                       
#>   day_start         4                                                           
#>   day_end           28                                                          
#>   day_tests         full_day, even_timesteps, complete_data, pos_discharge, p...
#>   required_timestep NA                                                          
#>   init.GPP.daily    8                                                           
#>   init.ER20         -10                                                         
#>   init.K600.daily   10                                                          
#> Fitting time: 0.281 secs elapsed
#> Parameters (3 dates):
#>         date GPP.daily GPP.daily.lower GPP.daily.upper       ER20 ER20.lower
#> 1 2012-09-18 2.056615         1.532462        2.580768 -2.704649   -3.453849
#> 2 2012-09-19 2.438948         1.833218        3.044678 -3.149906   -4.011733
#> 3 2012-09-20 2.097309         1.653788        2.540831 -2.377270   -2.962660
#>   ER20.upper K600.daily K600.daily.lower K600.daily.upper msgs.fit
#> 1  -1.955450  22.41849          17.33170         27.50528         
#> 2  -2.288080  24.24939          18.82827         29.67052         
#> 3  -1.791879  22.85723          18.55131         27.16315         
#> Predictions (3 dates):
#>         date      GPP GPP.lower GPP.upper        ER ER.lower ER.upper msgs.fit
#> 1 2012-09-18 2.056615  1.532462  2.580768 -1.546919       NA       NA         
#> 2 2012-09-19 2.438948  1.833218  3.044678 -1.836107       NA       NA         
#> 3 2012-09-20 2.097309  1.653788  2.540831 -1.386761       NA       NA         
#>   msgs.pred
#> 1          
#> 2          
#> 3          
if (FALSE) { # \dontrun{
plot_DO_preds(predict_DO(mm))
} # }
```
