# Get the valid names for a given model type or types

Returns a vector of the `model_name`s for the type(s) indicated. If
`type` is not supplied, all model types will be included. After being
returned from this function, model names may be translated to something
slightly more readable with
[`mm_parse_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md)
if desired.

## Usage

``` r
mm_valid_names(type = c("bayes", "mle", "night", "Kmodel", "sim"))
```

## Arguments

- type:

  A string specifying the model type:

  - `"mle"`: Maximum likelihood estimation; see
    [`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md).

  - `"bayes"`: Bayesian hierarchical modeling; see
    [`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md).

  - `"night"`: Nighttime regression; see
    [`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md).

  - `"Kmodel"`: Regression of daily `K600.daily` estimates against
    discharge, time, or other predictors; see
    [`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md).

  - `"sim"`: Simulation of `DO.obs` data; see
    [`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md).

## Value

A character vector of valid encoded model names.

## Examples

``` r
mm_valid_names('mle')
#>   [1] "m_np_oi_tr_plrckm.nlm"  "m_np_pi_tr_plrckm.nlm"  "m_np_oi_eu_plrckm.nlm" 
#>   [4] "m_np_pi_eu_plrckm.nlm"  "m_np_oi_r2_plrckm.nlm"  "m_np_pi_r2_plrckm.nlm" 
#>   [7] "m_np_oi_o1_plrckm.nlm"  "m_np_pi_o1_plrckm.nlm"  "m_np_oi_o2_plrckm.nlm" 
#>  [10] "m_np_pi_o2_plrckm.nlm"  "m_np_oi_o3_plrckm.nlm"  "m_np_pi_o3_plrckm.nlm" 
#>  [13] "m_np_oi_o4_plrckm.nlm"  "m_np_pi_o4_plrckm.nlm"  "m_np_oi_o5_plrckm.nlm" 
#>  [16] "m_np_pi_o5_plrckm.nlm"  "m_np_oi_o6_plrckm.nlm"  "m_np_pi_o6_plrckm.nlm" 
#>  [19] "m_np_oi_o8_plrckm.nlm"  "m_np_pi_o8_plrckm.nlm"  "m_np_oi_o9_plrckm.nlm" 
#>  [22] "m_np_pi_o9_plrckm.nlm"  "m_np_oi_o10_plrckm.nlm" "m_np_pi_o10_plrckm.nlm"
#>  [25] "m_np_oi_o11_plrckm.nlm" "m_np_pi_o11_plrckm.nlm" "m_np_oi_o12_plrckm.nlm"
#>  [28] "m_np_pi_o12_plrckm.nlm" "m_np_oi_o13_plrckm.nlm" "m_np_pi_o13_plrckm.nlm"
#>  [31] "m_np_oi_o14_plrckm.nlm" "m_np_pi_o14_plrckm.nlm" "m_np_oi_o15_plrckm.nlm"
#>  [34] "m_np_pi_o15_plrckm.nlm" "m_np_oi_o16_plrckm.nlm" "m_np_pi_o16_plrckm.nlm"
#>  [37] "m_np_oi_Eu_plrckm.nlm"  "m_np_pi_Eu_plrckm.nlm"  "m_np_oi_pm_plrckm.nlm" 
#>  [40] "m_np_pi_pm_plrckm.nlm"  "m_np_oi_tr_psrckm.nlm"  "m_np_pi_tr_psrckm.nlm" 
#>  [43] "m_np_oi_eu_psrckm.nlm"  "m_np_pi_eu_psrckm.nlm"  "m_np_oi_r2_psrckm.nlm" 
#>  [46] "m_np_pi_r2_psrckm.nlm"  "m_np_oi_o1_psrckm.nlm"  "m_np_pi_o1_psrckm.nlm" 
#>  [49] "m_np_oi_o2_psrckm.nlm"  "m_np_pi_o2_psrckm.nlm"  "m_np_oi_o3_psrckm.nlm" 
#>  [52] "m_np_pi_o3_psrckm.nlm"  "m_np_oi_o4_psrckm.nlm"  "m_np_pi_o4_psrckm.nlm" 
#>  [55] "m_np_oi_o5_psrckm.nlm"  "m_np_pi_o5_psrckm.nlm"  "m_np_oi_o6_psrckm.nlm" 
#>  [58] "m_np_pi_o6_psrckm.nlm"  "m_np_oi_o8_psrckm.nlm"  "m_np_pi_o8_psrckm.nlm" 
#>  [61] "m_np_oi_o9_psrckm.nlm"  "m_np_pi_o9_psrckm.nlm"  "m_np_oi_o10_psrckm.nlm"
#>  [64] "m_np_pi_o10_psrckm.nlm" "m_np_oi_o11_psrckm.nlm" "m_np_pi_o11_psrckm.nlm"
#>  [67] "m_np_oi_o12_psrckm.nlm" "m_np_pi_o12_psrckm.nlm" "m_np_oi_o13_psrckm.nlm"
#>  [70] "m_np_pi_o13_psrckm.nlm" "m_np_oi_o14_psrckm.nlm" "m_np_pi_o14_psrckm.nlm"
#>  [73] "m_np_oi_o15_psrckm.nlm" "m_np_pi_o15_psrckm.nlm" "m_np_oi_o16_psrckm.nlm"
#>  [76] "m_np_pi_o16_psrckm.nlm" "m_np_oi_Eu_psrckm.nlm"  "m_np_pi_Eu_psrckm.nlm" 
#>  [79] "m_np_oi_pm_psrckm.nlm"  "m_np_pi_pm_psrckm.nlm"  "m_np_oi_tr_pqrckm.nlm" 
#>  [82] "m_np_pi_tr_pqrckm.nlm"  "m_np_oi_eu_pqrckm.nlm"  "m_np_pi_eu_pqrckm.nlm" 
#>  [85] "m_np_oi_r2_pqrckm.nlm"  "m_np_pi_r2_pqrckm.nlm"  "m_np_oi_o1_pqrckm.nlm" 
#>  [88] "m_np_pi_o1_pqrckm.nlm"  "m_np_oi_o2_pqrckm.nlm"  "m_np_pi_o2_pqrckm.nlm" 
#>  [91] "m_np_oi_o3_pqrckm.nlm"  "m_np_pi_o3_pqrckm.nlm"  "m_np_oi_o4_pqrckm.nlm" 
#>  [94] "m_np_pi_o4_pqrckm.nlm"  "m_np_oi_o5_pqrckm.nlm"  "m_np_pi_o5_pqrckm.nlm" 
#>  [97] "m_np_oi_o6_pqrckm.nlm"  "m_np_pi_o6_pqrckm.nlm"  "m_np_oi_o8_pqrckm.nlm" 
#> [100] "m_np_pi_o8_pqrckm.nlm"  "m_np_oi_o9_pqrckm.nlm"  "m_np_pi_o9_pqrckm.nlm" 
#> [103] "m_np_oi_o10_pqrckm.nlm" "m_np_pi_o10_pqrckm.nlm" "m_np_oi_o11_pqrckm.nlm"
#> [106] "m_np_pi_o11_pqrckm.nlm" "m_np_oi_o12_pqrckm.nlm" "m_np_pi_o12_pqrckm.nlm"
#> [109] "m_np_oi_o13_pqrckm.nlm" "m_np_pi_o13_pqrckm.nlm" "m_np_oi_o14_pqrckm.nlm"
#> [112] "m_np_pi_o14_pqrckm.nlm" "m_np_oi_o15_pqrckm.nlm" "m_np_pi_o15_pqrckm.nlm"
#> [115] "m_np_oi_o16_pqrckm.nlm" "m_np_pi_o16_pqrckm.nlm" "m_np_oi_Eu_pqrckm.nlm" 
#> [118] "m_np_pi_Eu_pqrckm.nlm"  "m_np_oi_pm_pqrckm.nlm"  "m_np_pi_pm_pqrckm.nlm" 
#> [121] "m_np_oi_tr_plrqkm.nlm"  "m_np_pi_tr_plrqkm.nlm"  "m_np_oi_eu_plrqkm.nlm" 
#> [124] "m_np_pi_eu_plrqkm.nlm"  "m_np_oi_r2_plrqkm.nlm"  "m_np_pi_r2_plrqkm.nlm" 
#> [127] "m_np_oi_o1_plrqkm.nlm"  "m_np_pi_o1_plrqkm.nlm"  "m_np_oi_o2_plrqkm.nlm" 
#> [130] "m_np_pi_o2_plrqkm.nlm"  "m_np_oi_o3_plrqkm.nlm"  "m_np_pi_o3_plrqkm.nlm" 
#> [133] "m_np_oi_o4_plrqkm.nlm"  "m_np_pi_o4_plrqkm.nlm"  "m_np_oi_o5_plrqkm.nlm" 
#> [136] "m_np_pi_o5_plrqkm.nlm"  "m_np_oi_o6_plrqkm.nlm"  "m_np_pi_o6_plrqkm.nlm" 
#> [139] "m_np_oi_o8_plrqkm.nlm"  "m_np_pi_o8_plrqkm.nlm"  "m_np_oi_o9_plrqkm.nlm" 
#> [142] "m_np_pi_o9_plrqkm.nlm"  "m_np_oi_o10_plrqkm.nlm" "m_np_pi_o10_plrqkm.nlm"
#> [145] "m_np_oi_o11_plrqkm.nlm" "m_np_pi_o11_plrqkm.nlm" "m_np_oi_o12_plrqkm.nlm"
#> [148] "m_np_pi_o12_plrqkm.nlm" "m_np_oi_o13_plrqkm.nlm" "m_np_pi_o13_plrqkm.nlm"
#> [151] "m_np_oi_o14_plrqkm.nlm" "m_np_pi_o14_plrqkm.nlm" "m_np_oi_o15_plrqkm.nlm"
#> [154] "m_np_pi_o15_plrqkm.nlm" "m_np_oi_o16_plrqkm.nlm" "m_np_pi_o16_plrqkm.nlm"
#> [157] "m_np_oi_Eu_plrqkm.nlm"  "m_np_pi_Eu_plrqkm.nlm"  "m_np_oi_pm_plrqkm.nlm" 
#> [160] "m_np_pi_pm_plrqkm.nlm"  "m_np_oi_tr_psrqkm.nlm"  "m_np_pi_tr_psrqkm.nlm" 
#> [163] "m_np_oi_eu_psrqkm.nlm"  "m_np_pi_eu_psrqkm.nlm"  "m_np_oi_r2_psrqkm.nlm" 
#> [166] "m_np_pi_r2_psrqkm.nlm"  "m_np_oi_o1_psrqkm.nlm"  "m_np_pi_o1_psrqkm.nlm" 
#> [169] "m_np_oi_o2_psrqkm.nlm"  "m_np_pi_o2_psrqkm.nlm"  "m_np_oi_o3_psrqkm.nlm" 
#> [172] "m_np_pi_o3_psrqkm.nlm"  "m_np_oi_o4_psrqkm.nlm"  "m_np_pi_o4_psrqkm.nlm" 
#> [175] "m_np_oi_o5_psrqkm.nlm"  "m_np_pi_o5_psrqkm.nlm"  "m_np_oi_o6_psrqkm.nlm" 
#> [178] "m_np_pi_o6_psrqkm.nlm"  "m_np_oi_o8_psrqkm.nlm"  "m_np_pi_o8_psrqkm.nlm" 
#> [181] "m_np_oi_o9_psrqkm.nlm"  "m_np_pi_o9_psrqkm.nlm"  "m_np_oi_o10_psrqkm.nlm"
#> [184] "m_np_pi_o10_psrqkm.nlm" "m_np_oi_o11_psrqkm.nlm" "m_np_pi_o11_psrqkm.nlm"
#> [187] "m_np_oi_o12_psrqkm.nlm" "m_np_pi_o12_psrqkm.nlm" "m_np_oi_o13_psrqkm.nlm"
#> [190] "m_np_pi_o13_psrqkm.nlm" "m_np_oi_o14_psrqkm.nlm" "m_np_pi_o14_psrqkm.nlm"
#> [193] "m_np_oi_o15_psrqkm.nlm" "m_np_pi_o15_psrqkm.nlm" "m_np_oi_o16_psrqkm.nlm"
#> [196] "m_np_pi_o16_psrqkm.nlm" "m_np_oi_Eu_psrqkm.nlm"  "m_np_pi_Eu_psrqkm.nlm" 
#> [199] "m_np_oi_pm_psrqkm.nlm"  "m_np_pi_pm_psrqkm.nlm"  "m_np_oi_tr_pqrqkm.nlm" 
#> [202] "m_np_pi_tr_pqrqkm.nlm"  "m_np_oi_eu_pqrqkm.nlm"  "m_np_pi_eu_pqrqkm.nlm" 
#> [205] "m_np_oi_r2_pqrqkm.nlm"  "m_np_pi_r2_pqrqkm.nlm"  "m_np_oi_o1_pqrqkm.nlm" 
#> [208] "m_np_pi_o1_pqrqkm.nlm"  "m_np_oi_o2_pqrqkm.nlm"  "m_np_pi_o2_pqrqkm.nlm" 
#> [211] "m_np_oi_o3_pqrqkm.nlm"  "m_np_pi_o3_pqrqkm.nlm"  "m_np_oi_o4_pqrqkm.nlm" 
#> [214] "m_np_pi_o4_pqrqkm.nlm"  "m_np_oi_o5_pqrqkm.nlm"  "m_np_pi_o5_pqrqkm.nlm" 
#> [217] "m_np_oi_o6_pqrqkm.nlm"  "m_np_pi_o6_pqrqkm.nlm"  "m_np_oi_o8_pqrqkm.nlm" 
#> [220] "m_np_pi_o8_pqrqkm.nlm"  "m_np_oi_o9_pqrqkm.nlm"  "m_np_pi_o9_pqrqkm.nlm" 
#> [223] "m_np_oi_o10_pqrqkm.nlm" "m_np_pi_o10_pqrqkm.nlm" "m_np_oi_o11_pqrqkm.nlm"
#> [226] "m_np_pi_o11_pqrqkm.nlm" "m_np_oi_o12_pqrqkm.nlm" "m_np_pi_o12_pqrqkm.nlm"
#> [229] "m_np_oi_o13_pqrqkm.nlm" "m_np_pi_o13_pqrqkm.nlm" "m_np_oi_o14_pqrqkm.nlm"
#> [232] "m_np_pi_o14_pqrqkm.nlm" "m_np_oi_o15_pqrqkm.nlm" "m_np_pi_o15_pqrqkm.nlm"
#> [235] "m_np_oi_o16_pqrqkm.nlm" "m_np_pi_o16_pqrqkm.nlm" "m_np_oi_Eu_pqrqkm.nlm" 
#> [238] "m_np_pi_Eu_pqrqkm.nlm"  "m_np_oi_pm_pqrqkm.nlm"  "m_np_pi_pm_pqrqkm.nlm" 
#> [241] "m_np_oi_tr_plrcko.nlm"  "m_np_pi_tr_plrcko.nlm"  "m_np_oi_eu_plrcko.nlm" 
#> [244] "m_np_pi_eu_plrcko.nlm"  "m_np_oi_r2_plrcko.nlm"  "m_np_pi_r2_plrcko.nlm" 
#> [247] "m_np_oi_o1_plrcko.nlm"  "m_np_pi_o1_plrcko.nlm"  "m_np_oi_o2_plrcko.nlm" 
#> [250] "m_np_pi_o2_plrcko.nlm"  "m_np_oi_o3_plrcko.nlm"  "m_np_pi_o3_plrcko.nlm" 
#> [253] "m_np_oi_o4_plrcko.nlm"  "m_np_pi_o4_plrcko.nlm"  "m_np_oi_o5_plrcko.nlm" 
#> [256] "m_np_pi_o5_plrcko.nlm"  "m_np_oi_o6_plrcko.nlm"  "m_np_pi_o6_plrcko.nlm" 
#> [259] "m_np_oi_o8_plrcko.nlm"  "m_np_pi_o8_plrcko.nlm"  "m_np_oi_o9_plrcko.nlm" 
#> [262] "m_np_pi_o9_plrcko.nlm"  "m_np_oi_o10_plrcko.nlm" "m_np_pi_o10_plrcko.nlm"
#> [265] "m_np_oi_o11_plrcko.nlm" "m_np_pi_o11_plrcko.nlm" "m_np_oi_o12_plrcko.nlm"
#> [268] "m_np_pi_o12_plrcko.nlm" "m_np_oi_o13_plrcko.nlm" "m_np_pi_o13_plrcko.nlm"
#> [271] "m_np_oi_o14_plrcko.nlm" "m_np_pi_o14_plrcko.nlm" "m_np_oi_o15_plrcko.nlm"
#> [274] "m_np_pi_o15_plrcko.nlm" "m_np_oi_o16_plrcko.nlm" "m_np_pi_o16_plrcko.nlm"
#> [277] "m_np_oi_Eu_plrcko.nlm"  "m_np_pi_Eu_plrcko.nlm"  "m_np_oi_pm_plrcko.nlm" 
#> [280] "m_np_pi_pm_plrcko.nlm"  "m_np_oi_tr_psrcko.nlm"  "m_np_pi_tr_psrcko.nlm" 
#> [283] "m_np_oi_eu_psrcko.nlm"  "m_np_pi_eu_psrcko.nlm"  "m_np_oi_r2_psrcko.nlm" 
#> [286] "m_np_pi_r2_psrcko.nlm"  "m_np_oi_o1_psrcko.nlm"  "m_np_pi_o1_psrcko.nlm" 
#> [289] "m_np_oi_o2_psrcko.nlm"  "m_np_pi_o2_psrcko.nlm"  "m_np_oi_o3_psrcko.nlm" 
#> [292] "m_np_pi_o3_psrcko.nlm"  "m_np_oi_o4_psrcko.nlm"  "m_np_pi_o4_psrcko.nlm" 
#> [295] "m_np_oi_o5_psrcko.nlm"  "m_np_pi_o5_psrcko.nlm"  "m_np_oi_o6_psrcko.nlm" 
#> [298] "m_np_pi_o6_psrcko.nlm"  "m_np_oi_o8_psrcko.nlm"  "m_np_pi_o8_psrcko.nlm" 
#> [301] "m_np_oi_o9_psrcko.nlm"  "m_np_pi_o9_psrcko.nlm"  "m_np_oi_o10_psrcko.nlm"
#> [304] "m_np_pi_o10_psrcko.nlm" "m_np_oi_o11_psrcko.nlm" "m_np_pi_o11_psrcko.nlm"
#> [307] "m_np_oi_o12_psrcko.nlm" "m_np_pi_o12_psrcko.nlm" "m_np_oi_o13_psrcko.nlm"
#> [310] "m_np_pi_o13_psrcko.nlm" "m_np_oi_o14_psrcko.nlm" "m_np_pi_o14_psrcko.nlm"
#> [313] "m_np_oi_o15_psrcko.nlm" "m_np_pi_o15_psrcko.nlm" "m_np_oi_o16_psrcko.nlm"
#> [316] "m_np_pi_o16_psrcko.nlm" "m_np_oi_Eu_psrcko.nlm"  "m_np_pi_Eu_psrcko.nlm" 
#> [319] "m_np_oi_pm_psrcko.nlm"  "m_np_pi_pm_psrcko.nlm"  "m_np_oi_tr_pqrcko.nlm" 
#> [322] "m_np_pi_tr_pqrcko.nlm"  "m_np_oi_eu_pqrcko.nlm"  "m_np_pi_eu_pqrcko.nlm" 
#> [325] "m_np_oi_r2_pqrcko.nlm"  "m_np_pi_r2_pqrcko.nlm"  "m_np_oi_o1_pqrcko.nlm" 
#> [328] "m_np_pi_o1_pqrcko.nlm"  "m_np_oi_o2_pqrcko.nlm"  "m_np_pi_o2_pqrcko.nlm" 
#> [331] "m_np_oi_o3_pqrcko.nlm"  "m_np_pi_o3_pqrcko.nlm"  "m_np_oi_o4_pqrcko.nlm" 
#> [334] "m_np_pi_o4_pqrcko.nlm"  "m_np_oi_o5_pqrcko.nlm"  "m_np_pi_o5_pqrcko.nlm" 
#> [337] "m_np_oi_o6_pqrcko.nlm"  "m_np_pi_o6_pqrcko.nlm"  "m_np_oi_o8_pqrcko.nlm" 
#> [340] "m_np_pi_o8_pqrcko.nlm"  "m_np_oi_o9_pqrcko.nlm"  "m_np_pi_o9_pqrcko.nlm" 
#> [343] "m_np_oi_o10_pqrcko.nlm" "m_np_pi_o10_pqrcko.nlm" "m_np_oi_o11_pqrcko.nlm"
#> [346] "m_np_pi_o11_pqrcko.nlm" "m_np_oi_o12_pqrcko.nlm" "m_np_pi_o12_pqrcko.nlm"
#> [349] "m_np_oi_o13_pqrcko.nlm" "m_np_pi_o13_pqrcko.nlm" "m_np_oi_o14_pqrcko.nlm"
#> [352] "m_np_pi_o14_pqrcko.nlm" "m_np_oi_o15_pqrcko.nlm" "m_np_pi_o15_pqrcko.nlm"
#> [355] "m_np_oi_o16_pqrcko.nlm" "m_np_pi_o16_pqrcko.nlm" "m_np_oi_Eu_pqrcko.nlm" 
#> [358] "m_np_pi_Eu_pqrcko.nlm"  "m_np_oi_pm_pqrcko.nlm"  "m_np_pi_pm_pqrcko.nlm" 
#> [361] "m_np_oi_tr_plrqko.nlm"  "m_np_pi_tr_plrqko.nlm"  "m_np_oi_eu_plrqko.nlm" 
#> [364] "m_np_pi_eu_plrqko.nlm"  "m_np_oi_r2_plrqko.nlm"  "m_np_pi_r2_plrqko.nlm" 
#> [367] "m_np_oi_o1_plrqko.nlm"  "m_np_pi_o1_plrqko.nlm"  "m_np_oi_o2_plrqko.nlm" 
#> [370] "m_np_pi_o2_plrqko.nlm"  "m_np_oi_o3_plrqko.nlm"  "m_np_pi_o3_plrqko.nlm" 
#> [373] "m_np_oi_o4_plrqko.nlm"  "m_np_pi_o4_plrqko.nlm"  "m_np_oi_o5_plrqko.nlm" 
#> [376] "m_np_pi_o5_plrqko.nlm"  "m_np_oi_o6_plrqko.nlm"  "m_np_pi_o6_plrqko.nlm" 
#> [379] "m_np_oi_o8_plrqko.nlm"  "m_np_pi_o8_plrqko.nlm"  "m_np_oi_o9_plrqko.nlm" 
#> [382] "m_np_pi_o9_plrqko.nlm"  "m_np_oi_o10_plrqko.nlm" "m_np_pi_o10_plrqko.nlm"
#> [385] "m_np_oi_o11_plrqko.nlm" "m_np_pi_o11_plrqko.nlm" "m_np_oi_o12_plrqko.nlm"
#> [388] "m_np_pi_o12_plrqko.nlm" "m_np_oi_o13_plrqko.nlm" "m_np_pi_o13_plrqko.nlm"
#> [391] "m_np_oi_o14_plrqko.nlm" "m_np_pi_o14_plrqko.nlm" "m_np_oi_o15_plrqko.nlm"
#> [394] "m_np_pi_o15_plrqko.nlm" "m_np_oi_o16_plrqko.nlm" "m_np_pi_o16_plrqko.nlm"
#> [397] "m_np_oi_Eu_plrqko.nlm"  "m_np_pi_Eu_plrqko.nlm"  "m_np_oi_pm_plrqko.nlm" 
#> [400] "m_np_pi_pm_plrqko.nlm"  "m_np_oi_tr_psrqko.nlm"  "m_np_pi_tr_psrqko.nlm" 
#> [403] "m_np_oi_eu_psrqko.nlm"  "m_np_pi_eu_psrqko.nlm"  "m_np_oi_r2_psrqko.nlm" 
#> [406] "m_np_pi_r2_psrqko.nlm"  "m_np_oi_o1_psrqko.nlm"  "m_np_pi_o1_psrqko.nlm" 
#> [409] "m_np_oi_o2_psrqko.nlm"  "m_np_pi_o2_psrqko.nlm"  "m_np_oi_o3_psrqko.nlm" 
#> [412] "m_np_pi_o3_psrqko.nlm"  "m_np_oi_o4_psrqko.nlm"  "m_np_pi_o4_psrqko.nlm" 
#> [415] "m_np_oi_o5_psrqko.nlm"  "m_np_pi_o5_psrqko.nlm"  "m_np_oi_o6_psrqko.nlm" 
#> [418] "m_np_pi_o6_psrqko.nlm"  "m_np_oi_o8_psrqko.nlm"  "m_np_pi_o8_psrqko.nlm" 
#> [421] "m_np_oi_o9_psrqko.nlm"  "m_np_pi_o9_psrqko.nlm"  "m_np_oi_o10_psrqko.nlm"
#> [424] "m_np_pi_o10_psrqko.nlm" "m_np_oi_o11_psrqko.nlm" "m_np_pi_o11_psrqko.nlm"
#> [427] "m_np_oi_o12_psrqko.nlm" "m_np_pi_o12_psrqko.nlm" "m_np_oi_o13_psrqko.nlm"
#> [430] "m_np_pi_o13_psrqko.nlm" "m_np_oi_o14_psrqko.nlm" "m_np_pi_o14_psrqko.nlm"
#> [433] "m_np_oi_o15_psrqko.nlm" "m_np_pi_o15_psrqko.nlm" "m_np_oi_o16_psrqko.nlm"
#> [436] "m_np_pi_o16_psrqko.nlm" "m_np_oi_Eu_psrqko.nlm"  "m_np_pi_Eu_psrqko.nlm" 
#> [439] "m_np_oi_pm_psrqko.nlm"  "m_np_pi_pm_psrqko.nlm"  "m_np_oi_tr_pqrqko.nlm" 
#> [442] "m_np_pi_tr_pqrqko.nlm"  "m_np_oi_eu_pqrqko.nlm"  "m_np_pi_eu_pqrqko.nlm" 
#> [445] "m_np_oi_r2_pqrqko.nlm"  "m_np_pi_r2_pqrqko.nlm"  "m_np_oi_o1_pqrqko.nlm" 
#> [448] "m_np_pi_o1_pqrqko.nlm"  "m_np_oi_o2_pqrqko.nlm"  "m_np_pi_o2_pqrqko.nlm" 
#> [451] "m_np_oi_o3_pqrqko.nlm"  "m_np_pi_o3_pqrqko.nlm"  "m_np_oi_o4_pqrqko.nlm" 
#> [454] "m_np_pi_o4_pqrqko.nlm"  "m_np_oi_o5_pqrqko.nlm"  "m_np_pi_o5_pqrqko.nlm" 
#> [457] "m_np_oi_o6_pqrqko.nlm"  "m_np_pi_o6_pqrqko.nlm"  "m_np_oi_o8_pqrqko.nlm" 
#> [460] "m_np_pi_o8_pqrqko.nlm"  "m_np_oi_o9_pqrqko.nlm"  "m_np_pi_o9_pqrqko.nlm" 
#> [463] "m_np_oi_o10_pqrqko.nlm" "m_np_pi_o10_pqrqko.nlm" "m_np_oi_o11_pqrqko.nlm"
#> [466] "m_np_pi_o11_pqrqko.nlm" "m_np_oi_o12_pqrqko.nlm" "m_np_pi_o12_pqrqko.nlm"
#> [469] "m_np_oi_o13_pqrqko.nlm" "m_np_pi_o13_pqrqko.nlm" "m_np_oi_o14_pqrqko.nlm"
#> [472] "m_np_pi_o14_pqrqko.nlm" "m_np_oi_o15_pqrqko.nlm" "m_np_pi_o15_pqrqko.nlm"
#> [475] "m_np_oi_o16_pqrqko.nlm" "m_np_pi_o16_pqrqko.nlm" "m_np_oi_Eu_pqrqko.nlm" 
#> [478] "m_np_pi_Eu_pqrqko.nlm"  "m_np_oi_pm_pqrqko.nlm"  "m_np_pi_pm_pqrqko.nlm" 
```
