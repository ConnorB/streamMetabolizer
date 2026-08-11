# Model Structures

## Basic model types in streamMetabolizer

Many model structures are available in the streamMetabolizer package.
These fall into three general `type`s:

- `bayes` - Inverse Bayesian modeling of GPP, ER, and K600
- `mle` - Inverse modeling by maximum likelihood estimation of GPP, ER,
  and optionally K600
- `night` - Nighttime regression for estimation of K600 and ER

Two additional model `type`s can be useful in specific situations:

- `Kmodel` - Builds a multi-day relationship between K600 and predictor
  variables; can be used in conjunction with `mle` and/or `night`
- `sim` - Simulates dissolved oxygen “data” for model testing

## Specific model structure

A complete specification of a model structure begins with a basic `type`
but also includes other information about the model error assumptions,
any hierarchical components, the numerical integration method, the
process equations used to describe GPP and ER, and more. A model
structure is fully specified by a concise if cryptic model name.
Examples are “b_np_oipi_tr_plrckm.stan” and “m_np_oi_tr_plrckm.nlm”. You
never need to remember these concise names; instead, you can construct
and interpret such names using the `mm_name` and `mm_parse_name`
functions, respectively.

## Setting a model name

The
[`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
function creates a cryptic (but complete and concise) model name from
several options that you can specify. For example:

``` r

mm_name(type='bayes') # the default Bayesian model
```

    [1] "b_np_oipi_tr_plrckm.stan"

``` r

mm_name(type='bayes', pool_K600='normal') # a Bayesian model with simple pooling of K600 to a shared mean
```

    [1] "b_Kn_oipi_tr_plrckm.stan"

``` r

mm_name(type='mle') # the default MLE model
```

    [1] "m_np_oi_tr_plrckm.nlm"

``` r

mm_name(type='mle', ode_method='euler') # an MLE model with a simpler ode_method
```

    [1] "m_np_oi_eu_plrckm.nlm"

See the help file at
[`?mm_name`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
to learn about the structural options that you can adjust.

## Interpreting a model name

The
[`mm_parse_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md)
function splits a cryptic model name into its component features. For
example:

``` r

mm_parse_name(c('b_np_oipi_tr_plrckm.stan', 'm_np_pi_tr_psrqkm.nlm'))
```

       type pool_K600 err_obs_iid err_proc_acor err_proc_acor_light err_proc_iid
    1 bayes      none        TRUE         FALSE               FALSE         TRUE
    2   mle      none       FALSE         FALSE               FALSE         TRUE
      err_proc_GPP ode_method  GPP_fun   ER_fun deficit_src engine
    1        FALSE  trapezoid linlight constant      DO_mod   stan
    2        FALSE  trapezoid satlight  q10temp      DO_mod    nlm

See the help file at
[`?mm_parse_name`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md),
and refer again to
[`?mm_name`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
for definitions of the column names in the output of `mm_parse_name`.

## Listing all the options

You can see the full list of available model structures by calling
[`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
(here we’ll just print a sampling of the full list):

``` r

valid_names <- mm_valid_names(type=c('bayes','mle','night'))
length(valid_names)
```

    [1] 961

``` r

c(valid_names[seq(1,length(valid_names),length.out=20)], '...')
```

     [1] "b_np_oipi_tr_plrckm.stan"    "b_Kb_pp_tr_plrckm.stan"
     [3] "b_Kbx_oi_eu_plrcko.stan"     "b_Kl_oi_eu_psrcko.stan"
     [5] "b_Kl0_oi_tr_psrckm.stan"     "b_Klx_oipi_eu_plrckm.stan"
     [7] "b_Kn_oipi_eu_psrcko.stan"    "b_Kn0_oipi_tr_plrcko.stan"
     [9] "b_Knx_oipipp_eu_plrckm.stan" "b_np_oipp_eu_plrckm.stan"
    [11] "m_np_pi_o11_plrckm.nlm"      "m_np_pi_o16_psrckm.nlm"
    [13] "m_np_oi_o1_plrqkm.nlm"       "m_np_oi_o6_psrqkm.nlm"
    [15] "m_np_pi_o12_pqrqkm.nlm"      "m_np_pi_Eu_plrcko.nlm"
    [17] "m_np_oi_o2_pqrcko.nlm"       "m_np_oi_o8_plrqko.nlm"
    [19] "m_np_pi_o13_psrqko.nlm"      "n_np_pi_eu_rckf.lm"
    [21] "..."                        

## Using a model name

Once you have a model name in hand, pass that name to
[`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md)
to fit a model with the selected structure. See the
[Quickstart](https://connorb.github.io/streamMetabolizer/articles/get_started.md)
tutorial for more.
