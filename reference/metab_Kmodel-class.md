# Interpolation model of daily K for metabolism

`metab_Kmodel` models use initial daily estimates of K, along with
predictors such as Q (`discharge.daily`), U (`velocity.daily`), or T
(time), to leverage all available data and produce more stable daily K
estimates.

## See also

Other metab.model.classes:
[`metab_bayes-class`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes-class.md),
[`metab_mle-class`](https://connorb.github.io/streamMetabolizer/reference/metab_mle-class.md),
[`metab_model-class`](https://connorb.github.io/streamMetabolizer/reference/metab_model-class.md),
[`metab_night-class`](https://connorb.github.io/streamMetabolizer/reference/metab_night-class.md),
[`metab_sim-class`](https://connorb.github.io/streamMetabolizer/reference/metab_sim-class.md)
