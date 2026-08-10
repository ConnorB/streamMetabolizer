# Metabolism model class

Metabolism model class

## Slots

- `info`:

  Any metadata the user chooses to package with metabolism model.

- `metab_daily`:

  A data.frame of daily metabolism predictions.

- `fit`:

  An internal representation of a fitted model.

- `fitting_time`:

  Usually stored as a proc_time; the time required to fit the model.

- `pkg_version`:

  A string indicating the package version used to create this
  metab_model object.

- `specs`:

  A list of model specifications that were supplied to the fitting
  function.

- `data`:

  The data that were used to fit the model.

- `data_daily`:

  The daily data, if any, that were used to fit the model.

## See also

Other metab.model.classes:
[`metab_Kmodel-class`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_Kmodel-class.md),
[`metab_bayes-class`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_bayes-class.md),
[`metab_mle-class`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_mle-class.md),
[`metab_night-class`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_night-class.md),
[`metab_sim-class`](https://connorb.github.io/streamMetabolizer/dev/reference/metab_sim-class.md)
