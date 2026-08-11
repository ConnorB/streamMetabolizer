# streamMetabolizer: Models for Estimating Aquatic Photosynthesis and Respiration

streamMetabolizer uses inverse modeling to estimate aquatic
photosynthesis and respiration (collectively, metabolism) from time
series data on dissolved oxygen, water temperature, depth, and light.
The package assists with data preparation, handles data gaps during
modeling, and provides tabular and graphical reports of model outputs.
Several time-honored methods are implemented along with many promising
new variants that produce more accurate and precise metabolism
estimates. This package is a fork of the original USGS-R
streamMetabolizer package. It includes enhancements and bug fixes beyond
the original version. The original package can be found at
https://github.com/DOI-USGS/streamMetabolizer.

This package uses inverse modeling to estimate aquatic photosynthesis
and respiration (collectively, metabolism) from time series data on
dissolved oxygen, water temperature, depth, and light. The package
assists with data preparation, handles data gaps during modeling, and
provides tabular and graphical reports of model outputs. Several
time-honored methods are implemented along with many promising new
variants that produce more accurate and precise metabolism estimates.

## Details

See <https://usgs-r.github.io/streamMetabolizer> for package vignettes.

## Calculate new input variables

- [`calc_depth()`](https://connorb.github.io/streamMetabolizer/reference/calc_depth.md)
  estimates stream depth from discharge.

- [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)
  calculates dissolved oxygen saturation.

- [`calc_light()`](https://connorb.github.io/streamMetabolizer/reference/calc_light.md)
  models photosynthetically active radiation.

## Convert existing input variables

- [`convert_localtime_to_UTC()`](https://connorb.github.io/streamMetabolizer/reference/convert_localtime_to_UTC.md)
  converts local time to UTC.

- [`convert_UTC_to_solartime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_solartime.md)
  converts UTC to local solar time.

- [`convert_k600_to_kGAS()`](https://connorb.github.io/streamMetabolizer/reference/convert_k600_to_kGAS.md)
  converts K600 to another gas.

- [`convert_PAR_to_SW()`](https://connorb.github.io/streamMetabolizer/reference/convert_PAR_to_SW.md)
  converts photosynthetically active radiation to shortwave radiation.

## Model metabolism

1.  [`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
    chooses a model structure.

2.  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
    sets model specifications.

3.  [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md)
    fits the model.

## Inspect model results

- [`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)
  predicts daily metabolism.

- [`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md)
  predicts dissolved oxygen.

- [`plot_metab_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_metab_preds.md)
  plots daily metabolism predictions.

- [`plot_DO_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_DO_preds.md)
  plots dissolved oxygen predictions.

- [`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md)
  extracts fitted and fixed parameters.

- [`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md)
  extracts the internal fitted model.

- [`get_mcmc()`](https://connorb.github.io/streamMetabolizer/reference/get_mcmc.md)
  extracts Bayesian MCMC model objects.

- [`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md)
  extracts the model fitting time.

## Inspect model inputs/properties

- [`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md)
  extracts model specifications.

- [`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md)
  extracts subdaily fitting data.

- [`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md)
  extracts daily fitting data.

- [`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md)
  extracts user-supplied metadata.

- [`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md)
  extracts the package version used to fit the model.

## See also

Useful links:

- <https://connorb.github.io/streamMetabolizer/>

- <https://github.com/ConnorB/streamMetabolizer>

- Report bugs at <https://github.com/ConnorB/streamMetabolizer/issues>

## Author

**Maintainer**: Connor Brown <ConnorBrown1996@gmail.com>
([ORCID](https://orcid.org/0000-0002-9680-8930)) \[contributor\]

Authors:

- Alison P. Appling <aappling@usgs.gov>
  ([ORCID](https://orcid.org/0000-0003-3638-8572))

- Robert O. Hall ([ORCID](https://orcid.org/0000-0002-0763-5346))

- Maite Arroita ([ORCID](https://orcid.org/0000-0001-8754-7604))

- Charles B. Yackulic ([ORCID](https://orcid.org/0000-0001-9661-0724))
