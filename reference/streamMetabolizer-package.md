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

See http://usgs-r.github.io/streamMetabolizer for vignettes on the web.

## Calculate new input variables

- [`calc_depth()`](https://connorb.github.io/streamMetabolizer/reference/calc_depth.md)

- [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)

- [`calc_light()`](https://connorb.github.io/streamMetabolizer/reference/calc_light.md)

## Convert existing input variables

- [`convert_localtime_to_UTC()`](https://connorb.github.io/streamMetabolizer/reference/convert_localtime_to_UTC.md)

- [`convert_UTC_to_solartime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_solartime.md)

- [`convert_k600_to_kGAS()`](https://connorb.github.io/streamMetabolizer/reference/convert_k600_to_kGAS.md)

- [`convert_PAR_to_SW()`](https://connorb.github.io/streamMetabolizer/reference/convert_PAR_to_SW.md)

## Model metabolism

- [`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md) 1.
  Choose a model structure

- [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md) 2.
  Set the specifications

- [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md) 3.
  Fit the model

## Inspect model results

- [`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)

- [`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md)

- [`plot_metab_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_metab_preds.md)

- [`plot_DO_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_DO_preds.md)

- [`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md)

- [`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md)

- [`get_mcmc()`](https://connorb.github.io/streamMetabolizer/reference/get_mcmc.md)
  (Bayesian models only)

- [`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md)

## Inspect model inputs/properties

- [`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md)

- [`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md)

- [`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md)

- [`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md)

- [`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md)

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

- Robert O. Hall

- Maite Arroita

- Charles B. Yackulic
