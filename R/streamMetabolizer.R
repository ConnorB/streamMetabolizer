#' Model aquatic ecosystem metabolism in streams
#'
#' This package uses inverse modeling to estimate aquatic photosynthesis and
#' respiration (collectively, metabolism) from time series data on dissolved
#' oxygen, water temperature, depth, and light. The package assists with data
#' preparation, handles data gaps during modeling, and provides tabular and
#' graphical reports of model outputs. Several time-honored methods are
#' implemented along with many promising new variants that produce more accurate
#' and precise metabolism estimates.
#'
#' See <https://usgs-r.github.io/streamMetabolizer> for package vignettes.
#'
#' @section Calculate new input variables:
#'
#' * [calc_depth()] estimates stream depth from discharge.
#' * [calc_DO_sat()] calculates dissolved oxygen saturation.
#' * [calc_light()] models photosynthetically active radiation.
#'
#' @section Convert existing input variables:
#'
#' * [convert_localtime_to_UTC()] converts local time to UTC.
#' * [convert_UTC_to_solartime()] converts UTC to local solar time.
#' * [convert_k600_to_kGAS()] converts K600 to another gas.
#' * [convert_PAR_to_SW()] converts photosynthetically active radiation to
#'   shortwave radiation.
#'
#' @section Model metabolism:
#'
#' 1. [mm_name()] chooses a model structure.
#' 2. [specs()] sets model specifications.
#' 3. [metab()] fits the model.
#'
#' @section Inspect model results:
#'
#' * [predict_metab()] predicts daily metabolism.
#' * [predict_DO()] predicts dissolved oxygen.
#' * [plot_metab_preds()] plots daily metabolism predictions.
#' * [plot_DO_preds()] plots dissolved oxygen predictions.
#' * [get_params()] extracts fitted and fixed parameters.
#' * [get_fit()] extracts the internal fitted model.
#' * [get_mcmc()] extracts Bayesian MCMC model objects.
#' * [get_fitting_time()] extracts the model fitting time.
#'
#' @section Inspect model inputs/properties:
#'
#' * [get_specs()] extracts model specifications.
#' * [get_data()] extracts subdaily fitting data.
#' * [get_data_daily()] extracts daily fitting data.
#' * [get_info()] extracts user-supplied metadata.
#' * [get_version()] extracts the package version used to fit the model.
#'
#' @keywords internal
"_PACKAGE"
