# Package index

## Plotting

- [`plot_DO_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_DO_preds.md)
  : Plot predictions produced with predict_DO
- [`plot_distribs()`](https://connorb.github.io/streamMetabolizer/reference/plot_distribs.md)
  : Plot the prior/posterior distributions of a parameter
- [`plot_metab_data()`](https://connorb.github.io/streamMetabolizer/reference/plot_metab_data.md)
  : Plot metabolism input data
- [`plot_metab_preds()`](https://connorb.github.io/streamMetabolizer/reference/plot_metab_preds.md)
  : Plot predictions produced with predict_DO

## Other

- [`calc_DO_deficit()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_deficit.md)
  **\[deprecated\]** : Calculate a vector of dissolved oxygen deficits

- [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)
  **\[deprecated\]** : Calculates the equilibrium saturation
  concentration of oxygen in water at the supplied conditions

- [`calc_air_pressure()`](https://connorb.github.io/streamMetabolizer/reference/calc_air_pressure.md)
  : Calculates the average air pressure for a site

- [`calc_bins()`](https://connorb.github.io/streamMetabolizer/reference/calc_bins.md)
  : Assign continuous values in a vector to discrete bins

- [`calc_declination_angle()`](https://connorb.github.io/streamMetabolizer/reference/calc_declination_angle.md)
  : Calculate declination angle as in Yard et al. (2005)

- [`calc_depth()`](https://connorb.github.io/streamMetabolizer/reference/calc_depth.md)
  : Estimate depth from discharge and hydraulic geometry coefficients

- [`calc_hour_angle()`](https://connorb.github.io/streamMetabolizer/reference/calc_hour_angle.md)
  : Calculate hour angle as in
  http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html.

- [`calc_is_daytime()`](https://connorb.github.io/streamMetabolizer/reference/calc_is_daytime.md)
  **\[deprecated\]** : Determines if specified datetime is during the
  daytime Returns T/F indicating whether a datetime occurs during the
  daytime (sunlight hours)

- [`calc_light()`](https://connorb.github.io/streamMetabolizer/reference/calc_light.md)
  : Calculate modeled light from solar.time

- [`calc_light_merged()`](https://connorb.github.io/streamMetabolizer/reference/calc_light_merged.md)
  : Merge modeled and observed PAR into a single timeseries

- [`calc_solar_insolation()`](https://connorb.github.io/streamMetabolizer/reference/calc_solar_insolation.md)
  : Model solar insolation on a horizontal surface (W/m2 == J/s/m2) as
  in
  http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html

- [`calc_solar_time()`](https://connorb.github.io/streamMetabolizer/reference/calc_solar_time.md)
  : Calculate solar.time from local.time

- [`calc_velocity()`](https://connorb.github.io/streamMetabolizer/reference/calc_velocity.md)
  : Estimate velocity from discharge and hydraulic geometry coefficients

- [`calc_zenith_angle()`](https://connorb.github.io/streamMetabolizer/reference/calc_zenith_angle.md)
  : Calculate zenith angle as in
  http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html

- [`convert_PAR_to_SW()`](https://connorb.github.io/streamMetabolizer/reference/convert_PAR_to_SW.md)
  : Convert from photosynthetically active to shortwave radiation

- [`convert_SW_to_PAR()`](https://connorb.github.io/streamMetabolizer/reference/convert_SW_to_PAR.md)
  : Convert from shortwave to photosynthetically active radiation

- [`convert_UTC_to_localtime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_localtime.md)
  : Convert time from UTC to local time.

- [`convert_UTC_to_solartime()`](https://connorb.github.io/streamMetabolizer/reference/convert_UTC_to_solartime.md)
  : Convert DateTime from UTC to local solar time

- [`convert_k600_to_kGAS()`](https://connorb.github.io/streamMetabolizer/reference/convert_k600_to_kGAS.md)
  : Returns the gas exchange velocity for gas of interest w/ no unit
  conversions

- [`convert_kGAS_to_k600()`](https://connorb.github.io/streamMetabolizer/reference/convert_kGAS_to_k600.md)
  : Returns the gas exchange velocity as k600 for gas of interest w/ no
  unit conversions

- [`convert_localtime_to_UTC()`](https://connorb.github.io/streamMetabolizer/reference/convert_localtime_to_UTC.md)
  : Convert time from local time to UTC.

- [`convert_solartime_to_UTC()`](https://connorb.github.io/streamMetabolizer/reference/convert_solartime_to_UTC.md)
  : Convert DateTime from local solar time to UTC

- [`create_calc_DO()`](https://connorb.github.io/streamMetabolizer/reference/create_calc_DO.md)
  : Create a function to compute the numerical integration of a dDOdt
  function

- [`create_calc_NLL()`](https://connorb.github.io/streamMetabolizer/reference/create_calc_NLL.md)
  : Create a function to compute the negative log likelihood of a set of
  metabolism parameter values

- [`create_calc_dDOdt()`](https://connorb.github.io/streamMetabolizer/reference/create_calc_dDOdt.md)
  : Create a function that generates a 1-day timeseries of DO.mod

- [`data_metab()`](https://connorb.github.io/streamMetabolizer/reference/data_metab.md)
  : Get a demo dataset for modeling metabolism

- [`get_data()`](https://connorb.github.io/streamMetabolizer/reference/get_data.md)
  : Extract the fitting data from a metabolism model.

- [`get_data_daily()`](https://connorb.github.io/streamMetabolizer/reference/get_data_daily.md)
  : Extract the daily fitting data, if any, from a metabolism model.

- [`get_fit()`](https://connorb.github.io/streamMetabolizer/reference/get_fit.md)
  : Extract the internal model from a metabolism model.

- [`get_fitting_time()`](https://connorb.github.io/streamMetabolizer/reference/get_fitting_time.md)
  : Extract the amount of time that was required to fit the metabolism
  model.

- [`get_info()`](https://connorb.github.io/streamMetabolizer/reference/get_info.md)
  : Extract the user-supplied metadata about a metabolism model.

- [`get_log()`](https://connorb.github.io/streamMetabolizer/reference/get_log.md)
  : Return the log file(s) from a model run

- [`get_mcmc()`](https://connorb.github.io/streamMetabolizer/reference/get_mcmc.md)
  : Extract any MCMC model objects that were stored with the model

- [`get_mcmc_data()`](https://connorb.github.io/streamMetabolizer/reference/get_mcmc_data.md)
  : Extract any MCMC data list(s) that were stored with the model

- [`get_param_names()`](https://connorb.github.io/streamMetabolizer/reference/get_param_names.md)
  : Extract the daily parameter names from a metabolism model.

- [`get_params()`](https://connorb.github.io/streamMetabolizer/reference/get_params.md)
  : Extract the metabolism parameters (fitted and/or fixed) from a
  model.

- [`get_specs()`](https://connorb.github.io/streamMetabolizer/reference/get_specs.md)
  : Extract the fitting specifications from a metabolism model.

- [`get_version()`](https://connorb.github.io/streamMetabolizer/reference/get_version.md)
  : Extract the version of streamMetabolizer that was used to fit the
  model.

- [`load_french_creek()`](https://connorb.github.io/streamMetabolizer/reference/load_french_creek.md)
  : Load a short dataset from French Creek

- [`load_french_creek_std()`](https://connorb.github.io/streamMetabolizer/reference/load_french_creek_std.md)
  : Load a short dataset from French Creek using Bob Hall's code

- [`load_french_creek_std_mle()`](https://connorb.github.io/streamMetabolizer/reference/load_french_creek_std_mle.md)
  : Generate outputs using Bob's code for comparison

- [`load_spring_creek()`](https://connorb.github.io/streamMetabolizer/reference/load_spring_creek.md)
  : Load a short dataset from Spring Creek

- [`lookup_timezone()`](https://connorb.github.io/streamMetabolizer/reference/lookup_timezone.md)
  : Determine the local time zone from the coordinates

- [`lookup_usgs_elevation()`](https://connorb.github.io/streamMetabolizer/reference/lookup_usgs_elevation.md)
  : Use USGS API (USGS Elevation Point Query Service) to determine
  approximate local elevation

- [`metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.md)
  : Fit a metabolism model to data

- [`metab_Kmodel-class`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel-class.md)
  : Interpolation model of daily K for metabolism

- [`metab_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/metab_Kmodel.md)
  : Combine a time series of K estimates to predict consistent values

- [`metab_bayes-class`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes-class.md)
  : Metabolism model fitted by Bayesian MCMC

- [`metab_bayes()`](https://connorb.github.io/streamMetabolizer/reference/metab_bayes.md)
  : Basic Bayesian metabolism model fitting function

- [`metab_inputs()`](https://connorb.github.io/streamMetabolizer/reference/metab_inputs.md)
  : Describe the requirements for an argument to metab()

- [`metab_mle-class`](https://connorb.github.io/streamMetabolizer/reference/metab_mle-class.md)
  : Metabolism model fitted by maximum likelihood estimation

- [`metab_mle()`](https://connorb.github.io/streamMetabolizer/reference/metab_mle.md)
  : Maximum likelihood metabolism model fitting function

- [`metab_model-class`](https://connorb.github.io/streamMetabolizer/reference/metab_model-class.md)
  : A metabolism model class.

- [`metab_model()`](https://connorb.github.io/streamMetabolizer/reference/metab_model.md)
  : Create a metab_model object.

- [`metab_model_interface`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md)
  :

  Functions implemented by any `streamMetabolizer`-compatible metabolism
  model.

- [`metab_night-class`](https://connorb.github.io/streamMetabolizer/reference/metab_night-class.md)
  : Reaeration model fitted by nighttime regression

- [`metab_night()`](https://connorb.github.io/streamMetabolizer/reference/metab_night.md)
  : Nighttime regression for K estimation

- [`metab_night_predict_1ply()`](https://connorb.github.io/streamMetabolizer/reference/metab_night_predict_1ply.md)
  : Helper to predict_DO.metab_model

- [`metab_sim-class`](https://connorb.github.io/streamMetabolizer/reference/metab_sim-class.md)
  : Data simulator

- [`metab_sim()`](https://connorb.github.io/streamMetabolizer/reference/metab_sim.md)
  : Simulate dissolved oxygen data from input data

- [`mm_data()`](https://connorb.github.io/streamMetabolizer/reference/mm_data.md)
  : Return the data types that may be used by metab_models using the
  metab_model_interface.

- [`mm_filter_valid_days()`](https://connorb.github.io/streamMetabolizer/reference/mm_filter_valid_days.md)
  : Remove entries in data

- [`mm_get_timestep()`](https://connorb.github.io/streamMetabolizer/reference/mm_get_timestep.md)
  : Return the average timestep in days

- [`mm_is_valid_day()`](https://connorb.github.io/streamMetabolizer/reference/mm_is_valid_day.md)
  : Validate one day of data, returning a vector of error strings if
  needed

- [`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/reference/mm_model_by_ply.md)
  : Split and label data into \>=24-hr days for fitting daily metabolism

- [`mm_model_by_ply_prototype()`](https://connorb.github.io/streamMetabolizer/reference/mm_model_by_ply_prototype.md)
  : A prototype for the model_fun argument to mm_model_by_ply

- [`mm_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_name.md)
  : Find the name of a model by its features

- [`mm_parse_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_parse_name.md)
  : Parse a model name into its features

- [`mm_predict_DO_1ply()`](https://connorb.github.io/streamMetabolizer/reference/mm_predict_DO_1ply.md)
  : Helper to predict_DO.metab_model

- [`mm_predict_metab_1ply()`](https://connorb.github.io/streamMetabolizer/reference/mm_predict_metab_1ply.md)
  : Helper to predict_metab.metab_model

- [`mm_valid_names()`](https://connorb.github.io/streamMetabolizer/reference/mm_valid_names.md)
  : Get the valid names for a given model type or types

- [`mm_validate_data()`](https://connorb.github.io/streamMetabolizer/reference/mm_validate_data.md)
  : Evaluate whether the data argument is properly formatted.

- [`mm_validate_name()`](https://connorb.github.io/streamMetabolizer/reference/mm_validate_name.md)
  : Check the validity of a model name

- [`predict_DO()`](https://connorb.github.io/streamMetabolizer/reference/predict_DO.md)
  : Predict DO from a fitted model.

- [`predict_metab()`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.md)
  : Predict metabolism from a fitted model.

- [`predict_metab(`*`<metab_Kmodel>`*`)`](https://connorb.github.io/streamMetabolizer/reference/predict_metab.metab_Kmodel.md)
  : Override generic predict_metab for metab_Kmodel, which can't predict
  metab

- [`prepdata_Kmodel()`](https://connorb.github.io/streamMetabolizer/reference/prepdata_Kmodel.md)
  : Prepare data_daily by aggregating any daily data, renaming
  K600.daily to K600.daily.obs, & setting data_daily\$weight to reflect
  user weights & filters

- [`print(`*`<logs_metab>`*`)`](https://connorb.github.io/streamMetabolizer/reference/print.logs_metab.md)
  : Print metab logs

- [`print(`*`<specs>`*`)`](https://connorb.github.io/streamMetabolizer/reference/print.specs.md)
  : Display the specs object

- [`revise()`](https://connorb.github.io/streamMetabolizer/reference/revise.md)
  : Change or add named elements of a list

- [`show(`*`<metab_model>`*`)`](https://connorb.github.io/streamMetabolizer/reference/show-metab_model-method.md)
  : Display the metab_model object

- [`show(`*`<specs>`*`)`](https://connorb.github.io/streamMetabolizer/reference/show-specs-method.md)
  : Display the specs object

- [`sim_Kb()`](https://connorb.github.io/streamMetabolizer/reference/sim_Kb.md)
  : Simulate a lnK ~ lnQ relationship in the Kb format

- [`sim_pred_Kb()`](https://connorb.github.io/streamMetabolizer/reference/sim_pred_Kb.md)
  : Predict ln(K600) as Kb-style function of discharge

- [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  : Generate a coherent list of model specs

- [`streamMetabolizer-deprecated`](https://connorb.github.io/streamMetabolizer/reference/streamMetabolizer-deprecated.md)
  : Deprecated Functions in package streamMetabolizer

- [`to_degrees()`](https://connorb.github.io/streamMetabolizer/reference/to_degrees.md)
  : Convert radians to degrees

- [`to_radians()`](https://connorb.github.io/streamMetabolizer/reference/to_radians.md)
  : Convert degrees to radians
