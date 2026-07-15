#' Functions implemented by compatible metabolism models
#'
#' Metabolism models in streamMetabolizer all implement a
#' common set of core functions. These functions are conceptually packaged as
#' the `metab_model_interface` defined here.
#'
#' @section Functions in the interface:
#'
#' * `show(metab_model)` displays the model.
#' * [get_params()] returns a data frame of model parameters.
#' * [get_param_names()] returns required and optional parameter names.
#' * [predict_metab()] returns a data frame of metabolism predictions.
#' * [predict_DO()] returns a data frame of dissolved oxygen predictions.
#' * [get_fit()] returns the internal fitted model.
#' * [get_fitting_time()] returns the model fitting time.
#' * [get_info()] returns user-supplied metadata.
#' * [get_specs()] returns model specifications.
#' * [get_data()] returns the subdaily fitting data.
#' * [get_data_daily()] returns the daily fitting data.
#' * [get_version()] returns the package version used to fit the model.
#'
#' @name metab_model_interface
#' @rdname metab_model_interface
#' @examples
#' methods(class = "metab_model")
NULL

#### show ####
# show() is already a generic S4 function.

#### S3 generics ####

#' Extract user-supplied model metadata
#'
#' A function in the `metab_model_interface`. Returns any user-supplied
#' metadata.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns The user-supplied metadata in the original format.
#' @examples
#' get_info(metab_model(info = list(site = "Example stream")))
#' @export
#' @family metab_model_interface
get_info <- function(metab_model) {
  UseMethod("get_info")
}

#' Extract the internal fitted model
#'
#' A function in the `metab_model_interface`. Returns the internal model
#' representation as fitted to the supplied data and arguments.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns An internal model representation, which may have any class.
#' @examples
#' get_fit(metab_model(fit = list(converged = TRUE)))
#' @export
#' @family metab_model_interface
get_fit <- function(metab_model) {
  UseMethod("get_fit")
}

#' Extract model fitting time
#'
#' A function in the `metab_model_interface`. Returns the time that was taken to
#' fit the model; see [proc.time()] for details.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns A `proc_time` object.
#' @examples
#' get_fitting_time(metab_model())
#' @export
#' @family metab_model_interface
get_fitting_time <- function(metab_model) {
  UseMethod("get_fitting_time")
}

#' Extract model fitting specifications
#'
#' A function in the `metab_model_interface`. Returns the specifications that
#' were
#' passed in when fitting the metabolism model.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns The list of specifications passed to [metab()].
#' @examples
#' get_specs(metab_model(specs = list(day_start = 4, day_end = 28)))
#' @export
#' @family metab_model_interface
get_specs <- function(metab_model) {
  UseMethod("get_specs")
}


#' Extract model fitting data
#'
#' A function in the `metab_model_interface`. Returns the data that were passed
#' to
#' a metabolism model.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns A data frame.
#' @examples
#' get_data(metab_model())
#' @export
#' @family metab_model_interface
get_data <- function(metab_model) {
  UseMethod("get_data")
}

#' Extract daily model fitting data
#'
#' A function in the `metab_model_interface`. Returns the daily data that were
#' passed to a metabolism model.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns A data frame or `NULL` when no daily data were supplied.
#' @examples
#' get_data_daily(metab_model())
#' @export
#' @family metab_model_interface
get_data_daily <- function(metab_model) {
  UseMethod("get_data_daily")
}

#' Extract the streamMetabolizer version used to fit a model
#'
#' A function in the `metab_model_interface`. Returns the version of
#' streamMetabolizer that was used to fit the model.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @returns A character representation of the package version.
#' @examples
#' get_version(metab_model())
#' @export
#' @family metab_model_interface
get_version <- function(metab_model) {
  UseMethod("get_version")
}

#' Extract metabolism model parameters
#'
#' A function in the `metab_model_interface`. Returns estimates of the
#' parameters describing the rates and/or shapes of GPP, ER, or reaeration.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @param date_start A `Date` or an object coercible with [as.Date()]. The first
#'   date (inclusive) for which to report parameters. If `NA`, no filtering is
#'   done.
#' @param date_end A `Date` or an object coercible with [as.Date()]. The last
#'   date (inclusive) for which to report parameters. If `NA`, no filtering is
#'   done.
#' @param uncertainty A string. Should columns for parameter uncertainty be
#'   excluded (`"none"`), reported as standard deviations (`"sd"`), or
#'   reported as lower and upper bounds of a 95 percent confidence interval
#'   (`"ci"`)? When available (e.g., for Bayesian models), if `"ci"` then the
#'   central value will be the median (50th quantile) and the ranges will be the
#'   2.5th and 97.5th quantiles. If `"sd"` then the central value will be
#'   the mean.
#' @param messages A logical. Should warning and error messages from the fitting
#'   procedure be included in the output?
#' @param fixed A string. Should values pulled from `data_daily` (i.e., fixed
#'   rather than fitted) be treated identically (`"none"`), paired with logical
#'   columns ending in `.fixed` (`"columns"`), or converted to character and
#'   marked with a leading asterisk (`"stars"`)?
#' @param ... Other arguments passed to class-specific implementations of
#'   [get_params()].
#' @param attach.units Deprecated. A logical indicating whether to attach units
#'   to the output.
#' @returns A data frame of the parameters needed to predict GPP, ER, D, and DO,
#'   with one row per date.
#' @importFrom lifecycle deprecated is_present
#' @examples
#' dat <- data_metab("3", day_start = 12, day_end = 36)
#' mm <- metab_night(specs(mm_name("night")), data = dat)
#' get_params(mm)
#' get_params(mm, date_start = get_fit(mm)$date[2])
#' @export
#' @family metab_model_interface
#' @seealso [predict_metab()] for daily average rates of GPP and ER.
get_params <- function(
  metab_model,
  date_start = NA,
  date_end = NA,
  uncertainty = c('sd', 'ci', 'none'),
  messages = TRUE,
  fixed = c('none', 'columns', 'stars'),
  ...,
  attach.units = deprecated()
) {
  UseMethod("get_params")
}

#' Extract daily metabolism parameter names
#'
#' A function in the `metab_model_interface`. Returns vectors of the required
#' and
#' optional daily metabolism parameters for the model.
#'
#' @param metab_model A metabolism model object or model name for which to
#'   return the list of required and optional metabolism parameters.
#' @param ... Reserved for future arguments.
#' @returns A list of two vectors containing the names of required and optional
#'   daily metabolism parameters, respectively.
#' @export
#' @family metab_model_interface
get_param_names <- function(metab_model, ...) {
  UseMethod("get_param_names")
}

#' Predict metabolism from a fitted model
#'
#' A function in the `metab_model_interface`. Returns estimates of
#' GPP, ER, and K600.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @param date_start A `Date` or an object coercible with [as.Date()]. The first
#'   date (inclusive) for which to report metabolism predictions. If `NA`, no
#'   filtering is done.
#' @param date_end A `Date` or an object coercible with [as.Date()]. The last
#'   date (inclusive) for which to report metabolism predictions. If `NA`, no
#'   filtering is done.
#' @param day_start Start time (inclusive) of a day's data in number of hours
#'   from the midnight that begins the date. For example, `day_start = -1.5`
#'   indicates that data describing 2006-06-26 begin at 2006-06-25 22:30, or at
#'   the first observation time that occurs after that time if day_start doesn't
#'   fall exactly on an observation time. For daily metabolism predictions,
#'   `day_end - day_start` should probably equal 24 so that each day's estimate
#'   is representative of a 24-hour period.
#' @param day_end End time (exclusive) of a day's data in number of hours from
#'   the midnight that begins the date. For example, `day_end = 30` indicates
#'   that
#'   data describing 2006-06-26 end at the last observation time that occurs
#'   before 2006-06-27 06:00.
#' @param ... Other arguments passed to class-specific implementations of
#'   [predict_metab()].
#' @param attach.units Deprecated. A logical indicating whether to attach units
#'   to the output.
#' @param use_saved A logical. Is it OK to use predictions that were saved with
#'   the model?
#' @returns A data frame with one row per date and columns that include:
#'
#'   * `GPP`: Gross primary production, which is positive when realistic, in
#'     gO₂ m⁻² d⁻¹.
#'   * `ER`: Ecosystem respiration, which is negative when realistic, in
#'     gO₂ m⁻² d⁻¹.
#'   * `K600`: The reaeration rate, in d⁻¹.
#' @importFrom lifecycle deprecated is_present
#' @examples
#' dat <- data_metab("3", day_start = 12, day_end = 36)
#' mm <- metab_night(specs(mm_name("night")), data = dat)
#' predict_metab(mm)
#' predict_metab(mm, date_start = get_fit(mm)$date[2])
#' @export
#' @family metab_model_interface
predict_metab <- function(
  metab_model,
  date_start = NA,
  date_end = NA,
  day_start = get_specs(metab_model)$day_start,
  day_end = min(day_start + 24, get_specs(metab_model)$day_end),
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
) {
  UseMethod("predict_metab")
}


#' Predict dissolved oxygen from a fitted model
#'
#' A function in the `metab_model_interface`. Returns predictions of dissolved
#' oxygen.
#'
#' @param metab_model A metabolism model that implements the
#'   `metab_model_interface`.
#' @param date_start A `Date` or an object coercible with [as.Date()]. The first
#'   date (inclusive) for which to report DO predictions. If `NA`, no filtering
#'   is done.
#' @param date_end A `Date` or an object coercible with [as.Date()]. The last
#'   date (inclusive) for which to report DO predictions. If `NA`, no filtering
#'   is done.
#' @param ... Other arguments passed to class-specific implementations of
#'   [predict_DO()].
#' @param attach.units Deprecated. A logical indicating whether to attach units
#'   to the output.
#' @param use_saved A logical. Is it OK to use predictions that were saved with
#'   the model?
#' @returns A data frame of dissolved oxygen predictions at the temporal
#'   resolution of the input data.
#' @importFrom lifecycle deprecated is_present
#' @examples
#' dat <- data_metab("3", day_start = 12, day_end = 36)
#' mm <- metab_night(specs(mm_name("night")), data = dat)
#' preds <- predict_DO(mm, date_start = get_fit(mm)$date[3])
#' head(preds)
#' @export
#' @family metab_model_interface
predict_DO <- function(
  metab_model,
  date_start = NA,
  date_end = NA,
  ...,
  attach.units = deprecated(),
  use_saved = TRUE
) {
  UseMethod("predict_DO")
}
