#' Describe model input data types
#'
#' @description Produces a data frame containing the column names and formats
#'   used by models that comply with [metab_model_interface()]. The available
#'   columns are:
#'
#'   * `solar.time`: Datetimes in mean solar time, stored as `POSIXct` with a
#'     `tzone` attribute of `"UTC"`. See [calc_solar_time()] and
#'     [convert_UTC_to_solartime()]. Local standard time may approximate solar
#'     time, but mean solar time better aligns model windows with the diel light
#'     cycle. In this package, `solar.time` means mean solar time,
#'     `app.solar.time` means apparent solar time, and `any.solar.time` means
#'     either.
#'   * `DO.obs`: Dissolved oxygen concentration observations, mgO₂ L⁻¹.
#'   * `DO.sat`: Dissolved oxygen concentrations at equilibrium saturation,
#'     mgO₂ L⁻¹. Calculate with [calc_DO_sat()].
#'   * `depth`: Stream depth, m.
#'   * `temp.water`: Water temperature, °C.
#'   * `light`: Photosynthetically active radiation, µmol m⁻² s⁻¹.
#'   * `date`: Dates of interest as `Date` values.
#'   * `err.obs.sigma`: Observation-error standard deviation for simulations.
#'   * `err.obs.phi`: Observation-error autocorrelation for simulations.
#'   * `err.proc.sigma`: Process-error standard deviation for simulations.
#'   * `err.proc.phi`: Process-error autocorrelation for simulations.
#'   * `GPP`: Daily GPP estimates, gO₂ m⁻² d⁻¹.
#'   * `ER`: Daily ER estimates, gO₂ m⁻² d⁻¹.
#'   * `K600`: Daily K600 estimates, d⁻¹.
#'   * `GPP.init`: Initial daily GPP values for maximum likelihood estimation,
#'     gO₂ m⁻² d⁻¹.
#'   * `ER.init`: Initial daily ER values for maximum likelihood estimation,
#'     gO₂ m⁻² d⁻¹.
#'   * `K600.init`: Initial daily K600 values for maximum likelihood estimation,
#'     d⁻¹.
#'   * `discharge.daily`: Daily mean river discharge, m³ s⁻¹.
#'   * `velocity.daily`: Daily mean river flow velocity, m s⁻¹.
#'
#' @details Most models will require a subset of these data columns. Specialized
#'   models may deviate from this format, but this is discouraged.
#'
#' @param ... Column names to select, as passed to [dplyr::select()].
#' @param optional One or more character strings listing the columns, if any,
#'   that may be excluded. If 'all', the entire data.frame may be omitted. If
#'   'none', the entire data.frame must be included as prototyped. If specific
#'   column names are given, those columns may be omitted entirely or passed to
#'   [metab()] as all NAs.
#' @returns A data frame containing the columns described above.
#'
#' @export
#' @import dplyr
#' @examples
#' # all possible columns
#' mm_data()
#'
#' # columns typical of instantaneous data
#' mm_data(solar.time, DO.obs, DO.sat, depth, temp.water, light)
#'
#' # columns typical of daily data
#' mm_data(date, K600.daily, discharge.daily, velocity.daily)
mm_data <- function(..., optional = 'none') {
  dat <- data.frame(
    solar.time = as.POSIXct("2050-03-14 15:10:00", tz = "UTC"),
    DO.obs = 10.1,
    DO.sat = 14.2,
    depth = 0.5,
    temp.water = 21.8,
    light = 300.9,
    discharge = 9,
    velocity = 2,
    date = as.Date("2050-03-14"),
    DO.mod.1 = 7.5,
    err.obs.sigma = 0.01,
    err.obs.phi = 0,
    err.proc.sigma = 5,
    err.proc.phi = 0,
    GPP.daily = 5,
    Pmax = 10,
    alpha = 0.0001,
    ER.daily = -10,
    ER20 = -10,
    K600.daily = 10,
    K600.daily.lower = 4.5,
    K600.daily.upper = 15.6,
    init.GPP.daily = 5,
    init.Pmax = 10,
    init.alpha = 0.0001,
    init.ER.daily = -10,
    init.ER20 = -10,
    init.K600.daily = 10,
    discharge.daily = 9,
    velocity.daily = 2,
    GPP = 5,
    GPP.lower = 4,
    GPP.upper = 6,
    ER = -5,
    ER.lower = -6,
    ER.upper = -4,
    D = 5,
    D.lower = 5,
    D.upper = 5,
    stringsAsFactors = FALSE
  )
  .dots <- as.list(substitute(list(...)))[-1]
  .nulldot <- length(.dots) == 1 && is.null(.dots[[1]])
  dat <- if (isTRUE(.nulldot)) {
    NULL
  } else if (length(.dots) == 0) {
    dat
  } else {
    .dotnames <- vapply(.dots, as.character, character(1))
    dat[.dotnames]
  }

  # if dat is NULL (from passing NULL as ...), return NULL immediately
  if (is.null(dat)) {
    return(NULL)
  }

  # add information about which columns, if any, are optional.
  optional <- if (missing(optional)) {
    if (isTRUE(.nulldot)) {
      'all'
    } else {
      'none'
    }
  } else {
    opt <- match.arg(
      optional,
      choices = c('all', 'none', names(dat)),
      several.ok = TRUE
    )
    if (any(c('all', 'none') %in% optional) && length(optional) != 1) {
      .cli_abort(
        "{.arg optional} must have length 1 when it contains {.val all} or {.val none}."
      )
    }
    if (all(names(dat) %in% opt)) {
      opt <- 'all'
    }
    opt
  }
  attr(dat, 'optional') <- optional

  # return
  dat
}

mm_data_units <- function() {
  c(
    solar.time = NA_character_,
    DO.obs = "mgO\u2082 L\u207B\u00B9",
    DO.sat = "mgO\u2082 L\u207B\u00B9",
    depth = "m",
    temp.water = "\u00B0C",
    light = "\u00B5mol m\u207B\u00B2 s\u207B\u00B9",
    discharge = "m\u00B3 s\u207B\u00B9",
    velocity = "m s\u207B\u00B9",
    date = NA_character_,
    DO.mod.1 = "mgO\u2082 L\u207B\u00B9",
    err.obs.sigma = "mgO\u2082 L\u207B\u00B9",
    err.obs.phi = NA_character_,
    err.proc.sigma = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    err.proc.phi = NA_character_,
    GPP.daily = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    Pmax = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    alpha = "gO\u2082 s d\u207B\u00B9 \u00B5mol\u207B\u00B9",
    ER.daily = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    ER20 = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    K600.daily = "d\u207B\u00B9",
    K600.daily.lower = "d\u207B\u00B9",
    K600.daily.upper = "d\u207B\u00B9",
    init.GPP.daily = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    init.Pmax = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    init.alpha = "gO\u2082 s d\u207B\u00B9 \u00B5mol\u207B\u00B9",
    init.ER.daily = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    init.ER20 = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    init.K600.daily = "d\u207B\u00B9",
    discharge.daily = "m\u00B3 s\u207B\u00B9",
    velocity.daily = "m s\u207B\u00B9",
    GPP = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    GPP.lower = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    GPP.upper = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    ER = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    ER.lower = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    ER.upper = "gO\u2082 m\u207B\u00B2 d\u207B\u00B9",
    D = "gO\u2082 m\u207B\u00B3 d\u207B\u00B9",
    D.lower = "gO\u2082 m\u207B\u00B3 d\u207B\u00B9",
    D.upper = "gO\u2082 m\u207B\u00B3 d\u207B\u00B9"
  )
}

# Because metab_models will call mm_data(...) to define their default data, it
# makes sense to declare all the potential columns as global variables here;
# otherwise we'd need to do it before defining any of those functions.
globalVariables(names(mm_data()))
