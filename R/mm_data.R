#' Return the data types that may be used by metab_models using the
#' metab_model_interface.
#'
#' @description Produces a data.frame with the column names and
#'   data format to be used by metab_models that comply strictly with the
#'   metab_model_interface. These are the columns that may be included:
#'
#'   \itemize{
#'
#'   \item{`solar.time` date-time values in mean solar time (see
#'   [calc_solar_time()] and/or
#'   [convert_UTC_to_solartime()]), in POSIXct format with a tzone
#'   attribute of 'UTC'. May be approximated by local, non-daylight-savings
#'   clock time (still with nominal UTC timezone but with clock noons close to
#'   solar noon), but mean solar time is better for matching model time windows
#'   to the diel cycle of light availability. Throughout this package, variables
#'   named "solar.time" are mean solar time, "app.solar.time" means apparent
#'   solar time, and "any.solar.time" means either.}
#'
#'   \item{`DO.obs` dissolved oxygen concentration observations, \eqn{mg
#'   O_2 L^{-1}}{mg O2 / L}}
#'
#'   \item{`DO.sat` dissolved oxygen concentrations if the water were at
#'   equilibrium saturation \eqn{mg O_2 L^{-1}}{mg O2 / L}. Calculate using
#'   [calc_DO_sat]}
#'
#'   \item{`depth` stream depth, \eqn{m}{m}}.
#'
#'   \item{`temp.water` water temperature, \eqn{^\circ}{ }C}
#'
#'   \item{`light` photosynthetically active radiation, \eqn{\mu mol\
#'   m^{-2} s^{-1}}{micro mols / m^2 / s}}
#'
#'   \item{`date` dates of interest in Date format}
#'
#'   \item{`err.obs.sigma` SD of observation error to use in simulating
#'   data}
#'
#'   \item{`err.obs.phi` autocorrelation of observation error to use in
#'   simulating data}
#'
#'   \item{`err.proc.sigma` SD of process error to use in simulating data}
#'
#'   \item{`err.proc.phi` autocorrelation of process error to use in
#'   simulating data}
#'
#'   \item{`DO.obs` dissolved oxygen concentration observations, \eqn{mg
#'   O_2 L^{-1}}{mg O2 / L}}
#'
#'   \item{`GPP` daily estimates of GPP, \eqn{g O_2 m^{-2} d^{-1}}}
#'
#'   \item{`ER` daily estimates of ER, \eqn{g O_2 m^{-2} d^{-1}}}
#'
#'   \item{`K600` daily estimates of K600, \eqn{d^{-1}}}
#'
#'   \item{`GPP.init` daily initial values of GPP, \eqn{g O_2 m^{-2}
#'   d^{-1}}}, for use in maximum likelihood estimation
#'
#'   \item{`ER.init` daily initial values of ER, \eqn{g O_2 m^{-2} d^{-1}}},
#'   for use in maximum likelihood estimation
#'
#'   \item{`K600.init` daily initial values of K600, \eqn{d^{-1}}}, for use
#'   in maximum likelihood estimation
#'
#'   \item{`discharge.daily` daily mean river discharge, \eqn{m^3 s^{-1}}}
#'
#'   \item{`velocity.daily` daily mean river flow velocity, \eqn{m s^{-1}}}
#'
#'   }
#'
#' @details Most models will require a subset of these data columns. Specialized
#'   models may deviate from this format, but this is discouraged.
#'
#' @param ... column names to select, as passed to [dplyr::select()]
#' @param optional one or more character strings listing the columns, if any,
#'   that may be excluded. If 'all', the entire data.frame may be omitted. If
#'   'none', the entire data.frame must be included as prototyped. If specific
#'   column names are given, those columns may be omitted entirely or passed to
#'   [metab()] as all NAs.
#' @return data data.frame with columns as in the description
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
    DO.obs = "mgO2 L^-1",
    DO.sat = "mgO2 L^-1",
    depth = "m",
    temp.water = "degC",
    light = "umol m^-2 s^-1",
    discharge = "m^3 s^-1",
    velocity = "m s^-1",
    date = NA_character_,
    DO.mod.1 = "mgO2 L^-1",
    err.obs.sigma = "mgO2 L^-1",
    err.obs.phi = NA_character_,
    err.proc.sigma = "gO2 m^-2 d^-1",
    err.proc.phi = NA_character_,
    GPP.daily = "gO2 m^-2 d^-1",
    Pmax = "gO2 m^-2 d^-1",
    alpha = "gO2 s d^-1 umol^-1",
    ER.daily = "gO2 m^-2 d^-1",
    ER20 = "gO2 m^-2 d^-1",
    K600.daily = "d^-1",
    K600.daily.lower = "d^-1",
    K600.daily.upper = "d^-1",
    init.GPP.daily = "gO2 m^-2 d^-1",
    init.Pmax = "gO2 m^-2 d^-1",
    init.alpha = "gO2 s d^-1 umol^-1",
    init.ER.daily = "gO2 m^-2 d^-1",
    init.ER20 = "gO2 m^-2 d^-1",
    init.K600.daily = "d^-1",
    discharge.daily = "m^3 s^-1",
    velocity.daily = "m s^-1",
    GPP = "gO2 m^-2 d^-1",
    GPP.lower = "gO2 m^-2 d^-1",
    GPP.upper = "gO2 m^-2 d^-1",
    ER = "gO2 m^-2 d^-1",
    ER.lower = "gO2 m^-2 d^-1",
    ER.upper = "gO2 m^-2 d^-1",
    D = "gO2 m^-3 d^-1",
    D.lower = "gO2 m^-3 d^-1",
    D.upper = "gO2 m^-3 d^-1"
  )
}

# Because metab_models will call mm_data(...) to define their default data, it
# makes sense to declare all the potential columns as global variables here;
# otherwise we'd need to do it before defining any of those functions.
globalVariables(names(mm_data()))
