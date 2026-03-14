#' Return the data types that may be used by metab_models using the
#' metab_model_interface.
#'
#' @description Produces a data.frame with the column names and
#'   data format to be used by metab_models that comply strictly with the
#'   metab_model_interface. These are the columns that may be included:
#'
#'   \itemize{
#'
#'   \item{ \code{solar.time} date-time values in mean solar time (see
#'   \code{\link{calc_solar_time}} and/or
#'   \code{\link{convert_UTC_to_solartime}}), in POSIXct format with a tzone
#'   attribute of 'UTC'. May be approximated by local, non-daylight-savings
#'   clock time (still with nominal UTC timezone but with clock noons close to
#'   solar noon), but mean solar time is better for matching model time windows
#'   to the diel cycle of light availability. Throughout this package, variables
#'   named "solar.time" are mean solar time, "app.solar.time" means apparent
#'   solar time, and "any.solar.time" means either.}
#'
#'   \item{ \code{DO.obs} dissolved oxygen concentration observations, \eqn{mg
#'   O[2] L^{-1}}{mg O2 / L}}
#'
#'   \item{ \code{DO.sat} dissolved oxygen concentrations if the water were at
#'   equilibrium saturation \eqn{mg O[2] L^{-1}}{mg O2 / L}. Calculate using
#'   \link{calc_DO_sat}}
#'
#'   \item{ \code{depth} stream depth, \eqn{m}{m}}.
#'
#'   \item{ \code{temp.water} water temperature, \eqn{degC}}.
#'
#'   \item{ \code{light} photosynthetically active radiation, \eqn{\mu mol\
#'   m^{-2} s^{-1}}{micro mols / m^2 / s}}
#'
#'   \item{ \code{date} dates of interest in Date format}
#'
#'   \item{ \code{err.obs.sigma} SD of observation error to use in simulating
#'   data}
#'
#'   \item{ \code{err.obs.phi} autocorrelation of observation error to use in
#'   simulating data}
#'
#'   \item{ \code{err.proc.sigma} SD of process error to use in simulating data}
#'
#'   \item{ \code{err.proc.phi} autocorrelation of process error to use in
#'   simulating data}
#'
#'   \item{ \code{DO.obs} dissolved oxygen concentration observations, \eqn{mg
#'   O[2] L^{-1}}{mg O2 / L}}
#'
#'   \item{ \code{GPP} daily estimates of GPP, \eqn{g O[2] m^-2 d^-1}}
#'
#'   \item{ \code{ER} daily estimates of ER, \eqn{g O[2] m^-2 d^-1}}
#'
#'   \item{ \code{K600} daily estimates of K600, \eqn{d^-1}}
#'
#'   \item{ \code{GPP.init} daily initial values of GPP, \eqn{g O[2] m^-2
#'   d^-1}}, for use in maximum likelihood estimation
#'
#'   \item{ \code{ER.init} daily initial values of ER, \eqn{g O[2] m^-2 d^-1}},
#'   for use in maximum likelihood estimation
#'
#'   \item{ \code{K600.init} daily initial values of K600, \eqn{d^-1}}, for use
#'   in maximum likelihood estimation
#'
#'   \item{ \code{discharge.daily} daily mean river discharge, \eqn{m^3 s^-1}}
#'
#'   \item{ \code{velocity.daily} daily mean river flow velocity, \eqn{m s^-1}}
#'
#'   }
#'
#' @details Most models will require a subset of these data columns. Specialized
#'   models may deviate from this format, but this is discouraged.
#'
#' @param ... column names to select, as passed to \code{\link[dplyr]{select}}
#' @param optional one or more character strings listing the columns, if any,
#'   that may be excluded. If 'all', the entire data.frame may be omitted. If
#'   'none', the entire data.frame must be included as prototyped. If specific
#'   column names are given, those columns may be omitted entirely or passed to
#'   \code{\link{metab}()} as all NAs.
#' @return data data.frame with columns as in the description
#'
#' @export
#' @importFrom lazyeval lazy_dots
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
mm_data <- function(..., optional='none') {
  dat <- data.frame(
    solar.time = as.POSIXct("2050-03-14 15:10:00", tz="UTC"),
    DO.obs =     10.1,
    DO.sat =     14.2,
    depth =      0.5,
    temp.water = 21.8,
    light =      300.9,
    discharge =  9,
    velocity =   2,
    date =       as.Date("2050-03-14"),
    DO.mod.1 =   7.5,
    err.obs.sigma = 0.01,
    err.obs.phi = 0,
    err.proc.sigma = 5,
    err.proc.phi = 0,
    GPP.daily =  5,
    Pmax =       10,
    alpha =      0.0001,
    ER.daily =   -10,
    ER20 =       -10,
    K600.daily = 10,
    K600.daily.lower = 4.5,
    K600.daily.upper = 15.6,
    init.GPP.daily =  5,
    init.Pmax =       10,
    init.alpha =      0.0001,
    init.ER.daily =   -10,
    init.ER20 =       -10,
    init.K600.daily = 10,
    discharge.daily = 9,
    velocity.daily =  2,
    GPP =       5,
    GPP.lower = 4,
    GPP.upper = 6,
    ER =        -5,
    ER.lower =  -6,
    ER.upper =  -4,
    D =         5,
    D.lower =   5,
    D.upper =   5,
    stringsAsFactors = FALSE
  )
  .dots <- lazy_dots(...)
  .nulldot <- length(.dots) == 1 && is.null(.dots[[1]]$expr)
  dat <- if(isTRUE(.nulldot)) {
    NULL
  } else if(length(.dots) == 0) {
    dat
  } else {
    .dotnames <- sapply(.dots, function(dot) as.character(dot$expr))
    dat[.dotnames]
  }

  # if dat is NULL (from passing NULL as ...), return NULL immediately
  if(is.null(dat)) return(NULL)

  # add information about which columns, if any, are optional.
  optional <- if(missing(optional)) {
    if(isTRUE(.nulldot)) {
      'all'
    } else {
      'none'
    }
  } else {
    opt <- match.arg(optional, choices=c('all','none',names(dat)), several.ok=TRUE)
    if(any(c('all','none') %in% optional) && length(optional) != 1)
      stop("if optional is 'all' or 'none', it should be length 1")
    if(all(names(dat) %in% opt))
      opt <- 'all'
    opt
  }
  attr(dat, 'optional') <- optional

  # return
  dat
}

# Because metab_models will call mm_data(...) to define their default data, it
# makes sense to declare all the potential columns as global variables here;
# otherwise we'd need to do it before defining any of those functions.
globalVariables(names(mm_data()))
