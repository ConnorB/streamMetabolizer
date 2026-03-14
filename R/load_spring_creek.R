#' Load a short dataset from Spring Creek
#'
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom lubridate with_tz
#' @importFrom lifecycle deprecated is_present
#' @param attach.units (deprecated, effectively FALSE in future) logical,
#'   default TRUE for backward compatibility. Should units be attached to the
#'   data.frame?
#' @return a data.frame, unitted if attach.units==TRUE
load_spring_creek <- function(attach.units=deprecated()) {
  # check units arguments
  if (lifecycle::is_present(attach.units)) {
    # only warn if it's TRUE
    if(isTRUE(attach.units)) lifecycle::deprecate_warn("0.12.0", "streamMetabolizer::load_spring_creek(attach.units)")
  }
  attach.units <- FALSE

  # load the file
  file.name <- system.file("extdata", "spring14.csv", package="streamMetabolizer") # data from Spring Creek, Laramie, WY
  time <- utc.time <- oxy <- temp <- solar.time <- app.solar.time <- ".dplyr.var"
  spring <- read.csv(file.name, stringsAsFactors=FALSE, header=TRUE) %>%
    transmute(
      utc.time = as.POSIXct(time, origin="1970-01-01", tz="UTC"),
      local.time = with_tz(utc.time, "America/Denver"),
      DO.obs = oxy,
      temp.water = temp) %>%
    mutate(
      DO.sat = calc_DO_sat(temp.water=temp.water, pressure.air=595*1.33322368),
      depth = 0.18,
      solar.time = convert_UTC_to_solartime(utc.time, longitude=-105.6, time.type='mean solar'),
      app.solar.time = convert_UTC_to_solartime(utc.time, longitude=-105.6, time.type='apparent solar'),
      light = convert_SW_to_PAR(calc_solar_insolation(app.solar.time=app.solar.time, latitude=41.33, max.insolation=convert_PAR_to_SW(2326)))
    )

  spring[c("solar.time","DO.obs","DO.sat","depth","temp.water","light")]
}
