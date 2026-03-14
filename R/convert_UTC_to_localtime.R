#' Convert time from UTC to local time.
#' 
#' Convert time from UTC to local time, either standard or with daylight 
#' savings. Recommended for post-analysis visualization only; most functions in 
#' streamMetabolizer use times in UTC. If you know the timezone code for your
#' local site, use \code{\link[lubridate]{with_tz}} instead.
#' 
#' @param date.time POSIXct object the date and time in UTC
#' @param latitude numeric, in degrees, either positive and unitted ("degN" or 
#'   "degS") or with sign indicating direction (positive = North)
#' @param longitude numeric, in degrees, either positive and unitted ("degE" or 
#'   "degW") or with sign indicating direction (positive = East)
#' @param time.type character. The type of time zone desired - either standard 
#'   time without any daylight savings time or daylight time where daylight 
#'   savings is on during the appropriate days
#' @importFrom lubridate with_tz
#' @references 
#' https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude
#' @export
convert_UTC_to_localtime <- function(date.time, latitude, longitude, time.type=c("standard local", "daylight local")) {

  # format checking
  time.type <- match.arg(time.type)
  if(class(date.time)[1] != "POSIXct") stop("expecting date.time as a POSIXct object")
  if(!(tz(date.time) %in% c("GMT","Etc/GMT-0","Etc/GMT+0","UTC"))) stop("expecting tz=UTC")

  # ask the cache and/or Google for the timezone at these coordinates
  tz_info <- lookup_timezone(latitude, longitude)
  
  # return in specified format
  if(time.type == "daylight local") {
    lubridate::with_tz(date.time, tz_info$tz)
  } else {
    # "POSIX has positive signs west of Greenwich" - https://opensource.apple.com/source/system_cmds/system_cmds-230/zic.tproj/datfiles/etcetera
    std.tz <- sprintf("Etc/GMT%s%d", if(tz_info$std_offset > 0) "-" else "+", abs(as.numeric(tz_info$std_offset)))
    if(std.tz %in% c("Etc/GMT+0", "Etc/GMT-0")) std.tz <- "UTC"
    lubridate::with_tz(date.time, std.tz)
  }  
}

#' Convert time from local time to UTC.
#' 
#' Convert time from local time (either standard or with daylight savings) to 
#' UTC.
#' 
#' @param local.time POSIXct date+time of interest, already in local time as
#'   specified by the tz attribute
#' @importFrom lubridate with_tz
#' @references 
#' https://stackoverflow.com/questions/23414340/convert-to-local-time-zone-using-latitude-and-longitude
#' @export
convert_localtime_to_UTC <- function(local.time) {
  return(with_tz(local.time, "UTC"))
}

