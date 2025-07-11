#' @import methods
#' @importFrom utils available.packages contrib.url
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "streamMetabolizer (fork) - based on the original USGS-R package:\n",
    "  https://github.com/DOI-USGS/streamMetabolizer\n",
    "This fork is maintained at:\n",
    "  https://github.com/ConnorB/streamMetabolizer\n\n",
    "The original package was a USGS Archive Research Package:\n",
    "  https://owi.usgs.gov/R/packages.html#research\n",
    "Original project funding has ended. For historical documentation,\n",
    "see:\n",
    "  https://github.com/DOI-USGS/streamMetabolizer\n\n",
    "For issues specific to this fork, please open them at:\n",
    "  https://github.com/ConnorB/streamMetabolizer/issues"
  )
  
  
  # Load deSolve because otherwise after a few model runs we're likely to get
  # the following error. (It's possible this has been resolved by moving deSolve
  # from Suggests to Imports)
  requireNamespace('deSolve', quietly=TRUE)
  ## Error in .Call("call_rkFixed", as.double(y), as.double(times), Func, Initfunc,  :
  ##   "call_rkFixed" not resolved from current namespace (deSolve)
  ## Error in .C("unlock_solver") :
  ##   "unlock_solver" not resolved from current namespace (deSolve)
}

library(methods)

#' Define a package environment for storing data specific to a project during an
#' R session
#'
#' @importFrom unitted u
#' @importFrom lifecycle deprecate_warn
#' @return the package environment
#' @keywords internal
define_pkg_env <- function() {
  pkg.env <- new.env()
  pkg.env$tz_lookups <- list(
    # populate with values that are used in test-convert.R and load_french_creek.R
    "51.5000000000,-120.0000000000"=list(tz="America/Vancouver", dst_offset=u(0,"hours"), std_offset=u(8,"hours"), retry=0),
    "51.4800000000,-0.0000000000"=list(tz="Europe/London", dst_offset=u(0,"hours"), std_offset=u(0,"hours"), retry=0),
    "41.0000000000,105.3000000000"=list(tz="Asia/Shanghai", dst_offset=u(0,"hours"), std_offset=u(8,"hours"), retry=0),
    "37.0000000000,-105.3000000000"=list(tz="America/Denver", dst_offset=u(0,"hours"), std_offset=u(-7,"hours"), retry=0),
    "34.0000000000,-80.0000000000"=list(tz="America/New_York", dst_offset=u(0,"hours"), std_offset=u(-5,"hours"), retry=0),
    "44.3625940000,-106.7530990000"=list(tz="America/Denver", dst_offset=u(0,"hours"), std_offset=u(-7,"hours"), retry=0),
    "41.3300000000,-106.3000000000"=list(tz="America/Denver", dst_offset=u(0,"hours"), std_offset=u(-7,"hours"), retry=0), # French Creek
    "40.0000000000,-105.3000000000"=list(tz="America/Denver", dst_offset=u(0,"hours"), std_offset=u(-7,"hours"), retry=0),
    "40.0000000000,105.3000000000"=list(tz="Asia/Shanghai", dst_offset=u(0,"hours"), std_offset=u(8,"hours"), retry=0)
  )
  return(pkg.env)
}
pkg.env <- define_pkg_env()
