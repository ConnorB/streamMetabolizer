#' Functions for calculating ecosystem metabolism in streams
#'
#' This package uses inverse modeling to estimate aquatic photosynthesis and
#' respiration (collectively, metabolism) from time series data on dissolved
#' oxygen, water temperature, depth, and light. The package assists with data
#' preparation, handles data gaps during modeling, and provides tabular and
#' graphical reports of model outputs. Several time-honored methods are
#' implemented along with many promising new variants that produce more accurate
#' and precise metabolism estimates.
#' 
#' See http://usgs-r.github.io/streamMetabolizer for vignettes on the web.
#'
#' @section Calculate new input variables:
#'
#'   \itemize{
#'
#'   \item \code{\link{calc_depth}}
#'
#'   \item \code{\link{calc_DO_sat}}
#'
#'   \item \code{\link{calc_light}}
#'
#'   }
#'
#' @section Convert existing input variables:
#'
#'   \itemize{
#'
#'   \item \code{\link{convert_date_to_doyhr}}
#'
#'   \item \code{\link{convert_localtime_to_UTC}}
#'
#'   \item \code{\link{convert_UTC_to_solartime}}
#'
#'   \item \code{\link{convert_k600_to_kGAS}}
#'
#'   \item \code{\link{convert_PAR_to_SW}}
#'
#'   }
#'
#' @section Model metabolism:
#'
#'   \itemize{
#'
#'   \item \code{\link{mm_name}} 1. Choose a model structure
#'
#'   \item \code{\link{specs}} 2. Set the specifications
#'
#'   \item \code{\link{metab}} 3. Fit the model
#'
#'   }
#'
#' @section Inspect model results:
#'
#'   \itemize{
#'
#'   \item \code{\link{predict_metab}}
#'
#'   \item \code{\link{predict_DO}}
#'
#'   \item \code{\link{plot_metab_preds}}
#'
#'   \item \code{\link{plot_DO_preds}}
#'
#'   \item \code{\link{get_params}}
#'
#'   \item \code{\link{get_fit}}
#'
#'   \item \code{\link{get_mcmc}} (Bayesian models only)
#'
#'   \item \code{\link{get_fitting_time}}
#'
#'
#'   }
#'
#' @section Inspect model inputs/properties:
#'
#'   \itemize{
#'
#'   \item \code{\link{get_specs}}
#'
#'   \item \code{\link{get_data}}
#'
#'   \item \code{\link{get_data_daily}}
#'
#'   \item \code{\link{get_info}}
#'
#'   \item \code{\link{get_version}}
#'
#'   }
#'
#' @keywords internal
"_PACKAGE"

