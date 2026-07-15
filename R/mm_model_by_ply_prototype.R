#' A prototype for the model_fun argument to mm_model_by_ply
#'
#' This function does nothing but has the proper form for a model_fun passed to
#' [mm_model_by_ply()] Other functions to be used as model_fun may
#' call inheritParams to use the parameter definitions given here.
#'
#' @param data_ply A data frame or tibble containing all relevant, validated
#'   modeling data for a single ply of data. (1 ply ~= 1 date, although the day
#'   length has been specified by day_start and day_end and may not be exactly
#'   24 hours).
#' @param data_daily_ply `NULL` or a data frame or tibble containing inputs with
#'   a daily timestep.
#' @param day_start Start time (inclusive) of a day's data in number of hours
#'   from the midnight that begins the date. For example, day_start=-1.5
#'   indicates that data describing 2006-06-26 begin at 2006-06-25 22:30, or at
#'   the first observation time that occurs after that time if day_start doesn't
#'   fall exactly on an observation time.
#' @param day_end End time (exclusive) of a day's data in number of hours from
#'   the midnight that begins the date. For example, day_end=30 indicates that
#'   data describing 2006-06-26 end at the last observation time that occurs
#'   before 2006-06-27 06:00.
#' @param ply_date The modal date of this ply of data and data_daily, and the
#'   date by which this ply should be referred topresent.
#' @param timestep_days Numeric length of the mean timestep for this day, if
#'   requested by setting `timestep_days` to `TRUE` or a numeric value
#'   in the call to [mm_model_by_ply()].
#' @param ply_validity The output of `mm_is_valid_day` as applied to this
#'   data_ply for those tests specified in `day_tests`. Those tests will
#'   have been run before this function is called. The result is TRUE if the ply
#'   is entirely valid, or a character vector containing one or more error
#'   messages if any tests failed.
#' @param ... Other args that were passed untouched from the function calling
#'   mm_model_by_ply, through mm_model_by_ply, and finally to this function.
#' @returns A one-row tibble summarizing the supplied data ply and arguments.
#' @import dplyr
#' @import tibble
#' @examples
#' mm_model_by_ply_prototype()
#' mm_model_by_ply_prototype(extra_arg=7:12)
#' @export
mm_model_by_ply_prototype <- function(
  data_ply = mm_data(NULL),
  data_daily_ply = mm_data(NULL),
  day_start = NA,
  day_end = NA,
  ply_date = NA,
  ply_validity = NA,
  timestep_days = NA,
  ...
) {
  c(
    list(
      data_ply_start = if (!is.null(data_ply)) {
        data_ply[['solar.time']][1]
      } else {
        NA
      },
      data_ply_end = if (!is.null(data_ply)) {
        data_ply[['solar.time']][nrow(data_ply)]
      } else {
        NA
      },
      data_ply_nrow = if (!is.null(data_ply)) nrow(data_ply) else 0,
      data_daily_ply_date = paste0(
        if (!is.null(data_daily_ply)) unique(data_daily_ply$date),
        collapse = ';'
      ),
      day_start = day_start,
      day_end = day_end,
      ply_date = ply_date,
      ply_validity = paste0(ply_validity, collapse = ';'),
      timestep_days = timestep_days
    ),
    lapply(list(...), function(arg) {
      if (is.atomic(arg)) arg[1] else paste0('len=', length(arg))
    })
  ) |>
    as_tibble()
}
