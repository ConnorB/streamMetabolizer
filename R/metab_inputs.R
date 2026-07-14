#' Describe the requirements for an argument to metab()
#'
#' @param type the type of model you want to fit
#' @param input the name of an argument to pass into metab()
#' @returns For `data` and `data_daily`, a tibble describing the required
#'   columns when the selected model accepts that input. Otherwise, returns
#'   `NULL` invisibly after displaying guidance with [cli::cli_inform()].
#' @import dplyr
#' @examples
#' metab_inputs('night','specs')
#' metab_inputs('bayes','data')
#' metab_inputs('Kmodel','data_daily')
#' metab_inputs('mle','info')
#' @export
metab_inputs <- function(
  type = c('bayes', 'mle', 'night', 'Kmodel', 'sim'),
  input = c('specs', 'data', 'data_daily', 'info')
) {
  # check inputs
  type <- match.arg(type)
  input <- match.arg(input)

  if (input == 'specs') {
    .cli_inform(c(
      "i" = "Use {.code specs(mm_name('{type}'))}.",
      "*" = "See {.help [{.fun mm_name}](streamMetabolizer::mm_name)} and {.help [{.fun specs}](streamMetabolizer::specs)} for more options."
    ))
    invisible(NULL)
  } else if (input %in% c('data', 'data_daily')) {
    mfun <- paste0('metab_', type)
    eg <- eval(formals(mfun)[[input]])
    # reformat so there's a row each for units, format, example, and optional-T/F
    if (is.null(eg)) {
      .cli_inform(c(
        "i" = "{.val {type}} models do not use {.arg {input}}.",
        "*" = "Set {.arg {input}} to {.code NULL}."
      ))
      invisible(NULL)
    } else {
      . <- 'dplyr.var'
      units <- unname(mm_data_units()[names(eg)])
      units[is.na(units)] <- ""
      tibble::tibble(
        colname = {
          names(eg)
        },
        class = {
          sapply(unname(eg), function(col) paste0(class(col), collapse = ','))
        },
        units = units,
        need = {
          opt <- attr(eg, 'optional')
          opt_vec <- if (opt[1] == 'all') {
            rep('optional', length(eg))
          } else if (opt[1] == 'none') {
            rep('required', length(eg))
          } else {
            ifelse(names(eg) %in% opt, 'optional', 'required')
          }
        }
      )
    }
  } else if (input == 'info') {
    .cli_inform(c(
      "i" = "{.arg info} is optional metadata stored in the returned {.help [{.fun metab_model}](streamMetabolizer::metab_model)}.",
      "*" = "Use {.code NULL} (the default) or any R object, then retrieve it with {.help [{.fun get_info}](streamMetabolizer::get_info)}."
    ))
    invisible(NULL)
  }
}
