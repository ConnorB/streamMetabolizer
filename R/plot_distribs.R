#' Plot the prior/posterior distributions of a parameter
#'
#' Plot the prior and/or posterior disitrubtions as implied by the
#' hyperparameters in a specs list and/or the
#'
#' @param dist_data Either a specs list (for priors only) or a metab_model
#'   object (for both priors and posteriors).
#' @param parname character. the name of the parameter whose distribution(s) you
#'   wish to plot
#' @param index integer or logical. Applicable only if plotting posteriors, and
#'   useful only if the parname is for a parameter having multiple (e.g., daily)
#'   instances. In this case, the index selects the instance and corresponds to
#'   the row number in the data.frame element of `get_fit(metab_model)`
#'   that contains the parameter, e.g. `get_fit(metab_model)$daily` for
#'   `'GPP_daily'`. The default, TRUE, selects and pools all instances of
#'   the parameter.
#' @param style character indicating which graphics package to use
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stats dunif qnorm dnorm qlnorm dlnorm qbeta dbeta qgamma dgamma qcauchy dcauchy rcauchy density
#' @export
#' @examples
#' \dontrun{
#' # priors only
#' plot_distribs(specs('bayes', K600_daily_mu=30), 'K600_daily')
#'
#' # posteriors, too
#' mm <- metab(specs(mm_name('bayes')), data=data_metab('1', res='30'))
#' plot_distribs(mm, 'GPP_daily', 1)
#'
#' # with modifications
#' plot_distribs(mm, 'err_proc_iid_sigma') |>
#'   dygraphs::dyRangeSelector(dateWindow=c(-0.1,1.3)) |>
#'   dygraphs::dyAxis(name='y', valueRange=c(0,15))
#' }
plot_distribs <- function(
  dist_data,
  parname = c(
    'GPP_daily',
    'alpha',
    'Pmax',
    'ER_daily',
    'K600_daily',
    'K600_daily_meanlog',
    'lnK600_lnQ_intercept',
    'lnK600_lnQ_slope',
    'K600_lnQ_nodes',
    'K600_daily_sdlog',
    'K600_daily_sigma',
    'err_obs_iid_sigma',
    'err_proc_acor_phi',
    'err_proc_acor_sigma',
    'err_proc_iid_sigma',
    'err_mult_GPP_sdlog'
  ),
  index = TRUE,
  style = c('dygraphs', 'ggplot2')
) {
  # choosing not to expose this as an arg because most people shouldn't need it,
  # but want it up top to ease manual exploration of parameter scaling
  plot_prior_rescaled <- FALSE

  style <- match.arg(style)
  if (!class(dist_data)[1] %in% c('specs', 'metab_bayes')) {
    .cli_abort(
      "{.arg dist_data} must be a {.cls specs} or {.cls metab_bayes} object, not {.obj_type_friendly {dist_data}}."
    )
  }

  # extract just the parameters we're interested in. do it this way rather than
  # with match.arg because we can provide a more useful error message here
  sp <- switch(
    class(dist_data)[1],
    specs = dist_data,
    metab_bayes = get_specs(dist_data)
  )
  hpspecs <- if (length(parname) != 1) {
    c()
  } else {
    which(grepl(paste0("^", parname, "_"), names(sp)))
  }
  knownpars <- eval(formals(plot_distribs)$parname)
  if (length(hpspecs) == 0 || !parname %in% knownpars) {
    couldabeen <- unlist(lapply(knownpars, function(kp) {
      if (any(grepl(paste0("^", kp, "_"), names(sp)))) kp else c()
    }))
    msg1 <- if (length(parname) > 1) {
      paste0("parname must have length 1.")
    } else {
      paste0(
        "could not find ",
        parname,
        " hyperparameters in get_specs(dist_data)."
      )
    }
    .cli_abort(c(msg1, "i" = "Try {.val {couldabeen}}."))
  }
  hyperpars <- sp[hpspecs]
  names(hyperpars) <- substring(names(hyperpars), nchar(parname) + 2)

  # determine the appropriate distribution
  distrib <- c(
    GPP_daily = 'normal',
    alpha = 'lognormal',
    Pmax = 'normal',
    ER_daily = 'normal',
    K600_daily = 'lognormal',
    # Kn
    K600_daily_meanlog = 'lognormal',
    # Kl
    lnK600_lnQ_intercept = 'normal',
    lnK600_lnQ_slope = 'normal',
    # Kb
    K600_lnQ_nodes = 'lognormal', # K600_lnQ_nodediffs='lognormal', # means are [k-1]th values so are hard to represent here
    # Kn, Kl, and Kb
    K600_daily_sdlog = 'halfnormal',
    K600_daily_sigma = 'halfnormal',
    # errors
    err_obs_iid_sigma = 'halfcauchy',
    err_proc_acor_phi = 'beta',
    err_proc_acor_sigma = 'halfcauchy',
    err_proc_iid_sigma = 'halfcauchy',
    err_mult_GPP_sdlog = 'halfnormal'
  )[parname]

  # create a data.frame illustrating the prior distribution
  indexed_prior <- FALSE # the usual case
  . <- '.dplyr.var'
  densdf <- switch(
    distrib,
    beta = {
      xlim <- qbeta(c(0, 1), shape1 = hyperpars$alpha, shape2 = hyperpars$beta)
      tibble::tibble(
        dist = 'prior',
        x = seq(xlim[1], xlim[2], length.out = 1000),
        y = dbeta(x, shape1 = hyperpars$alpha, shape2 = hyperpars$beta)
      )
    },
    gamma = {
      xlim <- qgamma(c(0, 0.99), shape = hyperpars$shape, rate = hyperpars$rate)
      tibble::tibble(
        dist = 'prior',
        x = seq(xlim[1], xlim[2], length.out = 1000),
        y = dgamma(x, shape = hyperpars$shape, rate = hyperpars$rate)
      )
    },
    halfcauchy = {
      # half-Cauchy and half-normal are similarly shaped except that half-Cauchy
      # has heavier tails and therefore, for large values of scale, can be a
      # weaker prior than a corresponding half-normal. See
      # https://www.stat.columbia.edu/~gelman/research/published/taumain.pdf and
      # http://web.ipac.caltech.edu/staff/fmasci/home/mystats/CauchyVsGaussian.pdf
      xlim <- qcauchy(c(0.5, 0.95), location = 0, scale = hyperpars$scale)
      bind_rows(
        tibble::tibble(
          dist = 'prior',
          x = seq(xlim[1], xlim[2], length.out = 1000),
          y = dcauchy(x, location = 0, scale = hyperpars$scale)
        ),
        (rcauchy(1000000, 0, 1) * hyperpars$scale) |>
          density(from = xlim[1], to = xlim[2]) |>
          (\(x) x[c('x', 'y')])() |>
          as.data.frame() |>
          mutate(dist = 'prior_rescaled')
      )
    },
    halfnormal = {
      xlim <- qnorm(c(0.5, 0.999), mean = 0, sd = hyperpars$sigma)
      tibble::tibble(
        dist = 'prior',
        x = seq(xlim[1], xlim[2], length.out = 1000),
        y = dnorm(x, mean = 0, sd = hyperpars$sigma)
      )
    },
    lognormal = {
      # prior_rescaled and prior diverge from one another as sdlog goes > 1.
      # this seems to be mainly due to numerical computation/algorithm
      # challenges in density(), rather than any problem with the scaling
      # equation: high sdlog means high peak near 0 but also really long tail.
      # high sdlog may not actually be a problem for MCMCs, but regardless, it's
      # better to adjust meanlog than sdlog for getting a distribution close to
      # 0; e.g., meanlog=-5, sdlog=1 gives distribution w/ peak at ~0.003 and
      # not-too-long tail
      # K600_lnQ_nodes priors are indexed
      if (length(hyperpars$meanlog) > 1 || length(hyperpars$sdlog) > 1) {
        indexed_prior <- TRUE
        if (!isTRUE(index)) {
          if (length(index) > 1) {
            .cli_warn("Using only {.code index[1]} for the prior.")
          }
          if (length(hyperpars$meanlog) > 1) {
            hyperpars$meanlog <- hyperpars$meanlog[index[1]]
          }
          if (length(hyperpars$sdlog) > 1) {
            hyperpars$sdlog <- hyperpars$sdlog[index[1]]
          }
        } else {
          .cli_warn(
            "Multiple priors are available for {.arg parname}; showing only the first."
          )
          hyperpars$meanlog <- hyperpars$meanlog[1]
          hyperpars$sdlog <- hyperpars$sdlog[1]
        }
      }
      xlim <- qlnorm(
        c(0.0001, 0.9),
        meanlog = hyperpars$meanlog,
        sdlog = hyperpars$sdlog
      )
      bind_rows(
        tibble::tibble(
          dist = 'prior',
          x = exp(seq(log(xlim[1]), log(xlim[2]), length.out = 1000)),
          y = dlnorm(x, meanlog = hyperpars$meanlog, sdlog = hyperpars$sdlog)
        ),
        exp(hyperpars$meanlog + rnorm(1000000, 0, 1) * hyperpars$sdlog) |>
          (\(x) x[x < xlim[2]])() |>
          density(n = 512 * 12, from = xlim[1], to = xlim[2]) |>
          (\(x) x[c('x', 'y')])() |>
          as.data.frame() |>
          mutate(dist = 'prior_rescaled')
      )
    },
    normal = {
      xlim <- qnorm(c(0.001, 0.999), mean = hyperpars$mu, sd = hyperpars$sigma)
      tibble::tibble(
        dist = 'prior',
        x = seq(xlim[1], xlim[2], length.out = 1000),
        y = dnorm(x, mean = hyperpars$mu, sd = hyperpars$sigma)
      )
    },
    uniform = {
      xlim <- c(min = hyperpars$min, max = hyperpars$max)
      tibble::tibble(
        dist = 'prior',
        x = seq(xlim[1], xlim[2], length.out = 1000),
        y = dunif(x, min = hyperpars$min, max = hyperpars$max)
      )
    },

    .cli_abort("Unrecognized distribution function {.val {distrib}}.")
  )
  plot_prior_rescaled <- plot_prior_rescaled &&
    ('prior_rescaled' %in% densdf$dist)
  if (!plot_prior_rescaled) {
    densdf <- filter(densdf, dist != 'prior_rescaled')
  }

  # if available, augment the data.frame with data illustrating the posterior
  plot_posterior <- class(dist_data)[1] == 'metab_bayes' &&
    !is.null(get_mcmc(dist_data))
  if (plot_posterior) {
    mc <- get_mcmc(dist_data)
    if (inherits(mc, 'stanfit')) {
      if (!requireNamespace('rstan', quietly = TRUE)) {
        .cli_abort(
          "{.pkg rstan} is required to inspect Stan MCMC models."
        )
      }
      draws <- rstan::extract(mc, pars = parname)[[parname]]
      selected <- select_rstan_draws(draws, parname, index)
      draws <- selected$draws
      indexed_posterior <- selected$indexed
    } else if (inherits(mc, 'CmdStanMCMC')) {
      draws_arr <- mc$draws(variables = parname)
      selected <- select_cmdstan_draws(draws_arr, parname, index)
      draws <- selected$draws
      indexed_posterior <- selected$indexed
    } else {
      .cli_abort("Unknown MCMC object class: {.cls {class(mc)}}.")
    }
    # generate density w/ 1000 points along the line
    post <- density(draws, n = 1000)[c('x', 'y')] |>
      tibble::as_tibble() |>
      mutate(dist = 'posterior') |>
      select(dist, x, y)
    densdf <- bind_rows(densdf, post)
    if (!indexed_prior && !indexed_posterior && !missing(index)) {
      .cli_warn(
        "{.arg index} is ignored because neither the prior nor posterior is indexed."
      )
    }
  } else {
    indexed_posterior <- FALSE
    if (!indexed_prior && !missing(index)) {
      .cli_warn(
        "{.arg index} is ignored because the prior is unindexed and the posterior is unavailable."
      )
    }
  }

  # prepare the plot title
  ptitle <- paste0(
    parname,
    if (indexed_prior || indexed_posterior) {
      paste0(
        '[',
        if (is.logical(index) || length(index) == 1) {
          as.character(index)
        } else if (all(diff(index) == 1)) {
          paste0(min(index), ':', max(index))
        } else {
          paste0('c(', paste0(index, collapse = ','), ')')
        },
        ']'
      )
    }
  )

  # prepare shared plot style info
  colorvals <- c(prior = 'red', prior_rescaled = 'gold', posterior = 'blue')

  # make the plot
  plot_out <- switch(
    style,
    ggplot2 = {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        .cli_abort(
          c(
            "{.pkg ggplot2} is required for {.arg style} = {.val ggplot2}.",
            "i" = "Install it with {.run install.packages('ggplot2')}."
          )
        )
      }
      dist <- x <- y <- '.ggplot2Var'
      ggplot2::ggplot(
        densdf,
        ggplot2::aes(x = x, y = y, fill = dist, color = dist)
      ) +
        ggplot2::scale_color_manual(values = colorvals) +
        ggplot2::scale_fill_manual(values = colorvals) +
        ggplot2::geom_area(alpha = 0.4, na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(ptitle)
    },
    dygraphs = {
      if (!requireNamespace("dygraphs", quietly = TRUE)) {
        .cli_abort(
          c(
            "{.pkg dygraphs} is required for {.arg style} = {.val dygraphs}.",
            "i" = "Install it with {.run install.packages('dygraphs')}."
          )
        )
      }
      # prepare the data for dygraphs. if the distributions overlap on the x
      # axis, they'll look really funny unless we fill in the NA values, so also
      # approx those in
      dydensdf <- tidyr::pivot_wider(
        densdf,
        names_from = dist,
        values_from = y
      )
      if (plot_prior_rescaled || plot_posterior) {
        prior <- prior_rescaled <- posterior <- '.dplyr.var'
        dydensdf <- dydensdf |>
          arrange(x) |>
          mutate(prior = approx(x = x, y = prior, xout = x)$y)
        if (plot_prior_rescaled) {
          dydensdf <- dydensdf |>
            mutate(
              prior_rescaled = approx(x = x, y = prior_rescaled, xout = x)$y
            )
        }
        if (plot_posterior) {
          dydensdf <- dydensdf |>
            mutate(posterior = approx(x = x, y = posterior, xout = x)$y)
        }
      }
      d <- dygraphs::dygraph(dydensdf, main = ptitle) |>
        dygraphs::dyAxis('x', rangePad = 5) |>
        dygraphs::dyOptions(fillAlpha = 0.4) |>
        dygraphs::dyRangeSelector(height = 20) |>
        dygraphs::dySeries(
          'prior',
          color = colorvals[['prior']],
          fillGraph = TRUE
        )
      if (plot_prior_rescaled) {
        d <- d |>
          dygraphs::dySeries(
            'prior_rescaled',
            color = colorvals[['prior_rescaled']],
            fillGraph = TRUE
          )
      }
      if (plot_posterior) {
        d <- d |>
          dygraphs::dySeries(
            'posterior',
            color = colorvals[['posterior']],
            fillGraph = TRUE
          )
      }
      d
    }
  )

  plot_out
}

#' Select scalar or indexed RStan draws for a distribution plot
#'
#' @param extracted_draws The result for one parameter returned by
#'   [rstan::extract()].
#' @param parname The base parameter name.
#' @inheritParams plot_distribs
#' @return A list containing a numeric vector of draws and a logical indicating
#'   whether the parameter is indexed.
#' @keywords internal
select_rstan_draws <- function(extracted_draws, parname, index = TRUE) {
  draw_dims <- dim(extracted_draws)
  indexed <- length(draw_dims) > 1
  if (!indexed) {
    return(list(draws = as.vector(extracted_draws), indexed = FALSE))
  }

  draw_matrix <- matrix(extracted_draws, nrow = draw_dims[1])
  available <- seq_len(ncol(draw_matrix))
  selected <- if (isTRUE(index)) available else available[index]
  if (
    length(selected) == 0 ||
      any(is.na(selected)) ||
      !all(selected %in% available)
  ) {
    .cli_abort(
      "{.arg index} does not select a valid element of {.var {parname}}."
    )
  }

  list(
    draws = as.vector(draw_matrix[, selected, drop = FALSE]),
    indexed = TRUE
  )
}

#' Select scalar or indexed CmdStan draws for a distribution plot
#'
#' @param draws_array A CmdStanR `draws_array`.
#' @param parname The base parameter name.
#' @inheritParams plot_distribs
#' @return A list containing a numeric vector of draws and a logical indicating
#'   whether the parameter is indexed.
#' @keywords internal
select_cmdstan_draws <- function(draws_array, parname, index = TRUE) {
  variable_names <- dimnames(draws_array)[[3]]
  prefix <- paste0(parname, '[')
  indexed_names <- variable_names[
    substring(variable_names, 1, nchar(prefix)) == prefix
  ]
  indexed <- length(indexed_names) > 0

  if (indexed) {
    selected_names <- if (isTRUE(index)) indexed_names else indexed_names[index]
    if (
      length(selected_names) == 0 ||
        any(is.na(selected_names)) ||
        !all(selected_names %in% variable_names)
    ) {
      .cli_abort(
        "{.arg index} does not select a valid element of {.var {parname}}."
      )
    }
  } else {
    if (!(parname %in% variable_names)) {
      .cli_abort("Could not find {.var {parname}} in the CmdStan draws.")
    }
    selected_names <- parname
  }

  list(
    draws = as.vector(
      draws_array[,, match(selected_names, variable_names), drop = FALSE]
    ),
    indexed = indexed
  )
}
