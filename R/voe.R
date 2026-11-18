# -----------------------------------------------------------------------------
# Vibration of Effects (VoE)
# -----------------------------------------------------------------------------
#
# All the functions work on the table returned by spec_table(), so they accept
# pima objects, pima.multi objects, lists of lm/glm models and spec_table()
# tables. All summaries are computed separately for each association, i.e. for
# each response x coefficient pair. Pooling different
# coefficients or responses would mix different associations.

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

# Validate `q` and return its extremes
.check_q <- function(q) {
  q <- range(q)
  if (anyNA(q) || q[1L] == q[2L] || q[1L] < 0 || q[2L] > 1) {
    stop("`q` must contain probabilities in [0, 1] with distinct extremes.",
         call. = FALSE)
  }
  q
}

# Specification table used by the VoE functions: spec_table() of `x`, or `x`
# itself when it is already a table (e.g. a filtered spec_table())
.voe_table <- function(x) {
  tab <- if (is.data.frame(x)) x else spec_table(x)
  if (is.null(tab$response)) {
    tab$response <- "(response)"
  }
  tab
}

# Select the associations to summarise. Defaults as in spec_curve(): tested
# coefficients (pima), focal predictors (pima.multi), otherwise all the
# coefficients. The intercept is always dropped.
.voe_select <- function(tab, x, focal = NULL, focal.var = NULL,
                        which.response = NULL, xvar = "estimate") {
  if (!length(focal) && !length(focal.var)) {
    if (inherits(x, "pima")) focal <- unique(unlist(x$tested_coeffs))
    if (inherits(x, "pima.multi")) focal.var <- x$specification$focal
  }
  if (!is.null(which.response)) {
    tab <- tab[tab$response %in% which.response, , drop = FALSE]
  }
  tab <- .filter_focal(tab, focal = focal, focal.var = focal.var)
  tab <- tab[tab$coefficient != "(Intercept)", , drop = FALSE]

  if (!xvar %in% names(tab)) {
    stop(sprintf("`%s` is not a column of the specification table.", xvar),
         call. = FALSE)
  }
  if (!nrow(tab)) {
    stop("no coefficients left after filtering: check `focal`, `focal.var` ",
         "and `which.response`.", call. = FALSE)
  }
  rownames(tab) <- NULL
  tab
}

.voe_data <- function(x, focal = NULL, focal.var = NULL, which.response = NULL,
                      xvar = "estimate") {
  .voe_select(.voe_table(x), x, focal = focal, focal.var = focal.var,
              which.response = which.response, xvar = xvar)
}

# One group per association (response x coefficient)
.voe_groups <- function(d) {
  split(d, list(d$response, d$coefficient), drop = TRUE, sep = " ~ ")
}

# Index (in the original vector) of the specification closest to the q-th quantile
.qpos <- function(x, q) {
  which.min(abs(x - stats::quantile(x, q, na.rm = TRUE)))
}

# Warn when the amplitude is computed on non-comparable scales
.check_scale <- function(g, xvar, type) {
  lab <- sprintf("%s ~ %s", g$response[1L], g$coefficient[1L])
  links <- unique(g$link)
  
  if (xvar == "estimate" && length(links) > 1L) {
    warning(sprintf(
      "%s: specifications use different links (%s); quantiles of `estimate` mix different scales.",
      lab, paste(links, collapse = ", ")
    ), call. = FALSE)
  }
  if (type == "ratio" && !all(links %in% c("log", "logit", "cloglog"))) {
    warning(sprintf(
      "%s: `type = \"ratio\"` is exp(difference) on the link scale and is interpretable only for log, logit and cloglog links.",
      lab
    ), call. = FALSE)
  }
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Janus effect
# -----------------------------------------------------------------------------

#' Janus effect
#'
#' Computes the definitions of the Janus effect used in the vibration of
#' effects literature. The Janus effect indicates that the same association
#' changes direction across specifications.
#'
#' Definitions (M = number of specifications, null = value under H0):
#' \itemize{
#' \item `J_range`: minimum and maximum estimates on opposite sides of the null
#'   (Vinatier et al. 2025).
#' \item `J_pct`: lowest and highest quantiles in `q` on opposite sides of the
#'   null (Patel et al. 2015; Palpacuer et al. 2019; Vinatier et al. 2024).
#'   Quantiles are interpolated (`stats::quantile()` default).
#' \item `J_ext`: the specifications closest to those quantiles are on opposite
#'   sides of the null and both significant (Vinatier et al. 2024).
#' \item `J_sig`: at least one significant estimate in each direction
#'   (Klau et al. 2021, 2023).
#' \item `J_frac`: proportion of estimates greater than the null
#'   (Tierney et al. 2021).
#' }
#'
#' @param x a numeric vector of estimates, a `pima` object, a `pima.multi`
#'   object created with `fit.specs`, a list of fitted `lm` or `glm` models,
#'   or a table returned by [spec_table()].
#' @param p for vectors, a numeric vector of p-values, same length as `x`
#'   (if `NULL`, `J_sig` and `J_ext` are `NA`). Otherwise, the column of
#'   p-values of the specification table, as in [spec_curve()].
#' @param q probabilities; only the extremes are used.
#' @param null value under the null hypothesis (0 on the link scale).
#' @param alpha significance level.
#' @param focal,focal.var coefficients and variables to consider, as in
#'   [spec_curve()]; by default the tested coefficients (`pima`), the focal
#'   predictors (`pima.multi`) or all the coefficients except the intercept.
#' @param p.adjusted which p-values to use, with the same rules as
#'   [spec_curve()]: adjusted p-values only exist for `pima` objects analysed
#'   with a correction. Published values in the literature use raw p-values.
#' @param which.response responses to consider; defaults to all.
#' @param xvar column of the specification table whose sign is used. Any
#'   column with the same sign as the coefficient (`estimate`, `pcor`,
#'   `wald.t`) gives the same result.
#' @param ... further arguments (currently unused).
#' @return For vectors, a list with `J_range`, `J_pct`, `J_ext`, `J_sig`,
#'   `J_frac`. Otherwise, a data frame with one row per response x
#'   coefficient.
#' @references
#' \insertRef{patel2015assessment}{pima}
#'
#' \insertRef{klau2021examining}{pima}
#'
#' \insertRef{tierney2021leveraging}{pima}
#'
#' \insertRef{vinatier2024vibration}{pima}
#'
#' \insertRef{vinatier2025what}{pima}
#' @export
janus <- function(x, ...) UseMethod("janus")

#' @rdname janus
#' @export
janus.default <- function(x, p = NULL, q = c(0.01, 0.99), null = 0,
                          alpha = 0.05, ...) {
  q <- .check_q(q)
  if (!is.null(p) && length(p) != length(x)) {
    stop("`x` and `p` must have the same length.", call. = FALSE)
  }
  if (all(is.na(x))) {
    return(list(J_range = NA, J_pct = NA, J_ext = NA, J_sig = NA,
                J_frac = NA_real_))
  }
  
  # J_range: opposite estimates across the whole multiverse
  j_range <- any(x < null, na.rm = TRUE) && any(x > null, na.rm = TRUE)
  
  # J_pct: (interpolated) quantiles on opposite sides of the null
  xq <- stats::quantile(x, q, na.rm = TRUE, names = FALSE)
  j_pct <- xq[1L] < null && xq[2L] > null
  
  # J_frac: proportion of estimates greater than the null
  j_frac <- mean(x > null, na.rm = TRUE)
  
  if (is.null(p)) {
    j_sig <- NA
    j_ext <- NA
  } else {
    # J_sig: significant estimates in both directions
    s <- !is.na(p) & p <= alpha
    j_sig <- any(x[s] < null, na.rm = TRUE) && any(x[s] > null, na.rm = TRUE)
    
    # J_ext: specifications at opposite quantiles, both significant
    i_lo <- .qpos(x, q[1L])
    i_hi <- .qpos(x, q[2L])
    j_ext <- x[i_lo] < null && x[i_hi] > null &&
      p[i_lo] <= alpha && p[i_hi] <= alpha
  }
  
  list(J_range = j_range, J_pct = j_pct, J_ext = j_ext, J_sig = j_sig,
       J_frac = j_frac)
}

#' @rdname janus
#' @export
janus.pima <- function(x, focal = NULL, p.adjusted = NULL, q = c(0.01, 0.99),
                       alpha = 0.05, which.response = NULL, xvar = "estimate",
                       focal.var = NULL, p = NULL, ...) {
  d <- .voe_data(x, focal = focal, focal.var = focal.var,
                 which.response = which.response, xvar = xvar)
  pv <- .pick_p(d, p = p, p.adjusted = p.adjusted)
  
  out <- lapply(.voe_groups(d), function(g) {
    data.frame(
      response = g$response[1L],
      coefficient = g$coefficient[1L],
      n_spec = sum(!is.na(g[[xvar]])),
      janus.default(g[[xvar]], g[[pv]], q = q, null = 0, alpha = alpha),
      stringsAsFactors = FALSE
    )
  })
  .bind_rows(out)
}

#' @rdname janus
#' @export
janus.pima.multi <- janus.pima

#' @rdname janus
#' @export
janus.list <- janus.pima

#' @rdname janus
#' @export
janus.data.frame <- janus.pima

# -----------------------------------------------------------------------------
# Vibration of effects summaries
# -----------------------------------------------------------------------------

#' Vibration of effects
#'
#' Summarises how estimates and p-values vary across the specifications of a
#' multiverse, separately for each response x coefficient.
#'
#' For each association it returns the quantiles of `xvar` and of
#' -log10(p), the amplitude `x_voe` (difference between the extreme quantiles
#' of `xvar`, or its exponential when `type = "ratio"`), `p_voe` (difference
#' between the extreme quantiles of -log10(p), the RP of Patel et al. 2015) and
#' the Janus effect definitions (see [janus()]).
#'
#' `type = "ratio"` requires `xvar = "estimate"`: the ratio of the extreme
#' quantiles of exp(estimate) equals exp() of their difference on the link
#' scale (RHR/ROR of Patel et al. 2015). It is interpretable only for log,
#' logit and cloglog links. Quantiles are always returned on the scale of
#' `xvar`.
#'
#' @param x a `pima` object, a `pima.multi` object created with `fit.specs`, a
#'   list of fitted `lm` or `glm` models, or a table returned by
#'   [spec_table()].
#' @param xvar column of the specification table to summarise (e.g.
#'   `"pcor"`, `"estimate"`).
#' @param p.adjusted which p-values to use, with the same rules as
#'   [spec_curve()]: for `pima` objects `TRUE` uses the adjusted p-values,
#'   `FALSE` the unadjusted flipscores ones and `NULL` (default) the adjusted
#'   ones when [pima()] applied a correction; for lists of models and
#'   `pima.multi` objects only the unadjusted Wald p-values exist (`"wald.p"`)
#'   and `TRUE` is an error.
#' @param q probabilities for the quantiles; the extremes define the
#'   amplitudes and the Janus effect.
#' @param type `"additive"` (difference) or `"ratio"` (exp of the difference,
#'   only with `xvar = "estimate"`).
#' @param null value under the null hypothesis on the scale of `xvar`
#'   (0 for `estimate`, `pcor` and `z`).
#' @param alpha significance level.
#' @param decompose_variance if `TRUE`, adds [voe_variance()].
#' @param focal,focal.var coefficients and variables to consider, as in
#'   [spec_curve()]; by default the tested coefficients (`pima`), the focal
#'   predictors (`pima.multi`) or all the coefficients except the intercept.
#'   Each selected coefficient is a separate association.
#' @param which.response responses to consider; defaults to all.
#' @param p column of p-values of the specification table (e.g. one added
#'   with [stats::p.adjust()]). If `NULL`, chosen with the rules of
#'   `p.adjusted`.
#' @return An object of class `pima_voe`: a list with `summary` (one row per
#'   response x coefficient), `quantiles` (long format), the settings used and,
#'   optionally, `variance`.
#' @export
voe <- function(
    x,
    xvar = "pcor",
    p.adjusted = NULL,
    q = c(0.01, 0.50, 0.99),
    type = c("additive", "ratio"),
    null = 0,
    alpha = 0.05,
    decompose_variance = FALSE,
    focal = NULL,
    which.response = NULL,
    focal.var = NULL,
    p = NULL
) {
  type <- match.arg(type)
  if (type == "ratio" && xvar != "estimate") {
    stop("`type = \"ratio\"` requires `xvar = \"estimate\"` (link scale).",
         call. = FALSE)
  }
  
  q <- sort(unique(q))
  .check_q(q)
  d <- .voe_data(x, focal = focal, focal.var = focal.var,
                 which.response = which.response, xvar = xvar)
  pv <- .pick_p(d, p = p, p.adjusted = p.adjusted)

  # check p distribution
  if(.similar_p(d[[pv]])){
    warning(
      "The p values are extremely similar! Plots and VOE descriptives could be hard to read!"
    )
  }
  
  res <- lapply(.voe_groups(d), function(g) {
    .check_scale(g, xvar, type)
    
    xq <- stats::quantile(g[[xvar]], q, na.rm = TRUE, names = FALSE)
    pq <- stats::quantile(transf_p(g[[pv]], "-log10"), q, na.rm = TRUE,
                          names = FALSE)
    delta <- xq[length(xq)] - xq[1L]
    
    list(
      summary = data.frame(
        response = g$response[1L],
        coefficient = g$coefficient[1L],
        n_spec = sum(!is.na(g[[xvar]])),
        x_voe = if (type == "ratio") exp(delta) else delta,
        p_voe = pq[length(pq)] - pq[1L],
        janus.default(g[[xvar]], g[[pv]], q = q, null = null, alpha = alpha),
        stringsAsFactors = FALSE
      ),
      quantiles = data.frame(
        response = g$response[1L],
        coefficient = g$coefficient[1L],
        q = q,
        x = xq,
        p = pq,
        stringsAsFactors = FALSE
      )
    )
  })
  
  out <- list(
    summary = .bind_rows(lapply(res, `[[`, "summary")),
    quantiles = .bind_rows(lapply(res, `[[`, "quantiles")),
    xvar = xvar,
    type = type,
    null = null,
    p.values = pv,
    p.transf = "-log10",
    alpha = alpha,
    q = q
  )
  
  if (decompose_variance) {
    out$variance <- voe_variance(
      x,
      estimate = xvar,
      focal = focal,
      focal.var = focal.var,
      which.response = which.response
    )
  }
  
  class(out) <- c("pima_voe", "list")
  out
}

#' @export
print.pima_voe <- function(x, digits = 3, ...) {
  cat(sprintf(
    "Vibration of effects (xvar = %s, type = %s, p-values = %s, alpha = %s)\n\n",
    x$xvar, x$type, x$p.values, x$alpha
  ))
  print(x$summary, digits = digits, row.names = FALSE)
  if (!is.null(x$variance)) {
    cat("\nVariance decomposition:\n")
    print(x$variance$dec, digits = digits, row.names = FALSE)
  }
  invisible(x)
}

#' Variance decomposition of the vibration of effects
#'
#' Regresses `estimate` on the specification decisions and reports, for each
#' decision, the unique R-squared (R2 of the full model minus R2 without that
#' decision), separately for each response x coefficient. With a complete,
#' balanced grid of specifications and no interactions, the unique R-squared
#' equals the ANOVA share of variance (Klau et al. 2023).
#'
#' Decisions are the predictors (`attr(spec_table(x), "spec_predictors")`,
#' one factor per predictor whose levels are the terms used to include it and
#' `"<predictor> (removed)"` when it is excluded) and the other specification
#' choices (the remaining `attr(spec_table(x), "spec_vars")`, e.g. `subset`,
#' `fit_spec`, `family`, `link`). Decisions that do not vary within an association are dropped, and
#' so are decisions that define the same partition of the specifications as an
#' earlier one (e.g. `link` when it is fully determined by `fit_spec`). A
#' decision nested in another one (e.g. `link` with fewer levels than
#' `fit_spec`) is kept, but its unique R-squared is 0 by construction.
#'
#' For predictor decisions the unique R-squared is also split into two nested
#' parts that sum to it: `r2u_inclusion` (removed vs included, whatever the
#' form) and `r2u_transformation` (variation among the forms of an included
#' predictor). Both are `NA` for the other decisions.
#'
#' @inheritParams voe
#' @param estimate column of the specification table to decompose.
#' @param p.adjusted not used: p-values play no role in the decomposition.
#'   Kept for backward compatibility; when not `NULL` it is only checked with
#'   the rules of [voe()].
#' @return A list with `r2` (one row per response x coefficient) and `dec`
#'   (one row per response x coefficient x decision, with `source` equal to
#'   `"predictor"` or `"other"`, and `r2u_inclusion`, `r2u_transformation` for
#'   predictors).
#' @export
voe_variance <- function(
    x,
    estimate = "pcor",
    p.adjusted = NULL,
    focal = NULL,
    which.response = NULL,
    focal.var = NULL
) {
  d <- .voe_data(x, focal = focal, focal.var = focal.var,
                 which.response = which.response, xvar = estimate)
  if (!is.null(p.adjusted)) {
    .pick_p(d, p.adjusted = p.adjusted)  # validates `p.adjusted`
  }
  if (is.null(attr(d, "spec_vars"))) {
    stop("cannot find the specification columns (attribute `spec_vars`): ",
         "use a table returned by spec_table() and filter it with `[`.",
         call. = FALSE)
  }
  
  # decisions: predictors (xs) and the other specification choices (extra)
  xs <- intersect(attr(d, "spec_predictors"), names(d))
  extra <- intersect(setdiff(attr(d, "spec_vars"), xs), names(d))
  decisions <- c(xs, extra)
  
  res <- lapply(.voe_groups(d), function(g) {
    id <- list(response = g$response[1L], coefficient = g$coefficient[1L])
    dd <- g[, c(estimate, decisions), drop = FALSE]
    
    # make exclusion an explicit level, with the same label used by
    # spec_curve()
    for (v in xs) {
      dd[[v]] <- factor(ifelse(.is_excluded(dd[[v]]), sprintf("%s (removed)", v),
                               as.character(dd[[v]])))
    }
    for (v in extra) {
      dd[[v]] <- factor(ifelse(is.na(dd[[v]]), "(NA)", as.character(dd[[v]])))
    }
    
    # keep decisions that vary within the association
    terms <- decisions[vapply(decisions, function(v) {
      nlevels(droplevels(dd[[v]])) > 1L
    }, logical(1))]
    
    # drop decisions that define the same partition of the specifications as
    # an earlier one (e.g. `link` or `family` fully determined by `fit_spec`):
    # otherwise their unique R2 would be 0
    partition <- vapply(terms, function(v) {
      z <- as.character(dd[[v]])
      paste(match(z, z), collapse = ",")
    }, character(1))
    terms <- terms[!duplicated(partition)]
    
    if (!length(terms)) {
      return(list(
        r2 = data.frame(id, r2 = NA_real_, r2.adj = NA_real_,
                        stringsAsFactors = FALSE),
        dec = NULL
      ))
    }
    
    # inclusion indicator for each predictor decision (included vs removed)
    for (v in intersect(terms, xs)) {
      dd[[paste0(".inc_", v)]] <- dd[[v]] != sprintf("%s (removed)", v)
    }
    
    fit <- function(tt) {
      stats::lm(
        stats::reformulate(sprintf("`%s`", tt), response = estimate),
        data = dd
      )
    }
    r2 <- function(tt) if (length(tt)) summary(fit(tt))$r.squared else 0
    
    full_fit <- fit(terms)
    r2_full <- summary(full_fit)$r.squared
    
    unique_r2 <- vapply(terms, function(term) {
      r2_full - r2(setdiff(terms, term))
    }, numeric(1))
    
    # predictors: split the unique R2 into inclusion and transformation.
    # The two choices are nested (a transformation exists only when the
    # predictor is included), so the split is hierarchical:
    #   inclusion      = R2(others + included yes/no) - R2(others)
    #   transformation = R2(others + full decision)   - R2(others + included yes/no)
    # and the two parts sum to the unique R2 of the decision.
    split <- vapply(terms, function(term) {
      if (!term %in% xs) {
        return(c(NA_real_, NA_real_))
      }
      inc <- paste0(".inc_", term)
      if (length(unique(dd[[inc]])) < 2L) {
        # never removed: all the variation is due to the transformation
        return(c(0, unique_r2[[term]]))
      }
      others <- setdiff(terms, term)
      r2_inc <- r2(c(others, inc))
      c(r2_inc - r2(others), r2_full - r2_inc)
    }, numeric(2))
    
    # differences of nested R2 are >= 0: remove rounding errors (e.g. -1e-16)
    unique_r2 <- pmax(unique_r2, 0)
    split <- pmax(split, 0)
    
    list(
      r2 = data.frame(id, r2 = r2_full,
                      r2.adj = summary(full_fit)$adj.r.squared,
                      stringsAsFactors = FALSE),
      dec = data.frame(id, term = terms,
                       source = ifelse(terms %in% xs, "predictor", "other"),
                       r2u = unname(unique_r2),
                       pr2u = unname(unique_r2 / r2_full),
                       r2u_inclusion = unname(split[1L, ]),
                       r2u_transformation = unname(split[2L, ]),
                       stringsAsFactors = FALSE)
    )
  })
  
  list(
    r2 = .bind_rows(lapply(res, `[[`, "r2")),
    dec = .bind_rows(lapply(res, `[[`, "dec"))
  )
}

# -----------------------------------------------------------------------------
# Volcano plot
# -----------------------------------------------------------------------------

#' Volcano plot of the vibration of effects
#'
#' Plots `xvar` against -log10(p), adding the quantile lines computed by
#' [voe()] and p-value cutoffs. With more than one association (response x
#' coefficient) each one is drawn in its own panel, with its own lines.
#'
#' @inheritParams voe
#' @param p.cut p-value cutoffs drawn as horizontal lines.
#' @param median.traj if `TRUE`, adds the trajectory of the medians by the
#'   number of excluded predictors.
#' @param ... further arguments passed to [spec_scatter()] (e.g. `xlab`,
#'   `ylab`, `shapes`, `by`).
#' @return A ggplot object.
#' @export
volcano <- function(x,
                    focal = NULL,
                    xvar = "pcor",
                    p.adjusted = NULL,
                    p.cut = c(0.05, 0.001),
                    q = c(0.01, 0.50, 0.99),
                    type = c("additive", "ratio"),
                    null = 0,
                    alpha = 0.05,
                    which.response = NULL,
                    median.traj = FALSE,
                    focal.var = NULL,
                    p = NULL,
                    ...) {
  type <- match.arg(type)
  
  # specification table and selection built once, then reused
  tab <- .voe_table(x)
  d <- .voe_select(tab, x, focal = focal, focal.var = focal.var,
                   which.response = which.response, xvar = xvar)
  
  VOE <- voe(
    d,
    xvar = xvar,
    p.adjusted = p.adjusted,
    q = q,
    type = type,
    null = null,
    alpha = alpha,
    p = p
  )
  
  qd <- VOE$quantiles
  qd$label <- sprintf("%s%%", round(qd$q * 100))
  ycut <- transf_p(p.cut, "-log10")
  n_assoc <- nrow(VOE$summary)

  # label positions, per association (one panel each)
  pos <- .bind_rows(lapply(.voe_groups(d), function(g) {
    yg <- transf_p(g[[VOE$p.values]], "-log10")
    data.frame(response = g$response[1L], coefficient = g$coefficient[1L],
               x_cut = .rpos(g[[xvar]], 0.1), x_q = .rpos(g[[xvar]], 1.05),
               y_q = .rpos(yg[is.finite(yg)], -0.1), stringsAsFactors = FALSE)
  }))
  qd <- merge(qd, pos, by = c("response", "coefficient"))
  cuts <- merge(pos, data.frame(y = ycut, label = sprintf("p = %s", p.cut)), by = NULL)

  plt <- spec_scatter(d, xvar = xvar, p = VOE$p.values, p.transf = "-log10",
                      alpha = alpha, ...) +
    ggplot2::theme(plot.title = ggplot2::element_blank())

  if (n_assoc > 1L) {
    # one panel per association; one colour (unless `by` is given)
    plt <- plt +
      ggplot2::facet_wrap(
        c("response", "coefficient"), scales = "free_x",
        labeller = function(l) list(sprintf("%s ~ %s", l$response, l$coefficient))
      )
    if (!"by" %in% names(list(...))) {
      plt <- plt + ggplot2::scale_color_manual(values = rep(.single_color, n_assoc),
                                               guide = "none")
    }
  }

  if (median.traj) {
    # k is computed on the whole table (also the models without `focal`)
    perk <- .voe_per_k(tab, xvar = xvar, p.adjusted = p.adjusted,
                       focal = VOE$summary$coefficient,
                       which.response = VOE$summary$response, p = p)
    perk <- perk[order(perk$response, perk$coefficient, perk$k), , drop = FALSE]
    traj <- ggplot2::aes(x = .data[[".x"]], y = .data[[".logp"]],
                         group = interaction(.data[["response"]], .data[["coefficient"]]))
    plt <- plt +
      ggplot2::geom_path(data = perk, traj, colour = "red", inherit.aes = FALSE) +
      ggplot2::geom_point(data = perk, traj, colour = "red", shape = 1, size = 2.5,
                          inherit.aes = FALSE) +
      ggplot2::geom_text(data = perk,
                         ggplot2::aes(x = .data[[".x"]], y = .data[[".logp"]],
                                      label = .data[["k"]]),
                         vjust = -1, size = 3, inherit.aes = FALSE)
  }

  lab <- function(x, y) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]], label = .data[["label"]])
  }
  plt +
    ggplot2::geom_vline(data = qd, ggplot2::aes(xintercept = .data[["x"]]),
                        lty = "dotted") +
    ggplot2::geom_hline(data = qd, ggplot2::aes(yintercept = .data[["p"]]),
                        lty = "dotted") +
    ggplot2::geom_hline(yintercept = ycut, col = "darkgreen") +
    ggplot2::geom_label(data = cuts, lab("x_cut", "y"), inherit.aes = FALSE) +
    ggplot2::geom_label(data = qd, lab("x", "y_q"), inherit.aes = FALSE) +
    ggplot2::geom_label(data = qd, lab("x_q", "p"), inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = VOE$null)
}

.voe_per_k <- function(x, xvar = "estimate", p.adjusted = FALSE,
                       focal = NULL, which.response = NULL, focal.var = NULL,
                       p = NULL) {
  tab <- .voe_table(x)
  d <- .voe_select(tab, x, focal = focal, focal.var = focal.var,
                   which.response = which.response, xvar = xvar)
  pv <- .pick_p(d, p = p, p.adjusted = p.adjusted)
  
  # k = number of optional predictors included in each specification,
  # computed on all the specifications (also those without the focal
  # coefficient)
  xs <- intersect(attr(tab, "spec_predictors"), names(tab))
  if (!length(xs)) {
    stop("cannot find the predictors of the specifications (attribute ",
         "`spec_predictors`): use a table returned by spec_table().",
         call. = FALSE)
  }
  info <- unique(tab[, c("model", xs), drop = FALSE])
  optional <- xs[vapply(xs, function(v) any(.is_excluded(info[[v]])), logical(1))]
  info$k <- Reduce(`+`, lapply(info[optional], function(v) !.is_excluded(v)), 0)
  
  d <- merge(d, info[, c("model", "k")], by = "model")
  d$.x <- d[[xvar]]
  d$.logp <- transf_p(d[[pv]], "-log10")
  
  aggregate(cbind(.x, .logp) ~ response + coefficient + k, data = d, FUN = median)
}