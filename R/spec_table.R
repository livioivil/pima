# Specification table ---------------------------------------------------
#
# spec_table() stacks the summary of each model (coef(summary(model)) for
# lists and pima.multi objects, the pima() summary for pima objects) and adds
# the specification choices that define it.

#' Specification table
#'
#' Stacks the coefficient table of each model (as returned by
#' `summary(model)$coefficients`) and adds the analytical choices defining
#' each specification.
#'
#' @param x A list of fitted `lm` or `glm` models, a `pima.multi` object
#'   created by [create_multi()] with `fit.specs` (fitting `lm` or `glm`
#'   models), or a `pima` object. Other model classes are not supported.
#'   For a `pima` object the summary is the one computed by [pima()]
#'   (`se` is renamed `score.se` because it is the standard error of the
#'   score, not of the estimate).
#' @param focal Optional character vector of coefficient names to keep (exact
#'   match, e.g. `"log(x)"` or `"groupB"`).
#' @param focal.var Optional character vector of variable names: keeps all the
#'   coefficients coming from these variables (e.g. `"x"` keeps `x`, `log(x)`
#'   and `sqrt(x)`; `"group"` keeps `groupB` and `groupC`). With both `focal`
#'   and `focal.var`, the union is kept. With both `NULL` (default), all the
#'   coefficients are returned, intercept included.
#'
#' @details
#' For lists of models and `pima.multi` objects, `estimate`, `se`, `wald.t`
#' and `wald.p` are the columns of `summary(model)$coefficients`, unchanged.
#' The only derived column is `pcor`, the partial correlation computed from
#' the Wald statistic as `wald.t / sqrt(wald.t^2 + df)`, with `df` given by
#' `df.residual(model)`. It is scale-free, so it can be compared across
#' specifications using different transformations or link functions. For `lm`
#' models it is exactly the partial correlation; for `glm` models it is the
#' usual analogue based on the Wald z statistic. It inherits the model-based
#' standard error, so when the variance is misspecified (e.g., overdispersion
#' in a Poisson model) it can differ substantially from the `pcor` of a `pima`
#' object, which is based on the (standardized) score statistic. It is `NA`
#' when `df.residual()` is not available.
#'
#' The columns `term` and `variable` link each coefficient to the term of the
#' formula and to the variable it comes from. They are derived from the
#' structure of the fitted model, not from the coefficient name: the term is
#' given by `attr(model.matrix(model), "assign")` and the variable by
#' [all.vars()] applied to the term (e.g. `log(x)` -> `x`). `variable` is `NA`
#' for the intercept and for terms involving more than one variable
#' (interactions), which can be selected only through `focal`.
#'
#' @return A data.frame with one row per model x coefficient and columns
#'   `model`, `response`, `coefficient`, `term`, `variable`, `estimate`, `se`,
#'   `wald.t`, `wald.p`, `pcor`, followed by the specification columns and the
#'   model `formula` (for `pima` objects, the columns of the [pima()] summary
#'   replace the model summary). Two
#'   attributes describe the specification columns:
#'   \itemize{
#'   \item `spec_vars`: all the specification columns.
#'   \item `spec_predictors`: the subset of `spec_vars` that are predictor
#'     choices (inclusion and transformation, with `"excluded"` when the
#'     predictor is not in the model). The remaining columns are other
#'     choices (e.g., `subset`, `fit_spec`, `family`).
#'   }
#' @export
spec_table <- function(x, focal = NULL, focal.var = NULL) {
  tab <- if (inherits(x, "pima")) .spec_table_pima(x) else .spec_table_models(x)
  .filter_focal(tab, focal = focal, focal.var = focal.var)
}

# lists of models and pima.multi objects: glm-like summary of each model
.spec_table_models <- function(x) {
  s <- .spec_input(x)
  mods <- s$models

  coefs <- lapply(names(mods), function(nm) {
    d <- .get_glm_summary(mods[[nm]])
    # partial correlation from the Wald statistic: exact for lm, the usual
    # analogue (with z) for glm. NA when df.residual() is not available
    df <- tryCatch(stats::df.residual(mods[[nm]]), error = function(e) NULL)
    d$pcor <- if (length(df) == 1L && is.finite(df) && df > 0) {
      d$wald.t / sqrt(d$wald.t^2 + df)
    } else {
      NA_real_
    }
    map <- .coef_map(mods[[nm]])
    d$term <- map$term[match(d$coefficient, map$coefficient)]
    d$variable <- map$variable[match(d$coefficient, map$coefficient)]
    d$model <- nm
    d
  })
  coefs <- do.call(rbind, coefs)
  coefs$response <- unname(.get_response_name(mods)[coefs$model])

  # keep the original order (models, then coefficients within model)
  coefs$.row <- seq_len(nrow(coefs))
  tab <- merge(coefs, s$info, by = "model", sort = FALSE)
  tab <- tab[order(tab$.row), , drop = FALSE]

  cols <- c(
    "model", "response", "coefficient", "term", "variable",
    "estimate", "se", "wald.t", "wald.p", "pcor",
    s$spec_vars, "formula"
  )
  tab <- tab[, cols, drop = FALSE]
  rownames(tab) <- NULL
  attr(tab, "spec_vars") <- s$spec_vars
  attr(tab, "spec_predictors") <- s$spec_predictors
  tab
}

# Coefficient -> term -> variable, from the structure of the model:
# - the term of each coefficient is given by attr(model.matrix(), "assign")
# - the variable of each term is given by all.vars() (the R parser), e.g.
#   log(x) -> x. Terms with more than one variable (interactions) have
#   variable NA, as the intercept.
.coef_map <- function(model) {
  cf <- names(stats::coef(model))
  assign <- tryCatch(attr(stats::model.matrix(model), "assign"), error = function(e) NULL)
  labels <- attr(stats::terms(model), "term.labels")

  if (is.null(assign) || length(assign) != length(cf)) {
    return(data.frame(coefficient = cf, term = NA_character_, variable = NA_character_))
  }

  term <- ifelse(assign == 0L, "(Intercept)", labels[pmax(assign, 1L)])
  variable <- vapply(seq_along(term), function(i) {
    if (assign[i] == 0L) return(NA_character_)
    v <- all.vars(str2lang(term[i]))
    if (length(v) == 1L) v else NA_character_
  }, character(1))

  data.frame(coefficient = cf, term = term, variable = variable)
}

# Rows selected by `focal` (coefficient names, exact match) or by `focal.var`
# (variable names: all the coefficients coming from that variable). With both
# NULL the table is returned as is.
.filter_focal <- function(tab, focal = NULL, focal.var = NULL) {
  if (!length(focal) && !length(focal.var)) {
    return(tab)
  }

  miss <- setdiff(focal, tab$coefficient)
  if (length(miss)) {
    warning(sprintf("`focal` coefficient(s) not found: %s.", paste(miss, collapse = ", ")),
            call. = FALSE)
  }
  miss <- setdiff(focal.var, tab$variable)
  if (length(miss)) {
    warning(sprintf("`focal.var` variable(s) not found: %s.", paste(miss, collapse = ", ")),
            call. = FALSE)
  }

  keep <- tab$coefficient %in% focal | (!is.na(tab$variable) & tab$variable %in% focal.var)
  if (!any(keep)) {
    stop("no coefficients selected by `focal` / `focal.var`.", call. = FALSE)
  }
  out <- tab[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# pima objects: the summary is the one computed by pima() (flipscores), merged
# with the specification choices stored in x$info
.spec_table_pima <- function(x) {
  st <- x$summary_table
  st <- st[, setdiff(names(st), ".assign"), drop = FALSE]
  # in flipscores `se` is the standard error of the score, not of the estimate
  names(st)[names(st) == "se"] <- "score.se"
  st$.row <- seq_len(nrow(st))

  # coefficient -> term -> variable from the fitted models
  maps <- lapply(names(x$objects), function(nm) {
    d <- .coef_map(x$objects[[nm]])
    d$model <- nm
    d
  })
  st <- merge(st, do.call(rbind, maps), by = c("model", "coefficient"),
              all.x = TRUE, sort = FALSE)
  st <- st[order(st$.row), , drop = FALSE]

  info <- x$info
  xs <- setdiff(attr(info, "xs"), "Intercept")
  spec_vars <- c(xs, attr(info, "extra"))

  # older pima objects code an excluded predictor as NA
  for (v in intersect(xs, names(info))) {
    info[[v]][.is_excluded(info[[v]])] <- "excluded"
  }

  tab <- merge(
    st,
    info[, c("model", setdiff(names(info), names(st))), drop = FALSE],
    by = "model",
    sort = FALSE
  )
  tab <- tab[order(match(tab$model, info$model), tab$.row), , drop = FALSE]

  first <- c("model", "response", "coefficient", "term", "variable")
  values <- setdiff(names(st), c(first, spec_vars, ".row"))
  cols <- c(first, values, spec_vars, "formula")
  tab <- tab[, intersect(cols, names(tab)), drop = FALSE]
  rownames(tab) <- NULL
  attr(tab, "spec_vars") <- spec_vars
  attr(tab, "spec_predictors") <- intersect(xs, spec_vars)
  tab
}

# returns the (named) list of models, one row of info per model with the
# specification choices, and the names of the specification columns
.spec_input <- function(x) {
  if (inherits(x, "pima.multi")) {
    if (!is.null(x$models)) .check_lm_models(x$models)
    return(.spec_input_multi(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    .check_lm_models(x)
    return(.spec_input_list(x))
  }
  stop("`x` must be a list of fitted models or a `pima.multi` object.", call. = FALSE)
}

.spec_input_multi <- function(x) {
  if (is.null(x$models)) {
    stop(
      "the `pima.multi` object has no fitted models. Use `fit.specs` in create_multi().",
      call. = FALSE
    )
  }

  sc <- x$scenarios
  if ("fit_ok" %in% names(sc) && any(!sc$fit_ok)) {
    warning(sprintf("dropping %d scenario(s) whose fit failed.", sum(!sc$fit_ok)), call. = FALSE)
    sc <- sc[sc$fit_ok, , drop = FALSE]
  }

  # the choices are already stored in the scenarios
  spec_cols <- c("y.fun", grep("^spec_", names(sc), value = TRUE), "subset", "fit_spec", ".source")
  spec_cols <- intersect(spec_cols, names(sc))

  mods <- x$models[as.character(sc$model)]

  info <- sc[, c("model", spec_cols), drop = FALSE]
  names(info) <- sub("^spec_", "", names(info))
  info <- cbind(info, .spec_family(mods))
  info$formula <- as.character(sc$calls)
  rownames(info) <- NULL

  list(
    models = mods,
    info = info,
    spec_vars = setdiff(names(info), c("model", "formula")),
    spec_predictors = sub("^spec_", "", grep("^spec_", spec_cols, value = TRUE))
  )
}

.spec_input_list <- function(x) {
  if (!length(x)) {
    stop("the list of models is empty.", call. = FALSE)
  }

  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  nms[!nzchar(nms)] <- paste0("model", which(!nzchar(nms)))
  names(x) <- make.unique(nms)

  # one column per predictor with the term used in each model (or "excluded")
  terms_l <- lapply(x, function(m) attr(stats::terms(m), "term.labels"))
  all_terms <- unique(unlist(terms_l))
  vars <- if (length(all_terms)) unique(unname(.clean_x_name(all_terms))) else character(0)

  info <- data.frame(model = names(x))
  for (v in vars) {
    info[[v]] <- vapply(terms_l, function(tl) {
      hit <- tl[.clean_x_name(tl) == v]
      if (length(hit)) paste(hit, collapse = " + ") else "excluded"
    }, character(1))
  }

  info <- cbind(info, .spec_family(x))
  info$formula <- vapply(x, function(m) paste(deparse(stats::formula(m), width.cutoff = 500L), collapse = ""), character(1))
  rownames(info) <- NULL

  list(
    models = x,
    info = info,
    spec_vars = c(vars, "family", "link"),
    spec_predictors = vars
  )
}

# family and link of each model (NA when stats::family() is not available)
.spec_family <- function(mods) {
  fam <- lapply(mods, function(m) tryCatch(stats::family(m), error = function(e) NULL))
  data.frame(
    family = vapply(fam, function(f) if (is.null(f)) NA_character_ else f$family, character(1)),
    link = vapply(fam, function(f) if (is.null(f)) NA_character_ else f$link, character(1)),
    row.names = NULL
  )
}

# long format (one row per model x coefficient x specification variable), as
# needed by the bottom panel of the specification curve
.spec_long <- function(tab, spec_vars = NULL) {
  if (is.null(spec_vars)) {
    spec_vars <- attr(tab, "spec_vars")
  }
  id <- setdiff(names(tab), spec_vars)
  out <- lapply(spec_vars, function(v) {
    d <- tab[, id, drop = FALSE]
    d$var <- v
    d$var.spec <- as.character(tab[[v]])
    d
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# p-values ---------------------------------------------------------------
#
# Fixed rules to choose the column of p-values. Only the three columns
# produced by the package are considered by default:
#   - "p.adj":  adjusted p-values of a pima object (absent with method = "none")
#   - "p":      unadjusted p-values of a pima object (flipscores)
#   - "wald.p": Wald p-values of the model summary (any source)
# A table with "p" comes from a pima object; a table with only "wald.p" comes
# from fitted models (list or pima.multi), whose p-values are never adjusted.
# Any other column (e.g. one added by the user) is used only when requested
# explicitly through `p`.
.pick_p <- function(tab, p = NULL, p.adjusted = NULL) {
  has <- function(v) v %in% names(tab) && !all(is.na(tab[[v]]))

  if (!is.null(p)) {
    if (!has(p)) {
      stop(sprintf("`p` column \"%s\" not found in the table.", p), call. = FALSE)
    }
    return(p)
  }

  if (has("p")) {
    # pima object: adjusted p-values when pima() applied a correction,
    # flipscores p-values when it did not (method = "none")
    if (isFALSE(p.adjusted)) {
      return("p")
    }
    if (has("p.adj")) {
      return("p.adj")
    }
    if (isTRUE(p.adjusted)) {
      stop(
        "the pima object has no adjusted p-values (pima() with method = \"none\"). ",
        "Use `p.adjusted = FALSE` or apply a correction with p.adjust().",
        call. = FALSE
      )
    }
    return("p")
  }

  if (has("wald.p")) {
    # fitted models: only unadjusted Wald p-values
    if (isTRUE(p.adjusted)) {
      stop(
        "adjusted p-values are not available: the models were not analysed with pima(). ",
        "Use `p.adjusted = FALSE` or a column of your own through `p`.",
        call. = FALSE
      )
    }
    return("wald.p")
  }

  stop("no p-values column found: set `p`.", call. = FALSE)
}

# only lm and glm models are supported (glm inherits from lm)
.check_lm_models <- function(mods) {
  ok <- vapply(mods, inherits, logical(1), what = "lm")
  if (!all(ok)) {
    nms <- names(mods)
    if (is.null(nms)) nms <- paste0("model", seq_along(mods))
    stop(
      sprintf(
        "only lm and glm models are supported. Unsupported: %s (class %s).",
        paste(nms[!ok], collapse = ", "),
        paste(unique(vapply(mods[!ok], function(m) class(m)[1], character(1))), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
