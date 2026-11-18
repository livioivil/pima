#' Post-selection Inference in Multiverse Analysis
#' @description Applies the PIMA procedure to make selective inference on one or more parameters within a multiverse of models,
#' derived from different specifications of the data (e.g., pre-processing) and of the model itself (GLMs).
#' The method jointly computes resampling-based test statistics for all coefficients of interest in all models.
#' The output allows for multiple testing in multiverse analysis, providing weak and strong control of the Family-Wise Error Rate (FWER)
#' and confidence bounds for True Discovery Proportions (TDPs).
#' @param mods the models of the multiverse: a list of fitted models (usually
#'   `glm` or `lm`; unnamed lists are named `model1`, `model2`, ...), a
#'   `pima.multi` object created by [create_multi()] or a `multiverse` object
#'   of the \pkg{multiverse} package.
#' @param tested_coeffs tested coefficients. It can be a vector of names,
#' tested in each model where they are present (every model must contain at
#' least one of them), or a list of the same length as `mods`, where each
#' element is a vector of coefficient names of that model (matched by name when
#' the list is named). The list is needed when the same variable enters the
#' models in different forms (e.g. `x`, `log(x)`): build it with
#' `split()` on `spec_table(mods, focal.var = "x")`.
#' If `NULL`, all coefficients are tested.
#' @param method multiplicity correction, among the ones of the engine (can
#'   be abbreviated). With `engine = "flipscores"`: `"maxT"` (default),
#'   `"minP"` or `"none"`. With `engine = "multcomp"`, the types of
#'   [multcomp::adjusted()]: `"single-step"` (default), `"free"`,
#'   `"Westfall"`, `"Shaffer"`, or the methods of [stats::p.adjust()]
#'   (`"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
#'   `"fdr"`, `"none"`). With `"none"` there are no adjusted p-values. If
#'   `NULL` (default), the default of the engine.
#' @param tail direction of the tests: `0` (two-sided, default), a positive
#'   value (greater) or a negative value (less). With `engine = "flipscores"`
#'   it is used for the adjusted p-values (passed to [p.adjust()]); with
#'   `engine = "multcomp"` for all the p-values.
#' @param extra optional data frame with one row per model (in the order of
#'   `mods`) of additional information, added to `info` and `summary_table`.
#'   For `pima.multi` objects the columns `model`, `subset` and `fit_spec` of
#'   the scenarios (and `.source`, `y.fun` when present) are added
#'   automatically.
#' @param id identifier of the observations, passed to [flipscores::flipscores()]:
#'   the name of a column of the models' data or a one-sided formula (e.g.
#'   `~id`). Used to align the scores of models fitted on different rows (e.g.
#'   after subsetting or removing missing values); repeated values define
#'   clusters. If `NULL` (default), the scores are aligned by row names.
#' @param seed seed of the random numbers, for reproducible results: the sign
#'   flips with `engine = "flipscores"`, the numerical integration of the
#'   adjusted p-values with `engine = "multcomp"`. If `NULL` (default), the
#'   current state of the random number generator is used.
#' @param engine the test used: `"flipscores"` (default), the sign-flip score
#'   test, or `"multcomp"`, the experimental parametric test based on
#'   [multcomp::mmm()] (see *Engines* below). To set the arguments of the
#'   engine (e.g. `n_flips`), use [pima_flipscores()] or [pima_multcomp()].
#' @param ... not used: the arguments of the engines are given to
#'   [pima_flipscores()] or [pima_multcomp()].
#' @details The procedure builds on the sign-flip score test implemented in [flipscores::flipscores()].
#' For each tested coefficient in each model, it computes a set of resampling-based test statistics and a raw p-value
#' for the null hypothesis that the coefficient is null against a two-sided alternative.
#' If `method` is `maxT` (default) or `minP`, multiplicity-adjusted p-values are added
#' using the step-down method of \insertCite{westfall1993resampling;textual}{pima} (see *P-values* below). Adjusted p-values provide control of the FWER
#' asymptotically in the sample size and, for practical purposes, almost exact control for any sample size.
#' @details Post-selection inference is achieved by merging information from the considered models:
#' the adjusted p-values in the output summary provide strong FWER control and answer the question
#' "Which effects in which models are significant?".
#' @details The number of sign flips and the other arguments of
#' [flipscores::flipscores()] are given to [pima_flipscores()], e.g.
#' `score_type`, the type of score: the default `"standardized"` provides
#' almost exact control of the error for any sample size.
#' @section P-values:
#' Let \eqn{T_{jb}} be the test statistic of the \eqn{j}-th tested coefficient
#' (\eqn{j = 1, \dots, m}, over all the models) under the \eqn{b}-th
#' sign-flipping transformation (\eqn{b = 1, \dots, B}, with \eqn{B} =
#' `n_flips` of [pima_flipscores()] and \eqn{b = 1} the identity), taken in absolute value for
#' two-sided tests. The raw p-value is the proportion of transformations
#' giving a statistic at least as extreme as the observed one:
#' \deqn{p_j = \frac{1}{B} \sum_{b=1}^{B} \mathbf{1}\left(T_{jb} \ge T_{j1}\right).}{p_j = #{b : T_jb >= T_j1} / B.}
#' With `method = "maxT"`, a hypothesis \eqn{H_j} in the set \eqn{R} of the
#' hypotheses not yet rejected is rejected at level \eqn{\alpha} when
#' \deqn{T_{j1} > c_\alpha(R), \qquad c_\alpha(R) = Q_{1-\alpha}\left(\max_{k \in R} T_{kb};\ b = 1, \dots, B\right),}{T_j1 > c(R), with c(R) the (1 - alpha) quantile of the B values max_{k in R} T_kb,}
#' then \eqn{R} is updated and the step is repeated until no further
#' hypothesis is rejected. The adjusted p-value \eqn{\tilde p_j}{p.adj_j} is
#' the smallest \eqn{\alpha} in the grid \eqn{0.001, 0.002, \dots, 0.099,
#' 0.1, 0.2, \dots, 0.9} (rounded down to multiples of \eqn{1/B}) at which
#' \eqn{H_j} is rejected, and 1 if it is never rejected: adjusted p-values
#' are therefore reported on this grid. `method = "minP"` applies the same
#' procedure to the p-values of each transformation instead of the
#' statistics.
#'
#' @section Engines:
#' The engine is chosen with `engine`: a name (`"flipscores"` or
#' `"multcomp"`) uses its default arguments, [pima_flipscores()] and
#' [pima_multcomp()] set them. The other arguments of `pima()` are common to
#' the engines.
#'
#' With `engine = "flipscores"` (default) the tests are the sign-flip score
#' tests described above. With `engine = "multcomp"` (experimental, parametric) the
#' tested coefficients of all the models are tested jointly with
#' [multcomp::mmm()] and [multcomp::glht()]: the covariance between the
#' estimates of different models comes from the sandwich estimator of the
#' stacked estimating equations
#' \insertCite{pipper2012versatile,hothorn2008simultaneous}{pima}. `p` are
#' the univariate p-values and `p.adj` the p-values adjusted with the type
#' of [multcomp::adjusted()] given in `method`: `"single-step"` (default) and
#' `"free"` (its step-down version) use the correlation between the tests,
#' like the maxT procedure; `se` is the standard error of the estimate. The adjusted p-values are computed by
#' numerical integration, with a small random error: set `seed` (and, for more
#' precision, `algorithm` in [pima_multcomp()]).
#'
#' Without `id`, the multcomp engine requires the models to be fitted on the
#' same observations, in the same order. With `id` (the name of a column of
#' the models' data, with no repeated values within a model), the scores of
#' each model are aligned on the union of the observations, with zeros for
#' the observations a model does not use.
#'
#' @return Returns an object of class `pima`, containing:
#' \itemize{
#' \item `Tspace`: data frame of test statistics, where columns correspond to tests, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item `summary_table`: data frame containing a summary for each tested coefficient in each model:
#' estimate, score, standard error, z value, partial correlation, raw p-value, adjusted p-value.
#' \item `objects`: list of model objects computed by `flipscores`.
#' \item `tested_coeffs`: tested coefficients, as in input.
#' \item `engine`: the engine used, with its arguments (see [pima_flipscores()]).
#' }
#' With `engine = "multcomp"` there is no `Tspace`, `objects` are the
#' fitted models and `glht` is the [multcomp::glht()] object.
#' @references
#' \insertRef{girardi2024pima}{pima}
#'
#' \insertRef{hemerik2020robust}{pima}
#'
#' \insertRef{desantis2025inference}{pima}
#'
#' \insertAllCited{}
#' 
#' @examples
#' # Generate data
#' set.seed(123)
#' n <- 30
#' D <- data.frame(X1 = rnorm(n), X2 = rnorm(n)^2,
#'                 X3 = sample(c("A","B","C"), n, replace = TRUE),
#'                 Z1 = rnorm(n), Z2 = rnorm(n))
#' D$X3 <- factor(D$X3)
#' D$Y <- D$X1 + ifelse(D$X3 == "C", 3, 0) + D$Z1 + rnorm(n)
#' 
#' # Multiverse of 4 models (list)
#' mod1 <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2, data = D)
#' mod2 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + Z2, data = D)
#' mod3 <- glm(Y ~ X1 + X2 + X3 + Z1 + poly(Z2,2), data = D)
#' mod4 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + poly(Z2,2), data = D)
#' mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
#' 
#' # Test selected coefficients (raw and adjusted p-values)
#' res <- pima(mods, tested_coeffs = c("X1","X2","X3B","X3C"))
#' summary(res)
#' 
#' # Specifications and adjusted p-values
#' plot(res)
#' spec_curve(res)
#' @family PIMA inference
#' @export

pima <- function(mods, tested_coeffs = NULL,
                 method = NULL, tail = 0,
                 extra = NULL, id = NULL, seed = NULL, engine = "flipscores", ...) {
  UseMethod("pima")
}

# The methods share the arguments of the generic (shown by autocompletion
# whatever the input) and are documented by its usage: the input-specific
# methods only turn `mods` into a list of models plus `extra` columns and call
# pima.list().

#' @rdname pima
#' @usage NULL
#' @export
pima.list <- function(mods, tested_coeffs = NULL,
                      method = NULL, tail = 0,
                      extra = NULL, id = NULL, seed = NULL, engine = "flipscores", ...) {
  .check_no_dots(...)
  engine <- .as_engine(engine)
  method <- .engine_method(engine$name, method)
  mods <- .prepare_models(mods)
  tested_coeffs <- .check_tested_coeffs(mods, tested_coeffs)

  fit <- switch(
    engine$name,
    flipscores = .pima_flipscores(mods, tested_coeffs = tested_coeffs,
                                  method = method, tail = tail, id = id,
                                  seed = seed, args = engine$args),
    multcomp = do.call(.pima_multcomp,
                       c(list(mods, tested_coeffs = tested_coeffs, method = method,
                              tail = tail, id = id, seed = seed), engine$args))
  )
  .new_pima(fit, mods, tested_coeffs = tested_coeffs, extra = extra,
            method = method, engine = engine)
}

#' @rdname pima
#' @usage NULL
#' @export
pima.pima.multi <- function(mods, tested_coeffs = NULL,
                            method = NULL, tail = 0,
                            extra = NULL, id = NULL, seed = NULL, engine = "flipscores", ...) {
  .check_no_dots(...)
  if (is.null(mods$models)) {
    stop("the `pima.multi` object has no fitted models: use `fit.specs` in create_multi().",
         call. = FALSE)
  }
  cols <- intersect(c("model", "subset", "fit_spec", ".source", "y.fun"),
                    names(mods$scenarios))
  new_extra <- mods$scenarios[, cols, drop = FALSE]
  pima.list(mods$models, tested_coeffs = tested_coeffs, method = method,
            tail = tail, extra = .cbind_extra(extra, new_extra), id = id,
            seed = seed, engine = engine)
}

#' @rdname pima
#' @usage NULL
#' @export
pima.multiverse <- function(mods, tested_coeffs = NULL,
                            method = NULL, tail = 0,
                            extra = NULL, id = NULL, seed = NULL, engine = "flipscores", ...) {
  .check_no_dots(...)
  # TODO experimental multiverse support
  multi <- .parse_multiverse_object(mods)
  pima.list(multi$models, tested_coeffs = tested_coeffs, method = method,
            tail = tail, extra = .cbind_extra(extra, multi$extra), id = id,
            seed = seed, engine = engine)
}

#' @rdname pima
#' @usage NULL
#' @export
pima.default <- function(mods, tested_coeffs = NULL,
                         method = NULL, tail = 0,
                         extra = NULL, id = NULL, seed = NULL, engine = "flipscores", ...) {
  stop("`mods` must be a list of fitted models, a `pima.multi` object (see create_multi()) ",
       "or a `multiverse` object. For a single model use list(model).", call. = FALSE)
}

# The arguments of the engines (n_flips, ...) are not arguments of pima()
.check_no_dots <- function(...) {
  dots <- names(list(...))
  if (length(dots)) {
    dots[dots == ""] <- "<unnamed>"
    stop(sprintf(paste0("unused arguments in pima(): %s. The arguments of the engines ",
                        "(e.g. `n_flips`) go to pima_flipscores() or pima_multcomp(), ",
                        "e.g. engine = pima_flipscores(n_flips = 1000)."),
                 paste0("`", dots, "`", collapse = ", ")), call. = FALSE)
  }
}

# Models ready for the engines: named, and lm models refitted as glm
.prepare_models <- function(mods) {
  if (!length(mods)) {
    stop("`mods` is an empty list.", call. = FALSE)
  }
  if (is.null(names(mods))) {
    names(mods) <- paste0("model", seq_along(mods))
  }
  if (!all(vapply(mods, inherits, logical(1), what = "glm"))) {
    # TODO check if refitting is safe
    mods <- lapply(mods, as.glm)
  }
  mods
}

.cbind_extra <- function(extra, new_extra) {
  if (is.null(extra)) new_extra else cbind(extra, new_extra)
}

# Sign-flip score engine. `args` are the arguments of pima_flipscores().
# Returns the flipscores output (adjusted with p.adjust() unless method is
# "none"), the arguments passed to flipscores (stored in the pima object) and
# the class of the flipscores output.
.pima_flipscores <- function(mods, tested_coeffs, method, tail, id, seed, args) {
  args <- c(list(formula = mods, tested_coeffs = tested_coeffs, id = id, seed = seed), args)
  out <- do.call(flipscores::flipscores, args)
  cls <- class(out)

  # TODO check if removing res$call is problematic
  out <- out[names(out) != "call"]

  if (method != "none") {
    out <- flipscores::p.adjust(object = out, tail = tail, method = method)
  }

  list(
    out = out,
    args = .get_fn_args(flipscores::flipscores, new.args = args,
                        exclude = c("...", "mods")),
    class = cls
  )
}

# Common part of the pima object: responses, model information, extra
# columns and Wald p-values, added to the output of an engine.
.new_pima <- function(fit, mods, tested_coeffs, extra, method, engine) {
  out <- fit$out

  # flipscores labels the response with all.vars(), so log(y) and y would
  # share the same label: use the deparsed left-hand side instead.
  responses <- .get_response_name(out$objects)
  out$summary_table$response <- unname(responses[out$summary_table$model])

  info <- .get_info_models(out$objects, extra)
  out$info <- info$info
  out$tested_coeffs <- tested_coeffs
  out <- c(out, fit$args)
  out$p.adjust.method <- method
  out$extra <- info$extra
  out$engine <- engine

  # add extra to summary table
  out$summary_table <- merge(out$summary_table, info$extra, by = "model")

  # get wald statistics
  wald <- .bind_rows(lapply(mods, .get_glm_summary), "model")
  out$summary_table <- merge(out$summary_table,
                             wald[, c("model", "coefficient", "wald.p")],
                             by = c("model", "coefficient"))

  class(out) <- unique(c("pima", fit$class))
  out
}

#' Subset a multiverse object
#'
#' Subsets a `pima.multi` object by selecting rows from its `scenarios`
#' component. The corresponding fitted models are subset accordingly, preserving
#' the one-to-one correspondence between `scenarios$model` and the names of
#' `models`.
#'
#' @param x A `pima.multi` object.
#' @param subset A logical expression evaluated within `x$scenarios`. The
#'   expression must return one logical value for each scenario and cannot
#'   contain missing values.
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' Only the `scenarios` and `models` components are subset. Components describing
#' the original multiverse specification, such as `formula_specs` and
#' `specification`, are left unchanged.
#'
#' Model identifiers are not renumbered after subsetting. This preserves the
#' original correspondence between a scenario and its fitted model.
#'
#' @return A `pima.multi` object containing only the selected scenarios and,
#'   when available, their corresponding fitted models.
#'
#' @family multiverse construction
#' @export
#'
#' @examples
#' multi <- create_multi(
#'   Sepal.Width ~ Sepal.Length + Petal.Width,
#'   data = iris,
#'   nfuns = "log",
#'   fit.specs = list(lm = lm, glm = glm)
#' )
#'
#' # Keep only successfully fitted models
#' successful <- subset(multi, fit_ok)
#'
#' # Inspect failed fits
#' failed <- subset(multi, !fit_ok)
#'
#' # Keep scenarios using a particular fitting specification
#' lm_models <- subset(multi, fit_spec == "lm")
#'
#' # Filter according to an analytical decision
#' log_models <- subset(
#'   multi,
#'   spec_Sepal.Length == "log"
#' )
#'
subset.pima.multi <- function(x, subset, ...) {
  keep <- eval(
    substitute(subset),
    x$scenarios,
    parent.frame()
  )
  
  if (!is.logical(keep) || length(keep) != nrow(x$scenarios)) {
    stop(
      paste0(
        "`subset` must evaluate to a logical vector with one value ",
        "per scenario."
      ),
      call. = FALSE
    )
  }
  
  if (anyNA(keep)) {
    stop(
      "`subset` cannot contain missing values.",
      call. = FALSE
    )
  }
  
  x$scenarios <- x$scenarios[
    keep,
    ,
    drop = FALSE
  ]
  x$k <- nrow(x$scenarios)
  
  if (!is.null(x$models)) {
    x$models <- x$models[keep]
  }
  
  x
}
