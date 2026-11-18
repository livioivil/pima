#' Post-selection Inference in Multiverse Analysis
#' @description Applies the PIMA procedure to make selective inference on one or more parameters within a multiverse of models,
#' derived from different specifications of the data (e.g., pre-processing) and of the model itself (GLMs).
#' The method jointly computes resampling-based test statistics for all coefficients of interest in all models.
#' The output allows for multiple testing in multiverse analysis, providing weak and strong control of the Family-Wise Error Rate (FWER)
#' and confidence bounds for True Discovery Proportions (TDPs).
#' @param mods list of objects that can be evaluated by [flipscores::flipscores()], usually
#' model objects produced by `glm` or `flipscores`.
#' @param tested_coeffs tested coefficients. It can be a vector of names,
#' tested in each model where they are present (every model must contain at
#' least one of them), or a list of the same length as `mods`, where each
#' element is a vector of coefficient names of that model (matched by name when
#' the list is named). The list is needed when the same variable enters the
#' models in different forms (e.g. `x`, `log(x)`): build it with
#' `split()` on `spec_table(mods, focal.var = "x")`.
#' If `NULL`, all coefficients are tested.
#' @param n_flips number of sign flips.
#' @param method correction method among `maxT`, `minP` and `none`. Can be abbreviated.
#' @param tail direction of the tests used for the adjusted p-values: `0`
#'   (two-sided, default), a positive value (greater) or a negative value
#'   (less). Passed to [p.adjust()].
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
#' @param parametric if `TRUE`, uses the experimental parametric backend based
#'   on [multcomp::mmm()] instead of the sign-flip score test. Its output is
#'   not compatible with the other functions of the package.
#' @param ... further parameters of [flipscores::flipscores()].
#' @details The procedure builds on the sign-flip score test implemented in [flipscores::flipscores()].
#' For each tested coefficient in each model, it computes a set of resampling-based test statistics and a raw p-value
#' for the null hypothesis that the coefficient is null against a two-sided alternative.
#' If `method` is `maxT` (default) or `minP`, multiplicity-adjusted p-values are added
#' using the step-down method of \insertCite{westfall1993resampling;textual}{pima} (see *P-values* below). Adjusted p-values provide control of the FWER
#' asymptotically in the sample size and, for practical purposes, almost exact control for any sample size.
#' @details Post-selection inference is achieved by merging information from the considered models:
#' the adjusted p-values in the output summary provide strong FWER control and answer the question
#' "Which effects in which models are significant?".
#' @details Further parameters include:
#' \itemize{
#' \item `score_type`: type of score that is computed (see [flipscores::flipscores()] for more details).
#' The default `"standardized"` provides almost exact control of the error for any sample size.
#' \item `statistics`: test statistics computed by the procedure. Currently, `t` is the only implemented method.
#' Different statistics will affect the multivariate inference, but not the univariate.
#' \item `seed`: can be specified to ensure replicability of results.
#' \item `output_models`: `TRUE` to return the list of model objects produced by `flipscores`.
#' }
#' @section P-values:
#' Let \eqn{T_{jb}} be the test statistic of the \eqn{j}-th tested coefficient
#' (\eqn{j = 1, \dots, m}, over all the models) under the \eqn{b}-th
#' sign-flipping transformation (\eqn{b = 1, \dots, B}, with \eqn{B} =
#' `n_flips` and \eqn{b = 1} the identity), taken in absolute value for
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
#' @return Returns an object of class `pima`, containing:
#' \itemize{
#' \item `Tspace`: data frame of test statistics, where columns correspond to tests, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item `summary_table`: data frame containing a summary for each tested coefficient in each model:
#' estimate, score, standard error, z value, partial correlation, raw p-value, adjusted p-value.
#' \item `mods`: list of model objects computed by `flipscores`.
#' \item `tested_coeffs`: tested coefficients, as in input.
#' }
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

pima <- function(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), tail = 0, extra = NULL, id = NULL, parametric = FALSE, ...) {
  
  if(inherits(mods, "pima.multi")){
    old <- mods
    mods <- mods$models
    extra_columns <- c("model", "subset", "fit_spec")
    if (".source" %in% names(old$scenarios)) {
      extra_columns <- c(extra_columns, ".source")
    }
    if ("y.fun" %in% names(old$scenarios)) {
      extra_columns <- c(extra_columns, "y.fun")
    }
    new_extra <- old$scenarios[, extra_columns, drop = FALSE]
    if(!is.null(extra)){
      extra <- cbind(extra, new_extra)
    } else{
      extra <- new_extra
    }
  }
  
  # TODO experimental multiverse support
  if(inherits(mods, "multiverse")){
    multi <- .parse_multiverse_object(mods)
    mods <- multi$models
    new_extra <- multi$extra
    if(!is.null(extra)){
      extra <- cbind(extra, new_extra)
    } else{
      extra <- new_extra
    }
  }
  
  mods_are_glm <- sapply(mods, function(x) inherits(x, "glm"))

  if(!all(mods_are_glm)){
    # TODO check if refitting is safe
    # stop("all models within mods must be of class glm!")
    mods <- lapply(mods, as.glm)
  }
  
  tested_coeffs <- .check_tested_coeffs(mods, tested_coeffs)
  
  # TODO temporary testing the parametric pima
  if(parametric){
    warning("Current implementation of parametric pima based on multcomp::mmm() is experimental! The output is not compatible with other methods.")
    res <- pima.mmm(mods, tested_coeffs = tested_coeffs)
    return(res)
  }

  method <- match.arg(method, c("maxT", "minP", "none"))
  
  extra_args <- list(...)
  
  flipscores_args <- c(
    list(formula = mods, tested_coeffs = tested_coeffs, n_flips = n_flips, id = id), 
    extra_args
  )
  
  out <- do.call(flipscores::flipscores, flipscores_args)
  old_class <- class(out)
  
  # TODO check if removing res$call is problematic
  out <- out[names(out) != "call"]
  
  if(method != "none"){
    # TODO is tail something to put as parameter?
    out <- flipscores::p.adjust(object = out, tail = tail, method = method)
  }
  
  # get all the arguments of jointest::join_flipscores to be
  # used in other functions
  
  join_flipscores_args <- .get_fn_args(flipscores::flipscores, 
                                       new.args = flipscores_args, 
                                       exclude = c("...", "mods"))
  
  # flipscores labels the response with all.vars(), so log(y) and y would
  # share the same label: use the deparsed left-hand side instead.
  responses <- .get_response_name(out$objects)
  out$summary_table$response <- unname(responses[out$summary_table$model])

  info <- .get_info_models(out$objects, extra)
  out$info <- info$info
  out$tested_coeffs <- tested_coeffs
  out <- c(out, join_flipscores_args)
  out$p.adjust.method <- method
  out$extra <- info$extra
  
  # add extra to summary table
  out$summary_table <- merge(out$summary_table, info$extra, by = "model")

  # get wald statistics
  wald_l <- lapply(seq_along(mods), function(i) {
    d <- .get_glm_summary(mods[[i]])
    d$model <- names(mods[i])
    d
  })

  wald_l <- do.call(rbind, wald_l)
  out$summary_table <- merge(out$summary_table,
        wald_l[, c("model", "coefficient", "wald.p")],
        by = c("model", "coefficient"))
  
  class(out) <- unique(c("pima", old_class))
  return(out)
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
