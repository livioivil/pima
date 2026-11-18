.model_ids <- function(m, id) {
  mf  <- model.frame(m)
  emf <- expand.model.frame(m, id, na.expand = TRUE)   # rows used by the model + the id column
  if (nrow(emf) != nrow(mf) ||
      !isTRUE(all.equal(emf[[1]], mf[[1]], check.attributes = FALSE)))
    stop("the data used to fit the model cannot be recovered (has the data object been modified?).",
         call. = FALSE)
  emf[[id]]
}

.pad_models <- function(mods, id) {
  ids <- lapply(mods, .model_ids, id = id)
  if (any(vapply(ids, anyDuplicated, 1L) > 0)) {
    stop("`id` has repeated values within a model: clusters are not supported by the parametric method.",
         call. = FALSE)
  }
  id_all <- unique(unlist(ids))                         # union of the ids of all models
  Map(function(m, i) {
    attr(m, "ids") <- i; attr(m, "id_all") <- id_all
    class(m) <- c("padded", class(m)); m
  }, mods, ids)
}


#' @importFrom sandwich estfun
#' @importFrom stats model.frame
NULL

#' @method estfun padded
#' @export
estfun.padded <- function(x, ...) {
  s  <- NextMethod()
  na <- x$na.action
  if (inherits(na, "exclude")) s <- s[-na, , drop = FALSE]   # keep only the rows used
  out <- matrix(0, length(attr(x, "id_all")), ncol(s), dimnames = list(NULL, colnames(s)))
  out[match(attr(x, "ids"), attr(x, "id_all")), ] <- s
  out
}

#' Parametric simultaneous inference across models
#'
#' Experimental parametric backend of [pima()] (`parametric = TRUE`). The
#' tested coefficients of all the models are tested jointly with
#' [multcomp::mmm()] and [multcomp::glht()]: the covariance between the
#' estimates of different models comes from the sandwich estimator of the
#' stacked estimating equations
#' \insertCite{pipper2012versatile,hothorn2008simultaneous}{pima}, and the
#' p-values are adjusted with the single-step method.
#'
#' Without `id` the models must be fitted on the same observations, in the
#' same order. With `id`, the scores of each model are aligned on the union of
#' the observations, with zeros for the observations a model does not use.
#'
#' @param x named list of fitted models.
#' @param id name of a column of the models' data identifying the
#'   observations, used to align models fitted on different rows. Each value
#'   must appear at most once in each model (clusters are not supported).
#' @param tested_coeffs coefficients to test, as in [pima()]: a character
#'   vector (the coefficients present in each model are tested) or a list with
#'   one element per model. If `NULL`, all the coefficients of each model.
#' @return A data frame with one row per tested coefficient: `model`,
#'   `coefficient`, `estimate`, `se`, `z`, the Wald p-value of the single
#'   model (`p`) and the adjusted p-value (`p.adj`).
#' @references
#' \insertAllCited{}
#' @seealso [pima()]
#' @keywords internal
#' @export
#' @examples
#' mods <- list(
#'   a = glm(Sepal.Width ~ Sepal.Length, data = iris),
#'   b = glm(Sepal.Width ~ Sepal.Length + Species, data = iris),
#'   c = glm(Sepal.Width ~ log(Petal.Width), data = iris)
#' )
#' pima.mmm(mods, tested_coeffs = c("Sepal.Length", "log(Petal.Width)"))
pima.mmm <- function(x, id = NULL, tested_coeffs = NULL) {
  if (is.null(names(x))) {
    names(x) <- paste0("model", seq_along(x))
  }

  if (is.null(id)) {
    n <- vapply(x, stats::nobs, numeric(1))
    if (length(unique(n)) > 1L) {
      stop("the models are fitted on different numbers of observations: use `id` to align them.",
           call. = FALSE)
    }
    fit <- do.call(multcomp::mmm, x)
  } else {
    fit <- do.call(multcomp::mmm, .pad_models(x, id = id))
  }

  tested <- .mmm_tested(x, tested_coeffs)

  # one row per tested coefficient, selecting it from the stacked
  # coefficients of mmm() ("model: coefficient"): coefficient names are used
  # as they are, so transformed predictors such as log(x) work
  cf <- stats::coef(fit)
  lab <- unlist(Map(function(m, k) if (length(k)) paste0(m, ": ", k), names(tested), tested),
                use.names = FALSE)
  K <- matrix(0, length(lab), length(cf), dimnames = list(lab, names(cf)))
  K[cbind(lab, lab)] <- 1
  mc <- summary(multcomp::glht(fit, linfct = K))

  xs <- data.frame(
    model = rep(names(tested), lengths(tested)),
    coefficient = unlist(tested, use.names = FALSE),
    estimate = unname(mc$test$coefficients),
    se = unname(mc$test$sigma),
    z = unname(mc$test$tstat),
    p.adj = unname(mc$test$pvalues)
  )

  wald <- .bind_rows(lapply(x, .get_glm_summary), "model")
  xs$p <- wald$wald.p[match(paste(xs$model, xs$coefficient, sep = "\r"),
                            paste(wald$model, wald$coefficient, sep = "\r"))]
  xs[, c("model", "coefficient", "estimate", "se", "z", "p", "p.adj")]
}

# Tested coefficients of each model, with the names used by coef(): a vector
# is matched against each model, a list has one element per model, NULL
# selects all the coefficients. Spaces are ignored, as in .check_tested_coeffs().
.mmm_tested <- function(mods, tested_coeffs) {
  clean <- function(x) gsub(" ", "", x)
  if (!is.list(tested_coeffs)) {
    tested_coeffs <- list(tested_coeffs)
  }
  Map(function(m, k) {
    cf <- names(stats::coef(m))
    if (is.null(k)) cf else cf[clean(cf) %in% clean(k)]
  }, mods, tested_coeffs)
}
