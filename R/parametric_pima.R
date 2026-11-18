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
    stop("`id` has repeated values within a model: clusters are not supported by the multcomp engine.",
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

# multcomp engine (experimental, parametric): joint test of the tested
# coefficients of all the models with multcomp::mmm() and multcomp::glht().
# `method` is a type of multcomp::adjusted(); `algorithm` is the argument of
# pima_multcomp().
# Returns the same structure as .pima_flipscores(): see "Engines" in ?pima.
.pima_multcomp <- function(mods, tested_coeffs, method, tail, id, seed,
                           algorithm = NULL) {
  warning("the multcomp engine is experimental.", call. = FALSE)

  if (is.null(id)) {
    n <- vapply(mods, stats::nobs, numeric(1))
    if (length(unique(n)) > 1L) {
      stop("the models are fitted on different numbers of observations: use `id` to align them.",
           call. = FALSE)
    }
    fit <- do.call(multcomp::mmm, mods)
  } else {
    if (inherits(id, "formula")) id <- all.vars(id)
    if (!is.character(id) || length(id) != 1L) {
      stop("with the multcomp engine `id` must be the name of a column of the models' data.",
           call. = FALSE)
    }
    fit <- do.call(multcomp::mmm, .pad_models(mods, id = id))
  }

  tested <- .mmm_tested(mods, tested_coeffs)

  # one row per tested coefficient, selecting it from the stacked
  # coefficients of mmm() ("model: coefficient"): coefficient names are used
  # as they are, so transformed predictors such as log(x) work
  cf <- stats::coef(fit)
  lab <- unlist(Map(function(m, k) if (length(k)) paste0(m, ": ", k), names(tested), tested),
                use.names = FALSE)
  K <- matrix(0, length(lab), length(cf), dimnames = list(lab, names(cf)))
  K[cbind(lab, lab)] <- 1
  alternative <- if (tail > 0) "greater" else if (tail < 0) "less" else "two.sided"
  g <- multcomp::glht(fit, linfct = K, alternative = alternative)
  uni <- summary(g, test = multcomp::univariate())$test

  model <- rep(names(tested), lengths(tested))
  df <- vapply(mods, stats::df.residual, numeric(1))[model]
  st <- data.frame(
    model = model,
    coefficient = unlist(tested, use.names = FALSE),
    estimate = unname(uni$coefficients),
    se = unname(uni$sigma),
    z = unname(uni$tstat),
    pcor = unname(uni$tstat / sqrt(uni$tstat^2 + df)),
    p = unname(uni$pvalues)
  )
  if (method != "none") {
    # "single-step", "free", "Westfall" and "Shaffer" integrate the
    # multivariate normal numerically (`algorithm`, `seed`)
    test <- if (is.null(algorithm)) {
      multcomp::adjusted(method)
    } else {
      multcomp::adjusted(method, algorithm = algorithm)
    }
    if (!is.null(seed)) set.seed(seed)
    st$p.adj <- unname(summary(g, test = test)$test$pvalues)
  }

  list(
    out = list(summary_table = st, objects = mods, glht = g),
    args = list(id = id, seed = seed),
    class = character(0)
  )
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
