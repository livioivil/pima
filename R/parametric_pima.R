.model_ids <- function(m, id) {
  mf  <- model.frame(m)
  emf <- expand.model.frame(m, id, na.expand = TRUE)   # stesse righe usate dal modello + colonna id
  if (nrow(emf) != nrow(mf) ||
      !isTRUE(all.equal(emf[[1]], mf[[1]], check.attributes = FALSE)))
    stop("i dati del modello non sono più ricostruibili (dataset modificato?)")
  emf[[id]]
}

.pad_models <- function(mods, id) {
  ids <- lapply(mods, .model_ids, id = id)
  if (any(vapply(ids, anyDuplicated, 1L) > 0)) stop("ID duplicati")
  id_all <- unique(unlist(ids))                         # unione degli ID di tutti i modelli
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
  if (inherits(na, "exclude")) s <- s[-na, , drop = FALSE]   # tengo solo le righe usate
  out <- matrix(0, length(attr(x, "id_all")), ncol(s), dimnames = list(NULL, colnames(s)))
  out[match(attr(x, "ids"), attr(x, "id_all")), ] <- s
  out
}

#' @export
pima.mmm <- function(x, id = NULL, tested_coeffs = NULL){
  if(is.null(id)){
    x <- do.call(multcomp::mmm, x)
  } else{
    x <- do.call(multcomp::mmm, .pad_models(x, id = id))
  }

  all_coefs <- sapply(x, function(x) names(stats::coef(x)))
  all_coefs <- Reduce(intersect, all_coefs)

  if(!is.null(tested_coeffs)){
    tested_coeffs <- sprintf("%s = 0", tested_coeffs)
  } else{
    tested_coeffs <- sprintf("%s = 0", all_coefs)
  }
  mc <- multcomp::glht(x, multcomp::mlf(tested_coeffs))
  mc <- summary(mc)

  xl <- lapply(x, .get_glm_summary)
  xl <- .bind_rows(xl, "model")

  xs <- data.frame(
    model = names(mc$model),
    estimate = mc$test$coefficients,
    se = mc$test$sigma,
    z = mc$test$tstat,
    p.adj = mc$test$pvalues
  )

  xs$coefficient <- .coef_from_rn(rownames(xs), all_coefs)
  rownames(xs) <- NULL
  xs <- merge(xs, xl[, c("model", "coefficient", "wald.p")], by = c("model", "coefficient"))
  xs <- xs[, c("model", "coefficient", "estimate", "se", "z", "wald.p", "p.adj")]
  names(xs)[6] <- "p"
  xs
}

.coef_from_rn <- function(rn, coefs) {
    coefs <- coefs[order(nchar(coefs), decreasing = TRUE)]
    out <- rep(NA_character_, length(rn))
    for (cf in coefs) out[is.na(out) & endsWith(rn, cf)] <- cf
    out
}