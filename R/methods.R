#' @title Methods for pima objects
#'
#' @description Methods for \code{pima} objects.
#' The following are methods to extract and manipulate relevant information from
#' a \code{pima} object.
#'
#' @name pima-method
#' @docType methods

NULL

#' print.pima print method for a pima object.
#' @rdname pima-method
#' @param x an object of class \code{pima}.
#' @param n number of rows to print from the beginning and end of the scenario table.
#' @param ... additional arguments to be passed
#' @method print pima
#' @docType methods
#' @export
print.pima <- function(x, n = 4, ...) {
  nr <- nrow(x$info)
  msg <- sprintf("== Multiverse analysis with %s scenarios ==", nr)
  cat("\n")
  cat(msg)
  cat("\n\n")
  rownames(x$info) <- NULL
  .trim(x$info, n = n)
  cat("\n")
}

#' summary.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param digits number of digits when rounding. Default to `NULL` thus no rounding.
#' @param ... additional arguments to be passed
#' @method  summary pima
#' @docType methods
#' @export

summary.pima <- function(object, digits = NULL, ...) {
  summ <- object$summary_table
  summ <- summ[, !colnames(summ) %in% c(".assign")]
  if (!is.null(digits)) {
    clapply(summ, "numeric", round, digits)
  } else {
    summ
  }
}

#' plot.pima summary method for a pima object.
#'
#' Scatter plot of the specifications: calls [spec_scatter()] on the `pima`
#' object.
#' @rdname pima-method
#' @param x an object of class \code{pima}.
#' @param by,focal,xvar,yvar,p.adjusted,p.transf,alpha,xlab,ylab,shapes,facet,facet.scales,which.response
#'   see [spec_scatter()].
#' @param regex logical. If \code{TRUE}, `focal` is a regular expression
#'   matched against the coefficient names. Default is \code{FALSE}.
#' @param ... further arguments passed to [spec_scatter()] (e.g. `focal.var`,
#'   `p`, `title`).
#' @method  plot pima
#' @docType methods
#' @export

plot.pima <- function(
  x,
  by = "coefficient",
  focal = NULL,
  xvar = "pcor",
  yvar = NULL,
  p.adjusted = NULL,
  p.transf = "z",
  alpha = 0.05,
  xlab = NULL,
  ylab = NULL,
  regex = FALSE,
  shapes = NULL,
  facet.scales = "free_x",
  facet = NULL,
  which.response = NULL,
  ...
) {
  if (regex && length(focal)) {
    coefs <- unique(x$summary_table$coefficient)
    focal <- coefs[grepl(paste0(focal, collapse = "|"), coefs)]
    if (!length(focal)) {
      stop("`focal` does not match any coefficient.", call. = FALSE)
    }
  }
  spec_scatter(
    x,
    focal = focal,
    xvar = xvar,
    yvar = yvar,
    p.adjusted = p.adjusted,
    p.transf = p.transf,
    by = by,
    alpha = alpha,
    xlab = xlab,
    ylab = ylab,
    shapes = shapes,
    facet = facet,
    facet.scales = facet.scales,
    which.response = which.response,
    ...
  )
}

#' print.pima.multi
#' @rdname pima-method
#' @param object an object of class \code{pima.multi}.
#' @method  print pima.multi
#' @docType methods
#' @export

print.pima.multi <- function(x, ...) {
  .header("Multiverse Object")
  n_scen <- nrow(x$scenarios)
  combined <- !is.null(x$specification$components)
  if (combined) {
    n_sub <- sum(unique(x$scenarios[c(".source", "subset")])$subset != "all")
  } else {
    n_sub <- length(x$specification$subset)
    n_sub <- if(n_sub == 1) 0 else 1
  }
  n_form <- nrow(x$formula_specs)
  n_fit <- if(is.null(x$models)) 0 else length(x$models)
  
  if (combined) {
    .main("Blocks:", length(unique(x$formula_specs$.source)))
  } else {
    .main(
      "Formula:",
      paste(deparse(x$specification$formula), collapse = "")
    )
  }
  .space()
  .main(
    sprintf("Scenarios: n = %s", n_scen)
  )
  .main(
    sprintf("  > Formulas: n = %s", n_form)
  )
  .main(
    sprintf("  > Subsets: n = %s", n_sub)
  )
  .main(
    sprintf("  > Models: n = %s", n_fit)
  )
  
  if(n_fit != 0){
    n_error <- sum(!x$scenarios$fit_ok)
    .space()
    .main(
      "Models info"
    )
    .main(
      sprintf("  > Errors: n = %s", n_error)
    )
  }
  
  invisible(x)
}
