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
#' @param object an object of class \code{pima}.
#' @param ... additional arguments to be passed
#' @method print pima
#' @docType methods
#' @export
print.pima <- function (object, n = 4) {
  nr <- nrow(object$info)
  msg <- sprintf("== Multiverse analysis with %s scenarios ==", nr)
  cat("\n")
  cat(msg)
  cat("\n\n")
  rownames(object$info) <- NULL
  .trim(object$info, n = n)
  cat("\n")
}

#' summary.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param ... additional arguments to be passed
#' @method  summary pima
#' @docType methods
#' @export

summary.pima <- function(object, ...) {
  object$summary_table
}

.get_summary_table_from_flipscores <- function(object) {
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "Score"]), ]
  colnames(tab)[ncol(tab)] = "p"
  tab = cbind(Coeff = rownames(tab), tab)
}

#' as.pima method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param names_obj a vector of names, its length must be equal to the length of \code{object}
#' @param ... additional arguments to be passed
#' @method as pima
#' @docType methods
#' @export

as.pima <- function(object, names_obj = NULL, ...) {
  # TODO: calcolare summary_table in ogni elemento di object. se flipscores usa
  #     .get_summary_table_from_flipscores()
  if (!is.null(names_obj)) {
    names(object) = names_obj
  }
  if (is.null(names(object))) {
    names(object) = paste0("mod", 1:length(object))
  }
  class(object) <- unique(c("pima", class(object)))
  object
}

#' plot.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param p.values  use "raw" or "adjusted" (i.e. corrected for selective inference). Default to "raw".
#' @param y `"-log10(p)"` by default. It can be any column's name of the `summary()` of a `pima-object` (e.g. `"z value"`) or any other expression e.g. `"-log10(p)"`.
#' @param x `"Estimate"` by default. It can be any column's name of the `summary()` of a `pima-object` (e.g. `"Part. Cor"`) or any other expression e.g. `"-log10(p)"`.
#' @param alpha a value between 0 and 1. The plot will mark the p-values smaller than \code{alpha} (0.05 by default). If equal to 0 or 1 nothing will be marked.
#' @param ... additional arguments to be passed
#' @method  plot pima
#' @docType methods
#' @export

plot.pima <- function(
    object,
    p.adjusted = TRUE,
    alpha = .05,
    xvar = NULL,
    p.values = NULL,
    p.transf = "-log10",
    ...
  )
  {
  
  # TODO what about adding a way to transform the p value using custom formula? Also adding critical value (p = 0.05) when using a transformation.
  
  # avoid conflicting with base plot(x = ) argument
  
  if(is.null(xvar)) xvar <- "Estimate"
  
  D = object$summary_table
  D$.assign = NULL
  
  # check if is one of the available columns
  xvar <- match.arg(xvar, choices = colnames(D), several.ok = FALSE)
  
  # for yvar the name can be also an expression (as character) to be
  # evaluated. Thus check if y exist, otherwise try to evaluate the expression
  
  if(!yvar %in% colnames(D)){
    D[[yvar]] <- eval(parse(text = yvar), envir = D)
  }
  
  p.values = match.arg(p.values, choices = c("adjusted", "raw"))
  group = "Coeff"

  # RIRR and RP indices
  
  IRR <- exp(object$summary_table$Estimate)
  RIRR <- as.numeric(quantile(IRR, c(0.99)) / quantile(IRR, c(0.01)))
  
  # Relative pvalues
  RP_raw <- as.numeric(diff(
    -log10(quantile(object$summary_table$p, c(0.99, 0.01)))
  ))
  
  RP_adjusted <- as.numeric(diff(
    -log10(quantile(object$summary_table$p.adj, c(0.99, 0.01)))
  ))
  
  if (p.values == "raw") {
    D$is_signif = (D$p <= alpha)
    title = "(Raw) p-values"
    
    # TODO how to use thr RIIR and RP indexes?
    
    indices <- paste(
      "RIRR=",
      round(RIRR, 3),
      "\nRP=",
      round(RP_raw, 3),
      sep = ""
    )
    
  } else {
    # adj
    title = "Adjusted p-values"
    adj_id = grep("p.adj", colnames(D))[1]
    D$is_signif = (D[, adj_id] <= alpha)
    
    indices <- paste(
      "RIRR=",
      round(RIRR, 3),
      "\nRP=",
      round(RP_adjusted, 3),
      sep = ""
    )
    
  }

  p <- ggplot2::ggplot(D, 
               ggplot2::aes(x = .data[[xvar]], 
                  y = .data[[yvar]], 
                  group = .data[[group]], 
                  color = .data[[group]])) +
    ggplot2::geom_point(ggplot2::aes(shape = is_signif), size = 2) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal()
  
  if (!(alpha %in% c(0, 1))) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = c(3, 19),
        name = paste(p.values, "p-value"),
        labels = paste0(c("p >  ", "p <= "), alpha)
      )
  }
  p
}