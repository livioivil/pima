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
#' @param focal a character vector indicating which coefficients to plot. When > 1 coefficient is provided (or NULL) and `xvar` is not provided the `Part. Cor` column is used instead of the `Estimate`.
# TODO check the focal documentation
#' @param xvar character indicating the column of the `object$summary_table` to be plotted on the x axis. Default to "Estimate".
#' @param p.adjust logical indicating whether plotting raw (`FALSE`) or adjusted p.values (`TRUE`, default). 
#' @param p.transf can be a character vector indicating the transformation to use (see [transf_p()]) or a custom function.
#' @param alpha a value between 0 and 1. The plot will mark the p-values smaller than \code{alpha} (0.05 by default). If equal to 0 or 1 nothing will be marked.
#' @param xlab character vector indicating the x-axis label. Default to `xvar`
#' @param ylab character vector indicating the y-axis label. Default to `p` or `p.adjust.<method>` where method is `object$p.adjust.method`.
#' @param ... additional arguments to be passed
#' @method  plot pima
#' @docType methods
#' @export

plot.pima <- function(
    object,
    focal = NULL,
    xvar = NULL,
    p.adjusted = TRUE,
    p.transf = "-log10",
    alpha = 0.05,
    xlab = NULL,
    ylab = NULL,
    regex = FALSE,
    ...
  )
  {
  
  # TODO what about adding a way to transform the p value using custom formula? Also adding critical value (p = 0.05) when using a transformation.
  
  # avoid conflicting with base plot(x = ) argument
  
  # if parameters to be plotted > 1 and there is no focal, plot all of them but
  # use the partial correlation.
  
  if(is.null(focal) & length(object$tested_coeffs) > 1 & is.null(xvar)){
    xvar <- "Part. Cor"
    warning("the number of tested coefficients is > 1 and no xvar specified. Using 'Part. Cor' as xvar.")
  }
  
  if(is.null(xvar)) xvar <- "Estimate"
  # TODO fix this in jointest
  
  if(object$p.adjust.method == "none" & p.adjusted) {
    stop("the pima() functions as been called without p.values adjustments. Re-run without pima(..., method = 'none')" )
  }
  
  if(p.adjusted){
    p.values <- sprintf("p.adj.%s", object$p.adjust.method)
  } else{
    p.values <- "p"
  }
  
  D = object$summary_table
  D$.assign = NULL
  
  if(is.null(focal)) focal <- object$tested_coeffs
  
  if(!is.null(focal)){
    if(regex){
      D <- subset(D, grepl(paste0(focal, collapse = "|"), Coeff))
    } else{
      D <- subset(D, Coeff %in% focal)
    }
    
  }
  
  # check if is one of the available columns
  xvar <- match.arg(xvar, choices = colnames(D), several.ok = FALSE)
  p.values <- match.arg(p.values, choices = colnames(D), several.ok = FALSE)
  group <- "Coeff"
  
  # transform the p value
  D$.p.values.transf <- transf_p(D[[p.values]], method = p.transf)
  
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
  
  if (object$p.adjust.method == "none") {
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
  
  if(is.null(xlab)) xlab <- xvar
  
  p.transf.txt <- if(is.function(p.transf)) "custom" else p.transf
  
  if(is.null(ylab)) ylab <- sprintf("%s (%s)", p.values, p.transf.txt)

  p <- ggplot2::ggplot(D, 
               ggplot2::aes(x = .data[[xvar]], 
                  y = .data[[".p.values.transf"]], 
                  group = .data[[group]], 
                  color = .data[[group]])) +
    ggplot2::geom_point(ggplot2::aes(shape = is_signif), size = 2) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = xlab,
      y = ylab
    )
  
  if (!(alpha %in% c(0, 1))) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = c(3, 19),
        name = "p-value",
        labels = paste0(c("p >  ", "p <= "), alpha)
      )
  }
  p
}