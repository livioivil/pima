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
print.pima <- function (object, n = 4, ...) {
  # TODO check if print is useful like this and check the usage of n (not working here)
  n <- nrow(object$info)
  msg <- sprintf("Multiverse analysis with %s models", n)
  cat(msg)
  cat("\n\n")
  .trim(object$info, n = n)
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
  # do.call(rbind,lapply(object, function(ob) ob$summary_table))
}

.get_summary_table_from_flipscores <- function(object) {
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "Score"]), ]
  colnames(tab)[ncol(tab)] = "p"
  tab = cbind(Coeff = rownames(tab), tab)
}


is_signif = NULL
###########################
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
#############################################
#' plot.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param p.values  use "raw" or "adjusted" (i.e. corrected for selective inference)
#' @param y `"-log10(p)"` by default. It can be any column's name of the `summary()` of a `pima-object` (e.g. `"z value"`) or any other expression e.g. `"-log10(p)"`.
#' @param x `"Estimate"` by default. It can be any column's name of the `summary()` of a `pima-object` (e.g. `"Part. Cor"`) or any other expression e.g. `"-log10(p)"`.
#' @param alpha a value between 0 and 1. The plot will mark the p-values smaller than \code{alpha} (0.05 by default). If equal to 0 or 1 nothing will be marked.
#' @param ... additional arguments to be passed
#' @method  plot pima
#' @docType methods
#' @export
#' @import ggplot2

plot.pima <-
  function(
    object,
    p.values = c("adjusted", "raw"),
    alpha = .05,
    y = "-log10(p)",
    x = "Estimate"
  ) {
    p.values = p.values[1]
    group = "Coeff"
    D = object$summary_table
    D$.assign = NULL
    colnames(D) = gsub(" ", "", colnames(D))
    x = x[1] #match.arg(x[1],c("Estimate","Part. Cor"))
    y = y[1] #match.arg(y[1], c("-log10(p)","z value"))
    x = gsub(" ", "", x)
    y = gsub(" ", "", y)
    #
    if (p.values == "raw") {
      D$is_signif = (D$p <= alpha)
    } else {
      #adj
      adj_id = grep("p.adj", colnames(D))[1]
      D$is_signif = (D[, adj_id] <= alpha)
    }

    if (p.values == "raw") {
      title = "(Raw) p-values"
    } else {
      title = "Adjusted p-values"
    }

    p <- ggplot(D, aes_string(y = y, x = x, group = group, color = group)) +
      geom_point(aes(shape = is_signif), size = 2) + #is_signif))+
      ggtitle(title) +
      theme_minimal()
    if (!(alpha %in% c(0, 1))) {
      p <- p +
        scale_shape_manual(
          values = c(3, 19),
          name = paste(p.values, "p-value"),
          labels = paste0(c("p >  ", "p <= "), alpha)
        )
    }
    #RIRR and RP indices
    IRR <- exp(object$summary_table$Estimate)
    RIRR <- as.numeric(quantile(IRR, c(0.99)) / quantile(IRR, c(0.01)))
    #Relative pvalues
    RP_raw <- as.numeric(diff(
      -log10(quantile(object$summary_table$p, c(0.99, 0.01)))
    ))
    RP_adjusted <- as.numeric(diff(
      -log10(quantile(object$summary_table$p.adj, c(0.99, 0.01)))
    ))
    if (p.values == "raw") {
      indices <- paste(
        "RIRR=",
        round(RIRR, 3),
        "\nRP=",
        round(RP_raw, 3),
        sep = ""
      )
    }
    if (p.values == "adjusted") {
      indices <- paste(
        "RIRR=",
        round(RIRR, 3),
        "\nRP=",
        round(RP_adjusted, 3),
        sep = ""
      )
    }
    p
  }