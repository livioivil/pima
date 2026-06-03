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

.get_summary_table_from_flipscores <- function(object) {
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "score"]), ]
  colnames(tab)[ncol(tab)] = "p"
  tab = cbind(coefficient = rownames(tab), tab)
}

#' as.pima method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param names_obj a vector of names, its length must be equal to the length of \code{object}
#' @param ... additional arguments to be passed
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
#' @param p.transf can be a character vector indicating the transformation to use (see [transf_p()]) or a custom function.
#' @param alpha a value between 0 and 1. The plot will mark the p-values smaller than \code{alpha} (0.05 by default). If equal to 0 or 1 nothing will be marked.
#' @param xlab character vector indicating the x-axis label. Default to `xvar`
#' @param ylab character vector indicating the y-axis label. Default to `p` or `p.adjust.<method>` where method is `object$p.adjust.method`.
#' @param p.adjusted logical indicating whether to plot adjusted p-values (\code{TRUE}, default) or raw p-values (\code{FALSE}).
#' @param regex logical. If \code{TRUE}, the \code{focal} argument is treated as a regular expression to match coefficient names. Default is \code{FALSE}.
#' @param shapes a numeric vector of length 2 specifying the ggplot2 shapes to use for non-significant and significant points, respectively. Default is \code{c(4, 19)}.
#' @param facet.scales character string indicating if scales should be \code{"fixed"}, \code{"free"}, \code{"free_x"}, or \code{"free_y"}. Default is \code{"free_x"}.
#' @param facet a formula for facetting the plot, passed to \code{\link[ggplot2]{facet_grid}}.
#' @param which.response a character vector specifying a subset of response variables to be plotted.
#' @param ... additional arguments to be passed
#' @method  plot pima
#' @docType methods
#' @export

plot.pima <- function(
  x,
  by = "coefficient",
  focal = NULL,
  xvar = NULL,
  p.adjusted = TRUE,
  p.transf = "z",
  alpha = 0.05,
  xlab = NULL,
  ylab = NULL,
  regex = FALSE,
  shapes = NULL,
  facet.scales = NULL,
  facet = NULL,
  which.response = NULL,
  ...
) {
  # focal = NULL
  # xvar = NULL
  # p.adjusted = TRUE
  # p.transf = "z"
  # alpha = 0.05
  # xlab = NULL
  # ylab = NULL
  # regex = FALSE
  # shapes = NULL
  # facet.scales = NULL
  # facet = NULL
  # which.response = NULL
  
  object <- x
  # TODO what about adding a way to transform the p value using custom formula? Also adding critical value (p = 0.05) when using a transformation.

  # avoid conflicting with base plot(x = ) argument

  # if parameters to be plotted > 1 and there is no focal, plot all of them but
  # use the partial correlation.

  nspec <- nrow(object$info)

  if (!is.null(which.response)) {
    object$summary_table <- object$summary_table[
      object$summary_table$response %in% which.response,
      ,
      drop = FALSE
    ]
  }

  is_multi_y <- length(unique(object$summary_table$response)) > 1

  object$summary_table$.coefficient_y <- sprintf(
    "%s ~ %s",
    object$summary_table$response,
    object$summary_table$coefficient
  )

  if (
    is.null(focal) &&
      length(object$tested_coeffs) > 1 &&
      is.null(xvar) &&
      is.null(facet)
  ) {
    xvar <- "pcor"
    warning(
      "the number of tested coefficients is > 1 and no xvar specified. Using pcor as xvar."
    )
  }

  if (is.null(xvar)) {
    xvar <- "pcor"
  }
  # TODO fix this in jointest

  if (object$p.adjust.method == "none" & p.adjusted) {
    stop(
      "the pima() functions as been called without p.values adjustments. Re-run without pima(..., method = 'none')"
    )
  }

  p.values <- if (p.adjusted) "p.adj" else "p"
  D = object$summary_table
  D$.assign = NULL

  if (is.null(focal)) {
    focal <- object$tested_coeffs
  }

  if (!is.null(focal)) {
    if (regex) {
      D <- D[grepl(paste0(focal, collapse = "|"), D$coefficient), , drop = FALSE]
    } else {
      D <- D[D$coefficient %in% focal, , drop = FALSE]
    }
  }

  # check if is one of the available columns
  xvar <- match.arg(xvar, choices = colnames(D), several.ok = FALSE)
  p.values <- match.arg(p.values, choices = colnames(D), several.ok = FALSE)

  # transform the p value
  D$p.transf <- transf_p(D[[p.values]], method = p.transf)

  # RIRR and RP indices

  IRR <- exp(object$summary_table$estimate)
  RIRR <- as.numeric(quantile(IRR, c(0.99)) / quantile(IRR, c(0.01)))

  # Relative pvalues
  RP_raw <- as.numeric(diff(
    -log10(quantile(object$summary_table$p, c(0.99, 0.01)))
  ))

  RP_adjusted <- as.numeric(diff(
    -log10(quantile(object$summary_table$p.adj, c(0.99, 0.01)))
  ))

  if (object$p.adjust.method == "none" || !p.adjusted) {
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
    title = sprintf(
      "Multiverse Analysis (n = %s scenarios) - Adjusted p-values (%s)",
      nspec,
      object$p.adjust.method
    )
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

  D$is_signif <- ifelse(D$is_signif, 1, 0)
  D$is_signif <- factor(D$is_signif, levels = c(0, 1))

  if (is.null(xlab)) {
    xlab <- xvar
  }
  if (is.null(shapes)) {
    shapes <- c(4, 19)
  }

  p.transf.txt <- if (is.function(p.transf)) "custom" else p.transf

  if (is.null(ylab)) {
    ylab <- sprintf("%s (%s)", p.values, p.transf.txt)
  }
  
  group_for_names <- by
  if("coefficient" %in% by) {
    group <- ifelse(by == "coefficient", ".coefficient_y", by)
  } else{
    group_vars <- attr(object$info, "extra")
    group <- match.arg(by, group_vars, several.ok = TRUE)
  }
  
  if (length(group) > 1) {
    D[[".group"]] <- interaction(D[, group], drop = TRUE, sep = " | ")
    group_lab <- paste(group_for_names, collapse = " | ")
  } else {
    D[[".group"]] <- D[[group]]
    group_lab <- group_for_names
  }

  p <- ggplot2::ggplot(
    D,
    ggplot2::aes(
      x = .data[[xvar]],
      y = .data[["p.transf"]],
      group = .data[[".group"]],
      color = .data[[".group"]]
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[["is_signif"]]),
      size = 2,
      show.legend = TRUE
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      color = group_lab
    )

  if (!(alpha %in% c(0, 1))) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = c(4, 19),
        name = "p-value",
        drop = FALSE,
        labels = paste0(c("p >  ", "p <= "), alpha)
      )
  }

  if (!is.null(facet.scales)) {
    facet.scales <- match.arg(
      facet.scales,
      choices = c("fixed", "free", "free_x", "free_y"),
      several.ok = FALSE
    )
  } else {
    facet.scales <- "free_x"
  }

  if (is.null(facet) && !is_multi_y) {
    facet <- stats::as.formula("response ~ .")
  }
  p <- p + ggplot2::facet_grid(facet, scales = facet.scales)
  p
}
