#' Scatter plot of the specifications
#'
#' Plots one point per specification and coefficient: an estimate-like column
#' (`xvar`) against the transformed p-values, with significant specifications
#' marked by the shape. The plot is built from the table returned by
#' [spec_table()], so it works with `pima` objects, `pima.multi` objects,
#' lists of fitted models, or a (possibly filtered) specification table.
#' `plot()` on a `pima` object calls this function.
#'
#' @inheritParams spec_curve
#' @param xvar column of the specification table plotted on the x-axis.
#'   Default `"pcor"`, comparable across coefficients.
#' @param yvar column of the specification table plotted on the y-axis. If
#'   `NULL` (default), the p-values transformed with `p.transf`.
#' @param p.transf transformation of the p-values (see [transf_p()]) or a
#'   custom function. Used when `yvar` is `NULL`.
#' @param by colour of the points: `"coefficient"` (one colour per
#'   `response ~ coefficient`) and/or specification columns
#'   (`attr(spec_table(x), "spec_vars")`, e.g. `"fit_spec"`, `"link"` or a
#'   predictor). With more than one, their combinations are used. A single
#'   group is drawn in `"#0072B2"`.
#' @param alpha significance level marked by the shape. If 0 or 1 the shapes
#'   are not labelled.
#' @param xlab,ylab axis labels. Default to `xvar` and to the p-value column
#'   and its transformation (or `yvar`).
#' @param title plot title. If `NULL`, describes the p-values used.
#' @param shapes ggplot2 shapes for non-significant and significant points.
#'   Default `c(4, 19)`.
#' @param facet formula passed to [ggplot2::facet_grid()]. If `NULL`
#'   (default), `response ~ .` with one response and a single panel with
#'   more than one (use `response ~ .` to split them).
#' @param facet.scales `"fixed"`, `"free"`, `"free_x"` (default) or
#'   `"free_y"`.
#' @param which.response responses to plot; defaults to all.
#' @return A ggplot object.
#' @seealso [spec_table()], [spec_curve()], [volcano()]
#' @export
#'
#' @examples
#' mods <- list(
#'   a = lm(Sepal.Width ~ Sepal.Length, data = iris),
#'   b = lm(Sepal.Width ~ Sepal.Length + Species, data = iris),
#'   c = lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris)
#' )
#' spec_scatter(mods, focal = "Sepal.Length", p.transf = "-log10")
#'
#' # colour by a specification choice
#' spec_scatter(mods, focal = "Sepal.Length", by = "Species")
spec_scatter <- function(x,
                         focal = NULL,
                         focal.var = NULL,
                         xvar = "pcor",
                         yvar = NULL,
                         p = NULL,
                         p.adjusted = NULL,
                         p.transf = "z",
                         by = "coefficient",
                         alpha = 0.05,
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL,
                         shapes = NULL,
                         facet = NULL,
                         facet.scales = "free_x",
                         which.response = NULL) {
  tab <- .voe_table(x)
  nspec <- length(unique(tab$model))
  spec_vars <- attr(tab, "spec_vars")

  D <- .voe_select(tab, x, focal = focal, focal.var = focal.var,
                   which.response = which.response, xvar = xvar)
  pv <- .pick_p(D, p = p, p.adjusted = p.adjusted)

  D$p.transf <- transf_p(D[[pv]], method = p.transf)
  D$is_signif <- factor(as.integer(D[[pv]] <= alpha), levels = c(0, 1))

  if (is.null(yvar)) {
    yvar <- "p.transf"
  }
  if (!yvar %in% names(D)) {
    stop(sprintf("`%s` is not a column of the specification table.", yvar),
         call. = FALSE)
  }

  if (is.null(xlab)) {
    xlab <- xvar
  }
  if (is.null(ylab)) {
    ylab <- if (yvar == "p.transf") {
      sprintf("%s (%s)", pv, if (is.function(p.transf)) "custom" else p.transf)
    } else {
      yvar
    }
  }
  if (is.null(title)) {
    title <- .scatter_title(x, pv, nspec)
  }
  if (is.null(shapes)) {
    shapes <- c(4, 19)
  }

  # colour groups
  D$.coefficient_y <- sprintf("%s ~ %s", D$response, D$coefficient)
  bad <- setdiff(by, c("coefficient", spec_vars))
  if (length(bad)) {
    stop(sprintf("`by` must be \"coefficient\" or a specification column (%s): %s.",
                 paste(spec_vars, collapse = ", "), paste(bad, collapse = ", ")),
         call. = FALSE)
  }
  group <- ifelse(by == "coefficient", ".coefficient_y", by)
  D$.group <- if (length(group) > 1) {
    interaction(D[, group], drop = TRUE, sep = " | ")
  } else {
    D[[group]]
  }

  plt <- ggplot2::ggplot(
    D,
    ggplot2::aes(
      x = .data[[xvar]],
      y = .data[[yvar]],
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
    ggplot2::labs(x = xlab, y = ylab, color = paste(by, collapse = " | "))

  # a single colour group: one default colour, replaceable with another
  # colour scale (e.g. to combine several plots)
  if (length(unique(as.character(D$.group))) == 1L) {
    plt <- plt + ggplot2::scale_color_manual(values = .single_color)
  }

  if (!(alpha %in% c(0, 1))) {
    plt <- plt +
      ggplot2::scale_shape_manual(
        values = shapes,
        name = "p-value",
        drop = FALSE,
        labels = paste0(c("p >  ", "p <= "), alpha)
      )
  }

  facet.scales <- match.arg(facet.scales, c("fixed", "free", "free_x", "free_y"))
  if (is.null(facet)) {
    # one response: its name as a strip; several responses: a single panel
    # (the colours already distinguish response ~ coefficient)
    if (length(unique(D$response)) > 1L) {
      return(plt)
    }
    facet <- stats::as.formula("response ~ .")
  }
  plt + ggplot2::facet_grid(facet, scales = facet.scales)
}

# Title describing the p-values used
.scatter_title <- function(x, pv, nspec) {
  if (pv == "p.adj") {
    method <- if (inherits(x, "pima")) sprintf(" (%s)", x$p.adjust.method) else ""
    return(sprintf("Multiverse Analysis (n = %s scenarios) - Adjusted p-values%s",
                   nspec, method))
  }
  if (pv == "p") {
    return("(Raw) p-values")
  }
  if (pv == "wald.p") {
    return(sprintf("Multiverse Analysis (n = %s scenarios) - (Raw) Wald p-values",
                   nspec))
  }
  sprintf("Multiverse Analysis (n = %s scenarios) - p-values: %s", nspec, pv)
}
