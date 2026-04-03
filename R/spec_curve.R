#' Specification Curve Analysis
#'
#' This function performs a specification curve analysis based on the results of a set of regression models.
#' It visualizes the coefficient estimates with confidence intervals, p-values, and highlights
#' significant specifications.
#'
#' @param x An object of class `pima`, usually the result of the `pima()` function.
#' @param focal A character vector of focal coefficients to filter. If `NULL`, defaults to all tested coefficients in `x`.
#' @param yvar Character indicating the column of the `x$summary_table` object to be used in the y-axis of the top plot (usually the estimated parameter). Defaults to `"estimate"`.
#' @param yname Character indicating the name of the response variable to be plotted if the `pima` object contains more than one variable. If `NULL` (the default), all response variables are plotted.
#' @param p.adjusted Logical indicating whether to use adjusted p-values for determining significance. Defaults to `TRUE` if an adjustment method was specified in the `pima` object.
#' @param alpha A numeric value specifying the significance level for the confidence intervals and color-coding. Default is 0.05.
#' @param tbr A numeric vector of two elements indicating the vertical space ratio assigned to the top and bottom plots (e.g., `c(0.4, 0.6)`).
#' @param colors A character vector of two elements specifying the colors for non-significant and significant results.
#' @param shapes A numeric vector of two elements specifying the shapes for non-significant and significant results. Default to `c(4, 19)`.
#' @param title A character string for the overall plot title.
#' @param xlab A character string for the x-axis title. Default to "Specification".
#' @param ylab A character string for the y-axis title of the top plot. Default to the value of `yvar`.
#' @param top.theme A function returning a `ggplot2` theme for the top plot. Default to `ggplot2::theme_minimal()`.
#' @param bottom.theme A function returning a `ggplot2` theme for the bottom plot. Default to `ggplot2::theme_minimal()`.
#' @param redundant Logical. If `TRUE`, removes variables that do not vary across specifications from the bottom plot.
#' @param conf.int Logical. If `TRUE`, includes confidence intervals around estimated coefficients. (Ignored if `yvar` is not `"estimate"`).
#' @param facet.y Logical. If `TRUE`, creates separate facets for each response variable in the top plot. Default is `FALSE`.
#' @param which.response A character vector specifying which response variables to include in the plot.
#'
#' @return A [`patchwork`] object consisting of two aligned `ggplot2` plots. The top plot shows estimates/p-values, and the bottom plot shows the specification grid.
#'
#' @export
#'
#' @examples
#' # Example usage (assuming `res` is a pre-computed pima object):
#' # spec_curve(res, alpha = 0.05, conf.int = TRUE)
#'
spec_curve <- function(
  x,
  focal = NULL,
  yvar = NULL,
  yname = NULL,
  p.adjusted = NULL,
  alpha = 0.05,
  tbr = c(0.4, 0.6),
  colors = NULL,
  shapes = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  top.theme = NULL,
  bottom.theme = NULL,
  redundant = TRUE,
  conf.int = FALSE,
  facet.y = FALSE,
  which.response = NULL
) {
  # # default parameters for debugging
  #
  # focal = NULL
  # yvar = NULL
  # p.adjusted = NULL
  # alpha = 0.05
  # tbr = c(0.4, 0.6)
  # colors = NULL
  # shapes = NULL
  # title = NULL
  # xlab = NULL
  # ylab = NULL
  # top.theme = NULL
  # bottom.theme = NULL
  # redundant = TRUE
  # conf.int = FALSE

  stopifnot(inherits(x, "pima"))

  nspec <- nrow(x$info)

  if (is.null(title)) {
    title <- sprintf("Specification Curve (n = %s scenarios)", nspec)
  }

  if (is.null(top.theme)) {
    top.theme <- ggplot2::theme_minimal
  }
  if (is.null(bottom.theme)) {
    bottom.theme <- ggplot2::theme_minimal
  }
  if (is.null(focal)) {
    focal <- x$tested_coeffs
  }
  if (is.null(p.adjusted)) {
    p.adjusted <- x$p.adjust.method != "none"
  }

  if (length(focal) > 1 & is.null(yvar)) {
    yvar <- "pcor"
    warning(
      "the number of tested coefficients is > 1 and no yvar specified. Using 'pcor' as yvar."
    )
  }

  if (is.null(yvar)) {
    yvar <- "estimate"
  }
  yvar <- match.arg(
    yvar,
    choices = colnames(x$summary_table),
    several.ok = FALSE
  )

  p.values <- if (p.adjusted) "p.adj" else "p"
  p.values <- match.arg(
    p.values,
    choices = colnames(x$summary_table),
    several.ok = FALSE
  )

  if (is.null(xlab)) {
    xlab <- "Specification"
  }
  if (is.null(ylab)) {
    ylab <- yvar
  }

  xs <- attributes(x$info)$xs

  if (!is.null(focal)) {
    x$summary_table <- subset(x$summary_table, coefficient %in% focal)
  }

  if (!is.null(which.response)) {
    x$summary_table <- subset(x$summary_table, response %in% which.response)
  }

  spec_data <- .get_spec_curve_data(x, yvar, p.adjusted, p.values, alpha)

  spec_data$dbottom$var_txt <- ifelse(
    spec_data$dbottom$var.spec %in% focal,
    sprintf('atop(bold("%s"), "(focal)")', spec_data$dbottom$var),
    sprintf('atop(bold("%s"), "")', spec_data$dbottom$var)
  )

  if (!redundant) {
    # remove redundant variables from the plot
    redundant <- apply(x$info[, xs], 2, function(x) length(unique(x)) == 1)
    redundant <- names(redundant)[redundant]
    spec_data$dbottom <- subset(spec_data$dbottom, var != redundant)
  }

  # plotting elements

  if (is.null(shapes)) {
    shapes <- c(4, 19)
  }

  # check which response variables we have to plot

  if (!is.null(yname)) {
    spec_data$dtop <- subset(spec_data$dtop, response %in% yname)
  }

  is_multi_y <- length(unique(x$summary_table$response)) > 1

  if (facet.y) {
    spec_data$dtop$.coefficient_y <- sprintf("~ %s", spec_data$dtop$coefficient)
  } else {
    spec_data$dtop$.coefficient_y <- sprintf(
      "%s ~ %s",
      spec_data$dtop$response,
      spec_data$dtop$coefficient
    )
  }

  top <- ggplot2::ggplot(
    data = spec_data$dtop,
    ggplot2::aes(x = .id_spec, y = .data[[yvar]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .coefficient_y, shape = is_signif),
      show.legend = TRUE
    ) +
    top.theme() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_shape_manual(
      values = shapes,
      guide = "none",
      name = "p-value",
      labels = paste0(c("p >  ", "p <= "), alpha)
    ) +
    ggplot2::labs(
      y = ylab
    )

  if (!is.null(colors)) {
    top <- top + ggplot2::scale_color_manual(values = colors)
  }

  if (yvar != "estimate" & conf.int) {
    warning("confidence interval can be calculated only for the coefficients!")
  }

  if (yvar == "estimate" & conf.int) {
    top <- top +
      ggplot2::geom_segment(ggplot2::aes(
        x = .id_spec,
        xend = .id_spec,
        y = est.ci.lb,
        yend = est.ci.ub
      ))
  }

  if (facet.y) {
    top <- top +
      ggplot2::facet_grid(response ~ .)
  }

  bottom <- ggplot2::ggplot(
    spec_data$dbottom,
    ggplot2::aes(x = .id_spec, y = var.spec)
  ) +
    ggplot2::facet_grid(
      var_txt ~ .,
      scales = "free_y",
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = is_signif), show.legend = TRUE) +
    bottom.theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = xlab
    ) +
    ggplot2::scale_shape_manual(
      values = shapes,
      name = "p-value",
      drop = FALSE,
      labels = paste0(c("p >  ", "p <= "), alpha)
    )

  patchwork:::`/.ggplot`(top, bottom) +
    patchwork::plot_layout(heights = tbr)
}
