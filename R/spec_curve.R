#' Specification Curve Analysis
#'
#' This function performs a specification curve analysis based on the results of a set of regression models. It visualizes the coefficient estimates with confidence intervals, p-values, and highlights significant specifications.
#'
#' @param x an object of class `pima`, usually the result of the `pima()` function.
#' @param yvar character indicating the column of the `x$summary_table` object to be used in the y axes of the top part of the specification curve (usually the estimated parameter). Default to `"Estimate"`.
#' @param p.values character indicating the column of the `x$summary_table` object with the p values. Default to `""p.adj.maxT"` (maxT corrected p values). 
#' @param alpha A numeric value specifying the significance level for the confidence intervals. Default is 0.05.
#' @return A plot displaying the specification curve with confidence intervals and p-values, as well as a legend showing the variable combinations used in each specification. The output object is a [`patchwork`] object thus a collection of `ggplot2` objects. The underlying datasets can be accessed using `@data` for each plot.
#' @param tbr A numeric vector of two elements indicating the proportion of space assigned to the top and bottom part of the plot.
#' @param colors A character vector of two elements with the colors for the point representing non-significant and significant p-values.
#' @param A character vector of two elements with the shapes representing non-significant and significant p-values. Default to simple points.
#' @title A character vector with the title for the overall plot (applied to the top plot internally).
#' @param xlab a character vector for the x axis title. Default to "Specification"
#' @param ylab a character vector for the y axis title of the top plot. Default to `yvar`.
#' @param top.theme a function with a `ggplot2` compatible theme for the top plot. Default to ggplot2::theme_minimal()`
#' @param bottom.theme a function with a `ggplot2` compatible theme for the bottom plot Default to `ggplot2::theme_minimal()`
#' @export
#'
#' @examples
#' # Example usage (assuming `res` is a pre-computed result object):
#' # spec_curve(res, alpha = 0.05)
#' 
spec_curve <- function(x,
                       focal = NULL,
                       yvar = "Estimate",
                       p.values = "p.adj.maxT",
                       alpha = 0.05,
                       tbr = c(0.4, 0.6),
                       colors = NULL,
                       shapes = NULL,
                       title = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       top.theme = NULL,
                       bottom.theme = NULL) {
  
  stopifnot(inherits(x, "pima"))
  
  if(is.null(title)){
    title <- sprintf("Specification Curve (n = %s scenarios)", nrow(x$info))
  }
  
  if(is.null(colors)){
    colors <- c(scales::alpha("black", 0.5), "firebrick")
  }
  
  if(is.null(top.theme)) top.theme <- ggplot2::theme_minimal
  if(is.null(bottom.theme)) bottom.theme <- ggplot2::theme_minimal
  
  yvar <- match.arg(yvar, choices = colnames(x$summary_table), several.ok = FALSE)
  p.values <- match.arg(p.values, choices = colnames(x$summary_table), several.ok = FALSE)
  
  if(is.null(xlab)) xlab <- "Specification"
  if(is.null(ylab)) ylab <- yvar
  
  xs <- attributes(x$info)$xs
  
  if(is.null(focal) & is.null(x$tested_coeffs)){
    stop("when no tested_coeffs are indicated in the pima() call, focal cannot be NULL!")
  }
  
  if(is.null(focal)){
    focal <- x$tested_coeffs
    if(length(focal) > 1){
      # TODO better handling here of tested_coeffs >1 and focal is null
      warning("the number of tested_coeffs is greater than 1, the specification curve could be misleading!")
    }
  }
  
  x$summary_table <- subset(x$summary_table, Coeff == focal)
  spec_data <- .get_spec_curve_data(x, yvar, p.values, alpha)
  
  # remove redundant variables from the plot
  redundant <- apply(x$info[, xs], 2, function(x) length(unique(x)) == 1)
  redundant <- names(redundant)[redundant]
  
  spec_data$dbottom <- subset(spec_data$dbottom, var != redundant)
  
  top <- ggplot2::ggplot(data = spec_data$dtop,
                         ggplot2::aes(x = .id_spec,
                                      y = .data[[yvar]])) +
    ggplot2::geom_point(ggplot2::aes(color = is_signif), show.legend = FALSE) +
    top.theme() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(
      y = ylab
    )
  
  bottom <- ggplot2::ggplot(spec_data$dbottom, 
                            ggplot2::aes(x = .id_spec,
                                         y = var.spec)) +
    ggplot2::facet_grid(var ~ ., scales = "free_y") +
    ggplot2::geom_point() +
    bottom.theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = xlab
    )
  
  patchwork:::`/.ggplot`(top, bottom) +
    patchwork::plot_layout(heights = tbr)
  
}