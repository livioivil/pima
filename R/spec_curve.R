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
                       yvar = NULL,
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
                       conf.int = FALSE) {
  
  stopifnot(inherits(x, "pima"))
  
  if(is.null(title)){
    title <- sprintf("Specification Curve (n = %s scenarios)", nrow(x$info))
  }
  
  if(is.null(colors)){
    colors <- c(scales::alpha("black", 0.5), "firebrick")
  }
  
  if(is.null(top.theme)) top.theme <- ggplot2::theme_minimal
  if(is.null(bottom.theme)) bottom.theme <- ggplot2::theme_minimal
  if(is.null(focal)) focal <- x$tested_coeffs
  if(is.null(p.adjusted)) p.adjusted <- x$p.adjust.method != "none"
  
  if(length(focal) > 1 & is.null(yvar)){
    yvar <- "Part. Cor"
    warning("the number of tested coefficients is > 1 and no yvar specified. Using 'Part. Cor' as yvar.")
  }
  
  yvar <- match.arg(yvar, choices = colnames(x$summary_table), several.ok = FALSE)
  
  if(p.adjusted){
    p.values <- sprintf("p.adj.%s", x$p.adjust.method)
  } else{
    p.values <- "p"
  }
  
  p.values <- match.arg(p.values, choices = colnames(x$summary_table), several.ok = FALSE)
  
  if(is.null(xlab)) xlab <- "Specification"
  if(is.null(ylab)) ylab <- yvar
  
  xs <- attributes(x$info)$xs
  
  if(!is.null(focal)){
    x$summary_table <- subset(x$summary_table, Coeff %in% focal)
  }

  spec_data <- .get_spec_curve_data(x, yvar, p.adjusted, p.values, alpha)
  
  spec_data$dbottom$var_txt <- ifelse(
    spec_data$dbottom$var %in% focal,
    sprintf('atop(bold("%s"), "(focal)")', spec_data$dbottom$var),
    sprintf('atop(bold("%s"), "")', spec_data$dbottom$var)
  )
  
  if(!redundant){
    # remove redundant variables from the plot
    redundant <- apply(x$info[, xs], 2, function(x) length(unique(x)) == 1)
    redundant <- names(redundant)[redundant]
    spec_data$dbottom <- subset(spec_data$dbottom, var != redundant)
  }
  
  # plotting elements
  
  if(is.null(shapes)) shapes <- c(4, 19)
  
  top <- ggplot2::ggplot(data = spec_data$dtop,
                         ggplot2::aes(x = .id_spec,
                                      y = .data[[yvar]])) +
    #ggplot2::geom_point(ggplot2::aes(color = is_signif), show.legend = FALSE) +
    ggplot2::geom_point(ggplot2::aes(color = Coeff, shape = is_signif), show.legend = TRUE) +
    top.theme() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_shape_manual(
      values = shapes,
      guide = "none",
      # name = "p-value",
      # labels = paste0(c("p >  ", "p <= "), alpha)
    ) +
    ggplot2::labs(
      y = ylab
    )
  
  if(yvar != "Estimate" & conf.int) {
    warning("confidence interval can be calculated only for the coefficients!")
  }
  
  if(yvar == "Estimate" & conf.int){
    top <- top +
      ggplot2::geom_segment(ggplot2::aes(x = .id_spec,
                                         xend = .id_spec,
                                         y = Estimate.ci.lb,
                                         yend = Estimate.ci.ub))
  }
  
  bottom <- ggplot2::ggplot(spec_data$dbottom, 
                            ggplot2::aes(x = .id_spec,
                                         y = var.spec)) +
    ggplot2::facet_grid(var_txt ~ ., 
                        scales = "free_y",
                        labeller = ggplot2::label_parsed) +
    ggplot2::geom_point(ggplot2::aes(shape = is_signif)) +
    bottom.theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = xlab
    ) +
    ggplot2::scale_shape_manual(
      values = shapes,
      guide = "none",
      # name = "p-value",
      # labels = paste0(c("p >  ", "p <= "), alpha)
    )
  
  patchwork:::`/.ggplot`(top, bottom) +
    patchwork::plot_layout(heights = tbr)
}