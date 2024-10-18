#' Specification Curve Analysis
#'
#' This function performs a specification curve analysis based on the results of a set of regression models. It visualizes the coefficient estimates with confidence intervals, p-values, and highlights significant specifications.
#'
#' @param res A list object with regression model results, including `mods` (a list of models) and `summary_table` (a data frame containing estimates and p-values).
#' @param alpha A numeric value specifying the significance level for the confidence intervals. Default is 0.05.
#'
#' @return A plot displaying the specification curve with confidence intervals and p-values, as well as a legend showing the variable combinations used in each specification.
#'
#' @import ggplot2
#' @import dplyr
#' @import cowplot
#' @export
#'
#' @examples
#' # Example usage (assuming `res` is a pre-computed result object):
#' # spec_curve(res, alpha = 0.05)
#' 
spec_curve <- function(res, alpha = 0.05) {
  # Extract data from res
  data_ori <- res$mods[[1]]$data
  cmb <- names(res$mods)
  n_spec <- length(res$mods)
  spec <- list()
  
  for (i in 1:n_spec) {
    res$summary_table$std_glm[i] <- summary(glm(res$mods[[i]]$formula, data = data_ori, family = res$mods[[i]]$family$family))$coef[2, 2]
    spec[[i]] <- res$mods[[i]]$formula
  }
  
  # Calculate confidence intervals
  z_value <- qnorm(1 - alpha / 2)
  coeff_estimates <- res$summary_table$Estimate
  std_devs <- res$summary_table$std_glm
  lower_ci <- coeff_estimates - z_value * std_devs
  upper_ci <- coeff_estimates + z_value * std_devs
  t <- coeff_estimates / std_devs
  p <- 2 * (1 - pnorm(abs(t)))
  
  # Prepare data for plotting
  df <- data.frame(
    SpecID = rank(res$summary_table$Estimate),
    Coefficients = coeff_estimates,
    StdDev = std_devs,
    PValue = p,
    LowerCI = lower_ci,
    UpperCI = upper_ci,
    Significance = ifelse(p < 0.05, "Yes", "No")
  )
  
  # Plot the specification curve
  p1 <- ggplot(df, aes(x = SpecID, y = Coefficients)) +
    geom_point(aes(color = Significance), size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("Yes" = "blue", "No" = "grey")) +
    labs(
      title = "Specification Curve Analysis",
      x = "SpecID",
      y = "Coefficient Estimate",
      color = "p < .05"
    ) +
    theme_minimal()
  
  # Legend of specifications
  nvar <- sapply(spec, function(x) length(gregexpr("\\+", as.character(x))) + 1)
  combinations_num_long <- data.frame(Variable = as.vector(sapply(cmb, function(x) unlist(strsplit(x, split = "+", fixed = TRUE)))),
                                      SpecID = rep(rank(res$summary_table$Estimate), times = nvar))
  combinations_num_long$Type <- NULL
  combinations_num_long$Type[is.na(sapply(combinations_num_long$Variable, function(x) strsplit(x, "[(),]")[[1]][2]))] <- "original"
  combinations_num_long$Type[is.na(combinations_num_long$Type)] <- sapply(combinations_num_long$Variable[is.na(combinations_num_long$Type)], function(x) strsplit(x, "[(),]")[[1]][1])
  combinations_num_long$Variable[combinations_num_long$Type != "original"] <- sapply(combinations_num_long$Variable, function(x) strsplit(x, "[(),]")[[1]][2])[combinations_num_long$Type != "original"]
  combinations_num_long$Variable <- factor(combinations_num_long$Variable)
  
  p2 <- ggplot(combinations_num_long, aes(x = Variable, y = SpecID, col = Type)) +
    geom_point(aes(color = Type), shape = 1, size = 4, position = position_dodge(width = 0.4)) +
    theme_bw() + coord_flip()
  
  legend <- cowplot::get_plot_component(p1, "guide-box", return_all = TRUE)[[1]]
  legend1 <- cowplot::get_plot_component(p2, "guide-box", return_all = TRUE)[[1]]
  combineLegend <- cowplot::plot_grid(legend, legend1, nrow = 2)
  
  # Combine plots into a grid
  prow <- plot_grid(p1 + theme(legend.position = "none"),
                    p2 + theme(legend.position = "none"),
                    align = 'v',
                    labels = c("A", "B"),
                    hjust = -1,
                    nrow = 2)
  p <- plot_grid(prow, combineLegend, rel_widths = c(3, 0.3))
  
  return(p)
}
