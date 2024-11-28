#' Specification Curve Analysis
#'
#' This function performs a specification curve analysis based on the results of a set of regression models. It visualizes the coefficient estimates with confidence intervals, p-values, and highlights significant specifications.
#'
#' @param res A list object with regression model results, including `mods` (a list of models) and `summary_table` (a data frame containing estimates and p-values).
#' @param alpha A numeric value specifying the significance level for the confidence intervals. Default is 0.05.
#' @param p.values A char string indicating which type of p-values to use. Options are `"raw"` (default) or `"adjusted"`. When `"raw"`, the function uses the p-values from the `summary_table`. When `"adjusted"`, p-values are adjusted using the `maxT.light` function.
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
spec_curve <- function(res, alpha = 0.05,p.values=c("raw","adjusted")) {
  # Extract data from res
  data_ori <- res$mods[[1]]$data
  cmb <- names(res$mods)
  n_spec <- length(res$mods)
  num_var_test<-length(res$tested_coeffs)
  if(num_var_test>1){stop("The number of tested coefficients is higher 1")}
  
  # Calculate approximate confidence intervals 
  if(p.values=="raw"){
    z_value <- qnorm(1 - alpha / 2)
    coeff_estimates <- res$summary_table$Estimate
    std_devs<-res$summary_table$Estimate/qnorm(1-res$summary_table$p/2)
    lower_ci <- coeff_estimates - z_value * std_devs
    upper_ci <- coeff_estimates + z_value * std_devs
    p<-res$summary_table$p
  }
  # Calculate approximate confidence intervals
  if(p.values=="adjusted"){
    z_value <- qnorm(1 - alpha / 2)
    res$summary_table$p.adj<- maxT.light(res$Tspace,exp(seq(-8,0,0.5)))
    coeff_estimates <- res$summary_table$Estimate
    std_devs<-res$summary_table$Estimate/qnorm(1-res$summary_table$p.adj/2)
    lower_ci <- coeff_estimates - z_value * std_devs
    upper_ci <- coeff_estimates + z_value * std_devs
    p<-res$summary_table$p
    p.adj<-res$summary_table$p.adj
  }
  # Prepare data for plotting
  if(p.values=="raw"){
    df <- data.frame(
      SpecID = rank(res$summary_table$Estimate),
      Coefficients = coeff_estimates,
      StdDev = std_devs,
      PValue = p,
      LowerCI = lower_ci,
      UpperCI = upper_ci,
      Significance = ifelse(p < alpha, "Yes","No")
    )
  }
    if(p.values=="adjusted"){
      df <- data.frame(
        SpecID = rank(res$summary_table$Estimate),
        Coefficients = coeff_estimates,
        StdDev = std_devs,
        PValue = p,
        LowerCI = lower_ci,
        UpperCI = upper_ci,
        Significance = ifelse(p.adj < alpha, "Yes-adjusted",ifelse(p < alpha, "Yes","No"))
      )
  }
  # Plot the specification curve
  p1 <- ggplot(df, aes(x = SpecID, y = Coefficients)) +
    geom_point(aes(color = Significance), size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Specification Curve Analysis",
      x = "SpecID",
      y = "Coefficient Estimate",
      color = paste("p <", alpha)
    ) +
    theme_minimal()
  
  # Legend of specifications
  combinations_wide<-res$info
  combinations_wide$SpecID<-rank(res$summary_table$Estimate)
  confounders_names<-names(combinations_wide)[(4+num_var_test):(length(combinations_wide)-1)]
  combinations_long<-reshape(combinations_wide,direction="long",idvar = "SpecID", varying = list((4+num_var_test):(length(combinations_wide)-1)),v.names = "Type")
  names(combinations_long)[5+num_var_test]<-"Variable"
  combinations_long$Type[combinations_long$Type=="FALSE"]<-"false("
  combinations_long$Type[combinations_long$Type=="TRUE"]<-"true("
  combinations_long$Type[!grepl("([()])",combinations_long$Type)]<-"original"
  combinations_long$Type[grepl("([()])",combinations_long$Type)]<-sub("\\(.*", "", combinations_long$Type)[grepl("([()])",combinations_long$Type)]
  combinations_long$Variable<-as.factor(combinations_long$Variable)
  levels(combinations_long$Variable)<-confounders_names
  p2 <- ggplot(combinations_long, aes(x = Variable, y = SpecID, col = Type)) +
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
