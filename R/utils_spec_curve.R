.get_spec_curve_data <- function(x,
                                 yvar,
                                 p.adjusted = TRUE,
                                 p.values,
                                 include.p.raw = FALSE,
                                 alpha = 0.05){
  spec_data <- merge(x$summary_table, x$info, by = "Model")
  spec_data <- spec_data[order(spec_data[[yvar]]), ]
  spec_data$.id_spec <- 1:nrow(spec_data)
  
  # confidence intervals
  
  zc <- abs(qnorm(alpha/2))
  
  if(p.adjusted) {
    if(x$p.adjust.method == "none"){
      spec_data$p_for_plot <- jointest:::maxT.light(x$Tspace, exp(seq(-8, 0, 0.5)))
    } else{
      spec_data$p_for_plot <- spec_data[[p.values]]
    }
  } else{
    spec_data$p_for_plot <- spec_data$p
  }
  
  spec_data$se.adj <- with(spec_data, abs(Estimate) / abs(qnorm(1 - p_for_plot / 2)))
  spec_data$Estimate.ci.lb <- with(spec_data, Estimate - se.adj * zc)
  spec_data$Estimate.ci.ub <- with(spec_data, Estimate + se.adj * zc)
  spec_data$is_signif <- spec_data$p_for_plot <= alpha
  
  xs <- attributes(x$info)$xs
  
  spec_data_bottom <- reshape(spec_data,
                              varying = xs,
                              direction = "long",
                              times = xs,
                              v.names = "var.spec",
                              timevar = "var")
  
  # when missing add the (removed) string
  spec_data_bottom$var.spec <- ifelse(is.na(spec_data_bottom$var.spec), sprintf("%s (removed)", spec_data_bottom$var), spec_data_bottom$var.spec)
  
  list(
    dtop = spec_data,
    dbottom = spec_data_bottom
  )
}
