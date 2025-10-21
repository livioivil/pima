.get_spec_curve_data <- function(x,
                                 yvar,
                                 p.adjusted = TRUE,
                                 p.values,
                                 include.p.raw = FALSE,
                                 alpha = 0.05){
  spec_data <- merge(x$summary_table, x$info, by = "model")
  spec_data <- spec_data[order(spec_data[[yvar]]), ]
  spec_data$.id_spec <- 1:nrow(spec_data)

  spec_data <- .get_conf_int(x, spec_data, p.values, alpha, p.adjusted)
  
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

.get_conf_int <- function(x, data, p.values, alpha = 0.05, p.adjusted = TRUE) {
  # critical value
  zc <- abs(qnorm(alpha/2))
  
  if(p.adjusted) {
    if(x$p.adjust.method == "none"){
      data$p_for_plot <- jointest:::maxT.light(x$Tspace, exp(seq(-8, 0, 0.5)))
    } else{
      data$p_for_plot <- data[[p.values]]
    }
  } else{
    data$p_for_plot <- data$p
  }
  
  data$se.adj <- with(data, abs(estimate) / abs(qnorm(1 - p_for_plot / 2)))
  data$est.ci.lb <- with(data, estimate - se.adj * zc)
  data$est.ci.ub <- with(data, estimate + se.adj * zc)
  data$is_signif <- data$p_for_plot <= alpha
  data$is_signif <- factor(ifelse(data$is_signif, 1, 0), levels = c(0, 1))
  data
}