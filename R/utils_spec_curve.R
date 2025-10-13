.get_spec_curve_data <- function(x,
                                 yvar,
                                 p.values,
                                 include.p.raw = FALSE,
                                 alpha = 0.05){
  spec_data <- merge(x$summary_table, x$info, by = "Model")
  spec_data <- spec_data[order(spec_data[[yvar]]), ]
  spec_data$.id_spec <- 1:nrow(spec_data)
  
  spec_data$is_signif <- spec_data[[p.values]] <= alpha
  
  xs <- attributes(x$info)$xs
  
  spec_data_bottom <- reshape(spec_data,
                              varying = xs,
                              direction = "long",
                              times = xs,
                              v.names = "var.spec",
                              timevar = "var")
  
  spec_data_bottom <- spec_data_bottom[complete.cases(spec_data_bottom), ]
  
  list(
    dtop = spec_data,
    dbottom = spec_data_bottom
  )
}
