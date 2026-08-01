#' @export
volcano <- function(x, 
                    focal = NULL,
                    p.adjusted = TRUE,
                    p.cut = c(0.05, 0.001),
                    q = c(0.01, 0.05, 0.99),
                    type = c("additive", "ratio"),
                    null = NULL){
  
  data <- x$summary_table
  plt <- plot.pima(x, 
            p.transf = "-log10", 
            p.adjusted = p.adjusted,
            focal = focal) +
    ggplot2::theme(plot.title = ggplot2::element_blank())
  
  yvar <- if(p.adjusted) "p.adj" else "p"
  xvar <- plt@plot_env$xvar
  alpha <- plt@plot_env$alpha
  
  VOE <- voe(x, xvar, p.adjusted, q, type, null, alpha)

  plt + 
    ggplot2::geom_vline(xintercept = VOE$x, lty = "dotted") +
    ggplot2::geom_hline(yintercept = VOE$p, lty = "dotted") +
    ggplot2::geom_hline(yintercept = transf_p(p.cut, "-log10"), col = "darkgreen") +
    ggplot2::annotate(
      "label",
      x = .rpos(data[[xvar]], 0.1),
      y = transf_p(p.cut, "-log10"),
      label = sprintf("p = %s", p.cut)
    ) +
    ggplot2::annotate(
      "label",
      x = VOE$x,
      y = .rpos(data[[yvar]], -0.1),
      label = sprintf("%s%%", round(q * 100))
    ) +
    ggplot2::annotate(
      "label",
      x = .rpos(data[[xvar]], 1.05),
      y = VOE$p,
      label = sprintf("%s%%", round(q * 100))
    ) +
    ggplot2::geom_vline(xintercept = VOE$null)
}

#' @export
voe <- function(
    x,
    xvar = "pcor",
    p.adjusted = TRUE,
    q = c(0.01, 0.50, 0.99),
    type = c("additive", "ratio"),
    null = NULL,
    alpha = 0.05,
    decompose_variance = FALSE
) {
  type <- match.arg(type)
  
  data <- x$summary_table
  yvar <- if (p.adjusted) "p.adj" else "p"
  
  q <- sort(q)
  
  xvar_q <- quantile(data[[xvar]], q, na.rm = TRUE)
  yvar_q <- quantile(
    transf_p(data[[yvar]], "-log10"),
    q,
    na.rm = TRUE
  )
  
  if (is.null(null)) {
    null <- if (type == "ratio") 1 else 0
  }
  
  x_voe <- if (type == "additive") {
    xvar_q[length(xvar_q)] - xvar_q[1L]
  } else {
    xvar_q[length(xvar_q)] / xvar_q[1L]
  }
  
  p_voe <- yvar_q[length(yvar_q)] - yvar_q[1L]
  
  xsign <- data[[xvar]][data[[yvar]] <= alpha]
  
  janus <- any(xsign < null, na.rm = TRUE) &&
    any(xsign > null, na.rm = TRUE)
  
  out <- list(
    xvar_q,
    yvar_q,
    xn = xvar,
    x_voe = unname(x_voe),
    p_voe = unname(p_voe),
    janus = janus,
    null = null,
    type = type,
    p.transf = "-log10",
    p.adjusted = p.adjusted,
    alpha = alpha
  )
  
  names(out)[1:2] <- c("x", "p")
  
  if(decompose_variance){
    voe_var <- voe_variance(x, xvar, type)
    out <- c(out, voe_var)
  }
  
  out
}

#' @export
voe_variance <- function(
    x,
    estimate = "pcor",
    type = c("additive", "ratio")
) {
  type <- match.arg(type)
  
  xs <- attributes(x$info)$xs
  xs <- xs[!grepl("Intercept", xs)]
  
  data <- .get_spec_curve_data(
    x,
    yvar = estimate,
    p.values = "p.adj"
  )$dtop
  
  xs <- xs[
    vapply(
      data[xs],
      function(z) length(unique(z[!is.na(z)])) > 1L,
      logical(1)
    )
  ]
  
  data <- data[, unique(c(estimate, xs)), drop = FALSE]
  
  data[[estimate]] <- if (type == "ratio") {
    log(data[[estimate]])
  } else {
    data[[estimate]]
  }
  
  full_fit <- stats::lm(
    stats::reformulate(xs, response = estimate),
    data = data
  )
  
  r2_full <- summary(full_fit)$r.squared
  
  unique_r2 <- vapply(xs, function(term) {
    reduced_fit <- stats::lm(
      stats::reformulate(
        setdiff(xs, term),
        response = estimate
      ),
      data = data
    )
    
    r2_full - summary(reduced_fit)$r.squared
  }, numeric(1))
  
  decomposition <- data.frame(
    term = xs,
    r2u = unname(unique_r2),
    pr2u = unname(unique_r2 / r2_full),
    row.names = NULL
  )
  
  list(
    r2 = unname(r2_full),
    r2.adj = unname(summary(full_fit)$adj.r.squared),
    dec = decomposition
  )
}