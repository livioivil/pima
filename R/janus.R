#' @export
janus <- function(x, ...) UseMethod("janus")

#' @export
janus.default <- function(x, p, q = c(0.01, 0.99), null = 0, alpha = 0.05, ...) {
  q <- range(q)
  if (q[1L] == q[2L] || q[1L] < 0 || q[2L] > 1)
    stop("`q` must contain two distinct probabilities in [0, 1].", call. = FALSE)
  
  # J_pct: (interpolated) percentiles on opposite sides of the null (Patel et al. 2015)
  xq <- quantile(x, q, na.rm = TRUE, names = FALSE)
  j_pct <- xq[1L] < null && xq[2L] > null
  
  # J_sig: significant estimates in both directions (Klau et al. 2021, 2023)
  s <- !is.na(p) & p <= alpha
  j_sig <- any(x[s] < null, na.rm = TRUE) && any(x[s] > null, na.rm = TRUE)
  
  # J_frac: proportion of estimates greater than the null (Tierney et al. 2021)
  j_frac <- mean(x > null, na.rm = TRUE)
  
  # J_ext: specifications at opposite percentiles, both significant (Vinatier et al. 2024)
  i_lo <- .qpos(x, q[1L])
  i_hi <- .qpos(x, q[2L])
  j_ext <- x[i_lo] < null && x[i_hi] > null &&
    p[i_lo] <= alpha && p[i_hi] <= alpha
  
  # J_range: opposite estimates across the whole multiverse (Vinatier et al. 2025)
  j_range <- any(x < null, na.rm = TRUE) && any(x > null, na.rm = TRUE)
  
  list(J_range = j_range, J_pct = j_pct, J_ext = j_ext, J_sig = j_sig, J_frac = j_frac)
}

#' @export
janus.pima <- function(x, focal = NULL, p.adjusted = TRUE, q = c(0.01, 0.99),
                       alpha = 0.05, ...) {
  pv <- if (p.adjusted) "p.adj" else "p"
  d  <- x$summary_table
  if (is.null(focal)) focal <- x$tested_coeffs
  if (!is.null(focal)) d <- d[d$coefficient %in% focal, , drop = FALSE]
  
  groups <- split(d, list(d$response, d$coefficient), drop = TRUE)
  out <- lapply(groups, function(g) {
    data.frame(
      response = g$response[1L],
      coefficient = g$coefficient[1L],
      janus.default(g$estimate, g[[pv]], q = q, null = 0, alpha = alpha)
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

.qpos <- function(x, q) {
  which.min(abs(x - quantile(x, q, na.rm = TRUE)))
}