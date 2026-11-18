#' Transforming p-values
#'
#' @param p a numeric vector with p-values
#' @param method the transformation method. Can be one of "raw" (no transformation),
#' "-log10" or "z". Can also be a custom function.
#' @details
#' `"-log10"` returns \eqn{-\log_{10} p}{-log10(p)}. `"z"` returns the
#' absolute normal quantile corresponding to a two-sided p-value,
#' \deqn{z = \Phi^{-1}\left(1 - \frac{p}{2}\right),}{z = qnorm(1 - p/2),}
#' where \eqn{\Phi} is the standard normal distribution function.
#' @returns the transformed p-values
#' @family specification tables and plots
#' @export
#'
#' @examples
#' p <- c(0.01, 0.05, 0.8)
#' transf_p(p, method = "raw")
#' transf_p(p, method = "z")
#' # with custom function
#' transf_p(p, method = function(x) x/2)

transf_p <- function(p, method = "raw") {
  stopifnot(is.numeric(p), all(p >= 0 & p <= 1, na.rm = TRUE))
  
  if (is.function(method)) {
    return(method(p))
  }
  
  method <- match.arg(method, c("raw", "-log10", "z"))
  
  switch(
    method,
    "raw" = p,
    "-log10" = -log10(p),
    "z" = qnorm(p / 2, lower.tail = FALSE)
  )
}