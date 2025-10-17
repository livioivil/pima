#' Transforming p-values
#'
#' @param p a numeric vector with p-values
#' @param method the transformation method. Can be one of "raw" (no transformation),
#' "-log10" or "z". Can also be a custom function.
#' @returns the transformed p-values
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