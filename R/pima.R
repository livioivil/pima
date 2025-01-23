#' Post-selection Inference in Multiverse Analysis
#' @description Applies the PIMA procedure to make selective inference on one or more parameters within a multiverse of models,
#' derived from different specifications of the data (e.g., pre-processing) and of the model itself (GLMs).
#' The method jointly computes resampling-based test statistics for all coefficients of interest in all models.
#' The output allows for multiple testing in multiverse analysis, providing weak and strong control of the Family-Wise Error Rate (FWER)
#' and confidence bounds for True Discovery Proportions (TDPs).
#' @usage pima(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), ...) 
#' @param mods list of objects that can be evaluated by \code{\link[flipscores]{flipscores}}, usually
#' model objects produced by \code{glm} or \code{flipscores}.
#' @param tested_coeffs tested coefficients. It can be a vector of names (common to all models)
#' or a list of the same length as \code{mods}, where each element is a vector of names.
#' If \code{NULL}, all coefficients are tested.
#' @param n_flips number of sign flips.
#' @param method correction method among \code{maxT}, \code{minP} and \code{none}. Can be abbreviated.
#' @param ... further parameters of \code{\link[join_flipscores]{join_flipscores}}.
#' @details The procedure builds on the sign-flip score test implemented in the function \code{flipscores} of the \strong{flipscores} package.
#' For each tested coefficient in each model, it computes a set of resampling-based test statistics and a raw p-value
#' for the null hypothesis that the coefficient is null against a two-sided alternative.
#' If \code{method} is \code{maxT} (default) or \code{minP}, multiplicity-adjusted p-values are added
#' using the step-down method of Westfall and Young (1993). Adjusted p-values provide control of the FWER
#' asymptotically in the sample size and, for practical purposes, almost exact control for any sample size.
#' @details Post-selection inference is achieved by merging information from the considered models:
#' \itemize{
#' \item the output summary shows adjusted p-values for strong FWER control:
#' "Which effects in which models are significant?"
#' \item \code{global_tests} produces a global p-value for weak FWER control,
#' possibly by coefficient or by model: "Is there at least one significant effect?"
#' \item \code{pimaAnalysis} (\strong{sumSome}) computes a lower confidence bound for the TDP, among all tested effects or within a subset:
#' "How many of the considered effects are significant?"
#' }
#' @details Further parameters include:
#' \itemize{
#' \item \code{score_type}: type of score that is computed (see \code{\link[flipscores]{flipscores}} for more datails).
#' The almost exact control of the error is provided for the default setting \code{score_type = "standardized"}.
#' \item \code{statistics}: test statistics computed by the procedure. Currently, \code{t} is the only implemented method.
#' Different statistics will affect the multivariate inference, but not the univariate.
#' \item \code{seed}: can be specified to ensure replicability of results.
#' \item \code{output_models}: \code{TRUE} to return the list of model objects produced by \code{flipscores}.
#' }
#' @return Returns an object of class \code{pima}, containing:
#' \itemize{
#' \item \code{Tspace}: data frame of test statistics, where columns correspond to tests, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item \code{summary_table}: data frame containing a summary for each tested coefficient in each model:
#' estimate, score, standard error, z value, partial correlation, raw p-value, adjusted p-value.
#' \item \code{mods}: list of model objects computed by \code{flipscores}.
#' \item \code{tested_coeffs}: tested coefficients, as in input.
#' }
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
#' 
#' Hemerik, Goeman, Finos (2020). Robust testing in generalized linear models by sign flipping score contributions. Journal of the Royal Statistical Society, Series B (Statistical Methodology), doi: 10.1111/rssb.12369.
#' 
#' De Santis, Goeman, Hemerik, Davenport, Finos (2024). Inference in generalized linear models with robustness to misspecified variances. Pre-print, doi: 10.48550/arXiv.2209.13918.
#' 
#' @examples
#' # Generate data
#' set.seed(123)
#' n <- 20
#' D <- data.frame(X1 = rnorm(n), X2 = rnorm(n)^2, Z1 = rnorm(n), Z2 = rnorm(n))
#' D$Y <- D$X1 + D$Z1 + rnorm(n)
#' 
#' # Multiverse of 4 models (list)
#' mod1 <- glm(Y ~ X1 + X2 + Z1 + Z2, data = D)
#' mod2 <- glm(Y ~ X1 + X2 + poly(Z1,2) + Z2, data = D)
#' mod3 <- glm(Y ~ X1 + X2 + Z1 + poly(Z2,2), data = D)
#' mod4 <- glm(Y ~ X1 + X2 + poly(Z1,2) + poly(Z2,2), data = D)
#' mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
#' 
#' # Test the coefficients of X1 and X2 (raw and adjusted p-values)
#' res <- pima(mods, tested_coeffs = c("X1","X2"))
#' summary(res)
#' 
#' # Global p-values: overall, by model and by coefficient
#' summary(global_tests(res))
#' summary(global_tests(res, by = "Model"))
#' summary(global_tests(res, by = "Coeff"))
#' 
#' # Lower 95%-confidence bound for the TDP by coefficient
#' # require(sumSome)
#' # pimaAnalysis(obj, by = "Coeff", alpha = 0.05)
#' @export

pima <- function(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), ...) {
  
  method <- match.arg(method, c("maxT", "minP", "none"))
  
  extra_args <- list(...)
  
  out <- do.call(join_flipscores, c(
    list(mods=mods, tested_coeffs=tested_coeffs, n_flips=n_flips), 
    extra_args
  ))
  
  out$info <- .get_info_models(out$mods)
  out$tested_coeffs <- tested_coeffs
  class(out) <- unique(c("pima", class(out)))
  
  if(method != "none"){
    out <- jointest::p.adjust(out, method = method, tail = 0)
  }
  
  return(out)
}
