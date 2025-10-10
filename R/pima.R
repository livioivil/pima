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
#' The default \code{"standardized"} provides almost exact control of the error for any sample size.
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
#' De Santis, Goeman, Hemerik, Davenport, Finos (2025). Inference in generalized linear models with robustness to misspecified variances. Journal of the American Statistical Association, doi: 10.1080/01621459.2025.2491775.
#' 
#' @examples
#' # Generate data
#' set.seed(123)
#' n <- 30
#' D <- data.frame(X1 = rnorm(n), X2 = rnorm(n)^2,
#'                 X3 = sample(c("A","B","C"), n, replace = TRUE),
#'                 Z1 = rnorm(n), Z2 = rnorm(n))
#' D$X3 <- factor(D$X3)
#' D$Y <- D$X1 + ifelse(D$X3 == "C", 3, 0) + D$Z1 + rnorm(n)
#' 
#' # Multiverse of 4 models (list)
#' mod1 <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2, data = D)
#' mod2 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + Z2, data = D)
#' mod3 <- glm(Y ~ X1 + X2 + X3 + Z1 + poly(Z2,2), data = D)
#' mod4 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + poly(Z2,2), data = D)
#' mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
#' 
#' # Test selected coefficients (raw and adjusted p-values)
#' res <- pima(mods, tested_coeffs = c("X1","X2","X3B","X3C"))
#' summary(res)
#' 
#' # Global p-values: overall, by model and by coefficient
#' summary(global_tests(res))
#' summary(global_tests(res, by = "Model"))
#' summary(global_tests(res, by = "Coeff"))
#' 
#' # Global tests for each factor
#' summary(global_tests(res, by = "individual", comb_factors = TRUE))
#' 
#' # These tests can be aggregated as before (e.g., by coefficient)
#' summary(global_tests(res, by = "Coeff", comb_factors = TRUE))
#' 
#' # Lower 95%-confidence bound for the TDP
#' # require(sumSome)
#' # pimaAnalysis(res, alpha = 0.4)
#' # pimaAnalysis(res, by = "Model", alpha = 0.4)
#' # pimaAnalysis(res, by = "Coeff", alpha = 0.4)
#' @export

pima <- function(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), ...) {
  
  method <- match.arg(method, c("maxT", "minP", "none"))
  
  extra_args <- list(...)
  
  join_flipscores_args <- c(
    list(mods = mods, tested_coeffs = tested_coeffs, n_flips = n_flips), 
    extra_args
  )
  
  out <- do.call(jointest::join_flipscores, join_flipscores_args)
  
  old_class <- class(out)
  
  # TODO check if removing res$call is problematic
  out <- out[names(out) != "call"]
  
  if(method != "none"){
    # TODO is tail something to put as parameter?
    out <- jointest::p.adjust(out, method = method, tail = 0)
  }
  
  # get all the arguments of jointest::join_flipscores to be
  # used in other functions
  
  join_flipscores_args <- .get_fn_args(jointest::join_flipscores, 
                                       new.args = join_flipscores_args, 
                                       exclude = c("...", "mods"))
  out$info <- .get_info_models(out$mods)
  out$tested_coeffs <- tested_coeffs
  out <- c(out, join_flipscores_args)
  out$p.adjust.method <- method
  class(out) <- unique(c("pima", old_class))
  
  return(out)
}
