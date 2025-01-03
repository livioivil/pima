#' Post-selection Inference in Multiverse Analysis (PIMA)
#' @description This function applies the PIMA procedure to make selective inference on parameters of interest within a multiverse of models.
#' The method builds on the sign-flip score test implemented in the function \code{flipscores} of the \strong{flipscores} package.
#' @usage pima(mods, tested_coeffs = NULL, n_flips = 5000, score_type = "standardized", statistics = "t", seed = NULL, output_models = TRUE, ...) 
#' @param mods list of objects that can be evaluated by \code{flipscores}, usually
#' model objects produced by \code{glm} or \code{flipscores}.
#' @param tested_coeffs tested coefficients. It can be a vector of names (common to all models)
#' or a list of the same length as \code{mods}, where each element is a vector of names.
#' If \code{NULL}, all coefficients are tested.
#' @param n_flips number of sign flips.
#' @param score_type type of score that is computed (see \code{\link[flipscores]{flipscores}} for more datails).
#' The default \code{standardized} takes into account nuisance estimation and provides
#' good control of the type I error even for small sample sizes.
#' @param statistics test statistics computed by the procedure. Currently, \code{t} is the only implemented method.
#' Different statistics will affect the multivariate inference, but not the univariate.
#' @param seed seed. Can be specified to ensure replicability of results.
#' @param output_models \code{TRUE} to return the list of model objects produced by \code{flipscores}.
#' @param ... any other further parameters.
#' @return \code{pima} returns an object of class \code{pima}, containing:
#' \itemize{
#' \item \code{Tspace}: data frame of test statistics, where columns correspond to coefficients, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item \code{summary_table}: data frame containing a summary for each tested coefficient in each model:
#' estimate, score, standard error, z value, partial correlation, p-value.
#' \item \code{mods}: list of model objects computed by \code{flipscores}.
#' \item \code{tested_coeffs}: tested coefficients, as in input.
#' }
#'
#' @examples
#' # Generate data
#' n = 20
#' set.seed(123)
#' D = data.frame(X1 = rnorm(n), X2 = rnorm(n)^2, Z1 = rnorm(n), Z2 = rnorm(n))
#' D$Y = D$X1 + D$Z1 + rnorm(n)
#' 
#' # Multiverse of 4 models (list)
#' mod1 = glm(Y ~ X1 + X2 + Z1 + Z2, data = D)
#' mod2 = glm(Y ~ X1 + X2 + poly(Z1,2) + Z2, data = D)
#' mod3 = glm(Y ~ X1 + X2 + Z1 + poly(Z2,2), data = D)
#' mod4 = glm(Y ~ X1 + X2 + poly(Z1,2) + poly(Z2,2), data = D)
#' mods = list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
#' 
#' # Test the coefficients of X1 and X2, then add adjusted p-values
#' res = pima(mods, tested_coeffs = c("X1","X2"), seed = 123)
#' res = jointest:::p.adjust(res)
#' summary(res)
#' 
#' # Global p-values: overall, by model and by coefficient
#' summary(jointest::combine(res))
#' summary(jointest::combine(res, by="Model"))
#' summary(jointest::combine(res, by="Coeff"))
#' @export

pima <- function (mods, tested_coeffs = NULL, n_flips = 5000, score_type = "standardized", 
                             statistics = "t", seed = NULL, output_models, ...) 
{

  out <- join_flipscores(mods, tested_coeffs = tested_coeffs, n_flips = n_flips, score_type = score_type, 
                               statistics =statistics, seed=seed, output_models=output_models) 
  
  out$info <- .get_info_models(out$mods)
  out$tested_coeffs<-tested_coeffs
  class(out) <- unique(c("pima", class(out)))
  out
}

