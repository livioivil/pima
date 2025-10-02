#' Global tests in multiverse analysis
#' @description Tests global null hypotheses for weak control of the Family-Wise Error Rate, considering one or more parameters within a multiverse of models.
#' @usage global_tests(obj, by = NULL, comb_funct = "maxT", comb_factors = FALSE, comb_factors_funct = "Mahalanobis")
#' @param obj a \code{jointest} object, e.g., produced by \code{\link{pima}}.
#' @param by mode of combination (\code{NULL}, \code{Coeff}, \code{Model}, \code{individual}).
#' If \code{NULL} (default), a single global null hypothesis is tested.
#' @param comb_funct combining function (see \code{\link[jointest]{combine-methods}} for more datails).
#' @param comb_factors logical. If \code{FALSE}, coefficients for the levels of factor variables are considered separately.
#' If \code{TRUE}, a single global test is performed for each factor variable.
#' @param comb_factors_funct combining function for contrasts of factor variables (see \code{\link[jointest]{combine_contrasts}} for more datails).
#' @details This function builds on the functions \code{combine} and \code{combine_contrasts} of the \strong{jointest} package.
#' @details In the default \code{by = NULL}, the procedure tests the global null hypothesis
#' that there are no significant effects (all considered coefficients in all models are null)
#' against the alternative that there is at least one significant effect
#' (one non-null coefficient in one model). Other inputs of the argument \code{by}
#' test analogous null hypotheses, defined by model (\code{Model}), by coefficient (\code{Coeff}) or individually (\code{individual}).
#' @details The argument \code{comb_factors} affects only categorical predictors. For each categorical predictor, by default (\code{FALSE}), non-baseline coefficients are considered separately. 
#' If \code{TRUE}, a single global test is performed by combining the tests derived from the contrasts
#' with the function specified by \code{comb_factors_funct}.
#' @details With the option \code{by = "individual"}, the input p-values are returned unchanged, 
#' except when categorical predictors are present and \code{comb_factors = TRUE}.
#' @return Returns an object of class \code{jointest}, containing:
#' \itemize{
#' \item \code{Tspace}: data frame of test statistics, where columns correspond to tests, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item \code{summary_table}: data frame containing a summary for each test:
#' combined test statistic and p-value.
#' }
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
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
#' @export

global_tests <- function(obj, by = NULL, comb_funct = "maxT", comb_factors = FALSE, comb_factors_funct = "Mahalanobis"){
  
  if(!is.null(by)){by <- match.arg(by, c("Coeff", "Model", "individual"))}
  
  
  if(comb_factors){
    obj <- jointest::combine_contrasts(obj, comb_funct = comb_factors_funct, tail = 0)
  }
  
  if(is.null(by) || by != "individual"){
    obj <- jointest::combine(obj, comb_funct = comb_funct, by = by)
  }
  
  return(obj)
}

