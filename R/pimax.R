#' Post-selection Inference in Multiverse Analysis for Mixed Models
#' @description Applies the PIMAX procedure to make selective inference on one or more parameters within a multiverse of models,
#' derived from different specifications of the data (e.g., pre-processing) and of the model itself (GLMMs).
#' The method jointly computes resampling-based test statistics for all coefficients of interest in all models.
#' The output allows for multiple testing in multiverse analysis, providing weak and strong control of the Family-Wise Error Rate (FWER)
#' and confidence bounds for True Discovery Proportions (TDPs).
#' @usage pimax(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), ...) 
#' @param formulas A formula or a list of formulas. It can be a (list) of complete models as.formula or a list of formulas
#' @param summstats_within A (list of) vector of summary statistics model within the data or a (list of) function with argument data.
#' @param data The dataset to be used for fitting the model.
#' @param cluster A vector or a formula evaluated on the data that defines the clusters.
#' @param tested_coeffs list of the same length of \code{mods}, each element of the list being a vector of  
#' names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#' As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#' @param n_flips number of flips, default 5000
#' @param flips matrix fo +1 or -1, the matrix has \code{n_flips} rows and n (number of observations) columns
#' @param seed \code{NULL} by default. Use a number if you wanto to ensure replicability of the results
#' @param ... any other further parameter.
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
#' Andreella, A., Goeman, J., Hemerik, J., Finos, L. (2025). Robust Inference for Generalized Linear Mixed Models: A “Two-Stage Summary Statistics” Approach Based on Score Sign Flipping. Psychometrika, 1-23. doi: 10.1017/psy.2024.22  
#' 
#' @examples
#' @example 
#' N=20
#' n=rpois(N,20)
#' reff=rep(rnorm(N),n)
#' D=data.frame(X1=rnorm(length(reff)),
#'                X2=rep(rnorm(N),n),
#'                Grp=factor(rep(rep(LETTERS[1:3],length.out=N),n)),
#'                Subj=rep(1:N,n))
#'D$Y=rbinom(n=nrow(D),prob=plogis( 2*D$X1 * (D$Grp=="B") +  2*D$X2+reff),size=1)

#'summstats_within <- list('glm(Y ~ X1, family = binomial(link = "logit"))',
#'                         'logistf::logistf(Y ~ X1^2, family = binomial(link = "logit"), control=logistf::logistf.control(maxit=100))')

#'formulas <- list(Y ~ Grp * X1 + X2, Y ~ Grp + X1 + X2,  Y ~ Grp + X1)
#' 
#' # Test selected coefficients (raw and adjusted p-values)
#' res <- pimax(formulas = formulas, summstats_within = summstats_within, 
#'                    data = D, cluster = D$Subj)
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

pimax <-  function(formulas, summstats_within, data, cluster, tested_coeffs = NULL, n_flips = 5000, flips = NULL,
                   seed=NULL, ...) {
  
  

  extra_args <- list(...)
  
  join_flip2sss_args <- c(
    list(formulas = formulas, summstats_within = summstats_within, data = data, cluster = cluster, n_flips = n_flips), 
    extra_args
  )
  
  out <- do.call(join_flip2sss, join_flip2sss_args)
  
  old_class <- class(out)
  
  # TODO check if removing res$call is problematic
  out <- out[names(out) != "call"]
  
  if(method != "none"){
    # TODO is tail something to put as parameter?
    out <- jointest::p.adjust(out, method = method, tail = 0)
  }
  
  # get all the arguments of jointest::join_flipscores to be
  # used in other functions
  
  join_flip2sss_args <- .get_fn_args(jointest::join_flip2sss, 
                                       new.args = join_flip2sss_args, 
                                       exclude = c("...", "mods"))
  out$info <- .get_info_models(out$mods)
  out$tested_coeffs <- tested_coeffs
  out <- c(out, join_flipscores_args)
  out$p.adjust.method <- method
  class(out) <- unique(c("pima", old_class))
  
  return(out)
}
