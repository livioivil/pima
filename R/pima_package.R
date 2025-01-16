#' Post-selection Inference in Multiverse Analysis
#' @description Runs resampling-based tests jointly to allow for multiple testing in multiverse analysis, providing weak and strong control of the Family-Wise Error Rate and True Discovery Proportions.
#' As main feature, it applies the PIMA procedure, a general and flexible framework based on the sign-flip score test.
#' @import flipscores
#' @importFrom stats median
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats update
# @importFrom car Anova
# @importFrom MASS glm.nb
# @importFrom plyr laply
# @importFrom methods is
# @importFrom methods as
# @importFrom stats D as.formula model.matrix sd summary.glm update
#' @examples
#' # COPIARE ESEMPIO DA FUNZIONE PIMA
#' 
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
#' 
#' Hemerik, Goeman, Finos (2020). Robust testing in generalized linear models by sign flipping score contributions. Journal of the Royal Statistical Society, Series B (Statistical Methodology), doi: 10.1111/rssb.12369.
#' 
#' De Santis, Goeman, Hemerik, Davenport, Finos (2024). Inference in generalized linear models with robustness to misspecified variances. Pre-print, doi: 10.48550/arXiv.2209.13918.
#' 
# @docType _PACKAGE
#' @author Livio Finos
#' @name pima-package

NULL
