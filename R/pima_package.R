#' Post-selection Inference in Multiverse Analysis
#' @description Runs resampling-based tests jointly to allow for multiple testing in multiverse analysis.
#' As main feature, it applies the PIMA procedure, a general and flexible framework based on the sign-flip score test
#' that provides control of the Family-Wise Error Rate (FWER) and confidence bounds for True Discovery Proportions (TDPs).
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
#' @details
#' The starting point of the package is the \code{\link[pima]{pima}} function.
#' See examples there.
#' 
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
#' 
#' Hemerik, Goeman, Finos (2020). Robust testing in generalized linear models by sign flipping score contributions. Journal of the Royal Statistical Society, Series B (Statistical Methodology), doi: 10.1111/rssb.12369.
#' 
#' De Santis, Goeman, Hemerik, Davenport, Finos (2025). Inference in generalized linear models with robustness to misspecified variances. Journal of the American Statistical Association, doi: 10.1080/01621459.2025.2491775.
#' 
# @docType _PACKAGE
#' @author Livio Finos, Paolo Girardi, Filippo Gambarota, Anna Vesely, Giulia Calignano, Massimiliano Pastore, Gianmarco Altoè
#' @name pima-package

NULL
