#' Post-selection Inference in Multiverse Analysis
#' @description Runs resampling-based tests jointly to allow for multiple testing in multiverse analysis.
#' As main feature, it applies the PIMA procedure, a general and flexible framework based on the sign-flip score test
#' that provides control of the Family-Wise Error Rate (FWER) and confidence bounds for True Discovery Proportions (TDPs).
#' @importFrom graphics plot text
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @importFrom stats as.formula formula getCall median qnorm quantile reformulate reshape terms update
#' @importFrom utils combn head stack tail
# @importFrom car Anova
# @importFrom MASS glm.nb
# @importFrom plyr laply
# @importFrom methods is
# @importFrom methods as
# @importFrom stats D as.formula model.matrix sd summary.glm update
#' @details
#' The starting point of the package is the [pima()] function.
#' See examples there.
#' 
#' @references
#' \insertRef{girardi2024pima}{pima}
#'
#' \insertRef{hemerik2020robust}{pima}
#'
#' \insertRef{desantis2025inference}{pima}
#' 
# @docType _PACKAGE
#' @author Livio Finos, Paolo Girardi, Filippo Gambarota, Anna Vesely, Giulia Calignano, Massimiliano Pastore, Gianmarco Altoè
#' @name pima-package

NULL
