#' Post-selection Inference in Multiverse Analysis
#'
#' @description
#' Runs resampling-based tests jointly to allow for multiple testing in
#' multiverse analysis. As main feature, it applies the PIMA procedure, a
#' general and flexible framework based on the sign-flip score test that
#' provides control of the Family-Wise Error Rate (FWER) and confidence bounds
#' for True Discovery Proportions (TDPs).
#'
#' @details
#' A typical analysis has four steps:
#' 1. build and fit the multiverse of specifications with [create_multi()];
#' 2. collect the results in a table with [spec_table()] and plot them with
#'    [spec_curve()] and [spec_scatter()];
#' 3. describe the vibration of effects with [voe()], [janus()] and
#'    [volcano()];
#' 4. test the effects jointly across the specifications with [pima()],
#'    controlling the FWER.
#'
#' @references
#' \insertRef{girardi2024pima}{pima}
#'
#' \insertRef{hemerik2020robust}{pima}
#'
#' \insertRef{desantis2025inference}{pima}
#'
#' @importFrom graphics plot text
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @importFrom stats aggregate as.formula expand.model.frame formula getCall median qnorm quantile reformulate reshape terms update
#' @importFrom utils combn head stack tail
# @importFrom car Anova
# @importFrom MASS glm.nb
# @importFrom plyr laply
# @importFrom methods is
# @importFrom methods as
# @importFrom stats D as.formula model.matrix sd summary.glm update
"_PACKAGE"
