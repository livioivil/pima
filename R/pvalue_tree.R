
#' Classification Tree with Adjusted p-values
#'
#' This function builds a classification tree based on raw or adjusted p-values using the results of a regression model.
#'
#' @param res A list object with a specific structure containing regression model results. It should contain `mods`, a list of models, and `summary_table`, a data frame with the summary of results, including estimates and p-values.
#' @param p.values A character string indicating which type of p-values to use. Options are `"raw"` (default) or `"adjusted"`. When `"raw"`, the function uses the p-values from the `summary_table`. When `"adjusted"`, p-values are adjusted using the `maxT.light` function.
#'
#' @return A plot of the classification tree using `rpart.plot()`.
#'
#' @examples
#' # Example usage (assuming `res` is a pre-computed result object)
#' # pvalue_tree(res, p.values = "raw")
#' @import rpart
#' @import rpart.plot
#' @export
pvalue_tree <- function(res, p.values = "raw") {
  # Extract data
  data_ori <- res$mods[[1]]$data
  cmb <- names(res$mods)
  n_spec <- length(cmb)
  spec <- list()
  
  for (i in 1:n_spec) {
    spec[[i]] <- res$mods[[i]]$formula
  }
  
  ####################################################
  #DEVE DIVENTARE UNA PARTE DELL'OUTPUT DI PIMA
  # Organize combinations
  nvar <- sapply(spec, function(x) length(gregexpr("\\+", as.character(x))) + 1)
  comb_long <- data.frame(Variable = as.vector(sapply(cmb, function(x) unlist(strsplit(x, split = "+", fixed = TRUE)))),
                          SpecID = rep(rank(res$summary_table$Estimate), times = nvar))
  comb_long$Type <- NULL
  comb_long$Type[is.na(sapply(comb_long$Variable, function(x) strsplit(x, "[(),]")[[1]][2]))] <- "original"
  comb_long$Type[is.na(comb_long$Type)] <- sapply(comb_long$Variable[is.na(comb_long$Type)], function(x) strsplit(x, "[(),]")[[1]][1])
  comb_long$Variable[comb_long$Type != "original"] <- sapply(comb_long$Variable, function(x) strsplit(x, "[(),]")[[1]][2])[comb_long$Type != "original"]
  comb_long$Variable <- factor(comb_long$Variable)
  comb_long$SpecID <- rep(rank(res$summary_table$Estimate), times = nvar)
  comb_wide <- reshape(comb_long, direction = "wide", idvar = "SpecID", timevar = "Variable")
  names(comb_wide)[-1] <- sapply(names(comb_wide)[-1], function(x) strsplit(x, "[.]")[[1]][2])
  ############################
  
  if (p.values == "raw") comb_wide$p <- res$summary_table$p
  if (p.values == "adjusted") comb_wide$p <- maxT.light(res$Tspace, c(0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5))
  
  for (i in 1:max(nvar)) comb_wide[, i] <- factor(comb_wide[, i])
  
  tree_model <- rpart(p ~ ., data = comb_wide[, -1])
  rpart.plot(tree_model)
}
#' NOT EXPORTED
#' Compute Thresholds for maxT Adjustment
#'
#' This function computes thresholds for the maxT adjustment method.
#'
#' @param permT A matrix of permutation test statistics.
#' @param alphas A numeric vector of significance levels.
#'
#' @return A numeric vector of thresholds.
#'
#' @examples
#' # Example usage:
#' # compute_thresholds(permT, alphas = 0.05)

compute_thresholds <- function(permT, alphas) {
  quantile(apply(permT, 1, max), round(1 - alphas, 10), type = 1, names = FALSE)
}
