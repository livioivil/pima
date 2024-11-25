
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
pima_tree <- function(res, p.values = "raw",cp=0.05) {
  # Extract data
  data_ori <- res$mods[[1]]$data
  cmb <- names(res$mods)
  n_spec <- length(cmb)
  # Legend of specifications
  comb_wide<-res$info
   
  for (i in 4:length(comb_wide)) comb_wide[, i] <- factor(comb_wide[, i])
  if (p.values == "raw") comb_wide$p <- res$summary_table$p
  if (p.values == "adjusted") comb_wide$p <- maxT.light(res1$Tspace,exp(seq(-8,0,0.5)))
  
  tree_model <- rpart(p ~ ., data = comb_wide[, -c(1:4)], control = rpart.control(cp = cp))
  rpart.plot(tree_model)
}

