
#' Classification Tree with Adjusted p-values
#'
#' This function builds a classification tree based on raw or adjusted p-values using the results of a regression model.
#'
#' @param res A list object with a specific structure containing regression model results. It should contain `mods`, a list of models, and `summary_table`, a data frame with the summary of results, including estimates and p-values.
#' @param p.values A char string indicating which type of p-values to use. Options are `"raw"` or `"adjusted"` (default). When `"raw"`, the function uses the p-values from the `summary_table`.
#' @param method used in function `rpart::rpart` if `method="class"` it will classify significant p-values at level `alpha`
#' @param alpha used only when `method="class"`
#' @param ... any other parameter of `rpart::rpart`. 
#' @return A plot of the classification tree using `rpart.plot()`.
#'
#' @examples
#' # Example usage (assuming `res` is a pre-computed result object)
#' # pvalue_tree(res, p.values = "raw")
#' @export
pima_tree <- function(res, p.values="adjusted",method="class", alpha=0.05, ... ) {
  # Extract data
  # data_ori <- res$mods[[1]]$data
  # cmb <- names(res$mods)  # in altri modi: unique(res$summary_table$Model) ??
  # n_spec <- length(cmb)

  # Legend of specifications
  p.values=match.arg(arg = p.values,c("adjusted","raw"))
  if (p.values == "adjusted") res$summary_table$p <- res$summary_table$p.adj 
  if(method=="class") res$summary_table$p=res$summary_table$p<=alpha
  temp=res$summary_table[,c("model","p")]
  comb_wide<-merge(res$info,temp,by="model")
  
  comb_wide$formula=comb_wide$model=NULL
  tree_model <- rpart::rpart(p ~ ., data = comb_wide, method = method)
  rpart::printcp(tree_model)
  
  rpart.plot::rpart.plot(tree_model)
}

