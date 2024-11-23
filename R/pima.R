#'pima function
#'@param mods list of \code{glm}s (or list of any other object that can be evaluated by flipscores) 
#'@param tested_coeffs is a list of the same length of \code{mods}, each element of the list being a vector of  
#'of names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#'As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#'@param n_flips = 5000
#'@param score_type any valid type for \code{flipscores}, \code{"standardized"} is the default. see \code{\link[flipscores]{flipscores}} for more datails 
#'@param statistics "t" is the only method implemented (yet). Any other value will not modify the Score (a different statistic will only affect the multivariate inference, not the univariate one).
#'@param seed \code{NULL} by default. Use a number if you wanto to ensure replicability of the results
#'@param output_models \code{TRUE} by default. Should the \code{flipscores} model returned?
#'@param ... any other further parameter.
#'@export
#'
#'@examples
#'n=20
#'set.seed(123)
#'D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#'D$Y=D$Z1+D$X+rnorm(n)
#'mod1=glm(Y~X+Z1+Z2,data=D)
#'mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#'mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#'mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#'mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#'for(i in 1:length(mods))
#' mods[[i]]$call$data=eval(D)
#'library(pima)
#'res=pima(mods,n_flips = 5000, score_type = "standardized" ,
#'       seed = 1, tested_coeffs = "X")
#'summary(res)
#'summary(combine(res))
#'res=pima:::p.adjust.pima(res)
#'summary(res)
#'summary(combine(res,by="Model"))
pima <- function (mods, tested_coeffs = NULL, n_flips = 5000, score_type = "standardized", 
                             statistics = "t", seed=NULL, output_models, ...) 
{

  out <- join_flipscores(mods, tested_coeffs = tested_coeffs, n_flips = n_flips, score_type = score_type, 
                               statistics =statistics, seed=seed, output_models=output_models) 
  
  out$info <- .get_info_models(out$mods)
  out$tested_coeffs<-tested_coeffs
  class(out) <- unique(c("pima", class(out)))
  out
}
