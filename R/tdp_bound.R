#' True discovery guarantee in multiverse analysis
#' @description Computes lower confidence bounds for True Discovery Proportions, when studying one or more parameters within a multiverse of models.
#' @usage tdp_bound(obj, by = NULL, comb_funct = "sum", r = 0, conf_level = 0.95, ...) 
#' @param obj a \code{jointest} object, e.g., produced by \code{\link{pima}}.
#' @param by name of the column of \code{summary_table} used for combining.
#' If \code{NULL} (default), the TDP is evaluated over all coefficients in all models.
#' @param comb_funct combining function: \code{sum} uses the sum of scores as in \code{\link[sumSome]{sumStats}},
#' while different p-value combinations are defined as in \code{\link[sumSome]{sumPvals}}.
#' @param r parameter for Vovk and Wang's p-value combination.
#' @param conf_level confidence level.
#' @param ... further parameters of \code{sumStats} or \code{sumPvals} (truncation parameters and maximum number of iterations of the algorithm).
#' @details This function builds on the functions \code{sumStats} and \code{sumPvals} of the \strong{sumSome} package, which
#' combine scores or p-values (by default without truncation).
#' @details In the default \code{by = NULL}, the procedure computes a lower confidence bound for the
#' number of significant effects (non-null coefficients) among all.
#' Other inputs of the argument \code{by} return analogous bounds, defined by model (\code{"Model"}) or by coefficient (\code{"Coeff"}).
#' The bounds are simultaneous and remain valid under post-hoc selection.
#' @return Returns a data frame containing a summary for each subset:
#' number of elements and confidence bound for the TDP.
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
#' 
#' Vesely, Finos, Goeman (2023). Permutation-based true discovery guarantee by sum tests. Journal of the Royal Statistical Society, Series B (Statistical Methodology), doi: 10.1093/jrsssb/qkad019
#' @examples
#' # COPIARE PARTE DELL'ESEMPIO DELLA FUNZIONE pima()
#' @export

tdp_bound <- function(obj, by = NULL, comb_funct = "sum", r = 0, conf_level = 0.95, ...){
  
  type <- match.arg(tolower(comb_funct), c("sum", "fisher", "pearson", "liptak",  "edgington", "cauchy", "harmonic", "vovk.wang"))
  
  extra_args <- list(...)
  truncFrom <- extra_args$truncFrom %||% NULL
  truncTo <- extra_args$truncTo %||% NULL
  
  if(is.null(by)){
    S = list(Overall = 1:ncol(obj$Tspace))
  }else{
    smr = apply(obj$summary_table[, by, drop = FALSE], 1, paste, collapse = ".")
    uniq_nm = unique(smr)
    S = lapply(uniq_nm, function(nm) which(smr == nm))
    names(S) = uniq_nm
  }
  
  G <- abs(as.matrix(res$Tspace))
  alpha <- 1 - conf_level
  
  if(type == "sum"){
    
    get_tdp <- function(x){
      tmp <- do.call(sumSome::sumStats, c(
        list(G=G, S=S[[x]], alternative="greater", alpha=alpha), 
        extra_args
      ))
      return(tdp(tmp))
    }
  }else{
    
    P <- flip::t2p(G, obs.only = FALSE)
    
    get_tdp <- function(x){
      tmp <- do.call(sumSome::sumPvals, c(
        list(G=G, S=S[[x]], alpha=alpha, type = type, r = r), 
        extra_args
      ))
      return(tdp(tmp))
    }
    
    if(type == "vovk.wang"){type <- paste0("VW(", r, ")")}
  }
  
  out <- data.frame(
    Name = names(S),
    Method = type,
    alpha = alpha,
    n = sapply(1:length(S), FUN = function(i) length(S[[i]])),
    TDP = sapply(1:length(S), FUN=get_tdp)
  )
  
  return(out)
}

