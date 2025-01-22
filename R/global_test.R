#' Global tests in multiverse analysis
#' @description Tests global null hypotheses for weak FWER control, considering one or more parameters within a multiverse of models.
#' @usage global_tests(obj, comb_funct = "maxT", by = NULL) 
#' @param obj a \code{jointest} object, e.g., produced by \code{\link[pima]{pima}}.
#' @param comb_funct combining function (see \code{\link[combine-methods]{combine-methods}} for more datails).
#' @param by name of the column of \code{summary_table} used for combining.
#' If \code{NULL} (default), a single global null hypothesis is tested.
#' @details In the default \code{by = NULL}, the procedure tests the global null hypothesis
#' that there are no significant effects (all considered coefficients in all models are null)
#' against the alternative that there is at least one significant effect
#' (one non-null coefficient in one model). Other inputs of the argument \code{by}
#' test analogous global null hypotheses, defined by model (\code{"Model"}) or by coefficient (\code{"Coeff"}).
#' @return Returns an object of class \code{jointest}, containing:
#' \itemize{
#' \item \code{Tspace}: data frame of test statistics, where columns correspond to tests, and rows to sign-flipping transformations
#' (the first is the identity).
#' \item \code{summary_table}: data frame containing a summary for each test:
#' combined test statistic and p-value.
#' }
#' @references
#' Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, doi: 10.1007/s11336-024-09973-6.
#' 
#' @examples
#' # COPIARE PARTE DELL'ESEMPIO DELLA FUNZIONE pima()
#' @export

global_tests <- function(obj, comb_funct = "maxT", by = NULL){
  
  out <- jointest::combine(obj, comb_funct = comb_funct, by = by)
  return(out)
}

