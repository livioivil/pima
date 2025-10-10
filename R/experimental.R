#' Create the scenarios for the multiverse
#'
#' @param formula a formula describing the maximal model with bare variables (no tranformations). Currently not supporting interactions.
#' @param focal optional name of the coefficient that is the focus of the analysis. No transformation will be applied to that variable and no model will exclude this variable.
#' @param nfuns functions to be applied to numerical variables. functions need to be provided as characters. this argument can be a vector of functions (e.g., `c("log", "exp")`) and the functions will be applied al all numerical variable exluding the focal predictor. In alternative, can be a named list specifing the name of the variable and the functions as string (e.g., `list(x = "log", z = c("exp"))`) in this way the functions are variable-specific. If `NULL` no transformations will be applied.
#' @param cfuns same as the `nfuns` but for factor/character variables. If `NULL` no transformations will be applied.
#' @param data the dataset for the model
#'
#' @return a list
#' @examples
#' create_multi(~ Sepal.Length + Petal.Width + Species, 
#'              focal = "Sepal.Length", 
#'              nfuns = c("log"), 
#'              data = iris)
#' 
create_multi <- function(formula,
                         focal = NULL,
                         nfuns = NULL,
                         cfuns = NULL,
                         data,
                         fit = FALSE,
                         fit.fun = NULL,
                         fit.fun.args = NULL){
  xs <- formula.tools::rhs.vars(formula)
  y <- formula.tools::lhs.vars(formula)
  xs_type <- sapply(data, class)[xs]
  
  xs_num <- xs[xs_type == "numeric" | xs_type == "integer"]
  xs_chr <- xs[!(xs_type == "numeric" | xs_type == "integer")]
  
  xs_num <- if(length(xs_num) == 0) NULL else xs_num
  xs_chr <- if(length(xs_chr) == 0) NULL else xs_chr
  
  xx <- as.list(rep("identity", length(xs)))
  names(xx) <- xs
  
  xx <- stack(xx)
  xx <- .fac2char(xx)
  
  if(length(xs_num) != 0){
    if(!is.list(nfuns)){
      if(!is.null(nfuns)){
        nfuns <- rep(list(nfuns), length(xs_num))
        names(nfuns) <- xs_num
        nfuns <- stack(nfuns)
      }
    }
    xx <- rbind(xx, nfuns)
  }
  
  if(length(xs_chr) != 0){
    if(!is.list(cfuns)){
      if(!is.null(cfuns)){
        cfuns <- rep(list(cfuns), length(xs_chr))
        names(cfuns) <- xs_chr
        cfuns <- stack(cfuns)
      }
    }
    xx <- rbind(xx, cfuns)
  }
  
  rownames(xx) <- NULL
  
  xx$type <- xs_type[xx$ind]
  X <- xx
  names(X) <- c("fun", "x", "type")
  X <- .fac2char(X)
  
  X$focal <- FALSE
  X$focal[X$x == focal] <- TRUE
  
  if(!is.null(focal)){
    X$fun <- ifelse(X$x == focal & X$fun != "identity", "identity", X$fun)
  }
  
  X$.id_fun <- .make_id(X$fun)
  X$.id_x <- .make_id(X$x)
  
  X <- X[!duplicated(X), ]
  
  X$call <- NA
  for(i in 1:nrow(X)){
    if(X$fun[i] == "identity"){
      X$call[i] <- X$x[i]
    }else{
      X$call[i] <- .make_call(X$x[i], X$fun[i])
    }
  }
  
  xu <- X$call
  names(xu) <- X$x
  forms <- sapply(1:length(xu), function(i) combn(xu, i, simplify = FALSE))
  forms <- unlist(forms, recursive = FALSE)
  dup <- sapply(forms, function(x) length(unique(names(x))) != length(names(x)))
  forms <- forms[!dup]
  # remove calls without the focal predictor
  if(!is.null(focal)){
    has_focal <- sapply(forms, function(x) any(x == focal))
    forms <- forms[has_focal]
  }
  
  forms_call <- lapply(forms, paste, collapse = " + ")
  forms_call <- paste(y, "~", forms_call)
  
  # # check if the marginality principle is respected
  # forms_as_formula <- sapply(forms_call, as.formula)
  # is_marginal <- sapply(forms_as_formula, function(x) check_marginality(x))
  # forms_call <- forms_call[!is_marginal]
  out <- list(X = X, calls = unlist(forms_call), data = data)
  
  if(fit){
    mods <- vector(mode = "list", length = length(out$calls))
    
    for(i in 1:length(mods)){
      mm <-  do.call(fit.fun, c(list(formula = as.formula(out$calls[i]), data = data), fit.fun.args))
      mm$call$formula <- as.formula(out$calls[i])
      mods[[i]] <- mm
    }
    
    out$mods <- mods
  }
  
  return(out)
}

.fac2char <- function(x){
  isfactor <- sapply(x, is.factor)
  x <- lapply(x, function(c) if(is.factor(c)) as.character(c) else c)
  data.frame(x)
}

.make_id <- function(x){
  as.integer(factor(x))
}

.make_call <- function(x, FUN){
  deparse(call(FUN, as.name(x)))
}
