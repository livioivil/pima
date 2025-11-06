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
                         data,
                         focal = NULL,
                         nfuns = NULL,
                         cfuns = NULL,
                         transf.focal = FALSE,
                         subset = NULL,
                         fit.fun = NULL,
                         fit.fun.args = NULL){

  # formula <- Sepal.Length ~ Sepal.Width + Petal.Length * Species
  # data <- iris
  # focal <- c("Petal.Length * Species")
  # nfuns <- c("log")
  # cfuns <- NULL
  # transf.focal <- FALSE
  # fit.fun <- NULL
  # fit.fun.args <- NULL
  
  # some checks
  
  if(!is.null(focal)) focal_ff <- as.formula(paste("~", focal))
  
  if(has_ints(focal_ff) & !has_ints(formula)){
    stop("the focal predictor contains an interaction while the formula don't!")
  }
  
  if(!is.null(subset)){
    if(length(unique(sapply(subset, length))) != 1){
      stop("all vectors in subset need to be of the same length!")
    }
    datal <- lapply(subset, function(s) data[s, ])
  } else{
    subset <- rep(TRUE, nrow(data))
  }
  
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
  X$focal[X$x %in% focal] <- TRUE
  
  if(length(transf.focal) != 1 & length(transf.focal) != length(focal)) {
    stop("transf.focal need to be a logical vector of length 1 or length of focal!")
  } 
  
  if(length(transf.focal) == 1 & length(focal) > 1) {
    transf.focal <- rep(transf.focal, length(focal))
  }
  
  for(i in 1:length(focal)){
    if(!transf.focal[i]){
      X$fun <- ifelse(X$x == focal[i] & X$fun != "identity", "identity", X$fun)
    }
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
  
  if(has_ints(focal_ff)){
    focal_int_vars <- get_ints_vars(focal_ff)
    
    form_as_int_vars <- apply(sapply(focal_int_vars, function(x) grepl(x, forms)), 1, all)
    forms <- forms[form_as_int_vars]
    
    for(i in 1:length(forms)){
      ff <- forms[[i]]
      ff <- c(ff, paste(ff[focal_int_vars], collapse = " * "))
      forms[[i]] <- expand_ints(ff)
    }
  } else{
    has_focal <- matrix(NA, nrow = length(forms), ncol = length(focal))
    
    for(i in 1:length(focal)){
      has_focal[, i] <- sapply(forms, function(f) any(grepl(focal[i], f)))
    }
    
    has_focal <- apply(has_focal, 1, all)
    
    forms <- forms[has_focal]
  }
  
  forms_call <- lapply(forms, paste, collapse = " + ")
  forms_call <- paste(y, "~", forms_call)
  out <- list(variables = X, calls = unlist(forms_call), subset = subset)
  
  if(!is.null(fit.fun)){
    
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

get_ints_vars <- function(formula){
  vv <- expand_ints(formula)
  vv <- vv[sapply(vv, has_ints)]
  unlist(strsplit(vv, ":"))
}

expand_ints <- function(formula) {
  if (inherits(formula, "formula")) {
    formula <- as.character(formula)
  }
  labels(terms(reformulate(formula)))
}

has_ints <- function(formula, x = NULL){
  if(!is.character(formula)){
    labels <- attr(terms(formula), "term.labels")
  } else{
    labels <- formula
  }
  
  ints <- labels[grepl(":", labels)]
  if(!is.null(x)) {
    grepl(x, ints, fixed = TRUE)
  } else{
    length(ints) >= 1
  }
}
