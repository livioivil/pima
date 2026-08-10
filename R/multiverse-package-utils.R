.parse_multiverse_object <- function(x) {
  
  # TODO check this implementation
  rlang::check_installed("multiverse")
  
  u <- multiverse::expand(x)
  
  models <- lapply(u$.results, function(env) {
    model <- get("fit", envir = env, inherits = FALSE)
    
    if (!is.null(model$call$data)) {
      model$call$data <- eval(model$call$data, envir = env)
    }
    model
  })
  
  extra <- u[, names(multiverse::parameters(x)), drop = FALSE]
  nm <- paste0("mod", 1:nrow(extra))
  extra$model <- nm
  names(models) <- nm
  
  list(models = models,
       extra = as.data.frame(extra))
}

