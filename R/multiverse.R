#' Create the scenarios for the multiverse
#'
#' @param formula a formula describing the maximal model with bare variables.
#'   Interactions are currently supported only for focal terms.
#' @param data the dataset for the model.
#' @param focal optional character vector with the name(s) of the focal predictor(s).
#'   Focal predictors are always included in every model.
#' @param nfuns functions to be applied to numerical variables. Can be a character
#'   vector, e.g. c("log", "scale"), or a named list, e.g.
#'   list(x = "log", z = c("sqrt", "scale")). If NULL, no numerical
#'   transformations are added.
#' @param cfuns same as nfuns, but for non-numerical variables.
#' @param transf.focal logical; if TRUE, transformations are also applied to focal
#'   predictors. Can be length 1 or the same length as focal.
#' @param subset optional named list of logical vectors defining row subsets.
#' @param fit.fun optional model fitting function, such as lm or glm.
#' @param fit.fun.args optional list of additional arguments passed to fit.fun.
#' @param max_scenarios maximum number of scenarios allowed before stopping.
#' @param fit logical; if TRUE and fit.fun is supplied, models are fitted.
#' @param keep_failed logical; if TRUE, failed models are kept as NULL with their
#'   errors stored in the output.
#'
#' @return a list with variables, calls, subset, scenarios, and optionally mods.
#' @export
#'
#' @examples
#' create_multi(
#'   formula = Sepal.Width ~ Sepal.Length + Petal.Width + Species,
#'   focal = "Sepal.Length",
#'   nfuns = c("log", "scale"),
#'   data = iris
#' )
create_multi2 <- function(formula,
                         data,
                         focal = NULL,
                         nfuns = NULL,
                         cfuns = NULL,
                         transf.focal = FALSE,
                         subset = NULL,
                         fit.fun = NULL,
                         fit.fun.args = NULL,
                         max_scenarios = 10000,
                         fit = TRUE,
                         keep_failed = TRUE) {
  
  formula <- .validate_formula(formula)
  data <- .validate_data(data)
  focal <- .validate_focal(focal)
  
  vars <- .extract_formula_vars(formula)
  y <- vars$y
  xs <- vars$xs
  
  .validate_vars_in_data(c(y, xs), data)
  
  focal_info <- .prepare_focal_formula(focal)
  
  if (!is.null(focal_info$formula)) {
    if (.has_ints(focal_info$formula) && !.has_ints(formula)) {
      stop("The focal predictor contains an interaction, but the formula does not.",
           call. = FALSE)
    }
  }
  
  subset <- .prepare_subset(subset, data)
  
  xs_type <- .get_variable_types(data, xs)
  xs_num <- xs[sapply(data[xs], is.numeric)]
  xs_chr <- setdiff(xs, xs_num)
  
  X <- .build_variable_table(
    xs = xs,
    xs_type = xs_type,
    xs_num = xs_num,
    xs_chr = xs_chr,
    nfuns = nfuns,
    cfuns = cfuns,
    focal = focal,
    transf.focal = transf.focal
  )
  
  .validate_transform_functions(X$fun)
  
  X$call <- .make_transformed_calls(X$x, X$fun)
  
  X$.id_fun <- .make_id_safe(X$fun)
  X$.id_x <- .make_id_safe(X$x)
  
  X <- X[!duplicated(X[, c("fun", "x", "type", "focal", "call")]), ]
  rownames(X) <- NULL
  
  forms <- .generate_model_terms(X)
  
  forms <- .filter_focal_terms(
    forms = forms,
    focal = focal,
    focal_formula = focal_info$formula
  )
  
  calls <- .build_formula_calls(y = y, forms = forms)
  
  out <- list(
    variables = X,
    calls = calls,
    subset = subset
  )
  
  scenarios <- .build_scenarios(
    calls = calls,
    subset = subset,
    max_scenarios = max_scenarios
  )
  
  if (!is.null(fit.fun) && isTRUE(fit)) {
    fitted <- .fit_models_safely(
      scenarios = scenarios,
      subset = subset,
      data = data,
      fit.fun = fit.fun,
      fit.fun.args = fit.fun.args,
      keep_failed = keep_failed
    )
    
    out$mods <- fitted$mods
    out$fit_errors <- fitted$errors
    out$fit_warnings <- fitted$warnings
    
    scenarios$fit_ok <- fitted$fit_ok
    scenarios$error <- unlist(fitted$errors, use.names = FALSE)
    scenarios$warning <- unlist(fitted$warnings, use.names = FALSE)
  }
  
  out$scenarios <- scenarios
  
  class(out) <- c("multiverse_scenarios", class(out))
  
  out
}

.validate_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }
  
  if (length(formula) < 3) {
    stop("`formula` must include both a left-hand side and a right-hand side.",
         call. = FALSE)
  }
  
  formula
}


.validate_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("`data` has zero rows.", call. = FALSE)
  }
  
  data
}


.validate_focal <- function(focal) {
  if (is.null(focal)) {
    return(character(0))
  }
  
  if (!is.character(focal)) {
    stop("`focal` must be NULL or a character vector.", call. = FALSE)
  }
  
  if (anyNA(focal) || any(focal == "")) {
    stop("`focal` cannot contain NA or empty strings.", call. = FALSE)
  }
  
  unique(focal)
}


.extract_formula_vars <- function(formula) {
  xs <- formula.tools::rhs.vars(formula)
  y <- formula.tools::lhs.vars(formula)
  
  if (length(y) != 1) {
    stop("`formula` must have exactly one outcome variable.", call. = FALSE)
  }
  
  if (length(xs) == 0) {
    stop("`formula` must contain at least one predictor.", call. = FALSE)
  }
  
  list(y = y, xs = xs)
}


.validate_vars_in_data <- function(vars, data) {
  missing_vars <- setdiff(vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop(
      "Variables not found in `data`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


.prepare_focal_formula <- function(focal) {
  if (length(focal) == 0) {
    return(list(formula = NULL, terms = character(0)))
  }
  
  focal_formula <- as.formula(paste("~", paste(focal, collapse = " + ")))
  
  list(
    formula = focal_formula,
    terms = focal
  )
}


.prepare_subset <- function(subset, data) {
  if (is.null(subset)) {
    subset <- list()
  }
  
  if (!is.list(subset)) {
    stop("`subset` must be NULL or a named list of logical vectors.",
         call. = FALSE)
  }
  
  subset <- c(list(all = rep(TRUE, nrow(data))), subset)
  
  if (is.null(names(subset)) || any(names(subset) == "")) {
    stop("All elements of `subset` must be named.", call. = FALSE)
  }
  
  if (anyDuplicated(names(subset))) {
    stop("Subset names must be unique.", call. = FALSE)
  }
  
  is_logical <- sapply(subset, is.logical)
  if (!all(is_logical)) {
    bad <- names(subset)[!is_logical]
    stop(
      "All subset elements must be logical vectors. Invalid subset(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  
  valid_length <- sapply(subset, length) == nrow(data)
  if (!all(valid_length)) {
    bad <- names(subset)[!valid_length]
    stop(
      "All subset vectors must have length equal to nrow(data). Invalid subset(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  
  has_na <- sapply(subset, function(x) anyNA(x))
  if (any(has_na)) {
    bad <- names(subset)[has_na]
    stop(
      "Subset vectors cannot contain NA. Invalid subset(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  
  subset
}


.get_variable_types <- function(data, xs) {
  out <- sapply(data[xs], function(z) class(z)[1])
  names(out) <- xs
  out
}


.build_variable_table <- function(xs,
                                  xs_type,
                                  xs_num,
                                  xs_chr,
                                  nfuns,
                                  cfuns,
                                  focal,
                                  transf.focal) {
  
  base <- data.frame(
    fun = rep("identity", length(xs)),
    x = xs,
    stringsAsFactors = FALSE
  )
  
  nfuns_df <- .prepare_funs(nfuns, xs_num, arg_name = "nfuns")
  cfuns_df <- .prepare_funs(cfuns, xs_chr, arg_name = "cfuns")
  
  X <- rbind(base, nfuns_df, cfuns_df)
  
  X$type <- xs_type[X$x]
  X$focal <- X$x %in% focal
  
  X <- .apply_focal_transformation_policy(
    X = X,
    focal = focal,
    transf.focal = transf.focal
  )
  
  X <- X[!duplicated(X[, c("fun", "x")]), ]
  rownames(X) <- NULL
  
  X
}


.prepare_funs <- function(funs, vars, arg_name = "funs") {
  if (is.null(funs) || length(vars) == 0) {
    return(NULL)
  }
  
  if (!is.list(funs)) {
    if (!is.character(funs)) {
      stop("`", arg_name, "` must be NULL, a character vector, or a named list.",
           call. = FALSE)
    }
    
    funs <- rep(list(funs), length(vars))
    names(funs) <- vars
  }
  
  if (is.null(names(funs)) || any(names(funs) == "")) {
    stop("When `", arg_name, "` is a list, it must be a named list.",
         call. = FALSE)
  }
  
  unknown_vars <- setdiff(names(funs), vars)
  if (length(unknown_vars) > 0) {
    stop(
      "Unknown variable(s) in `", arg_name, "`: ",
      paste(unknown_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  bad_elements <- !sapply(funs, is.character)
  if (any(bad_elements)) {
    stop(
      "All elements of `", arg_name, "` must be character vectors. Invalid element(s): ",
      paste(names(funs)[bad_elements], collapse = ", "),
      call. = FALSE
    )
  }
  
  out <- stack(funs)
  names(out) <- c("fun", "x")
  out <- .fac2char_safe(out)
  
  out
}


.apply_focal_transformation_policy <- function(X, focal, transf.focal) {
  if (length(focal) == 0) {
    return(X)
  }
  
  if (!is.logical(transf.focal)) {
    stop("`transf.focal` must be logical.", call. = FALSE)
  }
  
  if (!(length(transf.focal) %in% c(1, length(focal)))) {
    stop(
      "`transf.focal` must be a logical vector of length 1 or length equal to `focal`.",
      call. = FALSE
    )
  }
  
  if (length(transf.focal) == 1 && length(focal) > 1) {
    transf.focal <- rep(transf.focal, length(focal))
  }
  
  for (i in seq_along(focal)) {
    if (!isTRUE(transf.focal[i])) {
      X$fun[X$x == focal[i]] <- "identity"
    }
  }
  
  X
}


.validate_transform_functions <- function(funs) {
  funs <- unique(funs)
  funs <- setdiff(funs, "identity")
  
  if (length(funs) == 0) {
    return(invisible(TRUE))
  }
  
  exists_fun <- sapply(funs, function(f) exists(f, mode = "function"))
  
  if (!all(exists_fun)) {
    missing_funs <- funs[!exists_fun]
    stop(
      "Transformation function(s) not found: ",
      paste(missing_funs, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


.make_transformed_calls <- function(x, fun) {
  out <- character(length(x))
  
  for (i in seq_along(x)) {
    if (fun[i] == "identity") {
      out[i] <- x[i]
    } else {
      out[i] <- .make_call_safe(x[i], fun[i])
    }
  }
  
  out
}


.generate_model_terms <- function(X) {
  xu <- X$call
  names(xu) <- X$x
  
  forms <- unlist(
    lapply(seq_along(xu), function(i) {
      utils::combn(xu, i, simplify = FALSE)
    }),
    recursive = FALSE
  )
  
  no_duplicate_source_vars <- sapply(forms, function(f) {
    length(unique(names(f))) == length(names(f))
  })
  
  forms <- forms[no_duplicate_source_vars]
  
  forms
}


.filter_focal_terms <- function(forms, focal, focal_formula = NULL) {
  if (length(focal) == 0) {
    return(forms)
  }
  
  if (!is.null(focal_formula) && .has_ints(focal_formula)) {
    focal_int_vars <- .get_ints_vars_safe(focal_formula)
    
    form_has_int_vars <- sapply(forms, function(f) {
      all(focal_int_vars %in% names(f))
    })
    
    forms <- forms[form_has_int_vars]
    
    for (i in seq_along(forms)) {
      ff <- forms[[i]]
      
      int_term <- paste(ff[focal_int_vars], collapse = " * ")
      ff <- c(ff, int_term)
      
      forms[[i]] <- .expand_ints_safe(ff)
    }
    
    return(forms)
  }
  
  has_focal <- sapply(forms, function(f) {
    all(focal %in% names(f))
  })
  
  forms[has_focal]
}


.build_formula_calls <- function(y, forms) {
  if (length(forms) == 0) {
    stop("No model specifications were generated.", call. = FALSE)
  }
  
  rhs <- vapply(forms, paste, collapse = " + ", FUN.VALUE = character(1))
  calls <- paste(y, "~", rhs)
  
  unique(calls)
}


.build_scenarios <- function(calls, subset, max_scenarios = 10000) {
  scenarios <- expand.grid(
    calls = calls,
    subset = names(subset),
    stringsAsFactors = FALSE
  )
  
  if (nrow(scenarios) > max_scenarios) {
    stop(
      "The multiverse contains ",
      nrow(scenarios),
      " scenarios, exceeding `max_scenarios = ",
      max_scenarios,
      "`.",
      call. = FALSE
    )
  }
  
  scenarios$model <- paste0("model", seq_len(nrow(scenarios)))
  
  scenarios$formula_id <- match(scenarios$calls, calls)
  scenarios$subset_n <- sapply(scenarios$subset, function(s) sum(subset[[s]]))
  scenarios$n_terms <- sapply(scenarios$calls, function(z) {
    length(attr(stats::terms(stats::as.formula(z)), "term.labels"))
  })
  
  scenarios
}


.fit_models_safely <- function(scenarios,
                               subset,
                               data,
                               fit.fun,
                               fit.fun.args = NULL,
                               keep_failed = TRUE) {
  
  if (!is.function(fit.fun)) {
    stop("`fit.fun` must be a function, e.g. lm or glm.", call. = FALSE)
  }
  
  if (is.null(fit.fun.args)) {
    fit.fun.args <- list()
  }
  
  if (!is.list(fit.fun.args)) {
    stop("`fit.fun.args` must be NULL or a list.", call. = FALSE)
  }
  
  mods <- vector("list", nrow(scenarios))
  errors <- vector("list", nrow(scenarios))
  warnings <- vector("list", nrow(scenarios))
  fit_ok <- rep(FALSE, nrow(scenarios))
  
  names(mods) <- scenarios$model
  names(errors) <- scenarios$model
  names(warnings) <- scenarios$model
  
  for (i in seq_len(nrow(scenarios))) {
    warning_messages <- character(0)
    
    result <- tryCatch(
      withCallingHandlers(
        {
          datas <- data[subset[[scenarios$subset[i]]], , drop = FALSE]
          
          mm <- do.call(
            fit.fun,
            c(
              list(
                formula = stats::as.formula(scenarios$calls[i]),
                data = datas
              ),
              fit.fun.args
            )
          )
          
          if (!is.null(mm$call)) {
            mm$call$formula <- stats::as.formula(scenarios$calls[i])
          }
          
          mm
        },
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    
    warnings[[i]] <- if (length(warning_messages) == 0) {
      NA_character_
    } else {
      paste(unique(warning_messages), collapse = " | ")
    }
    
    if (inherits(result, "error")) {
      errors[[i]] <- conditionMessage(result)
      
      if (isTRUE(keep_failed)) {
        mods[[i]] <- NULL
      }
    } else {
      mods[[i]] <- result
      errors[[i]] <- NA_character_
      fit_ok[i] <- TRUE
    }
  }
  
  list(
    mods = mods,
    errors = errors,
    warnings = warnings,
    fit_ok = fit_ok
  )
}

.fac2char_safe <- function(x) {
  if (is.data.frame(x)) {
    for (j in seq_along(x)) {
      if (is.factor(x[[j]])) {
        x[[j]] <- as.character(x[[j]])
      }
    }
    return(x)
  }
  
  if (is.factor(x)) {
    return(as.character(x))
  }
  
  x
}


.make_id_safe <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}


.make_call_safe <- function(x, fun) {
  paste0(fun, "(", x, ")")
}


.has_ints <- function(formula) {
  if (is.null(formula)) {
    return(FALSE)
  }
  
  tl <- attr(stats::terms(formula), "term.labels")
  any(grepl(":", tl, fixed = TRUE))
}


.get_ints_vars_safe <- function(formula) {
  tl <- attr(stats::terms(formula), "term.labels")
  int_terms <- tl[grepl(":", tl, fixed = TRUE)]
  
  if (length(int_terms) == 0) {
    return(character(0))
  }
  
  unique(unlist(strsplit(int_terms, ":", fixed = TRUE)))
}


.expand_ints_safe <- function(terms) {
  terms <- unique(terms)
  
  expanded <- unlist(lapply(terms, function(z) {
    if (grepl(" \\* ", z)) {
      vars <- unlist(strsplit(z, " \\* "))
      int <- paste(vars, collapse = ":")
      c(vars, int)
    } else {
      z
    }
  }))
  
  unique(expanded)
}