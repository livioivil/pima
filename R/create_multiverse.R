# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

.check_formula <- function(formula, data) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  
  tt <- stats::terms(formula)
  term_labels <- attr(tt, "term.labels")
  
  if (length(term_labels) == 0L) {
    stop("`formula` must contain at least one predictor.", call. = FALSE)
  }
  
  bare_terms <- vapply(
    term_labels,
    function(term) {
      expr <- tryCatch(str2lang(term), error = function(e) NULL)
      is.symbol(expr) && term != "."
    },
    logical(1)
  )
  
  if (!all(bare_terms)) {
    stop(
      paste0(
        "`formula` must contain only additive bare predictors. ",
        "Transformations, interactions, and `.` are currently not supported."
      ),
      call. = FALSE
    )
  }
  
  rhs <- if (length(formula) == 2L) formula[[2L]] else formula[[3L]]
  predictors <- all.vars(rhs)
  
  missing_vars <- setdiff(all.vars(formula), names(data))
  
  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in `data`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  list(
    terms = tt,
    predictors = predictors
  )
}


.check_focal <- function(focal, predictors, transf.focal) {
  if (is.null(focal)) {
    focal <- character(0)
  }
  
  if (!is.character(focal)) {
    stop("`focal` must be a character vector.", call. = FALSE)
  }
  
  if (anyNA(focal) || any(focal == "")) {
    stop("`focal` cannot contain missing or empty values.", call. = FALSE)
  }
  
  if (anyDuplicated(focal)) {
    stop("`focal` cannot contain duplicated predictor names.", call. = FALSE)
  }
  
  unknown_focal <- setdiff(focal, predictors)
  
  if (length(unknown_focal) > 0L) {
    stop(
      paste0(
        "Focal predictors not found in the right-hand side of `formula`: ",
        paste(unknown_focal, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if (!is.logical(transf.focal) || anyNA(transf.focal)) {
    stop(
      "`transf.focal` must be a logical vector without missing values.",
      call. = FALSE
    )
  }
  
  if (length(focal) == 0L) {
    return(
      list(
        focal = focal,
        transf.focal = stats::setNames(logical(0), character(0))
      )
    )
  }
  
  if (
    length(transf.focal) != 1L &&
    length(transf.focal) != length(focal)
  ) {
    stop(
      "`transf.focal` must have length 1 or the same length as `focal`.",
      call. = FALSE
    )
  }
  
  if (length(transf.focal) == 1L) {
    transf.focal <- rep(transf.focal, length(focal))
  }
  
  names(transf.focal) <- focal
  
  list(
    focal = focal,
    transf.focal = transf.focal
  )
}


.normalize_funs <- function(funs, vars, arg_name) {
  out <- stats::setNames(vector("list", length(vars)), vars)
  
  if (length(vars) == 0L || is.null(funs) || length(funs) == 0L) {
    return(out)
  }
  
  validate_funs <- function(x) {
    if (!is.character(x)) {
      stop(
        sprintf("All transformations in `%s` must be character vectors.", arg_name),
        call. = FALSE
      )
    }
    
    if (anyNA(x) || any(x == "")) {
      stop(
        sprintf("`%s` cannot contain missing or empty function names.", arg_name),
        call. = FALSE
      )
    }
    
    if ("excluded" %in% x) {
      stop(
        sprintf("`excluded` is reserved and cannot be used in `%s`.", arg_name),
        call. = FALSE
      )
    }
    
    unique(x)
  }
  
  # Same transformations for all eligible variables.
  if (!is.list(funs)) {
    funs <- validate_funs(funs)
    
    for (x in vars) {
      out[[x]] <- funs
    }
    
    return(out)
  }
  
  # Variable-specific transformations.
  if (
    is.null(names(funs)) ||
    anyNA(names(funs)) ||
    any(names(funs) == "")
  ) {
    stop(sprintf("`%s` must be a named list.", arg_name), call. = FALSE)
  }
  
  if (anyDuplicated(names(funs))) {
    stop(
      sprintf("`%s` cannot contain duplicated variable names.", arg_name),
      call. = FALSE
    )
  }
  
  unknown_vars <- setdiff(names(funs), vars)
  
  if (length(unknown_vars) > 0L) {
    stop(
      sprintf(
        "Variables in `%s` are not eligible predictors: %s",
        arg_name,
        paste(unknown_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  for (x in names(funs)) {
    if (is.null(funs[[x]])) {
      next
    }
    
    out[[x]] <- validate_funs(funs[[x]])
  }
  
  out
}


.normalize_subset <- function(subset, n) {
  if (is.null(subset) || length(subset) == 0L) {
    return(list(all = rep(TRUE, n)))
  }
  
  if (!is.list(subset)) {
    stop("`subset` must be a named list of logical vectors.", call. = FALSE)
  }
  
  if (
    is.null(names(subset)) ||
    anyNA(names(subset)) ||
    any(names(subset) == "")
  ) {
    stop("`subset` must be a named list.", call. = FALSE)
  }
  
  if (anyDuplicated(names(subset))) {
    stop("`subset` cannot contain duplicated names.", call. = FALSE)
  }
  
  if ("all" %in% names(subset)) {
    stop("`subset` cannot contain an element named `all`.", call. = FALSE)
  }
  
  correct_length <- vapply(subset, length, integer(1)) == n
  
  if (!all(correct_length)) {
    stop(
      "All elements of `subset` must have length `nrow(data)`.",
      call. = FALSE
    )
  }
  
  is_logical <- vapply(subset, is.logical, logical(1))
  
  if (!all(is_logical)) {
    stop("All elements of `subset` must be logical vectors.", call. = FALSE)
  }
  
  has_na <- vapply(subset, anyNA, logical(1))
  
  if (any(has_na)) {
    stop("Elements of `subset` cannot contain missing values.", call. = FALSE)
  }
  
  c(list(all = rep(TRUE, n)), subset)
}


.normalize_fit_specs <- function(fit.specs) {
  if (is.null(fit.specs) || length(fit.specs) == 0L) {
    return(NULL)
  }
  
  if (!is.list(fit.specs)) {
    stop("`fit.specs` must be a named list.", call. = FALSE)
  }
  
  if (
    is.null(names(fit.specs)) ||
    anyNA(names(fit.specs)) ||
    any(names(fit.specs) == "")
  ) {
    stop("`fit.specs` must be a named list.", call. = FALSE)
  }
  
  if (anyDuplicated(names(fit.specs))) {
    stop("`fit.specs` cannot contain duplicated names.", call. = FALSE)
  }
  
  for (name in names(fit.specs)) {
    spec <- fit.specs[[name]]
    
    # Shorthand: passing a fitting function directly is equivalent to a
    # standard fit specification with no additional arguments.
    if (is.function(spec)) {
      spec <- structure(
        list(
          fun = spec,
          adapter = NULL,
          args = list()
        ),
        class = "pima.fitspec"
      )
    }
    
    if (!is.list(spec)) {
      stop(
        sprintf(
          paste0(
            "`fit.specs[[\"%s\"]]` must be a fitting function or ",
            "a fitting specification created with `create_fit()`."
          ),
          name
        ),
        call. = FALSE
      )
    }
    
    # Backward compatibility with manually constructed specifications.
    if (is.null(spec$args)) {
      spec$args <- list()
    }
    
    has_fun <- !is.null(spec$fun)
    has_adapter <- !is.null(spec$adapter)
    
    if (!has_fun && !has_adapter) {
      stop(
        sprintf(
          paste0(
            "`fit.specs[[\"%s\"]]` must contain either `fun` ",
            "or `adapter`."
          ),
          name
        ),
        call. = FALSE
      )
    }
    
    if (has_fun && has_adapter) {
      stop(
        sprintf(
          paste0(
            "`fit.specs[[\"%s\"]]` cannot contain both `fun` ",
            "and `adapter`."
          ),
          name
        ),
        call. = FALSE
      )
    }
    
    if (has_fun && !is.function(spec$fun)) {
      stop(
        sprintf(
          "`fit.specs[[\"%s\"]]$fun` must be a function.",
          name
        ),
        call. = FALSE
      )
    }
    
    if (has_adapter && !is.function(spec$adapter)) {
      stop(
        sprintf(
          "`fit.specs[[\"%s\"]]$adapter` must be a function.",
          name
        ),
        call. = FALSE
      )
    }
    
    if (!is.list(spec$args)) {
      stop(
        sprintf(
          "`fit.specs[[\"%s\"]]$args` must be a list.",
          name
        ),
        call. = FALSE
      )
    }
    
    if (length(spec$args) > 0L) {
      if (
        is.null(names(spec$args)) ||
        anyNA(names(spec$args)) ||
        any(names(spec$args) == "")
      ) {
        stop(
          sprintf(
            "`fit.specs[[\"%s\"]]$args` must be a named list.",
            name
          ),
          call. = FALSE
        )
      }
      
      if (anyDuplicated(names(spec$args))) {
        stop(
          sprintf(
            "`fit.specs[[\"%s\"]]$args` cannot contain duplicated names.",
            name
          ),
          call. = FALSE
        )
      }
      
      reserved <- intersect(names(spec$args), c("formula", "data"))
      
      if (length(reserved) > 0L) {
        stop(
          sprintf(
            "`fit.specs[[\"%s\"]]$args` cannot contain: %s",
            name,
            paste(reserved, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    
    if (has_fun) {
      fun_formals <- names(formals(spec$fun))
      
      if (!all(c("formula", "data") %in% fun_formals)) {
        stop(
          sprintf(
            paste0(
              "The fitting function in `fit.specs[[\"%s\"]]` must ",
              "explicitly accept `formula` and `data`. Use an adapter for ",
              "functions with a different interface."
            ),
            name
          ),
          call. = FALSE
        )
      }
    }
    
    if (has_adapter) {
      adapter_formals <- names(formals(spec$adapter))
      required <- c("formula", "data", "args")
      
      if (!all(required %in% adapter_formals)) {
        stop(
          sprintf(
            paste0(
              "The adapter in `fit.specs[[\"%s\"]]` must explicitly ",
              "accept `formula`, `data`, and `args`."
            ),
            name
          ),
          call. = FALSE
        )
      }
    }
    
    fit.specs[[name]] <- spec
  }
  
  fit.specs
}


.make_id <- function(x) {
  match(x, unique(x))
}


.make_name_call <- function(x) {
  paste(
    deparse(as.name(x), width.cutoff = 500L, backtick = TRUE),
    collapse = ""
  )
}


.make_call <- function(x, fun) {
  fun_expr <- tryCatch(
    str2lang(fun),
    error = function(e) NULL
  )
  
  valid_fun <- is.symbol(fun_expr) ||
    (
      is.call(fun_expr) &&
        as.character(fun_expr[[1L]]) %in% c("::", ":::")
    )
  
  if (!valid_fun) {
    stop(
      sprintf("Invalid transformation function name: `%s`.", fun),
      call. = FALSE
    )
  }
  
  expr <- as.call(list(fun_expr, as.name(x)))
  
  paste(
    deparse(expr, width.cutoff = 500L, backtick = TRUE),
    collapse = ""
  )
}


.make_variable_specs <- function(predictors,
                                 data,
                                 focal,
                                 transf.focal,
                                 nfuns,
                                 cfuns) {
  is_num <- vapply(data[predictors], is.numeric, logical(1))
  xs_num <- predictors[is_num]
  xs_nonnum <- predictors[!is_num]
  
  xs_type <- vapply(
    data[predictors],
    function(x) class(x)[1L],
    character(1)
  )
  
  nfuns <- .normalize_funs(nfuns, xs_num, "nfuns")
  cfuns <- .normalize_funs(cfuns, xs_nonnum, "cfuns")
  
  specs <- lapply(
    predictors,
    function(x) {
      funs <- if (x %in% xs_num) nfuns[[x]] else cfuns[[x]]
      funs <- unique(c("identity", funs))
      
      if (x %in% focal && !transf.focal[[x]]) {
        funs <- "identity"
      }
      
      data.frame(
        fun = funs,
        x = x,
        type = xs_type[[x]],
        focal = x %in% focal,
        stringsAsFactors = FALSE
      )
    }
  )
  
  out <- do.call(rbind, specs)
  rownames(out) <- NULL
  
  out$.id_fun <- .make_id(out$fun)
  out$.id_x <- .make_id(out$x)
  
  out$call <- vapply(
    seq_len(nrow(out)),
    function(i) {
      if (out$fun[i] == "identity") {
        .make_name_call(out$x[i])
      } else {
        .make_call(out$x[i], out$fun[i])
      }
    },
    character(1)
  )
  
  out
}


.make_formula_grid <- function(formula,
                               predictor_specs,
                               focal,
                               terms_object) {
  predictors <- unique(predictor_specs$x)
  decision_names <- paste0("spec_", predictors)
  
  choices <- lapply(
    predictors,
    function(x) {
      funs <- predictor_specs$fun[predictor_specs$x == x]
      
      if (!(x %in% focal)) {
        funs <- c("excluded", funs)
      }
      
      unique(funs)
    }
  )
  
  decisions <- do.call(
    expand.grid,
    c(
      unname(choices),
      list(
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )
  
  names(decisions) <- decision_names
  
  # The empty model is not part of the multiverse.
  decisions <- decisions[
    rowSums(decisions != "excluded") > 0L,
    ,
    drop = FALSE
  ]
  rownames(decisions) <- NULL
  
  rhs <- vapply(
    seq_len(nrow(decisions)),
    function(i) {
      terms_i <- vapply(
        seq_along(predictors),
        function(j) {
          x <- predictors[j]
          fun <- decisions[[decision_names[j]]][i]
          
          if (fun == "excluded") {
            return(NA_character_)
          }
          
          if (fun == "identity") {
            return(.make_name_call(x))
          }
          
          .make_call(x, fun)
        },
        character(1)
      )
      
      paste(terms_i[!is.na(terms_i)], collapse = " + ")
    },
    character(1)
  )
  
  if (attr(terms_object, "intercept") == 0L) {
    rhs <- paste("0 +", rhs)
  }
  
  if (length(formula) == 2L) {
    calls <- paste("~", rhs)
  } else {
    lhs <- paste(
      deparse(
        formula[[2L]],
        width.cutoff = 500L,
        backtick = TRUE
      ),
      collapse = ""
    )
    calls <- paste(lhs, "~", rhs)
  }
  
  data.frame(
    formula_id = paste0("formula", seq_len(nrow(decisions))),
    calls = unname(calls),
    decisions,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


.make_scenarios <- function(formula_specs, subset, fit.specs) {
  scenario_grid <- list(
    formula_row = seq_len(nrow(formula_specs)),
    subset = names(subset)
  )
  
  if (!is.null(fit.specs)) {
    scenario_grid$fit_spec <- names(fit.specs)
  }
  
  scenario_grid <- do.call(
    expand.grid,
    c(
      scenario_grid,
      list(
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )
  
  scenarios <- formula_specs[
    scenario_grid$formula_row,
    ,
    drop = FALSE
  ]
  
  scenarios$subset <- scenario_grid$subset
  
  if (!is.null(fit.specs)) {
    scenarios$fit_spec <- scenario_grid$fit_spec
  }
  
  rownames(scenarios) <- NULL
  scenarios$model <- paste0("model", seq_len(nrow(scenarios)))
  
  scenarios[
    , c("model", setdiff(names(scenarios), "model")),
    drop = FALSE
  ]
}


.fit_multiverse <- function(scenarios,
                            data,
                            subset,
                            fit.specs,
                            formula_env) {
  mods <- vector("list", nrow(scenarios))
  names(mods) <- scenarios$model
  
  fit_ok <- logical(nrow(scenarios))
  fit_error <- rep(NA_character_, nrow(scenarios))
  
  for (i in seq_len(nrow(scenarios))) {
    datas <- data[
      subset[[scenarios$subset[i]]],
      ,
      drop = FALSE
    ]
    
    model_formula <- stats::as.formula(
      scenarios$calls[i],
      env = formula_env
    )
    
    spec <- fit.specs[[scenarios$fit_spec[i]]]
    
    mods[[i]] <- tryCatch(
      {
        if (!is.null(spec$adapter)) {
          spec$adapter(
            formula = model_formula,
            data = datas,
            args = spec$args
          )
        } else {
          do.call(
            spec$fun,
            c(
              list(
                formula = model_formula,
                data = datas
              ),
              spec$args
            )
          )
        }
      },
      error = function(e) e
    )
    
    fit_ok[i] <- !inherits(mods[[i]], "error")
    
    if (!fit_ok[i]) {
      fit_error[i] <- conditionMessage(mods[[i]])
    }
  }
  
  scenarios$fit_ok <- fit_ok
  scenarios$fit_error <- fit_error
  
  list(
    mods = mods,
    scenarios = scenarios
  )
}


# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Create scenarios for a multiverse analysis
#'
#' `create_multi()` generates a multiverse by crossing admissible analytical
#' decisions about predictor inclusion and transformation with optional sample
#' subsets and model-fitting specifications.
#'
#' @param formula A formula describing the maximal model. Predictors on the
#'   right-hand side must be bare variable names combined additively.
#'   Transformations, interactions, and `.` are currently not supported in
#'   `formula`. Transformations should instead be supplied through `nfuns` or
#'   `cfuns`. Both one-sided formulas (e.g. `~ x + z`) and two-sided formulas
#'   (e.g. `y ~ x + z`) are supported. Whether a one-sided formula can be fitted
#'   depends on the selected fitting function or adapter.
#' @param data A data frame containing all variables referenced in `formula`.
#' @param focal Optional character vector naming predictors that must be
#'   included in every formula specification. By default focal predictors are
#'   not transformed; use `transf.focal` to allow their transformations.
#' @param nfuns Optional transformations for numeric predictors. Supply either
#'   a character vector of function names to apply to every numeric predictor
#'   (e.g. `c("log", "sqrt")`) or a named list for variable-specific choices
#'   (e.g. `list(x = "log", z = c("log", "sqrt"))`). Functions are supplied
#'   as character strings and are applied to one predictor at a time.
#' @param cfuns Same as `nfuns`, but for non-numeric predictors.
#' @param transf.focal Logical indicating whether transformations should also
#'   be considered for focal predictors. It can have length 1 or the same
#'   length as `focal`.
#' @param subset Optional named list of logical vectors defining additional row
#'   subsets. Each vector must have length `nrow(data)` and contain no missing
#'   values. The full dataset is always added as the reserved subset `"all"`.
#' @param fit.specs Optional named list of model-fitting specifications. Each
#'   element can be either a fitting function that explicitly accepts `formula`
#'   and `data`, such as `stats::lm` or `stats::glm`, or a specification created
#'   with [create_fit()]. Passing a function directly is shorthand for a standard
#'   specification with no additional arguments. Use [create_fit()] when
#'   additional fitting arguments are needed or when an adapter is required for
#'   a non-standard fitting interface. Each fitting specification is crossed with
#'   every formula and subset specification. Manually constructed lists with the
#'   same internal structure are also accepted for backward compatibility.
#'
#' @details
#' Predictor choices are generated as a Cartesian product. For a non-focal
#' predictor the available decisions are exclusion, identity, and any supplied
#' transformations. A focal predictor cannot be excluded and can only be
#' transformed when permitted by `transf.focal`.
#'
#' The analytical decisions used to construct each formula are retained
#' explicitly. In `formula_specs` and `scenarios`, each predictor has a column
#' named `spec_<predictor>` whose value is `"excluded"`, `"identity"`, or the
#' name of the selected transformation. This makes downstream analyses of
#' specification choices possible without parsing formula strings.
#'
#' When `fit.specs` is supplied, fitting errors do not stop the entire
#' multiverse. The corresponding element of `mods` contains the error object,
#' while `scenarios$fit_ok` and `scenarios$fit_error` record its status.
#' Warnings emitted by fitting functions are handled normally by R and are not
#' stored in the returned object.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{variables}{A data frame describing all available predictor-level
#'     specifications and their corresponding formula calls.}
#'   \item{formula_specs}{A data frame containing one row per admissible formula,
#'     its `formula_id`, executable formula string in `calls`, and one
#'     `spec_<predictor>` column per predictor describing the selected analytical
#'     decision.}
#'   \item{calls}{A character vector containing all admissible formula strings.}
#'   \item{subset}{A named list of logical row selectors, including `all`.}
#'   \item{scenarios}{A data frame containing the Cartesian product of formula
#'     specifications, subsets, and, when supplied, fitting specifications.
#'     Each row has a unique `model` identifier. If models are fitted, this also
#'     contains `fit_ok` and `fit_error`.}
#'   \item{fit.specs}{The normalized fitting specifications, returned only when
#'     `fit.specs` is supplied.}
#'   \item{mods}{A named list of fitted model objects, returned only when
#'     `fit.specs` is supplied. Failed fits are stored as error objects.}
#' }
#'
#' @seealso [create_fit()]
#' @export
#'
#' @examples
#' # Generate formula specifications without fitting models.
#' m1 <- create_multi(
#'   ~ Sepal.Length + Petal.Width + Species,
#'   data = iris,
#'   focal = "Sepal.Length",
#'   nfuns = "log"
#' )
#'
#' m1$formula_specs
#'
#' # Allow transformations of the focal predictor and cross formulas with
#' # alternative fitting specifications.
#' m2 <- create_multi(
#'   Sepal.Width ~ Sepal.Length + Petal.Width,
#'   data = iris,
#'   focal = "Sepal.Length",
#'   nfuns = list(
#'     Sepal.Length = "log",
#'     Petal.Width = c("log", "sqrt")
#'   ),
#'   transf.focal = TRUE,
#'   fit.specs = list(
#'     lm = stats::lm,
#'     gaussian = stats::glm,
#'     poisson = create_fit(
#'       stats::glm,
#'       family = stats::poisson(link = "log")
#'     )
#'   )
#' )
#'
#' m2$scenarios
#'
create_multi <- function(formula,
                         data,
                         focal = NULL,
                         nfuns = NULL,
                         cfuns = NULL,
                         transf.focal = FALSE,
                         subset = NULL,
                         fit.specs = NULL) {
  formula_info <- .check_formula(formula, data)
  
  focal_info <- .check_focal(
    focal = focal,
    predictors = formula_info$predictors,
    transf.focal = transf.focal
  )
  
  subset <- .normalize_subset(subset, nrow(data))
  fit.specs <- .normalize_fit_specs(fit.specs)
  
  variables <- .make_variable_specs(
    predictors = formula_info$predictors,
    data = data,
    focal = focal_info$focal,
    transf.focal = focal_info$transf.focal,
    nfuns = nfuns,
    cfuns = cfuns
  )
  
  formula_specs <- .make_formula_grid(
    formula = formula,
    predictor_specs = variables,
    focal = focal_info$focal,
    terms_object = formula_info$terms
  )
  
  scenarios <- .make_scenarios(
    formula_specs = formula_specs,
    subset = subset,
    fit.specs = fit.specs
  )
  
  out <- list(
    variables = variables,
    formula_specs = formula_specs,
    calls = formula_specs$calls,
    subset = subset,
    scenarios = scenarios
  )
  
  models <- NULL
  
  if (!is.null(fit.specs)) {
    
    fit <- .fit_multiverse(
      scenarios = scenarios,
      data = data,
      subset = subset,
      fit.specs = fit.specs,
      formula_env = environment(formula)
    )
    
    models <- fit$mods
    scenarios <- fit$scenarios
  }
  
  out <- list(
    scenarios = scenarios,
    models = models,
    formula_specs = formula_specs,
    k = nrow(scenarios),
    specification = list(
      formula = formula,
      focal = focal_info$focal,
      nfuns = nfuns,
      cfuns = cfuns,
      transf.focal = focal_info$transf.focal,
      subset = subset,
      fit.specs = fit.specs,
      variables = variables
    ),
    call = match.call()
  )
  
  class(out) <- c("pima.multi", "list")
  
  out
}

#' Create a model-fitting specification
#'
#' `create_fit()` defines how a model should be fitted within a multiverse.
#' Standard model-fitting functions are supplied through `fun`; functions with
#' non-standard interfaces can be supported through `adapter`.
#'
#' @param fun Optional model-fitting function that explicitly accepts `formula`
#'   and `data`, such as `stats::lm`, `stats::glm`, `MASS::glm.nb`, or
#'   `lme4::lmer`. Supply either `fun` or `adapter`, but not both.
#' @param ... Additional named arguments for the fitting specification. With a
#'   standard `fun`, these arguments are passed directly to the fitting
#'   function. With an `adapter`, they are collected into the `args` list passed
#'   to the adapter.
#' @param adapter Optional function for fitting models that do not use the
#'   standard `formula` and `data` interface. An adapter must explicitly accept
#'   arguments named `formula`, `data`, and `args`, where `args` is the named
#'   list created from `...`. Because `adapter` follows `...`, it must be
#'   supplied by name.
#'
#' @details
#' Additional arguments must be named. `formula` and `data` are reserved because
#' they are supplied by [create_multi()] for each scenario.
#'
#' For a standard fitting function, arguments in `...` that are not explicitly
#' listed in the function formals generate a warning when the function accepts
#' `...`, because they may be forwarded to another method. If the function does
#' not accept `...`, unknown arguments generate an error.
#'
#' @return An object of class `"pima.fitspec"` containing the fitting function
#'   or adapter and its additional arguments. The object is intended for use in
#'   the `fit.specs` argument of [create_multi()].
#'
#' @seealso [create_multi()]
#' @export
#'
#' @examples
#' create_fit(stats::lm)
#'
#' create_fit(
#'   stats::glm,
#'   family = stats::poisson(link = "log")
#' )
#'
#' # Adapter interface for a non-standard fitting function.
#' lm_adapter <- function(formula, data, args) {
#'   do.call(
#'     stats::lm,
#'     c(list(formula = formula, data = data), args)
#'   )
#' }
#'
#' create_fit(
#'   adapter = lm_adapter,
#'   singular.ok = TRUE
#' )
#'
create_fit <- function(fun = NULL, ..., adapter = NULL) {
  if (is.null(fun) && is.null(adapter)) {
    stop(
      "Either `fun` or `adapter` must be supplied.",
      call. = FALSE
    )
  }
  
  if (!is.null(fun) && !is.null(adapter)) {
    stop(
      "Supply either `fun` or `adapter`, not both.",
      call. = FALSE
    )
  }
  
  if (!is.null(fun) && !is.function(fun)) {
    stop("`fun` must be a function.", call. = FALSE)
  }
  
  if (!is.null(adapter) && !is.function(adapter)) {
    stop("`adapter` must be a function.", call. = FALSE)
  }
  
  args <- list(...)
  
  if (length(args) > 0L) {
    if (
      is.null(names(args)) ||
      anyNA(names(args)) ||
      any(names(args) == "")
    ) {
      stop("All additional arguments must be named.", call. = FALSE)
    }
    
    if (anyDuplicated(names(args))) {
      stop(
        "Additional arguments cannot have duplicated names.",
        call. = FALSE
      )
    }
    
    reserved <- intersect(names(args), c("formula", "data"))
    
    if (length(reserved) > 0L) {
      stop(
        "`formula` and `data` cannot be supplied to `create_fit()`.",
        call. = FALSE
      )
    }
  }
  
  if (!is.null(fun)) {
    fun_name <- paste(deparse(substitute(fun)), collapse = "")
    fun_formals <- names(formals(fun))
    
    if (!all(c("formula", "data") %in% fun_formals)) {
      stop(
        sprintf(
          paste0(
            "`%s` must explicitly accept `formula` and `data` to be used ",
            "as a standard fitting function. Use `adapter` for functions ",
            "with a different interface."
          ),
          fun_name
        ),
        call. = FALSE
      )
    }
    
    if (length(args) > 0L) {
      has_dots <- "..." %in% fun_formals
      explicit_args <- setdiff(fun_formals, "...")
      unknown <- setdiff(names(args), explicit_args)
      
      if (length(unknown) > 0L) {
        if (has_dots) {
          warning(
            sprintf(
              paste0(
                "The following arguments are not explicitly declared by ",
                "`%s` and will be passed through `...`: %s. Check that ",
                "they are supported by the fitting method."
              ),
              fun_name,
              paste(unknown, collapse = ", ")
            ),
            call. = FALSE
          )
        } else {
          stop(
            sprintf(
              "The following arguments are not accepted by `%s`: %s",
              fun_name,
              paste(unknown, collapse = ", ")
            ),
            call. = FALSE
          )
        }
      }
    }
  }
  
  if (!is.null(adapter)) {
    adapter_formals <- names(formals(adapter))
    required <- c("formula", "data", "args")
    
    if (!all(required %in% adapter_formals)) {
      stop(
        "`adapter` must explicitly accept `formula`, `data`, and `args`.",
        call. = FALSE
      )
    }
  }
  
  structure(
    list(
      fun = fun,
      adapter = adapter,
      args = args
    ),
    class = "pima.fitspec"
  )
}