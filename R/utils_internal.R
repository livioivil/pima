.trim <- function(x, n = 4) {
  k <- nrow(x)
  if (k <= 4) {
    print(head(x, n = n))
  } else {
    print(head(x, n = n), row.names = FALSE)
    cat("\n")
    cat("...\n")
    cat("\n")
    print(tail(x, n = n), row.names = FALSE)
  }
}

.get_info_models <- function(mods, extra = NULL) {
  info <- data.frame(
    model = names(mods),
    formula = sapply(mods, .get_formula),
    family = sapply(mods, .get_family),
    link = sapply(mods, .get_link)
  )
  
  # extract the predictors expanding interactions and sorting them alphabetically
  XS <- lapply(mods, function(x) .get_x_name(x))

  # here we keep the full set of variables and we sort them alphabetically
  # thus we can match them to each specification
  all_x <- unique(unname(unlist(XS)))
  all_x_clean <- .clean_x_name(all_x)
  names(all_x) <- all_x_clean

  ll <- max(sapply(XS, length))
  XS <- lapply(XS, function(x) {
    if (length(x) < ll) {
      c(x, rep(NA, ll - length(x)))
    } else {
      x
    }
  })

  # # then we create a list where missing elements receive an NA (is not present)
  # # in the specification and other receive the specific specification
  # XS <- lapply(XS, function(spec) replace(all_x, !all_x %in% spec, NA))

  XS <- data.frame(t(data.frame(XS)))
  nn <- colnames(XS)
  XS$model <- rownames(XS)

  XSL <- reshape(
    XS,
    direction = "long",
    idvar = c("model"),
    varying = list(nn),
    v.names = "value",
    timevar = "var"
  )

  XSL$var <- .clean_x_name(XSL$value)
  XSL <- XSL[!is.na(XSL$value), ]

  XS <- reshape(
    XSL,
    idvar = "model",
    timevar = "var",
    direction = "wide"
  )

  colnames(XS) <- gsub("value\\.", "", colnames(XS))

  # a predictor absent from a model is an explicit choice, not a missing value
  xs_cols <- setdiff(colnames(XS), "model")
  XS[xs_cols] <- lapply(XS[xs_cols], function(v) ifelse(is.na(v), "excluded", v))

  info <- merge(info, XS, by = "model")

  if (names(info[, 3, drop = FALSE]) == "model") {
    info = info[, -3]
  }
  
  if(!is.null(extra)){
    if(!"model" %in% names(extra)){
      # TODO check this, probably flipscores change model order
      extra$model <- info$model
      warning("the extra data.frame does not have a `model` column (that is necessary) assigning the default ones that could be wrong!")
    }
    extra_names <- names(extra)[names(extra) != "model"]
  } else{
    extra_names <- NULL
  }
  
  extra_names <- c(extra_names, "family", "link")
  if(!is.null(extra)){
    info <- merge(info, extra, by = "model")
  }
  extra <- info[, c("model", extra_names)]
  attr(info, "extra") <- extra_names
  attr(info, "xs") <- unique(XSL$var)
  list(extra = extra, info = info)
}

.get_formula <- function(x) {
  # with formula.tools
  # formula.tools:::as.character.formula(x$formula)
  # without formula.tools
  Reduce(paste, deparse(x$formula))
}

.get_response_name <- function(mods) {
  vapply(
    mods,
    function(x) {
      f <- stats::formula(x)
      if (length(f) < 3L) {
        return(NA_character_)
      }
      paste(deparse(f[[2L]], width.cutoff = 500L, backtick = TRUE), collapse = "")
    },
    character(1)
  )
}

.get_x_name  <- function(x) {
  c(ifelse(attr(x$terms, which = "intercept"),"(Intercept)",NULL), 
    attr(x$terms, which = "term.labels"))
}

.keep_bare_x_name <- function(x) {
  match_par <- gregexpr("(?<=\\().+?(?=\\))", x, perl = TRUE)
  x_clean <- ifelse(unlist(match_par) == -1, x, regmatches(x, match_par))
  x_clean <- unlist(x_clean)
  # TODO check for extra operators such as ^
  x_clean <- sub("(.+?)\\s*[\\,\\+\\-\\^\\*/]\\s*.+", "\\1", x_clean)
  x_clean <- gsub("\\s*", "", unname(x_clean)) # TODO check this for ``
  return(x_clean)
}

.clean_x_name <- function(x) {
  xp <- strsplit(x, ":") # splitting interactions
  xp <- sapply(xp, .keep_bare_x_name)
  xp <- sapply(xp, paste0, collapse = ":")
  return(xp)
}

.get_fn_args <- function(f, new.args = NULL, exclude = c("...")) {
  f_args <- formals(f)
  if (!is.null(new.args)) {
    f_args[names(new.args)] <- new.args
  }
  f_args <- f_args[!names(f_args) %in% exclude]
  f_args
}

clapply <- function(x, class, FUN, ...) {
  stopifnot(inherits(x, c("list", "data.frame")))
  res <- lapply(x, function(cc) {
    if (inherits(cc, class)) {
      FUN(cc, ...)
    } else {
      cc
    }
  })
  if (inherits(x, "data.frame")) {
    res <- data.frame(res)
  }
  res
}

# TODO check if better using a different approach
as.glm <- function(x) {
  if (inherits(x, "glm")) {
    return(x)
  }
  
  if (!inherits(x, "lm")) {
    return(x)
  }
  
  cl <- getCall(x)
  cl[[1L]] <- quote(stats::glm)
  
  if (is.null(cl$family)) {
    cl$family <- quote(stats::gaussian())
  }
  
  eval(cl, envir = environment(stats::formula(x)))
}

.get_family <- function(x){
  x$family$family
}

.get_link <- function(x){
  x$family$link
}

.rpos <- function(x, p) {
  r <- range(x, na.rm = TRUE)
  r[1L] + diff(r) * p
}

.get_glm_summary <- function(x){
  ss <- data.frame(summary(x)$coefficients)
  ss$coefficient <- rownames(ss)
  rownames(ss) <- NULL
  names(ss) <- c("estimate", "se", "wald.t", "wald.p", "coefficient")
  ss[, c("coefficient", "estimate", "se", "wald.t", "wald.p")]
}

.similar_p <- function(x, k = 1, tol = sqrt(.Machine$double.eps)) {
  stopifnot(k >= 1)
  diff(range(x, na.rm = TRUE)) <= log10(k) + tol
}

.bind_rows <- function(x, id = NULL) {
  if (!is.null(id)) {
    x <- Map(function(df, nm) cbind(stats::setNames(data.frame(nm), id), df),
             x, names(x))
  }
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

# A predictor excluded from a specification is coded as "excluded". Older
# `pima` objects code it as NA: accept both.
.is_excluded <- function(x) {
  is.na(x) | x == "excluded"
}

# Check `tested_coeffs` before calling flipscores, which fails with obscure
# errors when a model has nothing to test. Names are compared without spaces,
# as flipscores does.
#   - vector: tested in each model where present, so every model must contain
#     at least one of them;
#   - list: one element per model (matched by name when named), each with
#     coefficients of that model.
# Returns `tested_coeffs`, with a named list reordered as `mods`.
.check_tested_coeffs <- function(mods, tested_coeffs) {
  if (is.null(tested_coeffs)) {
    return(tested_coeffs)
  }
  mod_names <- names(mods)
  if (is.null(mod_names)) mod_names <- paste0("model", seq_along(mods))
  coefs <- lapply(mods, function(m) gsub(" ", "", names(stats::coef(m))))
  clean <- function(x) gsub(" ", "", x)
  show <- function(x, n = 3) {
    paste0(paste(x[seq_len(min(n, length(x)))], collapse = ", "), if (length(x) > n) ", ..." else "")
  }
  hint <- paste0(
    "Use a list with one element per model, e.g. for all the coefficients ",
    "of a variable:\n  tab <- spec_table(mods, focal.var = \"x\")\n",
    "  split(tab$coefficient, factor(tab$model, levels = names(mods)))"
  )

  if (!is.list(tested_coeffs)) {
    none <- !vapply(coefs, function(cf) any(clean(tested_coeffs) %in% cf), logical(1))
    if (any(none)) {
      i <- which(none)[1L]
      stop(sprintf(
        "%s of the %s models contain none of `tested_coeffs` (%s), e.g. %s (coefficients: %s).\n%s",
        sum(none), length(mods), show(tested_coeffs), mod_names[i],
        show(names(stats::coef(mods[[i]])), 6), hint
      ), call. = FALSE)
    }
    return(tested_coeffs)
  }

  if (length(tested_coeffs) != length(mods)) {
    stop(sprintf("`tested_coeffs` is a list of length %s but there are %s models: it needs one element per model.",
                 length(tested_coeffs), length(mods)), call. = FALSE)
  }
  if (!is.null(names(tested_coeffs)) && !is.null(names(mods))) {
    if (!setequal(names(tested_coeffs), names(mods))) {
      stop(sprintf("the names of `tested_coeffs` must be the names of the models (e.g. %s).",
                   show(names(mods))), call. = FALSE)
    }
    tested_coeffs <- tested_coeffs[names(mods)]
  }

  empty <- lengths(tested_coeffs) == 0L
  if (any(empty)) {
    stop(sprintf("%s models have no tested coefficients in `tested_coeffs`, e.g. %s.",
                 sum(empty), show(mod_names[empty])), call. = FALSE)
  }
  miss <- Map(function(tc, cf) tc[!clean(tc) %in% cf], tested_coeffs, coefs)
  bad <- lengths(miss) > 0L
  if (any(bad)) {
    i <- which(bad)[1L]
    stop(sprintf(
      "%s models have `tested_coeffs` that are not among their coefficients, e.g. %s: %s (coefficients: %s).",
      sum(bad), mod_names[i], show(miss[[i]]), show(names(stats::coef(mods[[i]])), 6)
    ), call. = FALSE)
  }
  tested_coeffs
}

# default colour of the points when a plot has a single colour group
.single_color <- "#0072B2"
