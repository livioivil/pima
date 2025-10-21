.get_all_Tspace <- function(mods) {
  tab = lapply(mods, function(x) {
    if (is.null(x$Tspace)) {
      stop("At least one Tspace is missing")
    }
    x$Tspace
  })
  nms = sapply(tab, colnames)
  tab = do.call(cbind, tab)
  colnames(tab) = nms
  if (is.null(colnames(tab))) {
    colnames(tab) = paste0("S", 1:ncol(tab))
  }

  tab
}

.get_all_summary_table <- function(mods) {
  res = lapply(1:length(mods), function(i) {
    cbind(model = names(mods)[i], mods[[i]]$summary_table)
  })
  res = do.call(rbind, res)
  rownames(res) = NULL
  res
}

.get_all_tested_coeffs_names <- function(mods) {
  sapply(mods, function(md) colnames(md$Tspace))
}

.get_all_coeff_names_vect <- function(mods) {
  sapply(mods, function(md) {
    names(md$coefficients)
  })
}

.get_all_coeff_names_list <- function(mods) {
  lapply(mods, function(md) {
    names(md$coefficients)
  })
}

.get_all_sumScore2 <- function(mods) {
  sapply(mods, function(md) {
    sum(md$scores^2)
  })
}

.set_mods_names <- function(mods, force = FALSE) {
  if ((is.null(names(mods))) | force) {
    paste0("model", 1:length(mods))
  } else {
    names(mods)
  }
}

.set_comb_names_in_Tspace <- function(combs, comb_names) {
  res = lapply(1:length(combs), function(i) {
    colnames(combs[[i]]$Tspace) = comb_names[i]
    combs[[i]]
  })
  res
}

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

.get_info_models <- function(mods) {
  info <- data.frame(
    model = names(mods),
    formula = sapply(mods, .get_formula)
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

  info <- cbind(info, XS)

  if ("npreg" %in% colnames(info)) {
    info$npreg[is.na(info$npreg)] = info$`npreg^2`[is.na(info$npreg)]
    info$`npreg^2` = NULL
  }

  if (names(info[, 3, drop = FALSE]) == "model") {
    info = info[, -3]
  }

  attr(info, "xs") <- unique(XSL$var)
  info
}

.get_formula <- function(x) {
  # with formula.tools
  # formula.tools:::as.character.formula(x$formula)
  # without formula.tools
  Reduce(paste, deparse(x$formula))
}

.get_formula_x <- function(x) {
  # with formula tools
  formula.tools::rhs.vars(x)
}

.get_formula_y <- function(x) {
  # with formula tools
  formula.tools::lhs.vars(x)
}

.get_x_name <- function(x) {
  attr(x$terms, which = "term.labels")
}

.keep_bare_x_name <- function(x) {
  match_par <- gregexpr("(?<=\\().+?(?=\\))", x, perl = TRUE)
  x_clean <- ifelse(unlist(match_par) == -1, x, regmatches(x, match_par))
  x_clean <- unlist(x_clean)
  x_clean <- sub("(.+?)\\s*[\\,\\+\\-\\*/]\\s*.+", "\\1", x_clean)
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
    if(inherits(cc, class)){
      FUN(cc, ...)
    } else{
      cc
    }
  })
  if(inherits(x, "data.frame")){
    res <- data.frame(res)
  }
  res
}



