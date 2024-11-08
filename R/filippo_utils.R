# these are my functions to create the summary table of the models with specifications.
# the formula.tools(x) dependency could be quite useful and harmless but we can do
# the formula manipulation in other ways.

# .get_info_models <- function(mods){
#   info <- data.frame(
#     Model = names(mods),
#     Formula = sapply(mods, .get_formula)
#   )
#   # extract the predictors
#   xs <- lapply(mods, function(x) .get_formula_x(x$formula))
#   # return predictors in a vector, these will be all the unique columns
#   xs_vec <- unique(unname(unlist(xs)))
#   # binary output for each variable or transformation, TRUE/FALSE if present
#   # in that model
#   xs_bin <- lapply(mods, function(mod) xs_vec %in% .get_formula_x(mod$formula))
#   xs_bin <- t(data.frame(xs_bin))
#   colnames(xs_bin) <- paste0("x_", xs_vec) # this for rapid extracting the columns
#   info <- cbind(info, xs_bin)
#   return(info)
# }

.get_info_models <- function(mods){
  info <- data.frame(
    Model = names(mods),
    Formula = sapply(mods, .get_formula)
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
    if(length(x) < ll){
      c(x, rep(NA, ll - length(x)))
    }else{
      x
    }
  })
  
  # # then we create a list where missing elements receive an NA (is not present)
  # # in the specification and other receive the specific specification
  # XS <- lapply(XS, function(spec) replace(all_x, !all_x %in% spec, NA))
  
  XS <- data.frame(t(data.frame(XS)))
  nn <- colnames(XS)
  XS$Model <- rownames(XS)
  
  XSL <- reshape(
    XS, 
    direction = "long",
    idvar = c("Model"),
    varying = list(nn),
    v.names = "value",
    timevar = "var"
  )
  
  XSL$var <- .clean_x_name(XSL$value)
  XSL <- XSL[!is.na(XSL$value), ]
  
  XS <- reshape(
    XSL,
    idvar = "Model",
    timevar = "var",
    direction = "wide"
  )
  
  colnames(XS) <- gsub("value\\.", "", colnames(XS))
  
  info <- cbind(info, XS)
  
  # TODO fix this when outlier and leverage are implemented
  info$outlier <- FALSE
  info$leverage <- FALSE
  
  return(info)
}

.get_formula <- function(x){
  # with formula.tools
  # formula.tools:::as.character.formula(x$formula)
  # without formula.tools
  Reduce(paste, deparse(x$formula))
}

.get_formula_x <- function(x){
  # with formula tools
  formula.tools::rhs.vars(x)
}

.get_formula_y <- function(x){
  # with formula tools
  formula.tools::lhs.vars(x)
}

spec_curve_filippo <- function(res){
  # check if object of class pima
  stopifnot(inherits(res, "pima"))
  
  # arrange the dataset according to the coefficient
  
  summ <- res$summary_table
  info <- res$info
  summ <- summ[order(summ$Estimate), ]
  nn <- names(summ)
  summ$id <- 1:nrow(summ) # indicator for the plots
  summ <- summ[, c("id", nn)]
  
  # compute confidence interval TODO better method here
  summ$ci.lb <- with(summ, Estimate - `Std. Error` * 2)
  summ$ci.ub <- with(summ, Estimate + `Std. Error` * 2)
  
  # combine the coefficients summary with the table of specifications
  specs <- merge(summ, info, by = "Model")
  
  # upper specification
  upper <- ggplot(specs, aes(x = factor(id), y = exp(Estimate))) +
    #geom_pointrange(aes(ymin = ci.lb, ymax = ci.ub))
    geom_point()
  
  # lower specification
  
  xs <- names(info)[startsWith(names(info), "x_")]
  
  # putting specs in long format for having the tiles
  
  specs_long <- reshape(specs, 
                        direction = "long",
                        idvar = c("Model"),
                        varying = list(xs),
                        v.names = "value",
                        timevar = "var")
  
  specs_long_pl <- specs_long[specs_long$value, ]
  
  # remove the x_ payttern TODO create a function here
  specs_long_pl$var <- gsub("^x_", "", specs_long_pl$var)
  
  lower <- ggplot(specs_long_pl, aes(x = factor(id), y = var)) +
    geom_tile(width = 0.03,
              fill = "dodgerblue",
              height = 0.8)
  
  # TODO check a method without cowplot dependency
  cowplot::plot_grid(
    upper,
    lower,
    ncol = 1,
    align = "hv",
    rel_heights = c(0.7, 0.3)
  )
  
}

.get_x_name <- function(x){
  attr(x$terms, which = "term.labels")
}

.keep_bare_x_name <- function(x){
  match_par <- gregexpr("(?<=\\().+?(?=\\))", x, perl = TRUE)
  x_clean <- ifelse(unlist(match_par) == -1, x, regmatches(x, match_par))
  x_clean <- unlist(x_clean)
  x_clean <- sub("(.+?)\\s*[\\,\\+\\-\\*/]\\s*.+", "\\1", x_clean)
  x_clean <- gsub("\\s*", "", unname(x_clean)) # TODO check this for ``
  return(x_clean)
}

.clean_x_name <- function(x){
  xp <- strsplit(x, ":") # splitting interactions
  xp <- sapply(xp, .keep_bare_x_name)
  xp <- sapply(xp, paste0, collapse = ":")
  return(xp)
}
