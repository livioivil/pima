# Create a model-fitting specification

\`create_fit()\` defines how a model should be fitted within a
multiverse. Standard model-fitting functions are supplied through
\`fun\`; functions with non-standard interfaces can be supported through
\`adapter\`.

## Usage

``` r
create_fit(fun = NULL, ..., adapter = NULL)
```

## Arguments

- fun:

  Optional model-fitting function that explicitly accepts \`formula\`
  and \`data\`, such as \`stats::lm\`, \`stats::glm\`, \`MASS::glm.nb\`,
  or \`lme4::lmer\`. Supply either \`fun\` or \`adapter\`, but not both.

- ...:

  Additional named arguments for the fitting specification. With a
  standard \`fun\`, these arguments are passed directly to the fitting
  function. With an \`adapter\`, they are collected into the \`args\`
  list passed to the adapter.

- adapter:

  Optional function for fitting models that do not use the standard
  \`formula\` and \`data\` interface. An adapter must explicitly accept
  arguments named \`formula\`, \`data\`, and \`args\`, where \`args\` is
  the named list created from \`...\`. Because \`adapter\` follows
  \`...\`, it must be supplied by name.

## Value

An object of class \`"pima.fitspec"\` containing the fitting function or
adapter and its additional arguments. The object is intended for use in
the \`fit.specs\` argument of \[create_multi()\].

## Details

Additional arguments must be named. \`formula\` and \`data\` are
reserved because they are supplied by \[create_multi()\] for each
scenario.

For a standard fitting function, arguments in \`...\` that are not
explicitly listed in the function formals generate a warning when the
function accepts \`...\`, because they may be forwarded to another
method. If the function does not accept \`...\`, unknown arguments
generate an error.

## See also

\[create_multi()\]

## Examples

``` r
create_fit(stats::lm)
#> $fun
#> function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> {
#>     ret.x <- x
#>     ret.y <- y
#>     cl <- match.call()
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (method == "model.frame") 
#>         return(mf)
#>     else if (method != "qr") 
#>         warning(gettextf("method = '%s' is not supported. Using 'qr'", 
#>             method), domain = NA)
#>     mt <- attr(mf, "terms")
#>     y <- model.response(mf, "numeric")
#>     w <- as.vector(model.weights(mf))
#>     if (!is.null(w) && !is.numeric(w)) 
#>         stop("'weights' must be a numeric vector")
#>     offset <- model.offset(mf)
#>     mlm <- is.matrix(y)
#>     ny <- if (mlm) 
#>         nrow(y)
#>     else length(y)
#>     if (!is.null(offset)) {
#>         if (!mlm) 
#>             offset <- as.vector(offset)
#>         if (NROW(offset) != ny) 
#>             stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
#>                 NROW(offset), ny), domain = NA)
#>     }
#>     if (is.empty.model(mt)) {
#>         x <- NULL
#>         z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
#>             ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
#>             y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
#>             0) else ny)
#>         if (!is.null(offset)) {
#>             z$fitted.values <- offset
#>             z$residuals <- y - offset
#>         }
#>     }
#>     else {
#>         x <- model.matrix(mt, mf, contrasts)
#>         z <- if (is.null(w)) 
#>             lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
#>                 ...)
#>         else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
#>             ...)
#>     }
#>     class(z) <- c(if (mlm) "mlm", "lm")
#>     z$na.action <- attr(mf, "na.action")
#>     z$offset <- offset
#>     z$contrasts <- attr(x, "contrasts")
#>     z$xlevels <- .getXlevels(mt, mf)
#>     z$call <- cl
#>     z$terms <- mt
#>     if (model) 
#>         z$model <- mf
#>     if (ret.x) 
#>         z$x <- x
#>     if (ret.y) 
#>         z$y <- y
#>     if (!qr) 
#>         z$qr <- NULL
#>     z
#> }
#> <bytecode: 0x55b24a3b01d0>
#> <environment: namespace:stats>
#> 
#> $adapter
#> NULL
#> 
#> $args
#> list()
#> 
#> attr(,"class")
#> [1] "pima.fitspec"

create_fit(
  stats::glm,
  family = stats::poisson(link = "log")
)
#> $fun
#> function (formula, family = gaussian, data, weights, subset, 
#>     na.action, start = NULL, etastart, mustart, offset, control = list(...), 
#>     model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, ...) 
#> {
#>     cal <- match.call()
#>     if (is.character(family)) 
#>         family <- get(family, mode = "function", envir = parent.frame())
#>     if (is.function(family)) 
#>         family <- family()
#>     if (is.null(family$family)) {
#>         print(family)
#>         stop("'family' not recognized")
#>     }
#>     if (missing(data)) 
#>         data <- environment(formula)
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "etastart", "mustart", "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (identical(method, "model.frame")) 
#>         return(mf)
#>     if (!is.character(method) && !is.function(method)) 
#>         stop("invalid 'method' argument")
#>     if (identical(method, "glm.fit")) 
#>         control <- do.call("glm.control", control)
#>     mt <- attr(mf, "terms")
#>     Y <- model.response(mf, "any")
#>     if (length(dim(Y)) == 1L) {
#>         nm <- rownames(Y)
#>         dim(Y) <- NULL
#>         if (!is.null(nm)) 
#>             names(Y) <- nm
#>     }
#>     X <- if (!is.empty.model(mt)) 
#>         model.matrix(mt, mf, contrasts)
#>     else matrix(, NROW(Y), 0L)
#>     weights <- as.vector(model.weights(mf))
#>     if (!is.null(weights) && !is.numeric(weights)) 
#>         stop("'weights' must be a numeric vector")
#>     if (!is.null(weights) && any(weights < 0)) 
#>         stop("negative weights not allowed")
#>     offset <- as.vector(model.offset(mf))
#>     if (!is.null(offset)) {
#>         if (length(offset) != NROW(Y)) 
#>             stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
#>                 length(offset), NROW(Y)), domain = NA)
#>     }
#>     mustart <- model.extract(mf, "mustart")
#>     etastart <- model.extract(mf, "etastart")
#>     fit <- eval(call(if (is.function(method)) "method" else method, 
#>         x = X, y = Y, weights = weights, start = start, etastart = etastart, 
#>         mustart = mustart, offset = offset, family = family, 
#>         control = control, intercept = attr(mt, "intercept") > 
#>             0L, singular.ok = singular.ok))
#>     if (length(offset) && attr(mt, "intercept") > 0L) {
#>         fit2 <- eval(call(if (is.function(method)) "method" else method, 
#>             x = X[, "(Intercept)", drop = FALSE], y = Y, mustart = fit$fitted.values, 
#>             weights = weights, offset = offset, family = family, 
#>             control = control, intercept = TRUE))
#>         if (!fit2$converged) 
#>             warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
#>         fit$null.deviance <- fit2$deviance
#>     }
#>     if (model) 
#>         fit$model <- mf
#>     fit$na.action <- attr(mf, "na.action")
#>     if (x) 
#>         fit$x <- X
#>     if (!y) 
#>         fit$y <- NULL
#>     structure(c(fit, list(call = cal, formula = formula, terms = mt, 
#>         data = data, offset = offset, control = control, method = method, 
#>         contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, 
#>             mf))), class = c(fit$class, c("glm", "lm")))
#> }
#> <bytecode: 0x55b24a4c5ba0>
#> <environment: namespace:stats>
#> 
#> $adapter
#> NULL
#> 
#> $args
#> $args$family
#> 
#> Family: poisson 
#> Link function: log 
#> 
#> 
#> 
#> attr(,"class")
#> [1] "pima.fitspec"

# Adapter interface for a non-standard fitting function.
lm_adapter <- function(formula, data, args) {
  do.call(
    stats::lm,
    c(list(formula = formula, data = data), args)
  )
}

create_fit(
  adapter = lm_adapter,
  singular.ok = TRUE
)
#> $fun
#> NULL
#> 
#> $adapter
#> function (formula, data, args) 
#> {
#>     do.call(stats::lm, c(list(formula = formula, data = data), 
#>         args))
#> }
#> <environment: 0x55b24a3899f0>
#> 
#> $args
#> $args$singular.ok
#> [1] TRUE
#> 
#> 
#> attr(,"class")
#> [1] "pima.fitspec"
```
