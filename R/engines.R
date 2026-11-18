#' Engines of pima()
#'
#' The engine is the test used by [pima()] to test the coefficients of all
#' the models jointly. `pima_flipscores()` and `pima_multcomp()` create an
#' engine with its own arguments, passed to the `engine` argument of [pima()].
#' `engine = "flipscores"` and `engine = "multcomp"` use the engines with their
#' default arguments.
#'
#' The arguments shared by the engines (`tested_coeffs`, `method`, `tail`,
#' `extra`, `id`, `seed`) are arguments of [pima()]. See the *Engines* section of
#' [pima()] for the tests.
#'
#' @param n_flips number of sign flips.
#' @param ... further arguments of [flipscores::flipscores()], e.g.
#'   `score_type`. The arguments set by [pima()] (`formula`, `data`,
#'   `tested_coeffs`, `id`, `seed`) cannot be given.
#' @param algorithm algorithm for the multivariate normal probabilities of the
#'   adjusted p-values, e.g. `mvtnorm::GenzBretz(maxpts = 1e6, abseps = 1e-6)`
#'   for more precise (and slower) results. If `NULL` (default), the default
#'   of \pkg{multcomp}.
#' @param x an engine.
#' @return An object of class `pima_engine`: a list with the `name` of the
#'   engine and its `args`.
#' @family PIMA inference
#' @export
#' @examples
#' mods <- list(
#'   a = glm(Sepal.Width ~ Sepal.Length, data = iris),
#'   b = glm(Sepal.Width ~ Sepal.Length + Species, data = iris)
#' )
#' pima(mods, tested_coeffs = "Sepal.Length", seed = 1,
#'      engine = pima_flipscores(n_flips = 500, score_type = "basic"))
#'
#' pima_flipscores(n_flips = 500)
#' pima_multcomp()
pima_flipscores <- function(n_flips = 5000, ...) {
  args <- list(n_flips = n_flips, ...)
  if (any(names(args) == "")) {
    stop("all the arguments of pima_flipscores() must be named.", call. = FALSE)
  }
  reserved <- intersect(names(args), c("formula", "data", "tested_coeffs", "id", "seed"))
  if (length(reserved)) {
    stop(sprintf("%s set by pima(), not by the engine: %s.",
                 if (length(reserved) > 1L) "these arguments are" else "this argument is",
                 paste0("`", reserved, "`", collapse = ", ")), call. = FALSE)
  }
  unknown <- setdiff(names(args), names(formals(flipscores::flipscores)))
  if (length(unknown)) {
    stop(sprintf("not arguments of flipscores::flipscores(): %s.",
                 paste0("`", unknown, "`", collapse = ", ")), call. = FALSE)
  }
  .new_engine("flipscores", args)
}

#' @rdname pima_flipscores
#' @export
pima_multcomp <- function(algorithm = NULL) {
  .new_engine("multcomp", list(algorithm = algorithm))
}

#' @rdname pima_flipscores
#' @export
print.pima_engine <- function(x, ...) {
  cat(sprintf("<pima engine: %s>\n", x$name))
  for (nm in names(x$args)) {
    v <- x$args[[nm]]
    v <- if (is.null(v)) {
      "NULL"
    } else if (is.list(v) && !is.null(oldClass(v))) {
      # e.g. mvtnorm::GenzBretz(): shown as a call
      sprintf("%s(%s)", class(v)[1L],
              paste(names(v), vapply(v, format, character(1)), sep = " = ", collapse = ", "))
    } else {
      paste(deparse(v, width.cutoff = 60L), collapse = " ")
    }
    cat(sprintf("  %s: %s\n", nm, v))
  }
  invisible(x)
}

.new_engine <- function(name, args) {
  structure(list(name = name, args = args), class = "pima_engine")
}

# A name ("flipscores", "multcomp") or an engine object, as an engine object
.as_engine <- function(engine) {
  if (inherits(engine, "pima_engine")) {
    return(engine)
  }
  if (is.character(engine) && length(engine) == 1L) {
    if (engine == "flipscores") return(pima_flipscores())
    if (engine == "multcomp") return(pima_multcomp())
  }
  stop("`engine` must be \"flipscores\", \"multcomp\" or an engine created by ",
       "pima_flipscores() or pima_multcomp().", call. = FALSE)
}

# Corrections of each engine (`method` of pima()): the first is the default.
# For multcomp, the types of multcomp::adjusted().
.engine_methods <- list(
  flipscores = c("maxT", "minP", "none"),
  multcomp = c("single-step", "free", "Westfall", "Shaffer", stats::p.adjust.methods)
)

.engine_method <- function(engine, method) {
  choices <- .engine_methods[[engine]]
  if (is.null(method)) {
    return(choices[1L])
  }
  i <- if (is.character(method) && length(method) == 1L) pmatch(method, choices) else NA
  if (is.na(i)) {
    stop(sprintf("`method` must be one of the corrections of the %s engine: %s.",
                 engine, paste0("\"", choices, "\"", collapse = ", ")), call. = FALSE)
  }
  choices[i]
}

# Name of the engine of a pima object (objects without one come from flipscores)
.engine_name <- function(x) {
  e <- x$engine
  if (is.null(e)) "flipscores" else if (is.character(e)) e else e$name
}
