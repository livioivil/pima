#' Specification Curve Analysis
#'
#' Plots a specification curve: the top panel shows the result of each
#' specification (sorted), the bottom panel shows the analytical choices
#' defining it. The plot is built from the table returned by [spec_table()],
#' so it works with `pima` objects, `pima.multi` objects, lists of fitted
#' models, or a (possibly filtered) specification table.
#'
#' @param x A `pima` object, a `pima.multi` object created with `fit.specs`, a
#'   list of fitted `lm` or `glm` models, or a table returned by
#'   [spec_table()].
#' @param focal A character vector of coefficient names to plot (exact match,
#'   e.g. `"log(x)"` or `"groupB"`).
#' @param focal.var A character vector of variable names to plot: all the
#'   coefficients coming from these variables are selected, whatever their
#'   transformation (e.g. `"x"` selects `x`, `log(x)` and `sqrt(x)`) or
#'   coding (e.g. `"group"` selects `groupB` and `groupC`). When both `focal`
#'   and `focal.var` are supplied, the union is plotted. When both are `NULL`,
#'   the defaults are the tested coefficients (`focal`, for `pima` objects),
#'   the focal predictors of [create_multi()] (`focal.var`, for `pima.multi`
#'   objects), or all the coefficients except the intercept.
#' @param yvar Character indicating the column of the specification table
#'   plotted on the y-axis of the top plot. Defaults to `"estimate"` with one
#'   coefficient and to `"pcor"` with more than one.
#' @param p Character indicating the column of p-values used to define
#'   significance. Any column of the table can be used. If `NULL`, the column
#'   is chosen with fixed rules (see Details).
#' @param p.adjusted Logical. For `pima` objects, `TRUE` uses the adjusted
#'   p-values (`"p.adj"`) and `FALSE` the unadjusted flipscores p-values
#'   (`"p"`); `NULL` (default) uses the adjusted ones when [pima()] applied a
#'   correction and the flipscores ones otherwise. For lists of models and `pima.multi` objects the
#'   p-values are never adjusted: `FALSE` (or `NULL`) uses `"wald.p"` and
#'   `TRUE` is an error. Ignored when `p` is supplied.
#' @param spec_vars Character vector of the specification columns shown in the
#'   bottom plot. If `NULL`, taken from `attr(x, "spec_vars")`. Needed when the
#'   attribute has been lost (e.g., after [subset()]).
#' @param exclude.vars Character vector of specification columns removed from
#'   the bottom plot (e.g. a predictor such as `"AGE"`, or `"fit_spec"`).
#' @param alpha A numeric value specifying the significance level for the
#'   confidence intervals and the point shapes. Default is 0.05.
#' @param tbr A numeric vector of two elements indicating the vertical space
#'   ratio assigned to the top and bottom plots (e.g., `c(0.4, 0.6)`).
#' @param colors A character vector specifying the colors of the
#'   response ~ coefficient combinations. With a single combination the
#'   default is `"#0072B2"`.
#' @param shapes A numeric vector of two elements specifying the shapes for
#'   non-significant and significant results. Default to `c(4, 19)`.
#' @param title A character string for the overall plot title.
#' @param xlab A character string for the x-axis title. Default to
#'   "Specification". The x-axis (the index of the specifications) is shown in
#'   the bottom plot, or in the top plot when `which.plot = "top"`.
#' @param ylab A character string for the y-axis title of the top plot.
#'   Default to the value of `yvar`.
#' @param top.theme A function returning a `ggplot2` theme for the top plot.
#'   Default to `ggplot2::theme_minimal()`.
#' @param bottom.theme A function returning a `ggplot2` theme for the bottom
#'   plot. Default to `ggplot2::theme_minimal()`.
#' @param redundant Logical. If `FALSE`, removes from the bottom plot the
#'   choices that do not vary across specifications and those splitting the
#'   specifications exactly as a previous one (e.g., `family` and `link` when
#'   fully determined by `fit_spec`).
#' @param conf.int Logical. If `TRUE`, includes confidence intervals around the
#'   estimates (only when `yvar = "estimate"`). Wald intervals are used with
#'   `p = "wald.p"` when the standard error is available, otherwise intervals
#'   are obtained by inverting the selected p-values.
#' @param facet A formula with columns of the specification table, as in
#'   [ggplot2::facet_grid()], splitting the top plot into panels, e.g.
#'   `response ~ coefficient`, `response ~ .` or `. ~ coefficient`. If `NULL`
#'   (default), a single panel. See Details.
#' @param facet.scales Scales of the panels: `"free_y"` (default), `"fixed"`,
#'   `"free_x"` or `"free"`, as in [ggplot2::facet_grid()].
#' @param which.response A character vector specifying which response
#'   variables to include in the plot.
#' @param yname Deprecated alias of `which.response`.
#' @param which.plot One of `"both"` (default), `"top"` or `"bottom"`.
#'
#' @details
#' When `p` is `NULL`, the p-values are chosen among the three columns
#' produced by the package, with fixed rules:
#' \itemize{
#' \item `pima` objects (the table has `"p"`): the adjusted p-values
#'   (`"p.adj"`) or, when [pima()] applied no correction (`method = "none"`),
#'   the flipscores p-values (`"p"`). `p.adjusted = FALSE` always uses `"p"`;
#'   `p.adjusted = TRUE` is an error when there are no adjusted p-values.
#' \item lists of models and `pima.multi` objects (the table has only
#'   `"wald.p"`): `"wald.p"`, the unadjusted Wald p-values of the model
#'   summary.
#' }
#' Other columns (e.g., p-values adjusted by the user) are used only when
#' passed through `p`.
#'
#' The rows are ordered by `yvar`. The points of the top plot are coloured by
#' `response ~ coefficient` (e.g. `x` and `log(x)` have different colours),
#' leaving out the columns used in `facet`.
#'
#' With `facet`, the rows are ordered within each panel. With the bottom plot
#' (`which.plot = "both"`) each specification keeps its own position on the
#' x-axis, so that the top and bottom plots stay aligned: the panels are
#' stacked in rows (one for each combination of the `facet` columns) and
#' follow each other on the x-axis. With `which.plot = "top"` the panels are
#' laid out as in `facet`, each with the x-axis of its own specifications.
#' Whether the values in the same panel are comparable (e.g. the estimates of
#' `x` and `log(x)` with `facet = response ~ .`) is up to the user.
#'
#' @return A [`patchwork`] object consisting of two aligned `ggplot2` plots,
#'   or a single `ggplot2` plot when `which.plot` is `"top"` or `"bottom"`.
#'
#' @references
#' \insertRef{simonsohn2020specification}{pima}
#' @family specification tables and plots
#' @export
#'
#' @examples
#' mods <- list(
#'   a = lm(Sepal.Width ~ Sepal.Length, data = iris),
#'   b = lm(Sepal.Width ~ Sepal.Length + Species, data = iris),
#'   c = lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris)
#' )
#' spec_curve(mods, focal = "Sepal.Length")
#'
#' # from a (filtered) specification table
#' tab <- spec_table(mods, focal = "Sepal.Length")
#' spec_curve(tab[tab$model != "a", ])
#'
#' # p-values of your own are used only when requested
#' tab$p.holm <- stats::p.adjust(tab$wald.p, method = "holm")
#' spec_curve(tab, p = "p.holm")
spec_curve <- function(
  x,
  focal = NULL,
  focal.var = NULL,
  yvar = NULL,
  p = NULL,
  p.adjusted = NULL,
  spec_vars = NULL,
  exclude.vars = NULL,
  alpha = 0.05,
  tbr = c(0.4, 0.6),
  colors = NULL,
  shapes = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  top.theme = NULL,
  bottom.theme = NULL,
  redundant = TRUE,
  conf.int = FALSE,
  facet = NULL,
  facet.scales = "free_y",
  which.response = NULL,
  yname = NULL,
  which.plot = "both"
) {
  # data --------------------------------------------------------------------

  if (!is.null(yname)) {
    warning("`yname` is deprecated, use `which.response` instead.", call. = FALSE)
    which.response <- unique(c(which.response, yname))
  }

  # default selection: tested coefficients (pima), focal variables
  # (pima.multi), otherwise all the coefficients except the intercept
  if (!length(focal) && !length(focal.var)) {
    # tested_coeffs can be a list with one element per model
    if (inherits(x, "pima")) focal <- unique(unlist(x$tested_coeffs))
    if (inherits(x, "pima.multi")) focal.var <- x$specification$focal
  }

  tab <- if (is.data.frame(x)) x else spec_table(x)

  if (is.null(spec_vars)) {
    spec_vars <- attr(tab, "spec_vars")
  }
  if (is.null(spec_vars)) {
    stop("cannot find the specification columns: supply `spec_vars`.", call. = FALSE)
  }
  miss <- setdiff(spec_vars, names(tab))
  if (length(miss)) {
    stop(sprintf("`spec_vars` not found in the table: %s.", paste(miss, collapse = ", ")),
         call. = FALSE)
  }

  if (length(exclude.vars)) {
    bad <- setdiff(exclude.vars, spec_vars)
    if (length(bad)) {
      stop(sprintf("`exclude.vars` not among the specification columns (%s): %s.",
                   paste(spec_vars, collapse = ", "), paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    spec_vars <- setdiff(spec_vars, exclude.vars)
    if (!length(spec_vars)) {
      stop("`exclude.vars` removes all the specification columns.", call. = FALSE)
    }
  }

  if (!length(focal) && !length(focal.var)) {
    tab <- tab[tab$coefficient != "(Intercept)", , drop = FALSE]
  } else {
    tab <- .filter_focal(tab, focal = focal, focal.var = focal.var)
  }
  if (!is.null(which.response)) {
    tab <- tab[tab$response %in% which.response, , drop = FALSE]
  }
  if (!nrow(tab)) {
    stop("no rows left after filtering by `focal` / `focal.var` / `which.response`.", call. = FALSE)
  }

  p <- .pick_p(tab, p = p, p.adjusted = p.adjusted)

  # y-axis: estimate for one coefficient, partial correlation for more
  if (is.null(yvar)) {
    if (length(unique(tab$coefficient)) > 1) {
      yvar <- "pcor"
      warning(
        "more than one coefficient and no `yvar` specified. Using 'pcor'.",
        call. = FALSE
      )
    } else {
      yvar <- "estimate"
    }
  }
  if (!yvar %in% names(tab)) {
    stop(sprintf("`yvar` column \"%s\" not found in the table.", yvar), call. = FALSE)
  }

  which.plot <- match.arg(which.plot, choices = c("top", "bottom", "both"))
  facet.scales <- match.arg(facet.scales, c("free_y", "fixed", "free_x", "free"))
  facet_vars <- character(0)
  if (!is.null(facet)) {
    if (!inherits(facet, "formula")) {
      stop("`facet` must be a formula, e.g. `response ~ coefficient`.", call. = FALSE)
    }
    facet_vars <- setdiff(all.vars(facet), ".")
    miss <- setdiff(facet_vars, names(tab))
    if (length(miss)) {
      stop(sprintf("`facet` columns not found in the table: %s.",
                   paste(miss, collapse = ", ")), call. = FALSE)
    }
  }

  # top data: ordering, significance, confidence intervals
  if (length(facet_vars)) {
    # panels in order of appearance; the rows are ordered within each panel
    panel <- do.call(paste, c(lapply(facet_vars, function(v) as.character(tab[[v]])), sep = " | "))
    tab$.panel <- factor(panel, levels = unique(panel))
    tab <- tab[order(tab$.panel, tab[[yvar]]), , drop = FALSE]
  } else {
    tab <- tab[order(tab[[yvar]]), , drop = FALSE]
  }
  tab$.id_spec <- seq_len(nrow(tab))
  tab$is_signif <- factor(as.integer(tab[[p]] <= alpha), levels = c(0, 1))

  if (conf.int && yvar != "estimate") {
    warning("confidence interval can be calculated only for the coefficients!", call. = FALSE)
  }
  if (conf.int && yvar == "estimate") {
    zc <- stats::qnorm(1 - alpha / 2)
    # Wald interval when the standard error of the estimate is available and
    # the Wald p-values are used, otherwise invert the chosen p-value
    se_ci <- if (p == "wald.p" && "se" %in% names(tab)) {
      tab$se
    } else {
      abs(tab$estimate) / abs(stats::qnorm(tab[[p]] / 2))
    }
    tab$est.ci.lb <- tab$estimate - zc * se_ci
    tab$est.ci.ub <- tab$estimate + zc * se_ci
  }

  # bottom data
  if (!redundant) {
    # drop the choices that are constant ...
    keep <- vapply(spec_vars, function(v) length(unique(tab[[v]])) > 1, logical(1))
    spec_vars <- spec_vars[keep]
    # ... and those splitting the specifications exactly as an earlier one
    # (e.g. `family` and `link` fully determined by `fit_spec`)
    partition <- vapply(spec_vars, function(v) {
      z <- as.character(tab[[v]])
      paste(match(z, z), collapse = ",")
    }, character(1))
    spec_vars <- spec_vars[!duplicated(partition)]
  }
  dbottom <- .spec_long(tab, spec_vars)
  dbottom$var.spec <- ifelse(
    .is_excluded(dbottom$var.spec),
    sprintf("%s (removed)", dbottom$var),
    dbottom$var.spec
  )
  dbottom$var_txt <- ifelse(
    dbottom$var %in% c(focal, focal.var) | dbottom$var.spec %in% focal,
    sprintf('atop(bold("%s"), "(focal)")', dbottom$var),
    sprintf('atop(bold("%s"), "")', dbottom$var)
  )

  # plotting elements -------------------------------------------------------

  nspec <- length(unique(tab$model))
  if (is.null(title)) title <- sprintf("Specification Curve (n = %s scenarios)", nspec)
  if (is.null(top.theme)) top.theme <- ggplot2::theme_minimal
  if (is.null(bottom.theme)) bottom.theme <- ggplot2::theme_minimal
  if (is.null(xlab)) xlab <- "Specification"
  if (is.null(ylab)) ylab <- yvar
  if (is.null(shapes)) shapes <- c(4, 19)
  shapes <- stats::setNames(shapes, c("0", "1"))
  shape_labels <- paste0(c("p >  ", "p <= "), alpha)

  # colours: response ~ coefficient, without the columns of the panels
  tab$.coefficient_y <- sprintf(
    "%s ~ %s",
    if ("response" %in% facet_vars) "" else tab$response,
    if ("coefficient" %in% facet_vars) "" else tab$coefficient
  )
  tab$.coefficient_y <- trimws(tab$.coefficient_y)

  # x-axis of the top plot: with the bottom plot, the position of each
  # specification (shared with the bottom plot); without, within each panel
  tab$.x_top <- tab$.id_spec
  if (length(facet_vars) && which.plot == "top") {
    tab$.x_top <- stats::ave(tab$.id_spec, tab$.panel, FUN = seq_along)
  }

  top <- ggplot2::ggplot(
    data = tab,
    ggplot2::aes(x = .data[[".x_top"]], y = .data[[yvar]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = .data[[".coefficient_y"]],
        shape = .data[["is_signif"]]
      ),
      show.legend = TRUE
    ) +
    top.theme() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    # the x-axis and the p-value legend are in the bottom plot, unless the
    # top plot is alone
    ggplot2::scale_shape_manual(
      values = shapes,
      guide = if (which.plot == "top") "legend" else "none",
      name = "p-value",
      drop = FALSE,
      labels = shape_labels
    ) +
    ggplot2::labs(x = xlab, y = ylab)

  if (which.plot != "top") {
    top <- top + ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }

  if (!is.null(colors)) {
    top <- top + ggplot2::scale_color_manual(values = colors)
  } else if (length(unique(tab$.coefficient_y)) == 1L) {
    # a single colour; no legend when the panels already separate response
    # and coefficient
    top <- top + ggplot2::scale_color_manual(
      values = .single_color,
      guide = if (all(c("response", "coefficient") %in% facet_vars)) "none" else "legend"
    )
  }

  if (conf.int && yvar == "estimate") {
    top <- top +
      ggplot2::geom_segment(ggplot2::aes(
        x = .data[[".x_top"]],
        xend = .data[[".x_top"]],
        y = .data[["est.ci.lb"]],
        yend = .data[["est.ci.ub"]]
      ))
  }

  if (length(facet_vars)) {
    if (which.plot == "top") {
      top <- top + ggplot2::facet_grid(facet, scales = facet.scales)
    } else {
      # aligned with the bottom plot: one row of panels per combination, the
      # x-axis shared (only the y-axis can be free)
      y_scales <- if (facet.scales %in% c("free_y", "free")) "free_y" else "fixed"
      top <- top +
        ggplot2::facet_grid(stats::as.formula(".panel ~ ."), scales = y_scales) +
        ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
    }
  }

  bottom <- ggplot2::ggplot(
    dbottom,
    ggplot2::aes(x = .data[[".id_spec"]], y = .data[["var.spec"]])
  ) +
    ggplot2::facet_grid(
      stats::as.formula("var_txt ~ ."),
      scales = "free_y",
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[["is_signif"]]),
      show.legend = TRUE
    ) +
    bottom.theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(x = xlab) +
    ggplot2::scale_shape_manual(
      values = shapes,
      name = "p-value",
      drop = FALSE,
      labels = shape_labels
    )

  switch(
    which.plot,
    both = patchwork::wrap_plots(top, bottom, ncol = 1, heights = tbr),
    top = top,
    bottom = bottom
  )
}
