# Scatter plot of the specifications

Plots one point per specification and coefficient: an estimate-like
column (`xvar`) against the transformed p-values, with significant
specifications marked by the shape. The plot is built from the table
returned by
[`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md),
so it works with `pima` objects, `pima.multi` objects, lists of fitted
models, or a (possibly filtered) specification table.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a `pima`
object calls this function.

## Usage

``` r
spec_scatter(
  x,
  focal = NULL,
  focal.var = NULL,
  xvar = "pcor",
  yvar = NULL,
  p = NULL,
  p.adjusted = NULL,
  p.transf = "z",
  by = "coefficient",
  alpha = 0.05,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  shapes = NULL,
  facet = NULL,
  facet.scales = "free_x",
  which.response = NULL
)
```

## Arguments

- x:

  A `pima` object, a `pima.multi` object created with `fit.specs`, a
  list of fitted `lm` or `glm` models, or a table returned by
  [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md).

- focal:

  A character vector of coefficient names to plot (exact match, e.g.
  `"log(x)"` or `"groupB"`).

- focal.var:

  A character vector of variable names to plot: all the coefficients
  coming from these variables are selected, whatever their
  transformation (e.g. `"x"` selects `x`, `log(x)` and `sqrt(x)`) or
  coding (e.g. `"group"` selects `groupB` and `groupC`). When both
  `focal` and `focal.var` are supplied, the union is plotted. When both
  are `NULL`, the defaults are the tested coefficients (`focal`, for
  `pima` objects), the focal predictors of
  [`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md)
  (`focal.var`, for `pima.multi` objects), or all the coefficients
  except the intercept.

- xvar:

  column of the specification table plotted on the x-axis. Default
  `"pcor"`, comparable across coefficients.

- yvar:

  column of the specification table plotted on the y-axis. If `NULL`
  (default), the p-values transformed with `p.transf`.

- p:

  Character indicating the column of p-values used to define
  significance. Any column of the table can be used. If `NULL`, the
  column is chosen with fixed rules (see Details).

- p.adjusted:

  Logical. For `pima` objects, `TRUE` uses the adjusted p-values
  (`"p.adj"`) and `FALSE` the unadjusted flipscores p-values (`"p"`);
  `NULL` (default) uses the adjusted ones when
  [`pima()`](https://livioivil.github.io/pima/reference/pima.md) applied
  a correction and the flipscores ones otherwise. For lists of models
  and `pima.multi` objects the p-values are never adjusted: `FALSE` (or
  `NULL`) uses `"wald.p"` and `TRUE` is an error. Ignored when `p` is
  supplied.

- p.transf:

  transformation of the p-values (see
  [`transf_p()`](https://livioivil.github.io/pima/reference/transf_p.md))
  or a custom function. Used when `yvar` is `NULL`.

- by:

  colour of the points: `"coefficient"` (one colour per
  `response ~ coefficient`) and/or specification columns
  (`attr(spec_table(x), "spec_vars")`, e.g. `"fit_spec"`, `"link"` or a
  predictor). With more than one, their combinations are used. A single
  group is drawn in `"#0072B2"`.

- alpha:

  significance level marked by the shape. If 0 or 1 the shapes are not
  labelled.

- xlab, ylab:

  axis labels. Default to `xvar` and to the p-value column and its
  transformation (or `yvar`).

- title:

  plot title. If `NULL`, describes the p-values used.

- shapes:

  ggplot2 shapes for non-significant and significant points. Default
  `c(4, 19)`.

- facet:

  formula passed to
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  If `NULL` (default), `response ~ .` with one response and a single
  panel with more than one (use `response ~ .` to split them).

- facet.scales:

  `"fixed"`, `"free"`, `"free_x"` (default) or `"free_y"`.

- which.response:

  responses to plot; defaults to all.

## Value

A ggplot object.

## See also

Other specification tables and plots:
[`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md),
[`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md),
[`transf_p()`](https://livioivil.github.io/pima/reference/transf_p.md),
[`volcano()`](https://livioivil.github.io/pima/reference/volcano.md)

## Examples

``` r
mods <- list(
  a = lm(Sepal.Width ~ Sepal.Length, data = iris),
  b = lm(Sepal.Width ~ Sepal.Length + Species, data = iris),
  c = lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris)
)
spec_scatter(mods, focal = "Sepal.Length", p.transf = "-log10")


# colour by a specification choice
spec_scatter(mods, focal = "Sepal.Length", by = "Species")
```
