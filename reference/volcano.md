# Volcano plot of the vibration of effects

Plots `xvar` against -log10(p), adding the quantile lines computed by
[`voe()`](https://livioivil.github.io/pima/reference/voe.md) and p-value
cutoffs. With more than one association (response x coefficient) each
one is drawn in its own panel, with its own lines.

## Usage

``` r
volcano(
  x,
  focal = NULL,
  xvar = "pcor",
  p.adjusted = NULL,
  p.cut = c(0.05, 0.001),
  q = c(0.01, 0.5, 0.99),
  type = c("additive", "ratio"),
  null = 0,
  alpha = 0.05,
  which.response = NULL,
  median.traj = FALSE,
  focal.var = NULL,
  p = NULL,
  ...
)
```

## Arguments

- x:

  a `pima` object, a `pima.multi` object created with `fit.specs`, a
  list of fitted `lm` or `glm` models, or a table returned by
  [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md).

- focal, focal.var:

  coefficients and variables to consider, as in
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md);
  by default the tested coefficients (`pima`), the focal predictors
  (`pima.multi`) or all the coefficients except the intercept. Each
  selected coefficient is a separate association.

- xvar:

  column of the specification table to summarise (e.g. `"pcor"`,
  `"estimate"`).

- p.adjusted:

  which p-values to use, with the same rules as
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md):
  for `pima` objects `TRUE` uses the adjusted p-values, `FALSE` the
  unadjusted flipscores ones and `NULL` (default) the adjusted ones when
  [`pima()`](https://livioivil.github.io/pima/reference/pima.md) applied
  a correction; for lists of models and `pima.multi` objects only the
  unadjusted Wald p-values exist (`"wald.p"`) and `TRUE` is an error.

- p.cut:

  p-value cutoffs drawn as horizontal lines.

- q:

  probabilities for the quantiles; the extremes define the amplitudes
  and the Janus effect.

- type:

  `"additive"` (difference) or `"ratio"` (exp of the difference, only
  with `xvar = "estimate"`).

- null:

  value under the null hypothesis on the scale of `xvar` (0 for
  `estimate`, `pcor` and `z`).

- alpha:

  significance level.

- which.response:

  responses to consider; defaults to all.

- median.traj:

  if `TRUE`, adds the trajectory of the medians by the number of
  excluded predictors.

- p:

  column of p-values of the specification table (e.g. one added with
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)). If
  `NULL`, chosen with the rules of `p.adjusted`.

- ...:

  further arguments passed to
  [`spec_scatter()`](https://livioivil.github.io/pima/reference/spec_scatter.md)
  (e.g. `xlab`, `ylab`, `shapes`, `by`).

## Value

A ggplot object.

## See also

Other vibration of effects:
[`janus()`](https://livioivil.github.io/pima/reference/janus.md),
[`voe()`](https://livioivil.github.io/pima/reference/voe.md),
[`voe_variance()`](https://livioivil.github.io/pima/reference/voe_variance.md)

Other specification tables and plots:
[`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md),
[`spec_scatter()`](https://livioivil.github.io/pima/reference/spec_scatter.md),
[`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md),
[`transf_p()`](https://livioivil.github.io/pima/reference/transf_p.md)

## Examples

``` r
multi <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width + Species,
  data = iris,
  focal = "Sepal.Length",
  nfuns = "log",
  fit.specs = list(lm = lm)
)
volcano(multi, xvar = "estimate")


# one panel per association
volcano(multi, focal.var = c("Sepal.Length", "Petal.Width"), xvar = "estimate")
```
