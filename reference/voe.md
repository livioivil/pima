# Vibration of effects

Summarises how estimates and p-values vary across the specifications of
a multiverse, separately for each response x coefficient.

## Usage

``` r
voe(
  x,
  xvar = "pcor",
  p.adjusted = NULL,
  q = c(0.01, 0.5, 0.99),
  type = c("additive", "ratio"),
  null = 0,
  alpha = 0.05,
  decompose_variance = FALSE,
  focal = NULL,
  which.response = NULL,
  focal.var = NULL,
  p = NULL
)
```

## Arguments

- x:

  a `pima` object, a `pima.multi` object created with `fit.specs`, a
  list of fitted `lm` or `glm` models, or a table returned by
  [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md).

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

- decompose_variance:

  if `TRUE`, adds
  [`voe_variance()`](https://livioivil.github.io/pima/reference/voe_variance.md).

- focal, focal.var:

  coefficients and variables to consider, as in
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md);
  by default the tested coefficients (`pima`), the focal predictors
  (`pima.multi`) or all the coefficients except the intercept. Each
  selected coefficient is a separate association.

- which.response:

  responses to consider; defaults to all.

- p:

  column of p-values of the specification table (e.g. one added with
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)). If
  `NULL`, chosen with the rules of `p.adjusted`.

## Value

An object of class `pima_voe`: a list with `summary` (one row per
response x coefficient), `quantiles` (long format), the settings used
and, optionally, `variance`.

## Details

Let \\x_s\\ be the value of `xvar` and \\p_s\\ the p-value of
specification \\s\\, \\Q_q\\ the \\q\\-th quantile across the
specifications and \\q_1 \< q_2\\ the extremes of `q`. For each
association the function returns the quantiles of \\x_s\\ and of
\\-\log\_{10} p_s\\, the amplitudes \$\$x\_{\mathrm{voe}} =
Q\_{q_2}(x) - Q\_{q_1}(x), \qquad p\_{\mathrm{voe}} =
Q\_{q_2}(-\log\_{10} p) - Q\_{q_1}(-\log\_{10} p)\$\$ (`p_voe` is the
relative p-value of Patel et al. (2015) ), and the Janus effect
definitions (see
[`janus()`](https://livioivil.github.io/pima/reference/janus.md)).

`type = "ratio"` requires `xvar = "estimate"`: the ratio of the extreme
quantiles of exp(estimate) equals exp() of their difference on the link
scale (the relative hazard or odds ratio of Patel et al. (2015) ), that
is \\\exp(x\_{\mathrm{voe}})\\. It is interpretable only for log, logit
and cloglog links. Quantiles are always returned on the scale of `xvar`.

## References

Patel CJ, Burford B, Ioannidis JPA (2015). “Assessment of Vibration of
Effects due to Model Specification Can Demonstrate the Instability of
Observational Associations.” *Journal of Clinical Epidemiology*,
**68**(9), 1046–1058.
[doi:10.1016/j.jclinepi.2015.05.029](https://doi.org/10.1016/j.jclinepi.2015.05.029)
.

## See also

Other vibration of effects:
[`janus()`](https://livioivil.github.io/pima/reference/janus.md),
[`voe_variance()`](https://livioivil.github.io/pima/reference/voe_variance.md),
[`volcano()`](https://livioivil.github.io/pima/reference/volcano.md)

## Examples

``` r
multi <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width + Species,
  data = iris,
  focal = "Sepal.Length",
  nfuns = "log",
  fit.specs = list(lm = lm)
)
voe(multi)
#> Vibration of effects (xvar = pcor, type = additive, p-values = wald.p, alpha = 0.05)
#> 
#>     response  coefficient n_spec x_voe p_voe J_range J_pct J_ext J_sig J_frac
#>  Sepal.Width Sepal.Length      6 0.623  10.3    TRUE  TRUE FALSE FALSE  0.833
voe(multi, xvar = "estimate", decompose_variance = TRUE)
#> Vibration of effects (xvar = estimate, type = additive, p-values = wald.p, alpha = 0.05)
#> 
#>     response  coefficient n_spec x_voe p_voe J_range J_pct J_ext J_sig J_frac
#>  Sepal.Width Sepal.Length      6 0.394  10.3    TRUE  TRUE FALSE FALSE  0.833
#> 
#> Variance decomposition:
#>     response  coefficient        term    source   r2u  pr2u r2u_inclusion
#>  Sepal.Width Sepal.Length Petal.Width predictor 0.295 0.607         0.279
#>  Sepal.Width Sepal.Length     Species predictor 0.191 0.393         0.191
#>  r2u_transformation
#>              0.0158
#>              0.0000
```
