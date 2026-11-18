# Janus effect

Computes the definitions of the Janus effect used in the vibration of
effects literature. The Janus effect indicates that the same association
changes direction across specifications.

## Usage

``` r
janus(x, ...)

# Default S3 method
janus(x, p = NULL, q = c(0.01, 0.99), null = 0, alpha = 0.05, ...)

# S3 method for class 'pima'
janus(
  x,
  focal = NULL,
  p.adjusted = NULL,
  q = c(0.01, 0.99),
  alpha = 0.05,
  which.response = NULL,
  xvar = "estimate",
  focal.var = NULL,
  p = NULL,
  ...
)

# S3 method for class 'pima.multi'
janus(
  x,
  focal = NULL,
  p.adjusted = NULL,
  q = c(0.01, 0.99),
  alpha = 0.05,
  which.response = NULL,
  xvar = "estimate",
  focal.var = NULL,
  p = NULL,
  ...
)

# S3 method for class 'list'
janus(
  x,
  focal = NULL,
  p.adjusted = NULL,
  q = c(0.01, 0.99),
  alpha = 0.05,
  which.response = NULL,
  xvar = "estimate",
  focal.var = NULL,
  p = NULL,
  ...
)

# S3 method for class 'data.frame'
janus(
  x,
  focal = NULL,
  p.adjusted = NULL,
  q = c(0.01, 0.99),
  alpha = 0.05,
  which.response = NULL,
  xvar = "estimate",
  focal.var = NULL,
  p = NULL,
  ...
)
```

## Arguments

- x:

  a numeric vector of estimates, a `pima` object, a `pima.multi` object
  created with `fit.specs`, a list of fitted `lm` or `glm` models, or a
  table returned by
  [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md).

- ...:

  further arguments (currently unused).

- p:

  for vectors, a numeric vector of p-values, same length as `x` (if
  `NULL`, `J_sig` and `J_ext` are `NA`). Otherwise, the column of
  p-values of the specification table, as in
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md).

- q:

  probabilities; only the extremes are used.

- null:

  value under the null hypothesis (0 on the link scale).

- alpha:

  significance level.

- focal, focal.var:

  coefficients and variables to consider, as in
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md);
  by default the tested coefficients (`pima`), the focal predictors
  (`pima.multi`) or all the coefficients except the intercept.

- p.adjusted:

  which p-values to use, with the same rules as
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md):
  adjusted p-values only exist for `pima` objects analysed with a
  correction. Published values in the literature use raw p-values.

- which.response:

  responses to consider; defaults to all.

- xvar:

  column of the specification table whose sign is used. Any column with
  the same sign as the coefficient (`estimate`, `pcor`, `wald.t`) gives
  the same result.

## Value

For vectors, a list with `J_range`, `J_pct`, `J_ext`, `J_sig`, `J_frac`.
Otherwise, a data frame with one row per response x coefficient.

## Details

Let \\x_1, \dots, x_M\\ be the estimates of the \\M\\ specifications,
\\p_s\\ their p-values, \\\theta_0\\ the value under the null (`null`),
\\Q_q\\ the \\q\\-th quantile across the specifications (interpolated,
as in [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)) and
\\q_1 \< q_2\\ the extremes of `q`.

- `J_range` (Vinatier et al. 2025) : minimum and maximum estimates on
  opposite sides of the null, \\\min_s x_s \< \theta_0 \< \max_s x_s\\.

- `J_pct` (Patel et al. 2015; Palpacuer et al. 2019; Vinatier et
  al. 2024) : extreme quantiles on opposite sides of the null,
  \\Q\_{q_1}(x) \< \theta_0 \< Q\_{q_2}(x)\\.

- `J_ext` (Vinatier et al. 2024) : the specifications \\s_1\\ and
  \\s_2\\ closest to \\Q\_{q_1}(x)\\ and \\Q\_{q_2}(x)\\ are on opposite
  sides of the null and both significant, \\x\_{s_1} \< \theta_0 \<
  x\_{s_2}\\ and \\p\_{s_1}, p\_{s_2} \le \alpha\\.

- `J_sig` (Klau et al. 2021; Klau et al. 2023) : at least one
  significant estimate in each direction, i.e. there are \\s\\ and
  \\s'\\ with \\p_s, p\_{s'} \le \alpha\\ and \\x_s \< \theta_0 \<
  x\_{s'}\\.

- `J_frac` (Tierney et al. 2021) : proportion of estimates greater than
  the null, \\\frac{1}{M} \sum\_{s=1}^{M} \mathbf{1}(x_s \> \theta_0)\\.

## References

Klau S, Hoffmann S, Patel CJ, Ioannidis JPA, Boulesteix A (2021).
“Examining the Robustness of Observational Associations to Model,
Measurement and Sampling Uncertainty with the Vibration of Effects
Framework.” *International Journal of Epidemiology*, **50**(1), 266–278.
[doi:10.1093/ije/dyaa164](https://doi.org/10.1093/ije/dyaa164) .  
  
Klau S, Schönbrodt FD, Patel CJ, Ioannidis JPA, Boulesteix A, Hoffmann S
(2023). “Comparing the Vibration of Effects due to Model, Data
Pre-Processing and Sampling Uncertainty on a Large Data Set in
Personality Psychology.” *Meta-Psychology*, **7**.
[doi:10.15626/MP.2020.2556](https://doi.org/10.15626/MP.2020.2556) .  
  
Palpacuer C, Hammas K, Duprez R, Laviolle B, Ioannidis JPA, Naudet F
(2019). “Vibration of Effects from Diverse Inclusion/Exclusion Criteria
and Analytical Choices: 9216 Different Ways to Perform an Indirect
Comparison Meta-Analysis.” *BMC Medicine*, **17**(1), 174.
[doi:10.1186/s12916-019-1409-3](https://doi.org/10.1186/s12916-019-1409-3)
.  
  
Patel CJ, Burford B, Ioannidis JPA (2015). “Assessment of Vibration of
Effects due to Model Specification Can Demonstrate the Instability of
Observational Associations.” *Journal of Clinical Epidemiology*,
**68**(9), 1046–1058.
[doi:10.1016/j.jclinepi.2015.05.029](https://doi.org/10.1016/j.jclinepi.2015.05.029)
.  
  
Tierney BT, Anderson E, Tan Y, Claypool K, Tangirala S, Kostic AD,
Manrai AK, Patel CJ (2021). “Leveraging Vibration of Effects Analysis
for Robust Discovery in Observational Biomedical Data Science.” *PLOS
Biology*, **19**(9), e3001398.
[doi:10.1371/journal.pbio.3001398](https://doi.org/10.1371/journal.pbio.3001398)
.  
  
Vinatier C, Hoffmann S, Patel C, DeVito NJ, Cristea IA, Tierney B,
Ioannidis JPA, Naudet F (2025). “What Is the Vibration of Effects?” *BMJ
Evidence-Based Medicine*, **30**(1), 61–65.
[doi:10.1136/bmjebm-2023-112747](https://doi.org/10.1136/bmjebm-2023-112747)
.  
  
Vinatier C, Palpacuer C, Scanff A, Naudet F (2024). “Vibration of
Effects Resulting from Treatment Selection in Mixed-Treatment
Comparisons: A Multiverse Analysis on Network Meta-Analyses of
Antidepressants in Major Depressive Disorder.” *BMJ Evidence-Based
Medicine*, **29**(5), 324–332.
[doi:10.1136/bmjebm-2024-112848](https://doi.org/10.1136/bmjebm-2024-112848)
.

## See also

Other vibration of effects:
[`voe()`](https://livioivil.github.io/pima/reference/voe.md),
[`voe_variance()`](https://livioivil.github.io/pima/reference/voe_variance.md),
[`volcano()`](https://livioivil.github.io/pima/reference/volcano.md)

## Examples

``` r
# vectors of estimates and p-values
set.seed(1)
est <- rnorm(100, mean = 0.1, sd = 0.2)
p <- runif(100)
janus(est, p)
#> $J_range
#> [1] TRUE
#> 
#> $J_pct
#> [1] TRUE
#> 
#> $J_ext
#> [1] FALSE
#> 
#> $J_sig
#> [1] TRUE
#> 
#> $J_frac
#> [1] 0.75
#> 

# a multiverse of fitted models
multi <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width + Species,
  data = iris,
  focal = "Sepal.Length",
  nfuns = "log",
  fit.specs = list(lm = lm)
)
janus(multi)
#>      response  coefficient n_spec J_range J_pct J_ext J_sig    J_frac
#> 1 Sepal.Width Sepal.Length      6    TRUE  TRUE FALSE FALSE 0.8333333
```
