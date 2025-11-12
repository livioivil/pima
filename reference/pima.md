# Post-selection Inference in Multiverse Analysis

Applies the PIMA procedure to make selective inference on one or more
parameters within a multiverse of models, derived from different
specifications of the data (e.g., pre-processing) and of the model
itself (GLMs). The method jointly computes resampling-based test
statistics for all coefficients of interest in all models. The output
allows for multiple testing in multiverse analysis, providing weak and
strong control of the Family-Wise Error Rate (FWER) and confidence
bounds for True Discovery Proportions (TDPs).

## Usage

``` r
pima(mods, tested_coeffs = NULL, n_flips = 5000, method = c("maxT", "minP", "none"), ...)
```

## Arguments

- mods:

  list of objects that can be evaluated by
  [`flipscores`](https://rdrr.io/pkg/flipscores/man/flipscores.html),
  usually model objects produced by `glm` or `flipscores`.

- tested_coeffs:

  tested coefficients. It can be a vector of names (common to all
  models) or a list of the same length as `mods`, where each element is
  a vector of names. If `NULL`, all coefficients are tested.

- n_flips:

  number of sign flips.

- method:

  correction method among `maxT`, `minP` and `none`. Can be abbreviated.

- ...:

  further parameters of `join_flipscores`.

## Value

Returns an object of class `pima`, containing:

- `Tspace`: data frame of test statistics, where columns correspond to
  tests, and rows to sign-flipping transformations (the first is the
  identity).

- `summary_table`: data frame containing a summary for each tested
  coefficient in each model: estimate, score, standard error, z value,
  partial correlation, raw p-value, adjusted p-value.

- `mods`: list of model objects computed by `flipscores`.

- `tested_coeffs`: tested coefficients, as in input.

## Details

The procedure builds on the sign-flip score test implemented in the
function `flipscores` of the **flipscores** package. For each tested
coefficient in each model, it computes a set of resampling-based test
statistics and a raw p-value for the null hypothesis that the
coefficient is null against a two-sided alternative. If `method` is
`maxT` (default) or `minP`, multiplicity-adjusted p-values are added
using the step-down method of Westfall and Young (1993). Adjusted
p-values provide control of the FWER asymptotically in the sample size
and, for practical purposes, almost exact control for any sample size.

Post-selection inference is achieved by merging information from the
considered models:

- the output summary shows adjusted p-values for strong FWER control:
  "Which effects in which models are significant?"

- `global_tests` produces a global p-value for weak FWER control,
  possibly by coefficient or by model: "Is there at least one
  significant effect?"

- `pimaAnalysis` (**sumSome**) computes a lower confidence bound for the
  TDP, among all tested effects or within a subset: "How many of the
  considered effects are significant?"

Further parameters include:

- `score_type`: type of score that is computed (see
  [`flipscores`](https://rdrr.io/pkg/flipscores/man/flipscores.html) for
  more datails). The default `"standardized"` provides almost exact
  control of the error for any sample size.

- `statistics`: test statistics computed by the procedure. Currently,
  `t` is the only implemented method. Different statistics will affect
  the multivariate inference, but not the univariate.

- `seed`: can be specified to ensure replicability of results.

- `output_models`: `TRUE` to return the list of model objects produced
  by `flipscores`.

## References

Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024).
Post-selection Inference in Multiverse Analysis (PIMA): An Inferential
Framework Based on the Sign Flipping Score Test. Psychometrika, doi:
10.1007/s11336-024-09973-6.

Hemerik, Goeman, Finos (2020). Robust testing in generalized linear
models by sign flipping score contributions. Journal of the Royal
Statistical Society, Series B (Statistical Methodology), doi:
10.1111/rssb.12369.

De Santis, Goeman, Hemerik, Davenport, Finos (2025). Inference in
generalized linear models with robustness to misspecified variances.
Journal of the American Statistical Association, doi:
10.1080/01621459.2025.2491775.

## Examples

``` r
# Generate data
set.seed(123)
n <- 30
D <- data.frame(X1 = rnorm(n), X2 = rnorm(n)^2,
                X3 = sample(c("A","B","C"), n, replace = TRUE),
                Z1 = rnorm(n), Z2 = rnorm(n))
D$X3 <- factor(D$X3)
D$Y <- D$X1 + ifelse(D$X3 == "C", 3, 0) + D$Z1 + rnorm(n)

# Multiverse of 4 models (list)
mod1 <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2, data = D)
mod2 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + Z2, data = D)
mod3 <- glm(Y ~ X1 + X2 + X3 + Z1 + poly(Z2,2), data = D)
mod4 <- glm(Y ~ X1 + X2 + X3 + poly(Z1,2) + poly(Z2,2), data = D)
mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)

# Test selected coefficients (raw and adjusted p-values)
res <- pima(mods, tested_coeffs = c("X1","X2","X3B","X3C"))
#> Error in loadNamespace(x): there is no package called ‘jointest’
summary(res)
#> Error: object 'res' not found

# Global p-values: overall, by model and by coefficient
summary(global_tests(res))
#> Error in loadNamespace(x): there is no package called ‘jointest’
summary(global_tests(res, by = "Model"))
#> Error in match.arg(by, choices = c("coefficient", "model", "individual")): 'arg' should be one of “coefficient”, “model”, “individual”
summary(global_tests(res, by = "Coeff"))
#> Error in match.arg(by, choices = c("coefficient", "model", "individual")): 'arg' should be one of “coefficient”, “model”, “individual”

# Global tests for each factor
summary(global_tests(res, by = "individual", comb_factors = TRUE))
#> Error in loadNamespace(x): there is no package called ‘jointest’

# These tests can be aggregated as before (e.g., by coefficient)
summary(global_tests(res, by = "Coeff", comb_factors = TRUE))
#> Error in match.arg(by, choices = c("coefficient", "model", "individual")): 'arg' should be one of “coefficient”, “model”, “individual”

# Lower 95%-confidence bound for the TDP
# require(sumSome)
# pimaAnalysis(res, alpha = 0.4)
# pimaAnalysis(res, by = "Model", alpha = 0.4)
# pimaAnalysis(res, by = "Coeff", alpha = 0.4)
```
