# Global tests in multiverse analysis

Tests global null hypotheses for weak control of the Family-Wise Error
Rate, considering one or more parameters within a multiverse of models.

## Usage

``` r
global_tests(obj, by = NULL, comb_funct = "maxT", comb_factors = FALSE, comb_factors_funct = "Mahalanobis")
```

## Arguments

- obj:

  a `jointest` object, e.g., produced by
  [`pima`](https://livioivil.github.io/pima/reference/pima.md).

- by:

  mode of combination (`NULL`, `Coeff`, `Model`, `individual`). If
  `NULL` (default), a single global null hypothesis is tested.

- comb_funct:

  combining function (see `combine-methods` for more datails).

- comb_factors:

  logical. If `FALSE`, coefficients for the levels of factor variables
  are considered separately. If `TRUE`, a single global test is
  performed for each factor variable.

- comb_factors_funct:

  combining function for contrasts of factor variables (see
  `combine_contrasts` for more datails).

## Value

Returns an object of class `jointest`, containing:

- `Tspace`: data frame of test statistics, where columns correspond to
  tests, and rows to sign-flipping transformations (the first is the
  identity).

- `summary_table`: data frame containing a summary for each test:
  combined test statistic and p-value.

## Details

This function builds on the functions `combine` and `combine_contrasts`
of the **jointest** package.

In the default `by = NULL`, the procedure tests the global null
hypothesis that there are no significant effects (all considered
coefficients in all models are null) against the alternative that there
is at least one significant effect (one non-null coefficient in one
model). Other inputs of the argument `by` test analogous null
hypotheses, defined by model (`Model`), by coefficient (`Coeff`) or
individually (`individual`).

The argument `comb_factors` affects only categorical predictors. For
each categorical predictor, by default (`FALSE`), non-baseline
coefficients are considered separately. If `TRUE`, a single global test
is performed by combining the tests derived from the contrasts with the
function specified by `comb_factors_funct`.

With the option `by = "individual"`, the input p-values are returned
unchanged, except when categorical predictors are present and
`comb_factors = TRUE`.

## References

Girardi, Vesely, Lakens, Altoè, Pastore, Calcagnì, Finos (2024).
Post-selection Inference in Multiverse Analysis (PIMA): An Inferential
Framework Based on the Sign Flipping Score Test. Psychometrika, doi:
10.1007/s11336-024-09973-6.

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
```
