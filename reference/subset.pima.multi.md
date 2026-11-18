# Subset a multiverse object

Subsets a `pima.multi` object by selecting rows from its `scenarios`
component. The corresponding fitted models are subset accordingly,
preserving the one-to-one correspondence between `scenarios$model` and
the names of `models`.

## Usage

``` r
# S3 method for class 'pima.multi'
subset(x, subset, ...)
```

## Arguments

- x:

  A `pima.multi` object.

- subset:

  A logical expression evaluated within `x$scenarios`. The expression
  must return one logical value for each scenario and cannot contain
  missing values.

- ...:

  Additional arguments. Currently unused.

## Value

A `pima.multi` object containing only the selected scenarios and, when
available, their corresponding fitted models.

## Details

Only the `scenarios` and `models` components are subset. Components
describing the original multiverse specification, such as
`formula_specs` and `specification`, are left unchanged.

Model identifiers are not renumbered after subsetting. This preserves
the original correspondence between a scenario and its fitted model.

## See also

Other multiverse construction:
[`c.pima.multi()`](https://livioivil.github.io/pima/reference/c.pima.multi.md),
[`create_fit()`](https://livioivil.github.io/pima/reference/create_fit.md),
[`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md)

## Examples

``` r
multi <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width,
  data = iris,
  nfuns = "log",
  fit.specs = list(lm = lm, glm = glm)
)

# Keep only successfully fitted models
successful <- subset(multi, fit_ok)

# Inspect failed fits
failed <- subset(multi, !fit_ok)

# Keep scenarios using a particular fitting specification
lm_models <- subset(multi, fit_spec == "lm")

# Filter according to an analytical decision
log_models <- subset(
  multi,
  spec_Sepal.Length == "log"
)
```
