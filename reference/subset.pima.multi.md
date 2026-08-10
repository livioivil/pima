# Subset a multiverse object

Subsets a \`pima.multi\` object by selecting rows from its \`scenarios\`
component. The corresponding fitted models are subset accordingly,
preserving the one-to-one correspondence between \`scenarios\$model\`
and the names of \`models\`.

## Usage

``` r
# S3 method for class 'pima.multi'
subset(x, subset, ...)
```

## Arguments

- x:

  A \`pima.multi\` object.

- subset:

  A logical expression evaluated within \`x\$scenarios\`. The expression
  must return one logical value for each scenario and cannot contain
  missing values.

- ...:

  Additional arguments. Currently unused.

## Value

A \`pima.multi\` object containing only the selected scenarios and, when
available, their corresponding fitted models.

## Details

Only the \`scenarios\` and \`models\` components are subset. Components
describing the original multiverse specification, such as
\`formula_specs\` and \`specification\`, are left unchanged.

Model identifiers are not renumbered after subsetting. This preserves
the original correspondence between a scenario and its fitted model.

## Examples

``` r
# Keep only successfully fitted models
successful <- subset(multi, fit_ok)
#> Error: object 'multi' not found

# Inspect failed fits
failed <- subset(multi, !fit_ok)
#> Error: object 'multi' not found

# Keep scenarios using a particular fitting specification
lm_models <- subset(multi, fit_spec == "lm")
#> Error: object 'multi' not found

# Filter according to an analytical decision
log_models <- subset(
  multi,
  spec_Sepal.Length == "log"
)
#> Error: object 'multi' not found
```
