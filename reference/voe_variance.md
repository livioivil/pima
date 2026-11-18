# Variance decomposition of the vibration of effects

Regresses `estimate` on the specification decisions (as factors) and
reports, for each decision \\d\\, the unique R-squared
\$\$R^2\_{\mathrm{u}}(d) = R^2\_{\mathrm{full}} - R^2\_{-d},\$\$ where
\\R^2\_{\mathrm{full}}\\ is the R-squared of the model with all the
decisions and \\R^2\_{-d}\\ the one without \\d\\, separately for each
response x coefficient. With a complete, balanced grid of specifications
and no interactions, the unique R-squared equals the ANOVA share of
variance (Klau et al. 2023) .

## Usage

``` r
voe_variance(
  x,
  estimate = "pcor",
  p.adjusted = NULL,
  focal = NULL,
  which.response = NULL,
  focal.var = NULL
)
```

## Arguments

- x:

  a `pima` object, a `pima.multi` object created with `fit.specs`, a
  list of fitted `lm` or `glm` models, or a table returned by
  [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md).

- estimate:

  column of the specification table to decompose.

- p.adjusted:

  not used: p-values play no role in the decomposition. Kept for
  backward compatibility; when not `NULL` it is only checked with the
  rules of [`voe()`](https://livioivil.github.io/pima/reference/voe.md).

- focal, focal.var:

  coefficients and variables to consider, as in
  [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md);
  by default the tested coefficients (`pima`), the focal predictors
  (`pima.multi`) or all the coefficients except the intercept. Each
  selected coefficient is a separate association.

- which.response:

  responses to consider; defaults to all.

## Value

A list with `r2` (one row per response x coefficient) and `dec` (one row
per response x coefficient x decision, with `source` equal to
`"predictor"` or `"other"`, and `r2u_inclusion`, `r2u_transformation`
for predictors).

## Details

Decisions are the predictors (`attr(spec_table(x), "spec_predictors")`,
one factor per predictor whose levels are the terms used to include it
and `"<predictor> (removed)"` when it is excluded) and the other
specification choices (the remaining `attr(spec_table(x), "spec_vars")`,
e.g. `subset`, `fit_spec`, `family`, `link`). Decisions that do not vary
within an association are dropped, and so are decisions that define the
same partition of the specifications as an earlier one (e.g. `link` when
it is fully determined by `fit_spec`). A decision nested in another one
(e.g. `link` with fewer levels than `fit_spec`) is kept, but its unique
R-squared is 0 by construction.

For predictor decisions the unique R-squared is also split into two
nested parts that sum to it: `r2u_inclusion` (removed vs included,
whatever the form) and `r2u_transformation` (variation among the forms
of an included predictor). Both are `NA` for the other decisions.

## References

Klau S, Schönbrodt FD, Patel CJ, Ioannidis JPA, Boulesteix A, Hoffmann S
(2023). “Comparing the Vibration of Effects due to Model, Data
Pre-Processing and Sampling Uncertainty on a Large Data Set in
Personality Psychology.” *Meta-Psychology*, **7**.
[doi:10.15626/MP.2020.2556](https://doi.org/10.15626/MP.2020.2556) .

## See also

Other vibration of effects:
[`janus()`](https://livioivil.github.io/pima/reference/janus.md),
[`voe()`](https://livioivil.github.io/pima/reference/voe.md),
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
voe_variance(multi, estimate = "estimate")
#> $r2
#>      response  coefficient        r2     r2.adj
#> 1 Sepal.Width Sepal.Length 0.4855382 -0.2861545
#> 
#> $dec
#>      response  coefficient        term    source       r2u      pr2u
#> 1 Sepal.Width Sepal.Length Petal.Width predictor 0.2948403 0.6072443
#> 2 Sepal.Width Sepal.Length     Species predictor 0.1906979 0.3927557
#>   r2u_inclusion r2u_transformation
#> 1     0.2790126         0.01582772
#> 2     0.1906979         0.00000000
#> 
```
