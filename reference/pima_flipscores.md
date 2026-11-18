# Engines of pima()

The engine is the test used by
[`pima()`](https://livioivil.github.io/pima/reference/pima.md) to test
the coefficients of all the models jointly. `pima_flipscores()` and
`pima_multcomp()` create an engine with its own arguments, passed to the
`engine` argument of
[`pima()`](https://livioivil.github.io/pima/reference/pima.md).
`engine = "flipscores"` and `engine = "multcomp"` use the engines with
their default arguments.

## Usage

``` r
pima_flipscores(n_flips = 5000, ...)

pima_multcomp(algorithm = NULL)

# S3 method for class 'pima_engine'
print(x, ...)
```

## Arguments

- n_flips:

  number of sign flips.

- ...:

  further arguments of
  [`flipscores::flipscores()`](https://livioivil.github.io/flipscores/reference/flipscores.html),
  e.g. `score_type`. The arguments set by
  [`pima()`](https://livioivil.github.io/pima/reference/pima.md)
  (`formula`, `data`, `tested_coeffs`, `id`, `seed`) cannot be given.

- algorithm:

  algorithm for the multivariate normal probabilities of the adjusted
  p-values, e.g. `mvtnorm::GenzBretz(maxpts = 1e6, abseps = 1e-6)` for
  more precise (and slower) results. If `NULL` (default), the default of
  multcomp.

- x:

  an engine.

## Value

An object of class `pima_engine`: a list with the `name` of the engine
and its `args`.

## Details

The arguments shared by the engines (`tested_coeffs`, `method`, `tail`,
`extra`, `id`, `seed`) are arguments of
[`pima()`](https://livioivil.github.io/pima/reference/pima.md). See the
*Engines* section of
[`pima()`](https://livioivil.github.io/pima/reference/pima.md) for the
tests.

## See also

Other PIMA inference:
[`pima()`](https://livioivil.github.io/pima/reference/pima.md),
[`pima-method`](https://livioivil.github.io/pima/reference/pima-method.md),
[`pima_tree()`](https://livioivil.github.io/pima/reference/pima_tree.md)

## Examples

``` r
mods <- list(
  a = glm(Sepal.Width ~ Sepal.Length, data = iris),
  b = glm(Sepal.Width ~ Sepal.Length + Species, data = iris)
)
pima(mods, tested_coeffs = "Sepal.Length", seed = 1,
     engine = pima_flipscores(n_flips = 500, score_type = "basic"))
#> 
#> == Multiverse analysis with 2 scenarios ==
#> 
#>   model                              formula   family     link   Intercept
#> 1     a           Sepal.Width ~ Sepal.Length gaussian identity (Intercept)
#> 2     b Sepal.Width ~ Sepal.Length + Species gaussian identity (Intercept)
#>   Sepal.Length  Species
#> 1 Sepal.Length excluded
#> 2 Sepal.Length  Species
#> 

pima_flipscores(n_flips = 500)
#> <pima engine: flipscores>
#>   n_flips: 500
pima_multcomp()
#> <pima engine: multcomp>
#>   algorithm: NULL
```
