# Transforming p-values

Transforming p-values

## Usage

``` r
transf_p(p, method = "raw")
```

## Arguments

- p:

  a numeric vector with p-values

- method:

  the transformation method. Can be one of "raw" (no transformation),
  "-log10" or "z". Can also be a custom function.

## Value

the transformed p-values

## Details

`"-log10"` returns \\-\log\_{10} p\\. `"z"` returns the absolute normal
quantile corresponding to a two-sided p-value, \$\$z =
\Phi^{-1}\left(1 - \frac{p}{2}\right),\$\$ where \\\Phi\\ is the
standard normal distribution function.

## See also

Other specification tables and plots:
[`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md),
[`spec_scatter()`](https://livioivil.github.io/pima/reference/spec_scatter.md),
[`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md),
[`volcano()`](https://livioivil.github.io/pima/reference/volcano.md)

## Examples

``` r
p <- c(0.01, 0.05, 0.8)
transf_p(p, method = "raw")
#> [1] 0.01 0.05 0.80
transf_p(p, method = "z")
#> [1] 2.5758293 1.9599640 0.2533471
# with custom function
transf_p(p, method = function(x) x/2)
#> [1] 0.005 0.025 0.400
```
