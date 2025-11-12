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
