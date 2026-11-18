# Classification Tree with Adjusted p-values

This function builds a classification tree based on raw or adjusted
p-values using the results of a regression model.

## Usage

``` r
pima_tree(
  res,
  p.values = "adjusted",
  method = "class",
  alpha = 0.05,
  control = NULL,
  ...
)
```

## Arguments

- res:

  An object of class `pima`, as returned by
  [`pima()`](https://livioivil.github.io/pima/reference/pima.md). Other
  objects (lists of models, `pima.multi` objects, specification tables)
  are not supported.

- p.values:

  A char string indicating which type of p-values to use. Options are
  `"raw"` or `"adjusted"` (default). When `"raw"`, the function uses the
  p-values from the `summary_table`.

- method:

  used in [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html):
  if `method="class"` it will classify significant p-values at level
  `alpha`

- alpha:

  used only when `method="class"`

- control:

  control options passed to
  [`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).
  If `NULL`, `rpart.control(minsplit = 3)` is used.

- ...:

  additional arguments passed to
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html).

## Value

Invisibly returns the fitted `rpart` object.

## See also

Other PIMA inference:
[`pima()`](https://livioivil.github.io/pima/reference/pima.md),
[`pima-method`](https://livioivil.github.io/pima/reference/pima-method.md),
[`pima_flipscores()`](https://livioivil.github.io/pima/reference/pima_flipscores.md)

## Examples

``` r
# Example usage (assuming `res` is a pre-computed result object)
# pima_tree(res, p.values = "raw")
```
