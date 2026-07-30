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

  A list object with a specific structure containing regression model
  results. It should contain \`mods\`, a list of models, and
  \`summary_table\`, a data frame with the summary of results, including
  estimates and p-values.

- p.values:

  A char string indicating which type of p-values to use. Options are
  \`"raw"\` or \`"adjusted"\` (default). When \`"raw"\`, the function
  uses the p-values from the \`summary_table\`.

- method:

  used in function \`rpart::rpart\` if \`method="class"\` it will
  classify significant p-values at level \`alpha\`

- alpha:

  used only when \`method="class"\`

- control:

  control options passed to
  [`rpart.control`](https://rdrr.io/pkg/rpart/man/rpart.control.html).
  If `NULL`, `rpart.control(minsplit = 3)` is used.

- ...:

  additional arguments passed to
  [`rpart`](https://rdrr.io/pkg/rpart/man/rpart.html).

## Value

Invisibly returns the fitted `rpart` object.

## Examples

``` r
# Example usage (assuming `res` is a pre-computed result object)
# pima_tree(res, p.values = "raw")
```
