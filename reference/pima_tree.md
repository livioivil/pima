# Classification Tree with Adjusted p-values

This function builds a classification tree based on raw or adjusted
p-values using the results of a regression model.

## Usage

``` r
pima_tree(res, p.values = "adjusted", method = "class", alpha = 0.05, ...)
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

- ...:

  any other parameter of \`rpart::rpart\`.

## Value

A plot of the classification tree using \`rpart.plot()\`.

## Examples

``` r
# Example usage (assuming `res` is a pre-computed result object)
# pvalue_tree(res, p.values = "raw")
```
