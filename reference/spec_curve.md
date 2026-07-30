# Specification Curve Analysis

This function performs a specification curve analysis based on the
results of a set of regression models. It visualizes the coefficient
estimates with confidence intervals, p-values, and highlights
significant specifications.

## Usage

``` r
spec_curve(
  x,
  focal = NULL,
  yvar = NULL,
  yname = NULL,
  p.adjusted = NULL,
  alpha = 0.05,
  tbr = c(0.4, 0.6),
  colors = NULL,
  shapes = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  top.theme = NULL,
  bottom.theme = NULL,
  redundant = TRUE,
  conf.int = FALSE,
  facet.y = FALSE,
  which.response = NULL
)
```

## Arguments

- x:

  An object of class \`pima\`, usually the result of the \`pima()\`
  function.

- focal:

  A character vector of focal coefficients to filter. If \`NULL\`,
  defaults to all tested coefficients in \`x\`.

- yvar:

  Character indicating the column of the \`x\$summary_table\` object to
  be used in the y-axis of the top plot (usually the estimated
  parameter). Defaults to \`"estimate"\`.

- yname:

  Character indicating the name of the response variable to be plotted
  if the \`pima\` object contains more than one variable. If \`NULL\`
  (the default), all response variables are plotted.

- p.adjusted:

  Logical indicating whether to use adjusted p-values for determining
  significance. Defaults to \`TRUE\` if an adjustment method was
  specified in the \`pima\` object.

- alpha:

  A numeric value specifying the significance level for the confidence
  intervals and color-coding. Default is 0.05.

- tbr:

  A numeric vector of two elements indicating the vertical space ratio
  assigned to the top and bottom plots (e.g., \`c(0.4, 0.6)\`).

- colors:

  A character vector of two elements specifying the colors for
  non-significant and significant results.

- shapes:

  A numeric vector of two elements specifying the shapes for
  non-significant and significant results. Default to \`c(4, 19)\`.

- title:

  A character string for the overall plot title.

- xlab:

  A character string for the x-axis title. Default to "Specification".

- ylab:

  A character string for the y-axis title of the top plot. Default to
  the value of \`yvar\`.

- top.theme:

  A function returning a \`ggplot2\` theme for the top plot. Default to
  \`ggplot2::theme_minimal()\`.

- bottom.theme:

  A function returning a \`ggplot2\` theme for the bottom plot. Default
  to \`ggplot2::theme_minimal()\`.

- redundant:

  Logical. If \`TRUE\`, removes variables that do not vary across
  specifications from the bottom plot.

- conf.int:

  Logical. If \`TRUE\`, includes confidence intervals around estimated
  coefficients. (Ignored if \`yvar\` is not \`"estimate"\`).

- facet.y:

  Logical. If \`TRUE\`, creates separate facets for each response
  variable in the top plot. Default is \`FALSE\`.

- which.response:

  A character vector specifying which response variables to include in
  the plot.

## Value

A \[\`patchwork\`\] object consisting of two aligned \`ggplot2\` plots.
The top plot shows estimates/p-values, and the bottom plot shows the
specification grid.

## Examples

``` r
# Example usage (assuming `res` is a pre-computed pima object):
# spec_curve(res, alpha = 0.05, conf.int = TRUE)
```
