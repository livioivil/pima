# A character vector with the title for the overall plot (applied to the top plot internally).

Specification Curve Analysis

## Usage

``` r
spec_curve(
  x,
  focal = NULL,
  yvar = NULL,
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
  conf.int = FALSE
)
```

## Arguments

- x:

  an object of class \`pima\`, usually the result of the \`pima()\`
  function.

- yvar:

  character indicating the column of the \`x\$summary_table\` object to
  be used in the y axes of the top part of the specification curve
  (usually the estimated parameter). Default to \`"Estimate"\`.

- alpha:

  A numeric value specifying the significance level for the confidence
  intervals. Default is 0.05.

- tbr:

  A numeric vector of two elements indicating the proportion of space
  assigned to the top and bottom part of the plot.

- colors:

  A character vector of two elements with the colors for the point
  representing non-significant and significant p-values.

- xlab:

  a character vector for the x axis title. Default to "Specification"

- ylab:

  a character vector for the y axis title of the top plot. Default to
  \`yvar\`.

- top.theme:

  a function with a \`ggplot2\` compatible theme for the top plot.
  Default to ggplot2::theme_minimal()\`

- bottom.theme:

  a function with a \`ggplot2\` compatible theme for the bottom plot
  Default to \`ggplot2::theme_minimal()\`

- p.values:

  character indicating the column of the \`x\$summary_table\` object
  with the p values. Default to \`""p.adj.maxT"\` (maxT corrected p
  values).

- A:

  character vector of two elements with the shapes representing
  non-significant and significant p-values. Default to simple points.

## Value

A plot displaying the specification curve with confidence intervals and
p-values, as well as a legend showing the variable combinations used in
each specification. The output object is a \[\`patchwork\`\] object thus
a collection of \`ggplot2\` objects. The underlying datasets can be
accessed using \`@data\` for each plot.

## Details

This function performs a specification curve analysis based on the
results of a set of regression models. It visualizes the coefficient
estimates with confidence intervals, p-values, and highlights
significant specifications.

## Examples

``` r
# Example usage (assuming `res` is a pre-computed result object):
# spec_curve(res, alpha = 0.05)
```
