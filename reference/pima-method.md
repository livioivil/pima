# Methods for pima objects

Methods for `pima` objects. The following are methods to extract and
manipulate relevant information from a `pima` object.

## Usage

``` r
# S3 method for class 'pima'
print(x, n = 4, ...)

# S3 method for class 'pima'
summary(object, digits = NULL, ...)

as.pima(object, names_obj = NULL, ...)

# S3 method for class 'pima'
plot(
  x,
  by = "coefficient",
  focal = NULL,
  xvar = NULL,
  p.adjusted = TRUE,
  p.transf = "z",
  alpha = 0.05,
  xlab = NULL,
  ylab = NULL,
  regex = FALSE,
  shapes = NULL,
  facet.scales = NULL,
  facet = NULL,
  which.response = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `pima`.

- n:

  number of rows to print from the beginning and end of the scenario
  table.

- ...:

  additional arguments to be passed

- object:

  an object of class `pima`.

- digits:

  number of digits when rounding. Default to \`NULL\` thus no rounding.

- names_obj:

  a vector of names, its length must be equal to the length of `object`

- focal:

  a character vector indicating which coefficients to plot. When \> 1
  coefficient is provided (or NULL) and \`xvar\` is not provided the
  \`Part. Cor\` column is used instead of the \`Estimate\`.

- xvar:

  character indicating the column of the \`object\$summary_table\` to be
  plotted on the x axis. Default to "Estimate".

- p.adjusted:

  logical indicating whether to plot adjusted p-values (`TRUE`, default)
  or raw p-values (`FALSE`).

- p.transf:

  can be a character vector indicating the transformation to use (see
  \[transf_p()\]) or a custom function.

- alpha:

  a value between 0 and 1. The plot will mark the p-values smaller than
  `alpha` (0.05 by default). If equal to 0 or 1 nothing will be marked.

- xlab:

  character vector indicating the x-axis label. Default to \`xvar\`

- ylab:

  character vector indicating the y-axis label. Default to \`p\` or
  \`p.adjust.\<method\>\` where method is \`object\$p.adjust.method\`.

- regex:

  logical. If `TRUE`, the `focal` argument is treated as a regular
  expression to match coefficient names. Default is `FALSE`.

- shapes:

  a numeric vector of length 2 specifying the ggplot2 shapes to use for
  non-significant and significant points, respectively. Default is
  `c(4, 19)`.

- facet.scales:

  character string indicating if scales should be `"fixed"`, `"free"`,
  `"free_x"`, or `"free_y"`. Default is `"free_x"`.

- facet:

  a formula for facetting the plot, passed to
  [`facet_grid`](https://ggplot2.tidyverse.org/reference/facet_grid.html).

- which.response:

  a character vector specifying a subset of response variables to be
  plotted.
