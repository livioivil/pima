# Methods for pima objects

Methods for `pima` objects. The following are methods to extract and
manipulate relevant information from a `pima` object.

## Usage

``` r
# S3 method for class 'pima'
print(object, n = 4)

# S3 method for class 'pima'
summary(object, digits = NULL, ...)

# S3 method for class 'pima'
as(object, names_obj = NULL, ...)

# S3 method for class 'pima'
plot(
  object,
  focal = NULL,
  xvar = NULL,
  p.adjusted = TRUE,
  p.transf = "z",
  alpha = 0.05,
  xlab = NULL,
  ylab = NULL,
  regex = FALSE,
  shapes = NULL,
  facet = FALSE,
  facet.scales = NULL,
  ...
)
```

## Arguments

- object:

  an object of class `pima`.

- digits:

  number of digits when rounding. Default to \`NULL\` thus no rounding.

- ...:

  additional arguments to be passed

- names_obj:

  a vector of names, its length must be equal to the length of `object`

- focal:

  a character vector indicating which coefficients to plot. When \> 1
  coefficient is provided (or NULL) and \`xvar\` is not provided the
  \`Part. Cor\` column is used instead of the \`Estimate\`.

- xvar:

  character indicating the column of the \`object\$summary_table\` to be
  plotted on the x axis. Default to "Estimate".

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

- p.adjust:

  logical indicating whether plotting raw (\`FALSE\`) or adjusted
  p.values (\`TRUE\`, default).
