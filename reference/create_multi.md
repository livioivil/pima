# Create the scenarios for the multiverse

Create the scenarios for the multiverse

## Usage

``` r
create_multi(
  formula,
  data,
  focal = NULL,
  nfuns = NULL,
  cfuns = NULL,
  transf.focal = FALSE,
  subset = NULL,
  fit.fun = NULL,
  fit.fun.args = NULL
)
```

## Arguments

- formula:

  a formula describing the maximal model with bare variables (no
  tranformations). Currently not supporting interactions.

- data:

  the dataset for the model

- focal:

  optional name of the coefficient that is the focus of the analysis. No
  transformation will be applied to that variable and no model will
  exclude this variable.

- nfuns:

  functions to be applied to numerical variables. functions need to be
  provided as characters. this argument can be a vector of functions
  (e.g., \`c("log", "exp")\`) and the functions will be applied al all
  numerical variable exluding the focal predictor. In alternative, can
  be a named list specifing the name of the variable and the functions
  as string (e.g., \`list(x = "log", z = c("exp"))\`) in this way the
  functions are variable-specific. If \`NULL\` no transformations will
  be applied.

- cfuns:

  same as the \`nfuns\` but for factor/character variables. If \`NULL\`
  no transformations will be applied.

## Value

a list

## Examples

``` r
create_multi(~ Sepal.Length + Petal.Width + Species, 
             focal = "Sepal.Length", 
             nfuns = c("log"), 
             data = iris)
#> Error in create_multi(~Sepal.Length + Petal.Width + Species, focal = "Sepal.Length",     nfuns = c("log"), data = iris): could not find function "create_multi"
```
