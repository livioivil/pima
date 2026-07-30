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

- transf.focal:

  logical; if `TRUE`, transformations are also applied to focal
  predictors. It can be length 1 or the same length as `focal`.

- subset:

  optional named list of logical vectors defining row subsets for
  additional scenarios.

- fit.fun:

  optional model fitting function, such as `glm`. If supplied, fitted
  models are added to the output.

- fit.fun.args:

  optional list of additional arguments passed to `fit.fun`.

## Value

a list

## Examples

``` r
create_multi(~ Sepal.Length + Petal.Width + Species, 
             focal = "Sepal.Length", 
             nfuns = c("log"), 
             data = iris)
#> $variables
#>        fun            x    type focal .id_fun .id_x             call
#> 1 identity Sepal.Length numeric  TRUE       1     2     Sepal.Length
#> 2 identity  Petal.Width numeric FALSE       1     1      Petal.Width
#> 3 identity      Species  factor FALSE       1     3          Species
#> 5      log  Petal.Width numeric FALSE       2     1 log(Petal.Width)
#> 
#> $calls
#> [1] " ~ Sepal.Length"                             
#> [2] " ~ Sepal.Length + Petal.Width"               
#> [3] " ~ Sepal.Length + Species"                   
#> [4] " ~ Sepal.Length + log(Petal.Width)"          
#> [5] " ~ Sepal.Length + Petal.Width + Species"     
#> [6] " ~ Sepal.Length + Species + log(Petal.Width)"
#> 
#> $subset
#> $subset$all
#>   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> [121] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> [136] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> 
#> $scenarios
#>                                          calls subset  model
#> 1                               ~ Sepal.Length    all model1
#> 2                 ~ Sepal.Length + Petal.Width    all model2
#> 3                     ~ Sepal.Length + Species    all model3
#> 4            ~ Sepal.Length + log(Petal.Width)    all model4
#> 5       ~ Sepal.Length + Petal.Width + Species    all model5
#> 6  ~ Sepal.Length + Species + log(Petal.Width)    all model6
#> 
```
