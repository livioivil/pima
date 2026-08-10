# Create scenarios for a multiverse analysis

\`create_multi()\` generates a multiverse by crossing admissible
analytical decisions about predictor inclusion and transformation with
optional sample subsets and model-fitting specifications.

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
  fit.specs = NULL
)
```

## Arguments

- formula:

  A formula describing the maximal model. Predictors on the right-hand
  side must be bare variable names combined additively. Transformations,
  interactions, and \`.\` are currently not supported in \`formula\`.
  Transformations should instead be supplied through \`nfuns\` or
  \`cfuns\`. Both one-sided formulas (e.g. \`~ x + z\`) and two-sided
  formulas (e.g. \`y ~ x + z\`) are supported. Whether a one-sided
  formula can be fitted depends on the selected fitting function or
  adapter.

- data:

  A data frame containing all variables referenced in \`formula\`.

- focal:

  Optional character vector naming predictors that must be included in
  every formula specification. By default focal predictors are not
  transformed; use \`transf.focal\` to allow their transformations.

- nfuns:

  Optional transformations for numeric predictors. Supply either a
  character vector of function names to apply to every numeric predictor
  (e.g. \`c("log", "sqrt")\`) or a named list for variable-specific
  choices (e.g. \`list(x = "log", z = c("log", "sqrt"))\`). Functions
  are supplied as character strings and are applied to one predictor at
  a time.

- cfuns:

  Same as \`nfuns\`, but for non-numeric predictors.

- transf.focal:

  Logical indicating whether transformations should also be considered
  for focal predictors. It can have length 1 or the same length as
  \`focal\`.

- subset:

  Optional named list of logical vectors defining additional row
  subsets. Each vector must have length \`nrow(data)\` and contain no
  missing values. The full dataset is always added as the reserved
  subset \`"all"\`.

- fit.specs:

  Optional named list of model-fitting specifications. Each element can
  be either a fitting function that explicitly accepts \`formula\` and
  \`data\`, such as \`stats::lm\` or \`stats::glm\`, or a specification
  created with \[create_fit()\]. Passing a function directly is
  shorthand for a standard specification with no additional arguments.
  Use \[create_fit()\] when additional fitting arguments are needed or
  when an adapter is required for a non-standard fitting interface. Each
  fitting specification is crossed with every formula and subset
  specification. Manually constructed lists with the same internal
  structure are also accepted for backward compatibility.

## Value

A list with the following components:

- variables:

  A data frame describing all available predictor-level specifications
  and their corresponding formula calls.

- formula_specs:

  A data frame containing one row per admissible formula, its
  \`formula_id\`, executable formula string in \`calls\`, and one
  \`spec\_\<predictor\>\` column per predictor describing the selected
  analytical decision.

- calls:

  A character vector containing all admissible formula strings.

- subset:

  A named list of logical row selectors, including \`all\`.

- scenarios:

  A data frame containing the Cartesian product of formula
  specifications, subsets, and, when supplied, fitting specifications.
  Each row has a unique \`model\` identifier. If models are fitted, this
  also contains \`fit_ok\` and \`fit_error\`.

- fit.specs:

  The normalized fitting specifications, returned only when
  \`fit.specs\` is supplied.

- mods:

  A named list of fitted model objects, returned only when \`fit.specs\`
  is supplied. Failed fits are stored as error objects.

## Details

Predictor choices are generated as a Cartesian product. For a non-focal
predictor the available decisions are exclusion, identity, and any
supplied transformations. A focal predictor cannot be excluded and can
only be transformed when permitted by \`transf.focal\`.

The analytical decisions used to construct each formula are retained
explicitly. In \`formula_specs\` and \`scenarios\`, each predictor has a
column named \`spec\_\<predictor\>\` whose value is \`"excluded"\`,
\`"identity"\`, or the name of the selected transformation. This makes
downstream analyses of specification choices possible without parsing
formula strings.

When \`fit.specs\` is supplied, fitting errors do not stop the entire
multiverse. The corresponding element of \`mods\` contains the error
object, while \`scenarios\$fit_ok\` and \`scenarios\$fit_error\` record
its status. Warnings emitted by fitting functions are handled normally
by R and are not stored in the returned object.

## See also

\[create_fit()\]

## Examples

``` r
# Generate formula specifications without fitting models.
m1 <- create_multi(
  ~ Sepal.Length + Petal.Width + Species,
  data = iris,
  focal = "Sepal.Length",
  nfuns = "log"
)

m1$formula_specs
#>   formula_id                                       calls spec_Sepal.Length
#> 1   formula1                              ~ Sepal.Length          identity
#> 2   formula2                ~ Sepal.Length + Petal.Width          identity
#> 3   formula3           ~ Sepal.Length + log(Petal.Width)          identity
#> 4   formula4                    ~ Sepal.Length + Species          identity
#> 5   formula5      ~ Sepal.Length + Petal.Width + Species          identity
#> 6   formula6 ~ Sepal.Length + log(Petal.Width) + Species          identity
#>   spec_Petal.Width spec_Species
#> 1         excluded     excluded
#> 2         identity     excluded
#> 3              log     excluded
#> 4         excluded     identity
#> 5         identity     identity
#> 6              log     identity

# Allow transformations of the focal predictor and cross formulas with
# alternative fitting specifications.
m2 <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width,
  data = iris,
  focal = "Sepal.Length",
  nfuns = list(
    Sepal.Length = "log",
    Petal.Width = c("log", "sqrt")
  ),
  transf.focal = TRUE,
  fit.specs = list(
    lm = stats::lm,
    gaussian = stats::glm,
    poisson = create_fit(
      stats::glm,
      family = stats::poisson(link = "log")
    )
  )
)
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.400000
#> Warning: non-integer x = 3.900000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 4.100000
#> Warning: non-integer x = 4.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.500000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.400000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.900000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.600000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 2.200000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 3.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.800000
#> Warning: non-integer x = 2.600000
#> Warning: non-integer x = 3.400000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 3.100000
#> Warning: non-integer x = 2.700000
#> Warning: non-integer x = 3.200000
#> Warning: non-integer x = 3.300000
#> Warning: non-integer x = 2.500000
#> Warning: non-integer x = 3.400000

m2$scenarios
#>      model formula_id                                               calls
#> 1   model1   formula1                          Sepal.Width ~ Sepal.Length
#> 2   model2   formula2                     Sepal.Width ~ log(Sepal.Length)
#> 3   model3   formula3            Sepal.Width ~ Sepal.Length + Petal.Width
#> 4   model4   formula4       Sepal.Width ~ log(Sepal.Length) + Petal.Width
#> 5   model5   formula5       Sepal.Width ~ Sepal.Length + log(Petal.Width)
#> 6   model6   formula6  Sepal.Width ~ log(Sepal.Length) + log(Petal.Width)
#> 7   model7   formula7      Sepal.Width ~ Sepal.Length + sqrt(Petal.Width)
#> 8   model8   formula8 Sepal.Width ~ log(Sepal.Length) + sqrt(Petal.Width)
#> 9   model9   formula1                          Sepal.Width ~ Sepal.Length
#> 10 model10   formula2                     Sepal.Width ~ log(Sepal.Length)
#> 11 model11   formula3            Sepal.Width ~ Sepal.Length + Petal.Width
#> 12 model12   formula4       Sepal.Width ~ log(Sepal.Length) + Petal.Width
#> 13 model13   formula5       Sepal.Width ~ Sepal.Length + log(Petal.Width)
#> 14 model14   formula6  Sepal.Width ~ log(Sepal.Length) + log(Petal.Width)
#> 15 model15   formula7      Sepal.Width ~ Sepal.Length + sqrt(Petal.Width)
#> 16 model16   formula8 Sepal.Width ~ log(Sepal.Length) + sqrt(Petal.Width)
#> 17 model17   formula1                          Sepal.Width ~ Sepal.Length
#> 18 model18   formula2                     Sepal.Width ~ log(Sepal.Length)
#> 19 model19   formula3            Sepal.Width ~ Sepal.Length + Petal.Width
#> 20 model20   formula4       Sepal.Width ~ log(Sepal.Length) + Petal.Width
#> 21 model21   formula5       Sepal.Width ~ Sepal.Length + log(Petal.Width)
#> 22 model22   formula6  Sepal.Width ~ log(Sepal.Length) + log(Petal.Width)
#> 23 model23   formula7      Sepal.Width ~ Sepal.Length + sqrt(Petal.Width)
#> 24 model24   formula8 Sepal.Width ~ log(Sepal.Length) + sqrt(Petal.Width)
#>    spec_Sepal.Length spec_Petal.Width subset fit_spec fit_ok fit_error
#> 1           identity         excluded    all       lm   TRUE      <NA>
#> 2                log         excluded    all       lm   TRUE      <NA>
#> 3           identity         identity    all       lm   TRUE      <NA>
#> 4                log         identity    all       lm   TRUE      <NA>
#> 5           identity              log    all       lm   TRUE      <NA>
#> 6                log              log    all       lm   TRUE      <NA>
#> 7           identity             sqrt    all       lm   TRUE      <NA>
#> 8                log             sqrt    all       lm   TRUE      <NA>
#> 9           identity         excluded    all gaussian   TRUE      <NA>
#> 10               log         excluded    all gaussian   TRUE      <NA>
#> 11          identity         identity    all gaussian   TRUE      <NA>
#> 12               log         identity    all gaussian   TRUE      <NA>
#> 13          identity              log    all gaussian   TRUE      <NA>
#> 14               log              log    all gaussian   TRUE      <NA>
#> 15          identity             sqrt    all gaussian   TRUE      <NA>
#> 16               log             sqrt    all gaussian   TRUE      <NA>
#> 17          identity         excluded    all  poisson   TRUE      <NA>
#> 18               log         excluded    all  poisson   TRUE      <NA>
#> 19          identity         identity    all  poisson   TRUE      <NA>
#> 20               log         identity    all  poisson   TRUE      <NA>
#> 21          identity              log    all  poisson   TRUE      <NA>
#> 22               log              log    all  poisson   TRUE      <NA>
#> 23          identity             sqrt    all  poisson   TRUE      <NA>
#> 24               log             sqrt    all  poisson   TRUE      <NA>
```
