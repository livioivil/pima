# PIMA workflows

``` r

library(pima)
library(ggplot2)

n_flips_vignette <- 1000

spec_top_theme <- function() {
  ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12)
    )
}

spec_bottom_theme <- function() {
  ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      strip.text.y = ggplot2::element_text(size = 7, angle = 0),
      panel.spacing.y = grid::unit(0.08, "lines")
    )
}
```

`pima` is designed for post-selection inference after a multiverse
analysis: fit several plausible models, test one or more focal
coefficients jointly, and then inspect how estimates and p-values behave
across specifications.

The examples use `n_flips = 1000` and knitr caching, which is enough for
a stable workflow preview without making the vignette too slow. Use more
flips when the numerical result matters.

## A minimal multiverse as a list of fitted models

The most direct input to
[`pima()`](https://livioivil.github.io/pima/reference/pima.md) is a
named list of fitted models. Here we use the package’s Pima Indians
diabetes data and test the coefficient for number of pregnancies across
a few logistic regressions.

``` r

data("pimads")

pima_data <- na.omit(pimads)
pima_data$diabetes01 <- as.integer(pima_data$diabetes == "pos")

mods <- list(
  full = glm(
    diabetes01 ~ npreg + glucose + pressure + bmi + age,
    data = pima_data,
    family = binomial()
  ),
  compact = glm(
    diabetes01 ~ npreg + glucose + bmi,
    data = pima_data,
    family = binomial()
  ),
  sqrt_bmi = glm(
    diabetes01 ~ npreg + glucose + sqrt(bmi) + age,
    data = pima_data,
    family = binomial()
  ),
  glucose_only = glm(
    diabetes01 ~ npreg + glucose,
    data = pima_data,
    family = binomial()
  )
)

manual_res <- pima(
  mods,
  tested_coeffs = "npreg",
  n_flips = n_flips_vignette,
  method = "maxT",
  score_type = "standardized",
  seed = 1
)

summary(manual_res, digits = 3)
#>          model   response coefficient estimate   score     se     z  pcor     p
#> 1      compact diabetes01       npreg    0.168 159.150 31.103 5.117 0.226 0.001
#> 2         full diabetes01       npreg    0.114  64.253 23.866 2.692 0.119 0.014
#> 3 glucose_only diabetes01       npreg    0.151 152.211 31.762 4.792 0.208 0.001
#> 4     sqrt_bmi diabetes01       npreg    0.113  64.283 23.988 2.680 0.119 0.014
#>   p.adj   family  link
#> 1 0.001 binomial logit
#> 2 0.014 binomial logit
#> 3 0.001 binomial logit
#> 4 0.014 binomial logit
```

The main arguments are:

- `mods`: a list of model objects, usually `glm` objects.
- `tested_coeffs`: coefficients to test. It can be a character vector
  common to all models, or a list with one character vector per model.
- `n_flips`: number of sign flips.
- `method`: multiplicity adjustment, `"maxT"`, `"minP"`, or `"none"`.
- `score_type`, `seed`, `output_models`, and other arguments are passed
  to
  [`flipscores::flipscores()`](https://livioivil.github.io/flipscores/reference/flipscores.html).

If the same conceptual effect has different coefficient names across
models, use a list for `tested_coeffs`.

``` r

mods_mixed <- list(
  linear = glm(
    diabetes01 ~ npreg + glucose + bmi,
    data = pima_data,
    family = binomial()
  ),
  quadratic = glm(
    diabetes01 ~ I(npreg^2) + glucose + bmi,
    data = pima_data,
    family = binomial()
  )
)

mixed_res <- pima(
  mods_mixed,
  tested_coeffs = list(
    linear = "npreg",
    quadratic = "I(npreg^2)"
  ),
  n_flips = n_flips_vignette,
  seed = 1
)

summary(mixed_res, digits = 3)
#>       model   response coefficient estimate    score      se     z  pcor     p
#> 1    linear diabetes01       npreg    0.168  159.150  31.103 5.117 0.226 0.001
#> 2 quadratic diabetes01  I(npreg^2)    0.015 1807.835 366.472 4.933 0.218 0.001
#>   p.adj   family  link
#> 1 0.001 binomial logit
#> 2 0.001 binomial logit
```

## Adding specification metadata

When models are supplied manually,
[`pima()`](https://livioivil.github.io/pima/reference/pima.md) tries to
reconstruct the formula decisions from the fitted models. You can add
your own scenario-level metadata with `extra`; this is useful for
plotting or grouping by analytical decisions.

``` r

extra <- data.frame(
  model = names(mods),
  adjustment = c("full", "compact", "sqrt_bmi", "minimal"),
  includes_age = c(TRUE, FALSE, TRUE, FALSE)
)

manual_res <- pima(
  mods,
  tested_coeffs = "npreg",
  n_flips = n_flips_vignette,
  seed = 1,
  extra = extra
)

manual_res
#> 
#> == Multiverse analysis with 4 scenarios ==
#> 
#>          model                                             formula   family
#> 1      compact                  diabetes01 ~ npreg + glucose + bmi binomial
#> 2         full diabetes01 ~ npreg + glucose + pressure + bmi + age binomial
#> 3 glucose_only                        diabetes01 ~ npreg + glucose binomial
#> 4     sqrt_bmi      diabetes01 ~ npreg + glucose + sqrt(bmi) + age binomial
#>    link   Intercept npreg glucose pressure       bmi  age adjustment
#> 1 logit (Intercept) npreg glucose     <NA>       bmi <NA>    compact
#> 2 logit (Intercept) npreg glucose pressure       bmi  age       full
#> 3 logit (Intercept) npreg glucose     <NA>      <NA> <NA>    minimal
#> 4 logit (Intercept) npreg glucose     <NA> sqrt(bmi)  age   sqrt_bmi
#>   includes_age
#> 1        FALSE
#> 2         TRUE
#> 3        FALSE
#> 4         TRUE
```

## Plotting a PIMA result

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) creates a
volcano-style view: an estimate-like quantity on the x-axis and raw or
adjusted p-values on the y-axis. The p-value transformation can be
`"z"`, `"-log10"`, `"raw"`, or a custom function.

``` r

plot(
  manual_res,
  focal = "npreg",
  xvar = "estimate",
  p.adjusted = TRUE,
  p.transf = "-log10"
)
```

![](pima-workflows_files/figure-html/plot-basic-1.png)

The same plot can be tuned with the arguments users usually need in
practice:

- `xvar`: any numeric column in `summary(res)`, for example
  `"estimate"`, `"pcor"`, `"score"` or `"z"`;
- `p.adjusted`: `TRUE` for adjusted p-values, `FALSE` for raw p-values;
- `p.transf`: `"z"`, `"-log10"`, `"raw"` or a custom function;
- `by`: grouping variable, including metadata supplied through `extra`;
- `facet`, `facet.scales`, `shapes`, `alpha`, `xlab`, and `ylab`.

``` r

plot(
  manual_res,
  by = "adjustment",
  xvar = "pcor",
  p.adjusted = FALSE,
  p.transf = "z",
  alpha = 0.05,
  shapes = c(1, 16),
  xlab = "Partial correlation",
  ylab = "Raw p-value, z scale"
)
```

![](pima-workflows_files/figure-html/plot-by-extra-1.png)

Regular expressions are handy when coefficient names are generated by
transformations or factor contrasts.

``` r

plot(
  mixed_res,
  focal = "npreg",
  regex = TRUE,
  xvar = "estimate",
  p.adjusted = FALSE,
  p.transf = "raw",
  alpha = 0.10,
  facet.scales = "free"
)
```

![](pima-workflows_files/figure-html/plot-regex-1.png)

`volcano()` adds quantile reference lines and p-value cutoffs on top of
the same plotting idea.

``` r

volcano(
  manual_res,
  focal = "npreg",
  p.adjusted = FALSE,
  p.cut = c(0.10, 0.05, 0.01),
  q = c(0.10, 0.50, 0.90)
)
```

![](pima-workflows_files/figure-html/volcano-1.png)

## Specification curves

[`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md)
combines two panels: the top panel shows the result for each
specification, and the bottom panel shows the analytical decisions that
generated that specification.

``` r

spec_curve(
  manual_res,
  focal = "npreg",
  yvar = "estimate",
  p.adjusted = TRUE,
  alpha = 0.05,
  tbr = c(0.55, 0.45),
  colors = c("grey45", "#0072B2"),
  shapes = c(1, 16),
  title = "Manual multiverse: coefficient estimates",
  xlab = "Model specification",
  ylab = "Log-odds estimate",
  top.theme = spec_top_theme,
  bottom.theme = spec_bottom_theme,
  conf.int = TRUE,
  redundant = FALSE
)
```

![](pima-workflows_files/figure-html/spec-curve-manual-1.png)

If you test several coefficients or several responses,
[`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md)
can facet the result.

``` r

set.seed(123)
n <- 30
sim_data <- data.frame(
  x = rnorm(n),
  z1 = rnorm(n),
  z2 = rnorm(n)
)
sim_data$y1 <- sim_data$x + sim_data$z1 + rnorm(n)
sim_data$y2 <- sim_data$x + sim_data$z2 + rnorm(n)

sim_mods <- list(
  y1_linear = glm(y1 ~ x + z1 + z2, data = sim_data),
  y1_poly = glm(y1 ~ x + poly(z1, 2) + z2, data = sim_data),
  y2_linear = glm(y2 ~ x + z1 + z2, data = sim_data),
  y2_poly = glm(y2 ~ x + z1 + poly(z2, 2), data = sim_data)
)

sim_res <- pima(
  sim_mods,
  tested_coeffs = "x",
  n_flips = n_flips_vignette,
  seed = 1
)

plot(sim_res, facet = response ~ ., p.adjusted = FALSE)
```

![](pima-workflows_files/figure-html/multi-response-1.png)

``` r


spec_curve(
  sim_res,
  focal = "x",
  p.adjusted = FALSE,
  facet.y = TRUE,
  tbr = c(0.60, 0.40),
  which.response = c("y1", "y2"),
  title = "Two responses, one focal coefficient",
  top.theme = spec_top_theme,
  bottom.theme = spec_bottom_theme,
  redundant = FALSE
)
```

![](pima-workflows_files/figure-html/multi-response-2.png)

## Creating a multiverse with `create_multi()`

[`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md)
builds the scenario grid for you. It crosses:

- predictor inclusion and transformations;
- optional row subsets;
- optional fitting specifications.

The right-hand side of `formula` must contain bare additive predictors.
Put transformations in `nfuns` or `cfuns`, not directly inside the
formula.

``` r

multi <- create_multi(
  diabetes01 ~ npreg + glucose + bmi + age,
  data = pima_data,
  focal = "npreg",
  nfuns = list(
    glucose = "sqrt",
    bmi = "sqrt",
    age = "sqrt"
  ),
  subset = list(
    high_pressure = pima_data$pressure > median(pima_data$pressure)
  ),
  fit.specs = list(
    logit = create_fit(
      stats::glm,
      family = stats::binomial()
    ),
    probit = create_fit(
      stats::glm,
      family = stats::binomial(link = "probit")
    )
  )
)

multi
#> -----------------
#> Multiverse Object
#> -----------------
#> Formula: diabetes01 ~ npreg + glucose + bmi + age 
#> 
#> Scenarios: n = 108 
#>   > Formulas: n = 27 
#>   > Subsets: n = 1 
#>   > Models: n = 108 
#> 
#> Models info 
#>   > Errors: n = 0

head(multi$scenarios)
#>    model formula_id                                    calls spec_npreg
#> 1 model1   formula1                       diabetes01 ~ npreg   identity
#> 2 model2   formula2             diabetes01 ~ npreg + glucose   identity
#> 3 model3   formula3       diabetes01 ~ npreg + sqrt(glucose)   identity
#> 4 model4   formula4                 diabetes01 ~ npreg + bmi   identity
#> 5 model5   formula5       diabetes01 ~ npreg + glucose + bmi   identity
#> 6 model6   formula6 diabetes01 ~ npreg + sqrt(glucose) + bmi   identity
#>   spec_glucose spec_bmi spec_age subset fit_spec fit_ok fit_error
#> 1     excluded excluded excluded    all    logit   TRUE      <NA>
#> 2     identity excluded excluded    all    logit   TRUE      <NA>
#> 3         sqrt excluded excluded    all    logit   TRUE      <NA>
#> 4     excluded identity excluded    all    logit   TRUE      <NA>
#> 5     identity identity excluded    all    logit   TRUE      <NA>
#> 6         sqrt identity excluded    all    logit   TRUE      <NA>
```

The `spec_<variable>` columns record the analytical choice for each
predictor: `"excluded"`, `"identity"`, or the transformation name.

Models are fitted when `fit.specs` is supplied. Failed fits are kept as
error objects and marked in `scenarios$fit_ok`, so it is usually
convenient to subset before calling
[`pima()`](https://livioivil.github.io/pima/reference/pima.md).

``` r

multi_ok <- subset(multi, fit_ok)

multi_res <- pima(
  multi_ok,
  tested_coeffs = "npreg",
  n_flips = n_flips_vignette,
  seed = 1
)

head(summary(multi_res, digits = 3))
#>      model   response coefficient estimate   score     se     z  pcor     p
#> 1   model1 diabetes01       npreg    0.159 209.506 35.961 5.826 0.253 0.001
#> 2  model10 diabetes01       npreg    0.056  42.387 27.485 1.542 0.067 0.149
#> 3 model100 diabetes01       npreg    0.043  50.964 34.045 1.497 0.098 0.146
#> 4 model101 diabetes01       npreg    0.068  73.101 31.958 2.287 0.149 0.041
#> 5 model102 diabetes01       npreg    0.067  72.481 31.978 2.267 0.148 0.041
#> 6 model103 diabetes01       npreg    0.061  69.035 33.177 2.081 0.137 0.052
#>   p.adj        subset fit_spec   family   link
#> 1 0.001           all    logit binomial  logit
#> 2 0.300           all    logit binomial  logit
#> 3 0.300 high_pressure   probit binomial probit
#> 4 0.200 high_pressure   probit binomial probit
#> 5 0.200 high_pressure   probit binomial probit
#> 6 0.200 high_pressure   probit binomial probit
```

The metadata from
[`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md)
is carried into the `pima` result. For a large grid, the scatter plot is
useful as an overview.

``` r

plot(
  multi_res,
  by = c("subset", "fit_spec"),
  xvar = "pcor",
  p.adjusted = FALSE
)
```

![](pima-workflows_files/figure-html/create-multi-plots-1.png)

A full specification curve with more than one hundred scenarios is
usually too dense for a vignette page. A good reporting pattern is to
show the complete overview above and then draw focused curves for
meaningful slices of the multiverse.

``` r

multi_logit_all <- subset(
  multi_ok,
  subset == "all" & fit_spec == "logit"
)

multi_logit_res <- pima(
  multi_logit_all,
  tested_coeffs = "npreg",
  n_flips = n_flips_vignette,
  seed = 1
)

spec_curve(
  multi_logit_res,
  focal = "npreg",
  yvar = "pcor",
  p.adjusted = FALSE,
  alpha = 0.05,
  tbr = c(0.62, 0.38),
  colors = c("grey55", "#D55E00"),
  shapes = c(1, 16),
  title = "Focused specification curve: all rows, logit models",
  ylab = "Partial correlation",
  top.theme = spec_top_theme,
  bottom.theme = spec_bottom_theme,
  redundant = FALSE
)
```

![](pima-workflows_files/figure-html/create-multi-focused-1.png)

When you really want to inspect every scenario, export the complete
curve as a standalone SVG/PDF instead of forcing it into the vignette
body. The code below is not evaluated here because a 108-scenario curve
is too dense for inline reading.

``` r

full_curve <- spec_curve(
  multi_res,
  focal = "npreg",
  yvar = "pcor",
  p.adjusted = FALSE,
  tbr = c(0.55, 0.45),
  title = "Full specification curve",
  top.theme = spec_top_theme,
  bottom.theme = spec_bottom_theme,
  redundant = FALSE
)

ggplot2::ggsave(
  "multi-full-spec-curve.svg",
  full_curve,
  width = 16,
  height = 10
)
```

`transf.focal = TRUE` allows transformations of focal predictors too.
This is useful when the focal effect itself can be parameterized in
multiple ways.

``` r

multi_focal <- create_multi(
  diabetes01 ~ npreg + glucose + bmi,
  data = pima_data,
  focal = "npreg",
  nfuns = list(
    npreg = "sqrt",
    glucose = "sqrt",
    bmi = "sqrt"
  ),
  transf.focal = TRUE,
  fit.specs = list(
    logit = create_fit(
      stats::glm,
      family = stats::binomial()
    )
  )
)

head(multi_focal$formula_specs)
#>   formula_id                                    calls spec_npreg spec_glucose
#> 1   formula1                       diabetes01 ~ npreg   identity     excluded
#> 2   formula2                 diabetes01 ~ sqrt(npreg)       sqrt     excluded
#> 3   formula3             diabetes01 ~ npreg + glucose   identity     identity
#> 4   formula4       diabetes01 ~ sqrt(npreg) + glucose       sqrt     identity
#> 5   formula5       diabetes01 ~ npreg + sqrt(glucose)   identity         sqrt
#> 6   formula6 diabetes01 ~ sqrt(npreg) + sqrt(glucose)       sqrt         sqrt
#>   spec_bmi
#> 1 excluded
#> 2 excluded
#> 3 excluded
#> 4 excluded
#> 5 excluded
#> 6 excluded
```

## Using the `multiverse` package

[`pima()`](https://livioivil.github.io/pima/reference/pima.md) also has
experimental support for objects from the `multiverse` package. The key
convention is that each universe must create a fitted model called
`fit`.

This example uses the `hurricane` data shipped by `multiverse`, not the
`hurricane` data shipped by `pima`.

``` r

library(multiverse)

data("hurricane", package = "multiverse")

hurricane_mv <- transform(
  hurricane,
  log_NDAM = log(NDAM + 1)
)

M <- multiverse()

inside(M, {
  fit <- glm(
    alldeaths ~
      MasFem +
      branch(
        pressure,
        "original" ~ MinPressure_before,
        "updated" ~ Minpressure_Updated_2014
      ) +
      branch(
        damage,
        "raw" ~ NDAM,
        "logged" ~ log_NDAM
      ),
    data = hurricane_mv,
    family = quasipoisson()
  )
})

execute_multiverse(M)

parameters(M)
#> $pressure
#> $pressure[[1]]
#> [1] "original"
#> 
#> $pressure[[2]]
#> [1] "updated"
#> 
#> 
#> $damage
#> $damage[[1]]
#> [1] "raw"
#> 
#> $damage[[2]]
#> [1] "logged"

hurricane_res <- pima(
  M,
  tested_coeffs = "MasFem",
  n_flips = n_flips_vignette,
  seed = 1
)

summary(hurricane_res, digits = 3)
#>   model  response coefficient estimate    score       se     z  pcor     p
#> 1  mod1 alldeaths      MasFem    0.145 3106.494 1672.095 1.858 0.195 0.107
#> 2  mod2 alldeaths      MasFem    0.211 4889.632 1918.654 2.548 0.267 0.059
#> 3  mod3 alldeaths      MasFem    0.141 2963.215 1653.939 1.792 0.188 0.142
#> 4  mod4 alldeaths      MasFem    0.204 4518.155 1850.998 2.441 0.256 0.079
#>   p.adj pressure damage       family link
#> 1 0.200 original    raw quasipoisson  log
#> 2 0.069 original logged quasipoisson  log
#> 3 0.200  updated    raw quasipoisson  log
#> 4 0.081  updated logged quasipoisson  log
```

The branches declared in
[`multiverse::branch()`](https://mucollective.github.io/multiverse/reference/branch.html)
are stored as metadata and can be used for grouping and specification
curves.

``` r

plot(
  hurricane_res,
  by = c("pressure", "damage"),
  xvar = "estimate",
  p.transf = "-log10",
  p.adjusted = FALSE
)
```

![](pima-workflows_files/figure-html/multiverse-package-plots-1.png)

``` r


spec_curve(
  hurricane_res,
  focal = "MasFem",
  p.adjusted = FALSE,
  tbr = c(0.60, 0.40),
  colors = c("grey55", "#009E73"),
  title = "Hurricane multiverse from the multiverse package",
  top.theme = spec_top_theme,
  bottom.theme = spec_bottom_theme,
  redundant = FALSE
)
```

![](pima-workflows_files/figure-html/multiverse-package-plots-2.png)

## Adjusting and combining tests

[`pima()`](https://livioivil.github.io/pima/reference/pima.md) applies
multiplicity adjustment during the main call when `method = "maxT"` or
`method = "minP"`. You can also compute raw p-values first with
`method = "none"` and then call `p.adjust()` later.

The next example uses a factor predictor, because it also lets us show
[`combine_contrasts()`](https://livioivil.github.io/flipscores/reference/combine_tests.html).

``` r

set.seed(123)
n <- 40
factor_data <- data.frame(
  x = rnorm(n),
  z1 = rnorm(n),
  z2 = rnorm(n),
  group = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
factor_data$y <- 0.5 * factor_data$x +
  ifelse(factor_data$group == "C", 1.2, 0) +
  factor_data$z1 +
  rnorm(n)

factor_mods <- list(
  linear = glm(y ~ x + group + z1 + z2, data = factor_data),
  poly_z1 = glm(y ~ x + group + poly(z1, 2) + z2, data = factor_data),
  compact = glm(y ~ x + group + z1, data = factor_data)
)

raw_factor_res <- pima(
  factor_mods,
  tested_coeffs = c("x", "groupB", "groupC"),
  n_flips = n_flips_vignette,
  method = "none",
  seed = 1
)

head(summary(raw_factor_res, digits = 3))
#>     model response coefficient estimate  score    se      z   pcor     p
#> 1 compact        y           x    0.409 12.810 7.039  1.820  0.303 0.072
#> 2 compact        y      groupB   -0.676 -4.563 3.207 -1.423 -0.237 0.171
#> 3 compact        y      groupC    0.916  6.017 3.232  1.862  0.310 0.102
#> 4  linear        y           x    0.410 12.812 7.138  1.795  0.303 0.080
#> 5  linear        y      groupB   -0.680 -4.454 3.202 -1.391 -0.235 0.171
#> 6  linear        y      groupC    0.916  6.014 3.278  1.835  0.310 0.106
#>     family     link
#> 1 gaussian identity
#> 2 gaussian identity
#> 3 gaussian identity
#> 4 gaussian identity
#> 5 gaussian identity
#> 6 gaussian identity
```

Because `raw_factor_res` was computed with `method = "none"`, it
contains raw p-values only. The re-exported `p.adjust()` function adds
adjusted p-values from the same joint resampling distribution.

``` r

maxT_factor_res <- p.adjust(
  raw_factor_res,
  method = "maxT"
)

minP_factor_res <- p.adjust(
  raw_factor_res,
  method = "minP"
)

head(summary(maxT_factor_res, digits = 3))
#>     model response coefficient estimate  score    se      z   pcor     p
#> 1 compact        y           x    0.409 12.810 7.039  1.820  0.303 0.072
#> 2 compact        y      groupB   -0.676 -4.563 3.207 -1.423 -0.237 0.171
#> 3 compact        y      groupC    0.916  6.017 3.232  1.862  0.310 0.102
#> 4  linear        y           x    0.410 12.812 7.138  1.795  0.303 0.080
#> 5  linear        y      groupB   -0.680 -4.454 3.202 -1.391 -0.235 0.171
#> 6  linear        y      groupC    0.916  6.014 3.278  1.835  0.310 0.106
#>     family     link p.adj
#> 1 gaussian identity   0.3
#> 2 gaussian identity   0.4
#> 3 gaussian identity   0.3
#> 4 gaussian identity   0.4
#> 5 gaussian identity   0.4
#> 6 gaussian identity   0.4
head(summary(minP_factor_res, digits = 3))
#>     model response coefficient estimate  score    se      z   pcor     p
#> 1 compact        y           x    0.409 12.810 7.039  1.820  0.303 0.072
#> 2 compact        y      groupB   -0.676 -4.563 3.207 -1.423 -0.237 0.171
#> 3 compact        y      groupC    0.916  6.017 3.232  1.862  0.310 0.102
#> 4  linear        y           x    0.410 12.812 7.138  1.795  0.303 0.080
#> 5  linear        y      groupB   -0.680 -4.454 3.202 -1.391 -0.235 0.171
#> 6  linear        y      groupC    0.916  6.014 3.278  1.835  0.310 0.106
#>     family     link p.adj
#> 1 gaussian identity   0.4
#> 2 gaussian identity   0.5
#> 3 gaussian identity   0.4
#> 4 gaussian identity   0.5
#> 5 gaussian identity   0.5
#> 6 gaussian identity   0.5
```

[`combine_tests()`](https://livioivil.github.io/flipscores/reference/combine_tests.html)
answers global questions by combining test statistics. With no `by`
argument, it asks whether at least one tested effect is present anywhere
in the multiverse. With `by = "model"` or `by = "coefficient"`, it asks
the same question within each model or coefficient.

``` r

summary(combine_tests(maxT_factor_res))
#>     model coefficient stat ntests     S     p
#> 1 overall        many maxT      9 1.862 0.278

summary(combine_tests(
  maxT_factor_res,
  by = "model"
))
#>     model coefficient stat ntests     S     p
#> 1 compact        many maxT      3 1.835 0.221
#> 2  linear        many maxT      3 1.460 0.355
#> 3 poly_z1        many maxT      3 1.862 0.210

summary(combine_tests(
  maxT_factor_res,
  by = "coefficient"
))
#>    model coefficient stat ntests     S     p
#> 1      x           x maxT      3 1.820 0.108
#> 2 groupB      groupB maxT      3 1.460 0.209
#> 3 groupC      groupC maxT      3 1.862 0.128
```

You can also define custom groups with `by_list`. The list elements can
contain test positions, or column names from `Tspace`, when you want
groups that do not correspond to a single metadata column.

``` r

custom_groups <- list(
  focal_x = which(maxT_factor_res$summary_table$coefficient == "x"),
  factor_group = which(maxT_factor_res$summary_table$coefficient != "x")
)

summary(combine_tests(
  maxT_factor_res,
  by_list = custom_groups
))
#>          model coefficient stat ntests     S     p
#> 1      focal_x           x maxT      3 1.820 0.108
#> 2 factor_group        many maxT      6 1.862 0.199
```

[`combine_contrasts()`](https://livioivil.github.io/flipscores/reference/combine_tests.html)
combines the contrasts of factor variables into one global test for the
factor. In this example `groupB` and `groupC` become a single `group`
test within each model.

``` r

factor_global_res <- combine_contrasts(maxT_factor_res)

summary(factor_global_res)
#>       model coefficient        stat ntests       S     p
#> 1 compact.1           x Mahalanobis      1 0.05596 0.080
#> 2 compact.2       group Mahalanobis      2 8.00911 0.010
#> 3  linear.1           x Mahalanobis      1 0.03816 0.244
#> 4  linear.2       group Mahalanobis      2 6.31829 0.035
#> 5 poly_z1.1           x Mahalanobis      1 0.05690 0.072
#> 6 poly_z1.2       group Mahalanobis      2 8.27876 0.008

summary(combine_tests(
  factor_global_res,
  by = "coefficient"
))
#>   model coefficient stat ntests      S     p
#> 1     x           x maxT      3 0.0569 0.109
#> 2 group       group maxT      3 8.2788 0.014
```

## Variation of estimates

`voe()` summarizes the variation of estimates and p-values across the
multiverse. `voe_variance()` decomposes variation in a selected
estimate-like column over the specification decisions recorded in the
`pima` object.

``` r

voe(
  multi_res,
  xvar = "pcor",
  p.adjusted = FALSE
)
#> $x
#>         1%        50%        99% 
#> 0.04878427 0.16499093 0.27012554 
#> 
#> $p
#>        1%       50%       99% 
#> 0.5509425 1.7695511 3.0000000 
#> 
#> $xn
#> [1] "pcor"
#> 
#> $x_voe
#> [1] 0.2213413
#> 
#> $p_voe
#> [1] 2.449058
#> 
#> $janus
#> [1] FALSE
#> 
#> $null
#> [1] 0
#> 
#> $type
#> [1] "additive"
#> 
#> $p.transf
#> [1] "-log10"
#> 
#> $p.adjusted
#> [1] FALSE
#> 
#> $alpha
#> [1] 0.05

voe_variance(
  multi_res,
  estimate = "pcor"
)$dec
#>      term          r2u        pr2u
#> 1 glucose  0.011229936  0.16340211
#> 2     bmi -0.003059219 -0.04451342
#> 3     age  0.068689167  0.99946736
```

## A tree over specification decisions

[`pima_tree()`](https://livioivil.github.io/pima/reference/pima_tree.md)
fits an `rpart` tree using the scenario metadata as predictors. With
`method = "class"`, the response is whether a p-value is below `alpha`.
With `method = "anova"`, the response is the p-value itself.

``` r

pima_tree(
  multi_res,
  p.values = "raw",
  method = "anova"
)
#> 
#> Regression tree:
#> rpart::rpart(formula = p ~ ., data = comb_wide, method = method, 
#>     control = control)
#> 
#> Variables actually used in tree construction:
#> [1] age     bmi     glucose link    subset 
#> 
#> Root node error: 0.28346/108 = 0.0026246
#> 
#> n= 108 
#> 
#>         CP nsplit rel error  xerror    xstd
#> 1 0.278603      0  1.000000 1.02424 0.36090
#> 2 0.108938      1  0.721397 0.98940 0.35914
#> 3 0.027338      5  0.285646 0.90327 0.32905
#> 4 0.025501      8  0.203631 0.91440 0.32914
#> 5 0.022360     12  0.101625 0.92036 0.32906
#> 6 0.017599     13  0.079265 0.92723 0.32916
#> 7 0.010000     14  0.061666 1.00022 0.36997
```

![](pima-workflows_files/figure-html/pima-tree-1.png)

## Practical checklist

For a typical workflow:

1.  Define the universe of reasonable analytical choices.
2.  Fit it manually, with
    [`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md),
    or with `multiverse`.
3.  Call [`pima()`](https://livioivil.github.io/pima/reference/pima.md)
    with the focal coefficients and a real number of flips.
4.  Inspect [`summary()`](https://rdrr.io/r/base/summary.html),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
    [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md),
    and, when useful, `voe()` or
    [`pima_tree()`](https://livioivil.github.io/pima/reference/pima_tree.md).
5.  Report the specification decisions and whether the conclusions
    depend on those decisions.
