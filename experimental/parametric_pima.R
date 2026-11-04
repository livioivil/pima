library(multcomp)

multi <- create_multi(
  Sepal.Width ~ Sepal.Length + Petal.Width,
  data = iris,
  focal = "Sepal.Length",
  nfuns = c("log", "sqrt", "exp"),
  fit.specs = list(lm = stats::glm)
)

glht(do.call(mmm, multi$models), linfct = mlf("Sepal.Length = 0")) |> 
  summary()

r <- pima(multi, tested_coeffs = "Sepal.Length")
summary(r)

mods <- multi$models
names(mods)

par_pima <- function(mods, linfct){
  if(!inherits(mods, "pima.multi")){
    stop("Method not implemented for this class!")
  }
  mmm_mods <- do.call(mmm, mods$models)
  glht_mods <- glht(do.call(mmm, mods$models), linfct = linfct)
  summary(glht_mods)
}

par_pima(multi)
