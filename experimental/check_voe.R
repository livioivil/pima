# =============================================================================
# Vibration of Effects con pima::create_multi()
# Replica dell'esempio di chiragjp/voe: cadmio sierico e mortalità (NHANES 99-04)
# Patel, Burford & Ioannidis (2015), J Clin Epidemiol 68(9):1046-1058
# =============================================================================

devtools::load_all()
library(survival)

# Dati: data/nhanes9904_VoE.Rdata dal repo chiragjp/voe
load("nhanes9904_VoE.Rdata")

dat <- mainTab[, c("MORTSTAT", "WTMEC4YR", "PERMTH_EXM", "SES_LEVEL",
                   "RIDAGEYR", "male", "area", "LBXBCD",
                   "current_past_smoking", "any_cad", "any_ht",
                   "any_diabetes", "education", "RIDRETH1")]
dat <- dat[complete.cases(dat), ]

# create_multi() accetta solo predittori "nudi" nella formula:
# trasformazioni e factor() si calcolano prima, nel data frame.
dat$cadmium     <- as.numeric(scale(log(dat$LBXBCD)))  # = scale(I(log(LBXBCD)))
dat$smoking     <- factor(dat$current_past_smoking)
dat$ses         <- factor(dat$SES_LEVEL)
dat$education_f <- factor(dat$education)
dat$race        <- factor(dat$RIDRETH1)

adjustors <- c("smoking", "ses", "any_cad", "any_ht",
               "any_diabetes", "education_f", "race")

# Indici VoE come in voe::summary_vibration():
#   RHR = HR_99 / HR_01 = exp(q99 - q01) delle stime in scala log
#   RP  = -log10(p_01) + log10(p_99)
voe_index <- function(b, p, probs = c(.01, .5, .99)) {
  hr <- exp(quantile(b, probs))
  pq <- quantile(p, probs)
  c(HR_01 = hr[[1]], HR_50 = hr[[2]], HR_99 = hr[[3]],
    p_01  = pq[[1]], p_50  = pq[[2]], p_99  = pq[[3]],
    RHR   = hr[[3]] / hr[[1]],
    RP    = -log10(pq[[1]]) + log10(pq[[3]]),
    janus = sign(log(hr[[1]])) != sign(log(hr[[3]])))
}

n_adj <- function(scenarios) {
  rowSums(scenarios[paste0("spec_", adjustors)] != "excluded")
}

# -----------------------------------------------------------------------------
# B) Stessa vibration passata a pima() (flipscores richiede GLM)
# -----------------------------------------------------------------------------
# Cox -> Poisson con offset log(tempo) = modello esponenziale a rischi
# proporzionali: il coefficiente di cadmium resta un log-HR.
# Il cluster(area) di voe diventa id = area nel sign-flip.
#
# NB: pesi e offset vanno passati come VETTORI, non come nomi di colonna
# (es. weights = quote(w)). flipscores rifitta internamente un modello nullo
# su un data frame ricostruito che non contiene più quelle colonne.
w     <- dat$WTMEC4YR / mean(dat$WTMEC4YR)
log_t <- log(dat$PERMTH_EXM + 0.5)  # 3 soggetti hanno follow-up = 0

multi_glm <- create_multi(
  MORTSTAT ~ cadmium + RIDAGEYR + male +
    smoking + ses + any_cad + any_ht + any_diabetes + education_f + race,
  data  = dat,
  focal = c("cadmium", "RIDAGEYR", "male"),
  fit.specs = list(
    exp_ph = create_fit(stats::glm,
                        family  = stats::poisson(),
                        offset  = log_t,
                        weights = w)
  )
)

res <- pima(multi_glm, tested_coeffs = "cadmium", n_flips = 1000,
            method = "maxT", id = dat$area, seed = 1)

st   <- merge(res$summary_table,
              data.frame(model = multi_glm$scenarios$model,
                         k = n_adj(multi_glm$scenarios)),
              by = "model")
k1   <- st$k > 0

rbind(
  pima_p_raw  = voe_index(st$estimate[k1], st$p[k1]),
  pima_p_maxT = voe_index(st$estimate[k1], st$p.adj[k1])
)

# Grafici di pima
plot(res, xvar = "estimate", p.adjusted = FALSE)
spec_curve(res, yvar = "estimate")