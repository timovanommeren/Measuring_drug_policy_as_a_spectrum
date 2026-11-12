library(tidyverse)
library(lmerTest)   # lme4 with p values for the fixed effects (Satterthwaite)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Within- and between-party relation of the two party-map scores (figure 4a): legal-drug score (y: alcohol,
# tobacco) on illicit-drug score (x), with x split into the party mean (between) and the voter's distance from it
# (within), so the two slopes are estimated separately (Bell & Jones 2015, "within-between" model). Random intercept
# per party; a random within-slope adds nothing (its variance is ~0) and cannot be estimated with six parties.
# Two models: all 14 parties on the map, and the six largest (GL-PvdA, D66, VVD, PVV, CDA, JA21).
# Weights: the survey weights scaled to mean 1, as lme4's precision weights (an approximation; lme4 has no
# sampling weights). The contextual model (x and the party mean together) tests between minus within.
# Output: printed results and analysis/output/party_within_between.csv

largest <- c("GroenLinks-PvdA", "D66", "VVD", "PVV", "CDA", "JA21")

d <- resp |>
  mutate(x_between = weighted.mean(x, w), x_within = x - x_between, .by = party) |>
  mutate(w1 = w / mean(w))

samples <- list(`all 14 parties` = d, `6 largest parties` = filter(d, party %in% largest))

results <- imap_dfr(samples, \(data, name) {
  data <- mutate(data, w1 = w / mean(w))
  m <- lmer(y ~ x_within + x_between + (1 | party), data = data, weights = w1, REML = FALSE)
  ctx <- lmer(y ~ x + x_between + (1 | party), data = data, weights = w1, REML = FALSE)
  cat("\n====", name, "====\n"); print(summary(m))
  cf <- rbind(coef(summary(m))[c("x_within", "x_between"), ], coef(summary(ctx))["x_between", , drop = FALSE])
  tibble(model = name, parties = n_distinct(data$party), n = nrow(data), term = c("within", "between", "between - within"),
         estimate = cf[, "Estimate"], se = cf[, "Std. Error"], df = cf[, "df"], p = cf[, "Pr(>|t|)"])
})

cat("\nslopes of the legal-drug score on the illicit-drug score (scale points per scale point):\n")
print(mutate(results, across(c(estimate, se, df), \(v) round(v, 3)), p = signif(p, 2)), n = Inf, width = Inf)
write_csv(results, here::here("analysis", "output", "party_within_between.csv"))
