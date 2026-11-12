library(tidyverse)
library(lmerTest)   # lme4 with p values for the fixed effects
library(survey)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Within- and between-party relation of the two party-map scores (figure 4a): legal-drug score (y: alcohol,
# tobacco) on illicit-drug score (x), with x split into the party mean (between) and the voter's distance from it
# (within), so the two slopes are estimated separately (Bell & Jones 2015, "within-between" model). Random
# intercept and random within-slope per party. Party voters only, as on the map; 14 parties.
# lme4 has no sampling weights: its weights are precision weights, here the survey weights scaled to mean 1, an
# approximation; the unweighted fit and a design-based regression (svyglm, party as cluster) are the checks.
# Output: printed results and analysis/output/party_within_between.csv

d <- resp |>
  mutate(x_between = weighted.mean(x, w), x_within = x - x_between, .by = party) |>
  mutate(w1 = w / mean(w), n_party = n(), .by = NULL) |>
  mutate(n_party = n(), .by = party)

fit <- \(data, weights = data$w1, slope = TRUE) {
  f <- if (slope) y ~ x_within + x_between + (1 + x_within | party) else y ~ x_within + x_between + (1 | party)
  lmer(f, data = data, weights = weights, REML = FALSE)
}

m_int <- fit(d, slope = FALSE)
m <- fit(d)
print(summary(m))
cat("\nrandom within-slope, likelihood ratio test:\n"); print(anova(m_int, m))

# the contextual effect: with x itself and the party mean in the model, the party-mean coefficient is
# between minus within, so its test asks whether the two slopes differ
m_ctx <- lmer(y ~ x + x_between + (1 + x_within | party), data = d, weights = w1, REML = FALSE)
cat("\ncontextual effect (between - within):\n"); print(coef(summary(m_ctx))["x_between", ])

# sensitivity: unweighted; without the three small parties strict on both (SGP, CU, 50PLUS); parties with 40+ voters
fits <- list(
  weighted = m,
  unweighted = fit(d, weights = NULL),
  `without SGP, CU, 50PLUS` = fit(filter(d, !party %in% c("SGP", "CU", "50PLUS"))),
  `parties with 40+ voters` = fit(filter(d, n_party >= 40))
)
design <- svydesign(ids = ~party, weights = ~w, data = d)
svy <- svyglm(y ~ x_within + x_between, design = design)

slopes <- imap_dfr(fits, \(m, name) {
  cf <- coef(summary(m))
  tibble(model = name, parties = n_distinct(model.frame(m)$party), n = nobs(m),
         term = c("within", "between"), estimate = cf[c("x_within", "x_between"), "Estimate"],
         se = cf[c("x_within", "x_between"), "Std. Error"], p = cf[c("x_within", "x_between"), "Pr(>|t|)"])
}) |>
  bind_rows(tibble(model = "design-based (svyglm, party clusters)", parties = 14, n = nrow(d), term = c("within", "between"),
                   estimate = coef(svy)[c("x_within", "x_between")], se = SE(svy)[c("x_within", "x_between")],
                   p = summary(svy)$coefficients[c("x_within", "x_between"), 4]))
cat("\nslopes of the legal-drug score on the illicit-drug score (scale points per scale point):\n")
print(mutate(slopes, across(c(estimate, se), \(v) round(v, 3)), p = signif(p, 2)), n = Inf, width = Inf)

# each party's own within-slope (fixed + random part)
cat("\nwithin-slope per party:\n")
print(coef(m)$party |> rownames_to_column("party") |> select(party, within_slope = x_within) |>
        left_join(count(d, party), by = "party") |> arrange(within_slope) |> mutate(within_slope = round(within_slope, 2)))

write_csv(slopes, here::here("analysis", "output", "party_within_between.csv"))
