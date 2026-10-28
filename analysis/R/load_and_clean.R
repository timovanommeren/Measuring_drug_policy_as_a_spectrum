library(tidyverse)

# demographic variables used for weighting
vars <- c("sex_rec", "edu_3_rec", "age_4_rec", "vote_recall_rec", "geo_prov_rec")

#load data
df <- haven::read_sav(here::here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.sav")) |>
  filter(!is.na(weight_trimmed_0990)) |>
  mutate(across(all_of(vars), haven::as_factor),
         w = weight_trimmed_0990)

# 99 = "geen mening"
df <- df |>
  mutate(across(starts_with("beleid_voorkeur_"), \(x) replace(x, x == 99, NA)))

# grouped regimes: ban (1), medical/research (2-4), specialist shop (5-6), general market (7-9)
regime_labels <- c("ban", "medical/research", "specialist shop", "general market")

df <- df |>
  mutate(across(starts_with("beleid_voorkeur_"),
                \(x) factor(cut(as.numeric(x), c(0, 1, 4, 6, 9), labels = FALSE),
                            levels = 1:4, labels = regime_labels),
                .names = "{str_replace(.col, 'beleid_voorkeur_', 'regime_')}"))

# vote recall in 2 groups, left and right; Anders, Blanco and Niet-stemmers are NA
party_lr <- tribble(
  ~party,             ~vote_lr,
  "GroenLinks-PvdA",  "left",
  "SP",               "left",
  "PvdD",             "left",
  "Volt",             "left",
  "D66",              "left",
  "DENK",             "left",
  "CU",               "left",
  "PVV",              "right",
  "FvD",              "right",
  "JA21",             "right",
  "BBB",              "right",
  "VVD",              "right",
  "CDA",              "right",
  "50PLUS",           "right",
  "SGP",              "right"
)

df <- df |>
  mutate(vote_lr = factor(party_lr$vote_lr[match(as.character(vote_recall_rec), party_lr$party)],
                          levels = c("left", "right")))

des <- survey::svydesign(ids = ~1, weights = ~w, data = df)




