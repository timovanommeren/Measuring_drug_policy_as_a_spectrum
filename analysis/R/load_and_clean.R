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

# vote recall in 3 groups. PROVISIONAL: to be replaced by each party's position on the
# Kieskompas TK2025 left-right axis (split into equal thirds); parties without a
# position (Anders, Blanco, Niet-stemmers) are NA
party_lr <- tribble(
  ~party,             ~vote_3,
  "GroenLinks-PvdA",  "left",
  "SP",               "left",
  "PvdD",             "left",
  "DENK",             "left",
  "Volt",             "left",
  "D66",              "middle",
  "CDA",              "middle",
  "CU",               "middle",
  "50PLUS",           "middle",
  "BBB",              "middle",
  "VVD",              "right",
  "PVV",              "right",
  "JA21",             "right",
  "FvD",              "right",
  "SGP",              "right"
)

df <- df |>
  mutate(vote_3 = factor(party_lr$vote_3[match(as.character(vote_recall_rec), party_lr$party)],
                         levels = c("left", "middle", "right")))

des <- survey::svydesign(ids = ~1, weights = ~w, data = df)




