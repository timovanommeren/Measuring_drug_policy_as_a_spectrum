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

des <- survey::svydesign(ids = ~1, weights = ~w, data = df)




