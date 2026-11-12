library(tidyverse)

# demographic variables used for weighting
vars <- c("sex_rec", "edu_3_rec", "age_4_rec", "vote_recall_rec", "geo_prov_rec")

#load data
df <- haven::read_sav(here::here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.sav")) |>
  filter(!is.na(weight_trimmed_0990)) |>
  mutate(across(all_of(vars), haven::as_factor),
         w = weight_trimmed_0990)

# 99 = "geen mening" on every item; on the 0-100 involvement sliders (betrokkenheid_*) 99 is a position
df <- df |>
  mutate(across(c(starts_with("oefenvraag_"), starts_with("beleid_voorkeur_"), starts_with("beliefs_"),
                  starts_with("gedrag_")),
                \(x) replace(x, x == 99, NA)))

# feelings items: gevoelens_1_[topic]_[emotion] -> emo_[topic]_[emotion]
emo_topics <- c("alc", "tob", "soft", "hard", "wod", "legal") # wod = war on drugs, legal = legalisation of drugs
emo_feelings <- c("angry", "worried", "happy", "none")

emo_names <- expand_grid(t = seq_along(emo_topics), f = seq_along(emo_feelings)) |>
  with(set_names(paste0("gevoelens_1_", t, "_", f), paste0("emo_", emo_topics[t], "_", emo_feelings[f])))

df <- df |>
  rename(all_of(emo_names))

# value labels from the agency's codebook: the .sav's own labels of the policy and use items are garbled
# ("Meest strengverbod", "nooitNooit gebruikt"). Kept on the haven_labelled columns, so as_factor() uses them.
codebook <- readxl::read_excel(here::here("analysis", "assets", "NLOZ26-10_Timo_Drugsbeleid_codebook_261014.xlsx"),
                               sheet = "Variable Values", skip = 1, col_names = c("variable", "value", "label")) |>
  fill(variable) |>
  filter(!is.na(value)) |>
  mutate(variable = coalesce(names(emo_names)[match(variable, emo_names)], variable))

for (v in intersect(unique(codebook$variable), names(df))) {
  if (!haven::is.labelled(df[[v]])) next
  cb <- filter(codebook, variable == v)
  attr(df[[v]], "labels") <- set_names(as.numeric(cb$value), cb$label)
}

# reverse-scored copies (<item>_scored) of the belief and behaviour items, so that a high score means more
# liberal on every item: the anti items (agreeing favours prohibition) are reversed, 8 - x
anti_items <- c("beliefs_society_2", "beliefs_society_3", "beliefs_society_4", "beliefs_society_5",
                "beliefs_addiction_1", "beliefs_addiction_3", "beliefs_addiction_4", "beliefs_addiction_5",
                "beliefs_regulate_1", "beliefs_regulate_4", "beliefs_crime_2", "beliefs_crime_4",
                "gedrag_1", "gedrag_2")

df <- df |>
  mutate(across(c(starts_with("beliefs_"), starts_with("gedrag_")),
                \(x) if (cur_column() %in% anti_items) 8 - as.numeric(x) else as.numeric(x),
                .names = "{.col}_scored"))

# mean of the five involvement sliders
df <- df |>
  mutate(betrokkenheid_mean = rowMeans(across(c(betrokkenheid_1, betrokkenheid_1b, betrokkenheid_2,
                                                betrokkenheid_2b, betrokkenheid_3))))

# substance ladder: the heaviest group ever used (gebruik_1-4 = alcohol, tabak, soft-, harddrugs; 1 = never)
df <- df |>
  mutate(substance_ladder = case_when(if_all(num_range("gebruik_", 1:4), is.na) ~ NA,
                                      gebruik_4 >= 2 ~ "harddrugs",
                                      gebruik_3 >= 2 ~ "softdrugs",
                                      gebruik_1 >= 2 | gebruik_2 >= 2 ~ "legaal",
                                      .default = "nooit") |>
           factor(levels = c("nooit", "legaal", "softdrugs", "harddrugs")))

# support for regulating MDMA: rule 5 (specialist shop) or freer
df <- df |>
  mutate(mdma_support = factor(if_else(beleid_voorkeur_mdma >= 5, "voor", "tegen"), levels = c("tegen", "voor")))

# per emotion topic the combination of feelings ticked, e.g. "angry + worried"; NA if the topic was not answered
for (t in emo_topics) {
  ticks <- as.matrix(df[paste0("emo_", t, "_", emo_feelings)])
  df[[paste0("emo_", t)]] <- apply(ticks, 1, \(r) if (all(is.na(r))) NA_character_
                                   else paste(emo_feelings[which(r == 1)], collapse = " + "))
}

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

# vote recall in 3 groups by the party's general left-right position (lrgen, 0 = extreme left ...
# 10 = extreme right) in the Chapel Hill Expert Survey 2024 (Rovny et al. 2025, Electoral Studies 97,
# 102981; chesdata.eu), split into equal thirds of the scale. GroenLinks-PvdA = mean of GroenLinks
# (2.50) and PvdA (3.00); 50PLUS is not in the 2024 wave, so its 2019 score (trend file); JA21 has
# never been rated and is placed between VVD and PVV, as its founders positioned it. Volt (4.55)
# falls in the middle third but is set to left by hand.
# galtan: the cultural position (0 = green/alternative/libertarian ... 10 = traditional/authoritarian/
# nationalist), same wave and rules (GroenLinks-PvdA = mean of GL 0.92 and PvdA 2.58; 50PLUS 2019; JA21
# between VVD and PVV); orders the parties in figure 4b
# Anders, Blanco and Niet-stemmers are NA
party_ches <- tribble(
  ~party,             ~lrgen, ~galtan,
  "SP",               1.17,  4.67,
  "PvdD",             1.50,  2.25,
  "GroenLinks-PvdA",  2.75,  1.75,
  "DENK",             3.89,  6.29,
  "Volt",             4.55,  1.09,
  "CU",               4.58,  6.92,
  "D66",              4.67,  1.33,
  "50PLUS",           5.08,  5.60,
  "CDA",              5.75,  6.50,
  "VVD",              7.67,  5.92,
  "BBB",              7.75,  7.67,
  "SGP",              8.00,  9.42,
  "JA21",             8.50,  7.00,
  "PVV",              9.08,  8.09,
  "FvD",              9.83,  9.83
)

df <- df |>
  mutate(vote_3 = cut(party_ches$lrgen[match(as.character(vote_recall_rec), party_ches$party)],
                      c(0, 10 / 3, 20 / 3, 10), labels = c("left", "middle", "right")),
         vote_3 = replace(vote_3, as.character(vote_recall_rec) == "Volt", "left"))

# the age, education and urbanisation groups of figure 3, also the split buttons of the descriptive report.
# Urban = very high or high address density (urb 1-2), less urban = moderate, low or none
df <- df |>
  mutate(age_group = fct_relabel(age_4_rec, \(x) str_replace(x, "-", "–")),
         edu_group = fct_recode(edu_3_rec, Practical = "Praktisch-opgeleid", Middle = "Middel-opgeleid",
                                Theoretical = "Theoretisch-opgeleid") |> fct_rev(),
         urb_group = factor(if_else(urb <= 2, "Urban", "Less urban"), levels = c("Urban", "Less urban")))

des <- survey::svydesign(ids = ~1, weights = ~w, data = df)
