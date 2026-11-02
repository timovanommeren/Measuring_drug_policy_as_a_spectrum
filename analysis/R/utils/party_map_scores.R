# respondent scores for the party maps (figure 4a): per respondent the mean answer (1 = ban ... 9 = free
# sale) over the drugs answered, x = illicit drugs, y = alcohol and tobacco; per party the weighted mean
# (party_means) and the sample means (ref) that split the map into quadrants.
# Used by 03_fig_party_map.R and the scripts in exploratory/modality/; needs 00_load_and_clean.R (df)
# and the tidyverse.

drug_names <- c(alcohol = "alcohol", tabak_en_vapes = "tobacco and vapes", cannabis = "cannabis",
                psychedelica = "psychedelics", mdma = "MDMA", cocaine = "cocaine", speed = "speed",
                heroine = "heroin")
x_drugs <- c("cannabis", "psychedelica", "mdma", "speed", "cocaine", "heroine")
y_drugs <- c("alcohol", "tabak_en_vapes")

# parties only: non-voters, blank, "Anders" and DENK (n = 1) left out
excluded <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen", "DENK", "Anders")

answers <- df |>
  transmute(w = as.numeric(w), vote = as.character(vote_recall_rec),
            across(all_of(paste0("beleid_voorkeur_", names(drug_names))), as.numeric)) |>
  rename_with(\(x) str_remove(x, "^beleid_voorkeur_"))

block_mean <- function(d, drugs) {
  s <- rowMeans(as.matrix(d[drugs]), na.rm = TRUE)
  if_else(is.nan(s), NA_real_, s)
}

# all respondents with both scores, and the map parties only
scores <- answers |>
  mutate(x = block_mean(answers, x_drugs), y = block_mean(answers, y_drugs)) |>
  filter(!is.na(x), !is.na(y))

resp <- scores |>
  filter(!is.na(vote), !vote %in% excluded) |>
  rename(party = vote)

ref <- c(x = weighted.mean(resp$x, resp$w), y = weighted.mean(resp$y, resp$w))
party_means <- summarise(resp, x = weighted.mean(x, w), y = weighted.mean(y, w), n = n(), .by = party)

# weighted histogram of a score over the 9 answer options: a score counts for the nearest whole number;
# a score exactly halfway (e.g. 6.5, the mean of 6 and 7) counts half for each neighbour
nine_bins <- function(s, w) {
  edge <- abs(s - floor(s) - 0.5) < 1e-9
  bin <- c(round(s[!edge]), floor(s[edge]), floor(s[edge]) + 1)
  wt <- c(w[!edge], w[edge] / 2, w[edge] / 2)
  as.numeric(tapply(wt, factor(bin, levels = 1:9), sum, default = 0))
}
