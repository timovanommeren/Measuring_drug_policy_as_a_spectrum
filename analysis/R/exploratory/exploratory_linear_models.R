
source(here::here("analysis", "R", "00_load_and_clean.R"))

out_dir <- here::here("analysis", "output", "linear_models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)



df$vote_recall_rec <- relevel(factor(df$vote_recall_rec), ref = "GroenLinks-PvdA")
fit2 <- lm(beleid_voorkeur_tabak_en_vapes ~ vote_recall_rec + beliefs_addiction_5, data = df, weights = w)

df$gevoe

summary(fit2)


