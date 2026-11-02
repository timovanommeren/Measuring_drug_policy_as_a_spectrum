library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))

# Is the number of modes of a party's policy distribution related to the party?
# Unit: one party x drug distribution, rated by hand (modality_rater.R); 8 drugs per party.
# Multinomial model modality ~ party; predicted counts (out of 8 drugs) with delta-method SEs.

out_dir <- here::here("analysis", "output", "raw_distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ratings <- read_csv(here::here("analysis", "ratings", "modality_ratings.csv"), show_col_types = FALSE) |>
  mutate(modality = factor(modality, levels = c("unimodal", "bimodal", "trimodal")))

# party bloc from df (left/right as in 00_load_and_clean.R); other parties and non-voters as "other"
blocs <- df |>
  filter(!is.na(vote_lr)) |>
  distinct(party = as.character(vote_recall_rec), bloc = as.character(vote_lr))

ratings <- ratings |>
  left_join(blocs, by = "party") |>
  mutate(bloc = factor(replace_na(bloc, "other"), levels = c("left", "right", "other")))
ratings <- ratings |>
  mutate(party = factor(party, levels = distinct(ratings, party, bloc) |> arrange(bloc, party) |> pull(party)))

stopifnot(nrow(ratings) == n_distinct(ratings$party, ratings$drug), all(count(ratings, party)$n == 8))

fit <- nnet::multinom(modality ~ party, data = ratings, trace = FALSE)
fit_null <- nnet::multinom(modality ~ 1, data = ratings, trace = FALSE)

# predicted probabilities per party (delta-method SEs), times 8 drugs = expected count
n_drugs <- 8
pred <- marginaleffects::predictions(fit, newdata = data.frame(party = levels(ratings$party)), type = "probs") |>
  as_tibble() |>
  transmute(party = factor(party, levels = levels(ratings$party)),
            modality = factor(group, levels = levels(ratings$modality)),
            count = n_drugs * estimate, se = n_drugs * std.error) |>
  left_join(distinct(ratings, party, bloc), by = "party")

observed <- count(ratings, party, modality, .drop = FALSE)
stopifnot(all(abs(left_join(pred, observed, by = c("party", "modality")) |> with(count - n)) < 0.01))

# Main significance tests: Fisher's exact test (small counts), and for one factor given the other a
# stratified permutation test in the same spirit: labels permuted within strata, G statistic of the
# two-way table, so the margins of the other factor stay fixed. LR tests of the models are secondary.
g_stat <- function(x, y) {
  t <- table(x, y)
  e <- outer(rowSums(t), colSums(t)) / sum(t)
  2 * sum(ifelse(t > 0, t * log(t / e), 0))
}
stratified_test <- function(x, y, strata, B = 1e5) {
  obs <- g_stat(x, y)
  null <- replicate(B, g_stat(unsplit(lapply(split(x, strata), sample), strata), y))
  (1 + sum(null >= obs - 1e-9)) / (B + 1)
}

set.seed(2026)
fisher <- fisher.test(table(ratings$party, ratings$modality), simulate.p.value = TRUE, B = 1e5)
p_party_given_drug <- stratified_test(ratings$party, ratings$modality, ratings$drug)
lr <- anova(fit_null, fit)
print(fisher)
print(lr)
cat("party given drug, stratified permutation p =", p_party_given_drug, "\n")

write_csv(pred, file.path(out_dir, "modality_by_party.csv"))

p <- ggplot(pred, aes(modality, count, fill = modality)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = pmax(count - 1.96 * se, 0), ymax = pmin(count + 1.96 * se, n_drugs)),
                width = 0.2, linewidth = 0.3) +
  facet_wrap(~party, nrow = 3) +
  scale_fill_manual(values = c(unimodal = "#56B4E9", bimodal = "#E69F00", trimodal = "#CC79A7"), guide = "none") +
  scale_y_continuous(breaks = seq(0, n_drugs, 2), limits = c(0, n_drugs)) +
  labs(title = "Number of modes in each party's policy distributions",
       subtitle = sprintf(paste0("Count of the 8 drugs per modality; multinomial model modality ~ party, error bars ",
                                 "±1.96 delta-method SE (zero counts have SE 0).\nLeft-bloc parties first, then ",
                                 "right, then other. Party vs modality: Fisher's exact test p = %.3f; ",
                                 "given drug (permutation within drug): p = %.3f."),
                          fisher$p.value, p_party_given_drug),
       x = NULL, y = "Number of drugs") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "modality_by_party.pdf"), p, width = 11, height = 7.5, device = cairo_pdf)


## Type of drug: modality ~ drug, and party controlled for drug

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")
ratings <- ratings |> mutate(drug = factor(drug, levels = names(drug_labels)))
n_parties <- n_distinct(ratings$party)

fit_drug <- nnet::multinom(modality ~ drug, data = ratings, trace = FALSE)
fit_both <- nnet::multinom(modality ~ party + drug, data = ratings, trace = FALSE, maxit = 500)
lr_drug <- anova(fit_null, fit_drug)
lr_party_given_drug <- anova(fit_drug, fit_both)
lr_drug_given_party <- anova(fit, fit_both)
set.seed(2026)
fisher_drug <- fisher.test(table(ratings$drug, ratings$modality), simulate.p.value = TRUE, B = 1e5)
p_drug_given_party <- stratified_test(ratings$drug, ratings$modality, ratings$party)
print(fisher_drug)
cat("drug given party, stratified permutation p =", p_drug_given_party, "\n")
print(lr_drug)
print(lr_party_given_drug)
print(lr_drug_given_party)

# predicted counts per drug (out of the party groups), delta-method SEs
pred_drug <- marginaleffects::predictions(fit_drug, newdata = data.frame(drug = levels(ratings$drug)),
                                          type = "probs") |>
  as_tibble() |>
  transmute(drug = factor(drug, levels = names(drug_labels)),
            modality = factor(group, levels = levels(ratings$modality)),
            count = n_parties * estimate, se = n_parties * std.error)

observed_drug <- count(ratings, drug, modality, .drop = FALSE)
stopifnot(all(abs(left_join(pred_drug, observed_drug, by = c("drug", "modality")) |> with(count - n)) < 0.01))

write_csv(pred_drug, file.path(out_dir, "modality_by_drug.csv"))

p_drug <- ggplot(pred_drug, aes(modality, count, fill = modality)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = pmax(count - 1.96 * se, 0), ymax = pmin(count + 1.96 * se, n_parties)),
                width = 0.2, linewidth = 0.3) +
  facet_wrap(~drug, nrow = 2, labeller = labeller(drug = drug_labels)) +
  scale_fill_manual(values = c(unimodal = "#56B4E9", bimodal = "#E69F00", trimodal = "#CC79A7"), guide = "none") +
  scale_y_continuous(breaks = seq(0, n_parties, 2), limits = c(0, n_parties)) +
  labs(title = "Number of modes in each drug's policy distributions across parties",
       subtitle = sprintf(paste0("Count of the %d party groups per modality; multinomial model modality ~ drug, ",
                                 "error bars ±1.96 delta-method SE (zero counts have SE 0).\n",
                                 "Drug vs modality: Fisher's exact test p = %.1g; given party (permutation within ",
                                 "party): p = %.1g."),
                          n_parties, fisher_drug$p.value, p_drug_given_party),
       x = NULL, y = "Number of party groups") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "modality_by_drug.pdf"), p_drug, width = 11, height = 6, device = cairo_pdf)
