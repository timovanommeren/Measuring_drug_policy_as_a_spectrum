library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# answer options (1 = strictest, 9 = least strict); 99 "geen mening" is dropped
policy_labels <- c(
  "1" = "ban",
  "2" = "research only",
  "3" = "treatment only",
  "4" = "prescription only",
  "5" = "specialist shop, non-profit",
  "6" = "specialist shop, for-profit",
  "7" = "sold everywhere, no advertising",
  "8" = "sold everywhere, with advertising",
  "9" = "free sale"
)

drug_labels <- c(
  alcohol         = "Alcohol",
  tabak_en_vapes  = "Tobacco and vapes",
  cannabis        = "Cannabis",
  psychedelica    = "Psychedelics",
  mdma            = "MDMA",
  cocaine         = "Cocaine",
  speed           = "Speed",
  heroine         = "Heroin"
)

# long format: one row per respondent x drug
pref <- df |>
  select(weight, all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
  pivot_longer(-weight, names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "policy") |>
  filter(!is.na(policy), policy != 99) |>
  mutate(drug   = factor(drug_labels[drug], levels = drug_labels),
         policy = factor(policy, levels = names(policy_labels), labels = policy_labels))

# counts per answer option, weighted and unweighted
counts <- pref |>
  summarise(Weighted = sum(weight), Unweighted = n(), .by = c(drug, policy)) |>
  pivot_longer(c(Weighted, Unweighted), names_to = "version", values_to = "n") |>
  mutate(version = factor(version, levels = c("Weighted", "Unweighted")))

plot_drug <- function(d) {
  counts |>
    filter(drug == d) |>
    ggplot(aes(policy, n, fill = policy)) +
    geom_col() +
    facet_wrap(~version) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_viridis_d(option = "viridis", end = 0.9, drop = FALSE, guide = "none") +
    labs(title = d, x = "Preferred policy (strict → free)", y = "Number of respondents") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.margin = margin(10, 10, 10, 30))
}

# one page per drug
cairo_pdf(here::here("analysis", "output", "policy_preferences.pdf"), width = 11, height = 7, onefile = TRUE)
walk(levels(counts$drug), \(d) print(plot_drug(d)))
dev.off()
