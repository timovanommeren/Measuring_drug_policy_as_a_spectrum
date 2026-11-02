library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))

# Figure 1 (answer scale, out/fig_policy_scale.pdf) and Figure 2 (preferred policy per substance,
# out/policy_preferences_all_drugs.pdf + out/fig_policy_preferences.tex)

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

## Answer scale (figure for the introduction)
library(patchwork)

# icons of the answer options, as shown in the survey
policy_icons <- c("verbod", "onderzoek", "behandeling", "recept", "speciaalzaak",
                  "winst", "overal_te_koop", "reclame", "vrije_verkoop")

key <- tibble(x = 1:9, policy = factor(policy_labels, levels = policy_labels),
              label = str_wrap(policy_labels, 14))

p_scale <- ggplot(key, aes(x)) +
  geom_tile(aes(y = 1, fill = policy), width = 0.35, height = 0.35) +
  geom_text(aes(y = 1, label = x), size = 2.8, colour = "white", fontface = "bold") +
  geom_text(aes(y = 0.65, label = label), size = 2.6, vjust = 1, lineheight = 0.9) +
  scale_fill_viridis_d(option = "viridis", end = 0.9, guide = "none") +
  scale_x_continuous(limits = c(0.5, 9.5), expand = expansion(0)) +
  scale_y_continuous(limits = c(-0.6, 2.45), expand = expansion(0)) +
  theme_void()

for (i in seq_along(policy_icons)) {
  img <- png::readPNG(here::here("analysis", "icons", paste0(policy_icons[i], ".png")))
  p_scale <- p_scale + annotation_custom(grid::rasterGrob(img, interpolate = TRUE),
                                         xmin = i - 0.45, xmax = i + 0.45, ymin = 1.3, ymax = 2.4)
}

ggsave(here::here("out", "fig_policy_scale.pdf"), p_scale,
       width = 6.5, height = 1.6, device = cairo_pdf)


## All drugs together (figure for the paper)

# panel titles follow the item wording ("Psychedelica (zoals paddo's en LSD)" etc.)
panel_titles <- c(
  "Alcohol"           = "Alcohol\n",
  "Tobacco and vapes" = "Tobacco and vapes\n",
  "Cannabis"          = "Cannabis\n(weed, hash)",
  "Psychedelics"      = "Psychedelics*\n(mushrooms, LSD)",
  "MDMA"              = "MDMA\n(ecstasy)",
  "Cocaine"           = "Cocaine / crack\n",
  "Speed"             = "Speed\n(amphetamine)",
  "Heroin"            = "Heroin\n"
)

# weighted share of each answer option with 95% CI, from the survey design
shares <- names(drug_labels) |>
  map(\(d) {
    f <- as.formula(paste0("~factor(beleid_voorkeur_", d, ", levels = 1:9)"))
    m <- survey::svymean(f, des, na.rm = TRUE)
    tibble(drug = drug_labels[[d]], x = 1:9, p = coef(m), se = survey::SE(m),
           n = sum(!is.na(df[[paste0("beleid_voorkeur_", d)]])))
  }) |>
  list_rbind() |>
  mutate(policy = factor(policy_labels[x], levels = policy_labels),
         lo = pmax(p - 1.96 * se, 0), hi = p + 1.96 * se)

# panels ordered by the share choosing a ban
drug_order <- shares |> filter(x == 1) |> arrange(p) |> pull(drug)
shares <- shares |> mutate(drug = factor(drug, levels = drug_order))

p_all <- ggplot(shares, aes(x, p)) +
  geom_col(aes(fill = policy), width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.25) +
  facet_wrap(~drug, nrow = 2, labeller = as_labeller(panel_titles)) +
  scale_fill_viridis_d(option = "viridis", end = 0.9, drop = FALSE, guide = "none") +
  scale_x_continuous(breaks = 1:9, expand = expansion(add = 0.3)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.7),
                     breaks = seq(0, 0.6, 0.2), expand = expansion(0)) +
  labs(x = "Preferred policy (strict → free)", y = "Respondents (weighted %)") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 8))

ggsave(here::here("out", "policy_preferences_all_drugs.pdf"), p_all,
       width = 6.5, height = 4.2, device = cairo_pdf)

# figure environment with caption for main.tex: \input{out/fig_policy_preferences}
# (raw strings r"(...)" so LaTeX backslashes need no escaping)
caption <- paste0(
  r"(Preferred regulatory model per substance. Question: ``Which policy do you think is best?'' )",
  r"((\emph{Welk beleid vindt u het beste?}), with nine options from ban (1) to free sale (9); )",
  r"(colours and numbers as in Figure~\ref{fig:policy_scale}. )",
  r"(Bars show weighted shares with 95\% confidence intervals; ``no opinion'' is excluded )",
  sprintf("($n = %d$--$%d$ per substance). ", min(shares$n), max(shares$n)),
  "*The item named magic mushrooms and LSD, which are banned in the Netherlands, while psilocybin truffles ",
  "are legally sold in smartshops; respondents may have had different substances in mind. ",
  "Panels are ordered by the share choosing a ban. ",
  sprintf("Data: Kieskompas VIP panel, 13--22 October 2026, $n = %d$, ", nrow(df)),
  "weighted by iterative post-stratification to CBS margins (age, sex, education, migration background, region) ",
  "and the 2025 Tweede Kamer vote; weights trimmed at the 99th percentile."
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \includegraphics[width=\linewidth]{out/policy_preferences_all_drugs.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:policy_preferences})",
  r"(\end{figure})"
), here::here("out", "fig_policy_preferences.tex"))
