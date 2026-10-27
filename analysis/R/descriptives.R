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



## Answer scale (figure for the introduction)
library(patchwork)

# icons of the answer options, as shown in the survey
policy_icons <- c("verbod", "onderzoek", "behandeling", "recept", "speciaalzaak",
                  "winst", "overal_te_koop", "reclame", "vrije_verkoop")

# grouped regimes (regime_labels from load_and_clean.R) and the answer options they span
regimes <- tibble(regime = regime_labels, xmin = c(1, 2, 5, 7), xmax = c(1, 4, 6, 9))

# bracket over each regime group, used in both figures
regime_brackets <- function(y, tick = 0.03) {
  list(geom_segment(data = regimes, aes(x = xmin - 0.4, xend = xmax + 0.4, y = y, yend = y),
                    inherit.aes = FALSE, colour = "grey40", linewidth = 0.3),
       geom_segment(data = pivot_longer(regimes, c(xmin, xmax), values_to = "x") |>
                      mutate(x = x + if_else(name == "xmin", -0.4, 0.4)),
                    aes(x = x, xend = x, y = y, yend = y - tick),
                    inherit.aes = FALSE, colour = "grey40", linewidth = 0.3))
}

key <- tibble(x = 1:9, policy = factor(policy_labels, levels = policy_labels),
              label = str_wrap(policy_labels, 14))

p_scale <- ggplot(key, aes(x)) +
  regime_brackets(y = 2.55, tick = 0.12) +
  geom_text(data = regimes, aes(x = (xmin + xmax) / 2, y = 2.65, label = regime),
            inherit.aes = FALSE, vjust = 0, size = 2.8, fontface = "bold", colour = "grey25") +
  geom_tile(aes(y = 1, fill = policy), width = 0.35, height = 0.35) +
  geom_text(aes(y = 1, label = x), size = 2.8, colour = "white", fontface = "bold") +
  geom_text(aes(y = 0.65, label = label), size = 2.6, vjust = 1, lineheight = 0.9) +
  scale_fill_viridis_d(option = "viridis", end = 0.9, guide = "none") +
  scale_x_continuous(limits = c(0.5, 9.5), expand = expansion(0)) +
  scale_y_continuous(limits = c(-0.6, 3), expand = expansion(0)) +
  theme_void()

for (i in seq_along(policy_icons)) {
  img <- png::readPNG(here::here("analysis", "icons", paste0(policy_icons[i], ".png")))
  p_scale <- p_scale + annotation_custom(grid::rasterGrob(img, interpolate = TRUE),
                                         xmin = i - 0.45, xmax = i + 0.45, ymin = 1.3, ymax = 2.4)
}

ggsave(here::here("figures", "fig_policy_scale.pdf"), p_scale,
       width = 6.5, height = 1.9, device = cairo_pdf)


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

# weighted share per grouped regime
regime_shares <- names(drug_labels) |>
  map(\(d) {
    m <- survey::svymean(as.formula(paste0("~regime_", d)), des, na.rm = TRUE)
    tibble(drug = drug_labels[[d]], regime = regime_labels, p = coef(m))
  }) |>
  list_rbind() |>
  left_join(regimes, by = "regime")

# panels ordered by the share choosing a ban
drug_order <- shares |> filter(x == 1) |> arrange(p) |> pull(drug)
shares <- shares |> mutate(drug = factor(drug, levels = drug_order))
regime_shares <- regime_shares |> mutate(drug = factor(drug, levels = drug_order))
stopifnot(all(abs(summarise(regime_shares, s = sum(p), .by = drug)$s - 1) < 1e-6))

p_all <- ggplot(shares, aes(x, p)) +
  geom_col(aes(fill = policy), width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.25) +
  regime_brackets(y = 0.72, tick = 0.02) +
  geom_text(data = regime_shares, aes(x = (xmin + xmax) / 2, y = 0.735,
                                      label = scales::percent(p, 1)),
            vjust = 0, size = 2.3, colour = "grey25") +
  facet_wrap(~drug, nrow = 2, labeller = as_labeller(panel_titles)) +
  scale_fill_viridis_d(option = "viridis", end = 0.9, drop = FALSE, guide = "none") +
  scale_x_continuous(breaks = 1:9, expand = expansion(add = 0.3)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.8),
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
  "Percentages above the brackets give the weighted share per grouped regime: ban (1), ",
  "medical/research (2--4), specialist shop (5--6) and general market (7--9). ",
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
