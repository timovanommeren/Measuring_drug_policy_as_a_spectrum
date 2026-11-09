library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))

# Appendix: how preferred policy relates to the 18 attitude items (beliefs_*).
# 1. out/tab_attitude_correlations.tex: weighted correlations with the policy for the illicit drugs
#    (csv and markdown copies in analysis/output/attitudes/)
# 2. analysis/output/attitudes/policy_by_attitude.pdf (exploratory): per item, the policy distribution of
#    every substance split by the answer on the item

## 1. Correlation table (appendix): weighted Pearson correlations between the preferred policy for each illicit drug
# (beleid_voorkeur_*, 1 = ban ... 9 = free sale; columns) and the 18 attitude items (beliefs_*,
# 1 = helemaal mee oneens ... 7 = helemaal mee eens; rows). Rows sorted by the mean absolute
# correlation across the drugs. Geen mening (99) is missing; pairwise complete cases.

cor_drugs <- c(cannabis = "Cannabis", psychedelica = "Psychedelics", mdma = "MDMA",
                 cocaine = "Cocaine / crack", speed = "Speed", heroine = "Heroin")
cor_vars <- paste0("beleid_voorkeur_", names(cor_drugs))

item_vars <- grep("^beliefs_[a-z]+_[0-9]+$", names(df), value = TRUE)
stopifnot(length(item_vars) == 18)

item_labels <- map_chr(item_vars, \(v) attr(df[[v]], "label"))

dat <- df |>
  select(w, all_of(c(cor_vars, item_vars))) |>
  mutate(across(all_of(item_vars), as.numeric),
         across(all_of(cor_vars), as.numeric))

wcor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y)
  cov.wt(cbind(x[ok], y[ok]), wt = w[ok], cor = TRUE)$cor[1, 2]
}

cors <- expand_grid(item = item_vars, drug = cor_vars) |>
  mutate(r = map2_dbl(item, drug, \(i, d) wcor(dat[[i]], dat[[d]], dat$w)))

tab <- cors |>
  mutate(drug = cor_drugs[str_remove(drug, "beleid_voorkeur_")]) |>
  pivot_wider(names_from = drug, values_from = r) |>
  mutate(mean_abs_r = rowMeans(abs(across(all_of(unname(cor_drugs))))),
         wording = item_labels[match(item, item_vars)]) |>
  relocate(wording, .after = item) |>
  arrange(desc(mean_abs_r))

out_dir <- here::here("analysis", "output", "attitudes")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(tab, file.path(out_dir, "attitude_correlations.csv"))

tab |>
  mutate(across(where(is.numeric), \(x) sprintf("%.2f", x))) |>
  knitr::kable(format = "pipe", align = c("l", "l", rep("r", length(cor_drugs) + 1))) |>
  writeLines(file.path(out_dir, "attitude_correlations.md"))

print(select(tab, -wording), n = Inf, width = Inf)

# LaTeX table for the appendix: \input{out/tab_attitude_correlations}
tex_escape <- \(x) str_replace_all(x, c("\\\\" = "\\\\textbackslash{}", "([%&#_$])" = "\\\\\\1"))
fmt_r <- \(x) str_replace(sprintf("%.2f", x), "^-", "$-$")
rows <- tab |>
  mutate(across(all_of(c(unname(cor_drugs), "mean_abs_r")), fmt_r),
         wording = tex_escape(wording)) |>
  select(wording, all_of(unname(cor_drugs)), mean_abs_r) |>
  pmap_chr(\(...) paste0(paste(c(...), collapse = " & "), r"( \\)"))
writeLines(c(
  r"(\begin{table}[H])",
  r"(\centering\scriptsize)",
  paste0(r"(\caption{Weighted Pearson correlations between the preferred policy for each illicit drug )",
         r"((1 = ban \ldots{} 9 = free sale) and the attitude items (1 = strongly disagree \ldots{} 7 = strongly )",
         r"(agree), pairwise complete cases; ``no opinion'' excluded. Items in the survey wording, sorted by )",
         r"(the mean absolute correlation over the drugs ($|\bar{r}|$).})"),
  r"(\label{tab:attitude_correlations})",
  r"(\begin{tabularx}{\linewidth}{@{}Xrrrrrrr@{}})",
  r"(\toprule)",
  r"(Item & Cannabis & Psych. & MDMA & Cocaine & Speed & Heroin & $|\bar{r}|$ \\)",
  r"(\midrule)",
  rows,
  r"(\bottomrule)",
  r"(\end{tabularx})",
  r"(\end{table})"
), here::here("out", "tab_attitude_correlations.tex"))


## 2. Policy by attitude (exploratory): preferred policy for each of the nine substances, with each bar split by the answer
# on an attitude item. One PDF, one page per attitude item (beliefs_*), one stacked bar chart per
# substance: x = policy option (1 = ban ... 9 = free sale), bar height = weighted % of respondents
# choosing that option, segments = answer on the item (red = helemaal mee oneens ... yellow =
# neutraal ... green = helemaal mee eens). Geen mening (99) on the item and no opinion on the
# policy are excluded. Each chart title gives the weighted Pearson correlation between the policy
# option and the answer.

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin", pijnstillers = "Heavy painkillers")

option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")

answer_labels <- c("Helemaal mee oneens", "Mee oneens", "Enigszins mee oneens", "Neutraal",
                   "Enigszins mee eens", "Mee eens", "Helemaal mee eens")
answer_colours <- setNames(RColorBrewer::brewer.pal(7, "RdYlGn"), answer_labels)

item_labels <- set_names(item_labels, item_vars)

# one row per respondent x item x drug with an answer on both
long <- df |>
  select(w, all_of(item_vars), all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
  mutate(across(everything(), as.numeric)) |>
  pivot_longer(all_of(item_vars), names_to = "item", values_to = "answer") |>
  pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "x") |>
  filter(answer %in% 1:7, !is.na(x))

stopifnot(all(long$x %in% 1:9))

# weighted % of all respondents (per item x drug) in each option x answer cell
shares <- long |>
  mutate(answer = factor(answer, levels = 1:7, labels = answer_labels),
         drug = factor(drug_labels[drug], levels = drug_labels),
         option = factor(x, levels = 1:9)) |>
  summarise(w = sum(w), n = n(), .by = c(item, drug, option, answer)) |>
  complete(item, drug, option, answer, fill = list(w = 0, n = 0)) |>
  mutate(p = w / sum(w), .by = c(item, drug))

# each chart sums to 100%
stopifnot(all(abs(summarise(shares, s = sum(p), .by = c(item, drug))$s - 1) < 1e-9))

# weighted Pearson correlation between the policy option (1-9) and the answer (1-7)
cors_long <- long |>
  summarise(r = cov.wt(cbind(x, answer), wt = w, cor = TRUE)$cor[1, 2], .by = c(item, drug)) |>
  mutate(drug = drug_labels[drug])

plot_item <- function(item) {
  r_of <- filter(cors_long, item == .env$item) |> with(setNames(r, drug))
  strip <- setNames(sprintf("%s (r = %.2f)", drug_labels, r_of[drug_labels]), drug_labels)
  d <- filter(shares, item == .env$item) |>
    mutate(drug = factor(strip[as.character(drug)], levels = strip))
  totals <- summarise(d, p = sum(p), .by = c(drug, option))
  n_lab <- summarise(d, n = sum(n), .by = drug)
  ggplot(d, aes(option, p, fill = answer)) +
    geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
    geom_text(data = totals, aes(option, p, label = if_else(p >= 0.005, as.character(round(100 * p)), "")),
              inherit.aes = FALSE, vjust = -0.4, size = 2.6) +
    geom_text(data = n_lab, aes(x = 9.5, y = Inf, label = paste("n =", n)), inherit.aes = FALSE,
              hjust = 1, vjust = 1.5, size = 2.4, colour = "grey35") +
    facet_wrap(~drug, ncol = 3, drop = FALSE) +
    scale_fill_manual(values = answer_colours, drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(c(0, 0.12))) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = sprintf("%s: %s", item, item_labels[[item]]),
         subtitle = paste0("Preferred policy per substance; bar = weighted % of respondents choosing that option ",
                           "(total above the bar), split by answer on the item. Geen mening and no opinion excluded. ",
                           "r = weighted Pearson correlation between policy option (1–9) and answer (1–7).\n",
                           "Policy options: ", paste(1:9, option_labels, sep = " = ", collapse = ", "), "."),
         x = "Policy option (1 = ban … 9 = free sale)", y = "Respondents (weighted %)", fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8),
          strip.text = element_text(face = "bold", size = 10),
          legend.position = "bottom")
}

out_dir <- here::here("analysis", "output", "attitudes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cairo_pdf(file.path(out_dir, "policy_by_attitude.pdf"), width = 14, height = 11, onefile = TRUE)
walk(item_vars, \(item) print(plot_item(item)))
dev.off()
