library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# opening table of the results: weighted % per grouped regime, per demographic group,
# for currently legal/tolerated drugs and for illegal drugs

blocks <- c(alcohol = "legal", tabak_en_vapes = "legal", cannabis = "legal",
            psychedelica = "illegal", mdma = "illegal", cocaine = "illegal",
            speed = "illegal", heroine = "illegal")

# demographic variables and English labels for their categories (in level order)
demographics <- list(
  "Sex"          = list(var = "sex_rec",   labels = c("Man", "Woman")),
  "Education"    = list(var = "edu_3_rec", labels = c("Practical", "Middle", "Theoretical")),
  "Age"          = list(var = "age_4_rec", labels = c("18--34", "35--49", "50--64", "65+")),
  "Vote (TK 2025)" = list(var = "vote_3",  labels = c("Left", "Middle", "Right")),
  "Urbanisation" = list(var = "urb",       labels = c("Very high", "High", "Moderate", "Low", "Not urban"))
)

df <- df |> mutate(urb = haven::as_factor(urb), edu_2_rec = haven::as_factor(edu_2_rec),
                   total = factor("Total"))

# one row per respondent x drug
long <- df |>
  select(w, total, all_of(unname(map_chr(demographics, "var"))), edu_2_rec,
         all_of(paste0("regime_", names(blocks)))) |>
  pivot_longer(starts_with("regime_"), names_to = "drug", names_prefix = "regime_",
               values_to = "regime") |>
  filter(!is.na(regime)) |>
  mutate(block = factor(blocks[drug], levels = c("legal", "illegal")))

# weighted % per regime within each category and block; n = respondents (unweighted)
regime_table <- function(var) {
  shares <- long |>
    filter(!is.na(.data[[var]])) |>
    summarise(w = sum(w), .by = c(all_of(var), block, regime)) |>
    complete(.data[[var]], block, regime, fill = list(w = 0)) |>
    mutate(p = 100 * w / sum(w), .by = c(all_of(var), block)) |>
    arrange(block, regime) |>
    pivot_wider(id_cols = all_of(var), names_from = c(block, regime), values_from = p)
  n <- df |> filter(!is.na(.data[[var]])) |> count(.data[[var]])
  left_join(n, shares, by = var) |> rename(cat = 1) |> arrange(cat)
}

total <- regime_table("total")
by_demo <- map(demographics, \(d) regime_table(d$var) |> mutate(cat = d$labels[as.integer(cat)]))

# each block sums to 100 per row
check <- bind_rows(total, list_rbind(by_demo))
stopifnot(all(abs(rowSums(select(check, starts_with("legal_"))) - 100) < 1e-6),
          all(abs(rowSums(select(check, starts_with("illegal_"))) - 100) < 1e-6))

# LaTeX rows: label & n & 4 legal & 4 illegal
tex_row <- function(row, label) {
  pct <- row |> select(starts_with("legal_"), starts_with("illegal_")) |> unlist() |> round()
  paste0(label, " & ", row$n, " & ", paste(pct, collapse = " & "), r"( \\)")
}

body <- c(tex_row(total, r"(\textbf{Total})"), r"(\addlinespace)")
for (name in names(by_demo)) {
  tab <- by_demo[[name]]
  body <- c(body, paste0(r"(\textit{)", name, r"(} \\)"),
            map_chr(seq_len(nrow(tab)), \(i) tex_row(tab[i, ], paste0(r"(\quad )", tab$cat[i]))),
            r"(\addlinespace)")
}
body <- head(body, -1)

caption <- paste0(
  "Preferred grouped regime per demographic group (weighted \\%). ",
  "Ban = 1; Med.\\ = medical/research (2--4); Shop = specialist shop (5--6); Market = general market (7--9). ",
  "Shares are pooled over the drugs in each block: legal/tolerated = alcohol, tobacco and vapes, cannabis; ",
  "illegal = psychedelics, MDMA, cocaine/crack, speed, heroin. Each block sums to 100\\% per row (up to rounding). ",
  "$n$ = respondents (unweighted); ``no opinion'' answers are excluded. ",
  sprintf("Vote: parties grouped into left, middle and right; %d respondents who voted for another party, ",
          sum(is.na(df$vote_3))),
  "voted blank or did not vote are not in the vote rows."
)

writeLines(c(
  r"(\begin{table}[H])",
  r"(\centering\small)",
  paste0(r"(\caption{)", caption, "}"),
  r"(\label{tab:regimes_demographics})",
  r"(\begin{tabular}{@{}lrrrrrrrrr@{}})",
  r"(\toprule)",
  r"( & & \multicolumn{4}{c}{Legal/tolerated} & \multicolumn{4}{c}{Illegal} \\)",
  r"(\cmidrule(lr){3-6}\cmidrule(l){7-10})",
  r"( & $n$ & Ban & Med. & Shop & Market & Ban & Med. & Shop & Market \\)",
  r"(\midrule)",
  body,
  r"(\bottomrule)",
  r"(\end{tabular})",
  r"(\end{table})"
), here::here("out", "tab_regimes_demographics.tex"))


## Regime shifts by demographic (figure)
# per drug and regime: percentage points more (or fewer) in one group than in the other;
# per drug the four shifts sum to 0

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

# two groups per demographic, as level positions; bar = second minus first
contrasts <- list(
  "Age"            = list(var = "age_4_rec", first = 1:2, second = 3:4,
                          labels = c("18–49", "50+")),
  "Vote (TK 2025)" = list(var = "vote_3",    first = 1,   second = 3,
                          labels = c("Left", "Right")),
  "Education"      = list(var = "edu_2_rec", first = 2,   second = 1,
                          labels = c("Theoretical", "Practical")),
  "Urbanisation"   = list(var = "urb",       first = 1:2, second = 4:5,
                          labels = c("Urban", "Rural"))
)

# the respondent's group in a contrast: "first", "second" or NA
contrast_group <- function(x, cn) {
  pos <- as.integer(x)
  case_when(pos %in% cn$first ~ "first", pos %in% cn$second ~ "second")
}

regime_shift <- function(name) {
  cn <- contrasts[[name]]
  long |>
    mutate(group = contrast_group(.data[[cn$var]], cn)) |>
    filter(!is.na(group)) |>
    summarise(w = sum(w), .by = c(group, drug, regime)) |>
    complete(group, drug, regime, fill = list(w = 0)) |>
    mutate(p = 100 * w / sum(w), .by = c(group, drug)) |>
    select(-w) |>
    pivot_wider(names_from = group, values_from = p) |>
    transmute(demo = name, contrast = paste0(cn$labels[2], " vs ", cn$labels[1]), drug, regime,
              shift = second - first)
}

shifts <- map(names(contrasts), regime_shift) |> list_rbind()
stopifnot(all(abs(summarise(shifts, s = sum(shift), .by = c(demo, drug))$s) < 1e-6))

# drugs in the order of the main figure (by weighted % ban), first on top
drug_order <- long |>
  summarise(ban = sum(w[regime == "ban"]) / sum(w), .by = drug) |>
  arrange(ban) |>
  pull(drug)

shifts <- shifts |>
  mutate(drug = factor(drug_labels[drug], levels = rev(drug_labels[drug_order])),
         contrast = factor(contrast, levels = unique(contrast)),
         regime = factor(regime, levels = regime_labels))

# regime colours: the middle option of each group on the viridis scale of the main figure
regime_colours <- setNames(viridisLite::viridis(9, end = 0.9)[c(1, 3, 6, 8)], regime_labels)

p_shifts <- ggplot(shifts, aes(shift, drug, fill = regime)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(abs(shift) >= 15, sprintf("%+.0f", shift), ""),
                hjust = if_else(shift > 0, -0.15, 1.15)), size = 2) +
  facet_grid(contrast ~ regime, labeller = labeller(contrast = label_wrap_gen(12), regime = str_to_sentence)) +
  scale_fill_manual(values = regime_colours, guide = "none") +
  scale_x_continuous(limits = c(-60, 60), breaks = c(-40, 0, 40)) +
  labs(x = "Difference in percentage points (positive = more in first-named group)", y = NULL) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.text.y = element_text(angle = 0, hjust = 0))

ggsave(here::here("out", "fig_regime_shifts.pdf"), p_shifts,
       width = 6.5, height = 7, device = cairo_pdf)

# n per group in the contrasts, for the caption
contrast_n <- map_chr(contrasts, \(cn) {
  n <- table(contrast_group(df[[cn$var]], cn))
  sprintf("%s %d, %s %d", cn$labels[2], n[["second"]], cn$labels[1], n[["first"]])
})

caption_shifts <- paste0(
  "Shift in preferred grouped regime between two groups per demographic: age 50+ vs 18--49; ",
  "vote right vs left; practical (incl.\\ middle) vs theoretical education; ",
  "rural (low or no urbanisation) vs urban (high or very high). ",
  "Bars give the difference in weighted share choosing a regime (percentage points; positive = more ",
  "in the first-named group); per drug the four shifts sum to zero. Drugs are ordered as in ",
  r"(Figure~\ref{fig:policy_preferences}; values of 15 or more are labelled. )",
  "Group sizes ($n$, unweighted): ", paste(contrast_n, collapse = "; "), ". ",
  "Vote: parties grouped into left, middle and right (provisional)."
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \includegraphics[width=\linewidth]{out/fig_regime_shifts.pdf})",
  paste0(r"(  \caption{)", caption_shifts, "}"),
  r"(  \label{fig:regime_shifts})",
  r"(\end{figure})"
), here::here("out", "fig_regime_shifts.tex"))
