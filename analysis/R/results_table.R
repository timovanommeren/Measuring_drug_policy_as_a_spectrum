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
  "Vote (TK 2025)" = list(var = "vote_lr", labels = c("Left", "Right")),
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
  "Vote: left = GroenLinks-PvdA, D66, SP, PvdD, Volt, DENK, CU; right = PVV, VVD, CDA, JA21, FvD, BBB, ",
  sprintf("50PLUS, SGP; %d respondents who voted for another party, ", sum(is.na(df$vote_lr))),
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


## Gradient by number of group A traits (figure 3a) and party map (figure 3b)
# 3a: per drug, weighted share of each answer option per row; a row = how many of four traits a respondent
# has (age 18-49, theoretical education, high/very high urbanisation, left vote). Middle education and
# moderate urbanisation count as 0; respondents without urbanisation or a left/right vote are left out.
# 3b (out/fig_party_map.pdf) is made by analysis/R/party_map.R

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", speed = "Speed", cocaine = "Cocaine",
                 heroine = "Heroin")

# answer options as in descriptives.R (figure 2), with the same colours
option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")
option_colours <- setNames(viridisLite::viridis(9, end = 0.9), option_labels)

traits <- df |>
  transmute(w,
            age = as.integer(age_4_rec), edu = as.integer(haven::as_factor(edu_3_rec)),
            urb = as.integer(urb), left = as.integer(vote_lr == "left"),
            across(all_of(paste0("beleid_voorkeur_", names(drug_labels))), as.numeric)) |>
  filter(!is.na(urb), !is.na(left)) |>
  mutate(row = factor((age <= 2) + (edu == 3) + (urb <= 2) + left, levels = 0:4))

stopifnot(identical(as.vector(table(traits$row)), c(154L, 281L, 294L, 274L, 132L)))

row_n <- table(traits$row)
row_labels <- setNames(sprintf("%s (n = %d)", names(row_n), row_n), names(row_n))
row_labels[["4"]] <- paste0(row_labels[["4"]], "\nyoung, theoretical,\nurban, left")
row_labels[["0"]] <- paste0(row_labels[["0"]], "\nold, practical,\nrural, right")

# "no opinion" (99) is already NA
gradient <- traits |>
  select(w, row, starts_with("beleid_voorkeur_")) |>
  pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "x") |>
  filter(!is.na(x)) |>
  mutate(option = factor(option_labels[x], levels = option_labels)) |>
  summarise(w = sum(w), .by = c(row, drug, option)) |>
  complete(row, drug, option, fill = list(w = 0)) |>
  mutate(p = w / sum(w), .by = c(row, drug),
         drug = factor(drug_labels[drug], levels = drug_labels),
         # white text on the dark half of the palette
         txt = if_else(as.integer(option) <= 5, "white", "black"))

stopifnot(all(abs(summarise(gradient, s = sum(p), .by = c(row, drug))$s - 1) < 1e-9))

p_gradient <- ggplot(gradient, aes(p, row, fill = option)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
  # group = option keeps the labels stacked like the bars (the colour mapping would regroup them)
  geom_text(aes(label = if_else(p >= 0.1, scales::percent(p, 1), ""), colour = txt, group = option),
            position = position_fill(reverse = TRUE, vjust = 0.5), size = 1.9) +
  facet_wrap(~drug, nrow = 2) +
  scale_fill_manual(values = option_colours, drop = FALSE) +
  scale_colour_identity() +
  scale_x_continuous(labels = c("0", "50%", "100%"), breaks = c(0, 0.5, 1), expand = expansion(0)) +
  scale_y_discrete(labels = row_labels) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = NULL, y = "Number of group A traits", fill = NULL) +
  theme_minimal(base_size = 8) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 6, lineheight = 0.9),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6.5),
        panel.spacing.x = unit(1.4, "lines"),
        plot.margin = margin(4, 8, 4, 4))

ggsave(here::here("out", "fig_gradient.pdf"), p_gradient, width = 6.5, height = 4.4, device = cairo_pdf)

party_stats <- read_csv(here::here("out", "fig_party_map_stats.csv"), show_col_types = FALSE)

caption_gradient <- paste0(
  r"(\textbf{(a)} Preferred regulatory model per substance by the number of group A traits: age 18--49, )",
  r"(theoretical education, high or very high urbanisation and a left vote. Middle education and moderate )",
  r"(urbanisation count as not group A. Weighted shares; labels from 10\%; colours as in )",
  r"(Figure~\ref{fig:policy_preferences}. )",
  sprintf(r"(Respondents without urbanisation or a left/right vote are left out ($n = %d$). )", nrow(traits)),
  r"(\textbf{(b)} Parties by support for strict regulation and for punitive measures. )",
  r"(Horizontal: weighted share of answers in medical/research (2--4) or specialist shop (5--6), averaged per )",
  r"(respondent over the eight substances; the dashed line is the sample mean. Vertical: mean agreement )",
  r"((1--7) with the four crime items, two of them reversed so that higher is more punitive )",
  sprintf(r"(($\alpha = %.2f$; at least three items answered); the dashed line is neutral. )", party_stats$alpha),
  sprintf(r"(Logo size reflects $n$ ($n = %d$ in total); DENK ($n = 1$), other parties, blank and )", party_stats$n),
  r"(non-voters are left out. Vote: TK 2025. )",
  r"(Left = GroenLinks-PvdA, D66, SP, PvdD, Volt, DENK, CU; right = PVV, VVD, CDA, JA21, FvD, BBB, 50PLUS, SGP.)"
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \textbf{(a)}\par)",
  r"(  \includegraphics[width=\linewidth]{out/fig_gradient.pdf}\par\medskip)",
  r"(  \textbf{(b)}\par)",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map.pdf})",
  paste0(r"(  \caption{)", caption_gradient, "}"),
  r"(  \label{fig:gradient})",
  r"(\end{figure})"
), here::here("out", "fig_gradient.tex"))

