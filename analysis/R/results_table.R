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

# rows grouped by the alternative to a ban that the young/left/theoretical/urban group favours
drug_blocks <- tribble(
  ~drug,            ~block,
  "cannabis",       "Ban vs specialist shop",
  "psychedelica",   "Ban vs specialist shop",
  "mdma",           "Ban vs specialist shop",
  "speed",          "Ban vs medical/research",
  "cocaine",        "Ban vs medical/research",
  "heroine",        "Ban vs medical/research",
  "alcohol",        "Legal: specialist shop vs market",
  "tabak_en_vapes", "Legal: specialist shop vs market"
) |>
  mutate(block = factor(block, levels = unique(block)))
stopifnot(setequal(drug_blocks$drug, names(drug_labels)), !anyDuplicated(drug_blocks$drug))

# within a block, drugs in the order of the main figure (by weighted % ban), first on top
drug_order <- long |>
  summarise(ban = sum(w[regime == "ban"]) / sum(w), .by = drug) |>
  arrange(ban) |>
  pull(drug)

shifts <- shifts |>
  left_join(drug_blocks, by = "drug") |>
  mutate(drug = factor(drug_labels[drug], levels = rev(drug_labels[drug_order])),
         demo = factor(word(demo, 1), levels = c("Age", "Education", "Urbanisation", "Vote")),
         regime = factor(regime, levels = regime_labels))

ranges <- shifts |> summarise(lo = min(shift), hi = max(shift), .by = c(block, drug, regime))

# cells that carry the pattern get a light tint in the regime colour
highlight <- tribble(
  ~block,                             ~regime,
  "Ban vs specialist shop",           "ban",
  "Ban vs specialist shop",           "specialist shop",
  "Ban vs medical/research",          "ban",
  "Ban vs medical/research",          "medical/research",
  "Legal: specialist shop vs market", "specialist shop",
  "Legal: specialist shop vs market", "general market"
) |>
  mutate(block = factor(block, levels = levels(drug_blocks$block)),
         regime = factor(regime, levels = regime_labels))

# regime colours: the middle option of each group on the viridis scale of the main figure
regime_colours <- setNames(viridisLite::viridis(9, end = 0.9)[c(1, 3, 6, 8)], regime_labels)
# demographics: Okabe-Ito, distinct from the regime colours
demo_colours <- c(Age = "#E69F00", Education = "#56B4E9", Urbanisation = "#009E73", Vote = "#D55E00")

p_shifts <- ggplot(shifts, aes(shift, drug)) +
  geom_rect(data = highlight, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = regime),
            inherit.aes = FALSE, alpha = 0.12) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_segment(data = ranges, aes(x = lo, xend = hi, yend = drug), colour = "grey70", linewidth = 1) +
  geom_point(aes(colour = demo, shape = demo), size = 1.8) +
  facet_grid(block ~ regime, scales = "free_y", space = "free_y",
             labeller = labeller(block = label_wrap_gen(14), regime = str_to_sentence)) +
  scale_fill_manual(values = regime_colours, guide = "none") +
  scale_colour_manual(values = demo_colours, name = NULL) +
  scale_shape_manual(values = c(16, 17, 15, 18), name = NULL) +
  scale_x_continuous(limits = c(-40, 40), breaks = c(-30, 0, 30)) +
  labs(x = "← more among young, left, theoretical, urban          more among old, right, practical, rural →\nDifference in percentage points",
       y = NULL) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(0.8, "lines"),
        strip.text = element_text(face = "bold"),
        strip.text.y = element_text(angle = 0, hjust = 0))

ggsave(here::here("out", "fig_regime_shifts.pdf"), p_shifts,
       width = 6.5, height = 4.6, device = cairo_pdf)

# n per group in the contrasts, for the caption
contrast_n <- map_chr(contrasts, \(cn) {
  n <- table(contrast_group(df[[cn$var]], cn))
  sprintf("%s %d, %s %d", cn$labels[2], n[["second"]], cn$labels[1], n[["first"]])
})

caption_shifts <- paste0(
  "Older, right-wing, practically educated and rural respondents more often prefer a ban for every ",
  "substance except alcohol and tobacco. Their counterparts instead favour a specialist shop for cannabis, ",
  "psychedelics and MDMA, and medical or research supply for speed, cocaine and heroin. For alcohol and ",
  "tobacco, right-wing and rural respondents favour a general market over a specialist shop. ",
  "Each point is the difference in weighted share choosing a regime (percentage points) between two groups: ",
  "age 50+ vs 18--49; vote right vs left; practical (incl.\\ middle) vs theoretical education; ",
  "rural (low or no urbanisation) vs urban (high or very high). Positive values: more among the older, ",
  "right-wing, practically educated or rural group; per drug and demographic the four differences sum to zero. ",
  "Grey bars span the four demographics; shaded cells mark where the pattern lies. ",
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
