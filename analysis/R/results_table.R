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
