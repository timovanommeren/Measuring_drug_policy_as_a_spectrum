library(tidyverse)
library(patchwork)

source(here::here("analysis", "R", "load_and_clean.R"))

# figure 3: preferred policy per substance by demographic. One panel per substance x demographic, with a
# weighted 100% stacked bar of the nine answer options (colours as in figure 2) per category of the
# demographic. Substances in two blocks of four. Two versions:
# - coarse (paper, out/): age in 4 groups, education in 3, urban vs less urban, left/middle/right vote
# - detailed (exploratory, analysis/output/gradient/): age in 7, education in 7, urbanisation in 5, parties

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", speed = "Speed", cocaine = "Cocaine",
                 heroine = "Heroin")
drug_blocks <- list(names(drug_labels)[1:4], names(drug_labels)[5:8])

# answer options as in descriptives.R (figure 2), with the same colours
option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")
option_colours <- setNames(viridisLite::viridis(9, end = 0.9), option_labels)

edu_7 <- c("Master/doctorate", "Bachelor", "Propaedeutic year", "HAVO/VWO", "MBO 2–4", "MAVO/VMBO-t",
           "MBO 1/VMBO-b/k or lower")
urb_5 <- c("Very high", "High", "Moderate", "Low", "Not urban")
party_out <- c("DENK", "Anders", "Blanco", "Mocht (nog) niet stemmen", "Niet-stemmers")

stopifnot(sum(as.integer(df$edu_clean) == 10) == 0)

# per version: the category of each respondent per demographic (NA = not shown) and the order of the
# categories, top first (NULL = parties, ordered by their share choosing a ban below)
versions <- list(
  coarse = list(
    Age = list(cat = str_replace(as.character(df$age_4_rec), "-", "–"), order = c("18–34", "35–49", "50–64", "65+")),
    Education = list(cat = c("Practical", "Middle", "Theoretical")[as.integer(df$edu_3_rec)],
                     order = c("Theoretical", "Middle", "Practical")),
    Urbanisation = list(cat = c("Urban", "Urban", "Less urban", "Less urban", "Less urban")[as.integer(df$urb)],
                        order = c("Urban", "Less urban")),
    Vote = list(cat = str_to_sentence(as.character(df$vote_3)), order = c("Left", "Middle", "Right"))
  ),
  detailed = list(
    Age = list(cat = str_replace(as.character(haven::as_factor(df$age_7_rec)), "-", "–"),
               order = c("18–24", "25–34", "35–44", "45–54", "55–64", "65–74", "75+")),
    # MBO 1/VMBO-b/k (7), practical/special education (8) and primary school (9) together;
    # "geen onderwijs" (10) has no respondents
    Education = list(cat = edu_7[pmin(as.integer(df$edu_clean), 7)], order = edu_7),
    Urbanisation = list(cat = urb_5[as.integer(df$urb)], order = urb_5),
    Party = list(cat = if_else(df$vote_recall_rec %in% party_out, NA_character_, as.character(df$vote_recall_rec)),
                 order = NULL)
  )
)

# weighted share of each option per category and drug; "no opinion" (99) is already NA
gradient_shares <- function(version) {
  demos <- versions[[version]]
  resp <- df |>
    select(w, all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
    mutate(across(-w, as.numeric)) |>
    bind_cols(as_tibble(map(demos, "cat")))
  long <- resp |>
    pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
                 values_to = "option") |>
    filter(!is.na(option)) |>
    pivot_longer(all_of(names(demos)), names_to = "demo", values_to = "cat") |>
    filter(!is.na(cat))
  stopifnot(all(long$option %in% 1:9))

  # parties: by their weighted share choosing a ban, pooled over the substances, lowest on top
  order <- map(names(demos), \(d) {
    demos[[d]]$order %||% (long |> filter(demo == d) |> summarise(ban = sum(w[option == 1]) / sum(w), .by = cat) |>
                             arrange(ban) |> pull(cat))
  }) |> set_names(names(demos))
  stopifnot(all(map_lgl(names(demos), \(d) setequal(order[[d]], na.omit(unique(resp[[d]]))))))

  # respondents per category (unweighted), for the row labels
  cat_n <- resp |>
    pivot_longer(all_of(names(demos)), names_to = "demo", values_to = "cat") |>
    filter(!is.na(cat)) |>
    count(demo, cat)
  row_label <- \(d, c) sprintf("%s (%d)", c, cat_n$n[match(paste(d, c), paste(cat_n$demo, cat_n$cat))])
  # bottom first, so the first category of each demographic is on top
  row_levels <- rev(unlist(map(names(demos), \(d) row_label(d, order[[d]]))))

  shares <- long |>
    summarise(w = sum(w), .by = c(demo, cat, drug, option)) |>
    complete(nesting(demo, cat), drug, option = 1:9, fill = list(w = 0)) |>
    mutate(p = w / sum(w), .by = c(demo, cat, drug)) |>
    mutate(row = factor(row_label(demo, cat), levels = row_levels),
           demo = factor(demo, levels = names(demos)),
           drug_label = factor(drug_labels[drug], levels = drug_labels),
           option = factor(option_labels[option], levels = option_labels),
           # white text on the dark half of the palette
           txt = if_else(as.integer(option) <= 5, "white", "black"))
  stopifnot(all(abs(summarise(shares, s = sum(p), .by = c(demo, cat, drug))$s - 1) < 1e-9))
  list(shares = shares, resp = resp)
}

plot_block <- function(shares, drugs, label_min, label_size, row_text) {
  ggplot(filter(shares, drug %in% drugs), aes(p, row, fill = option)) +
    geom_col(position = position_fill(reverse = TRUE), width = 0.82) +
    # group = option keeps the labels stacked like the bars (the colour mapping would regroup them)
    geom_text(aes(label = if_else(p >= label_min, as.character(round(100 * p)), ""), colour = txt, group = option),
              position = position_fill(reverse = TRUE, vjust = 0.5), size = label_size) +
    facet_grid(demo ~ drug_label, scales = "free_y", space = "free_y") +
    scale_fill_manual(values = option_colours, drop = FALSE) +
    scale_colour_identity() +
    scale_x_continuous(labels = c("0", "50%", "100%"), breaks = c(0, 0.5, 1), expand = expansion(0)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 7) +
    theme(panel.grid = element_blank(),
          strip.text.x = element_text(face = "bold", size = 7.5, margin = margin(1, 0, 2, 0)),
          # horizontal: a demographic with two rows is too short for vertical text
          strip.text.y = element_text(face = "bold", angle = 0, hjust = 0, size = 6.5),
          axis.text.y = element_text(size = row_text),
          axis.text.x = element_text(size = 5.5),
          panel.spacing.x = unit(0.7, "lines"),
          panel.spacing.y = unit(0.35, "lines"),
          legend.position = "bottom",
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.25, "cm"),
          plot.margin = margin(2, 4, 2, 2))
}

plot_grid <- function(shares, ...) {
  (plot_block(shares, drug_blocks[[1]], ...) / plot_block(shares, drug_blocks[[2]], ...)) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

coarse <- gradient_shares("coarse")
detailed <- gradient_shares("detailed")

# the coarse version has three rows per demographic on average: thicker bars, larger labels
ggsave(here::here("out", "fig_gradient_grid.pdf"),
       plot_grid(coarse$shares, label_min = 0.08, label_size = 1.9, row_text = 6.5),
       width = 6.5, height = 5.4, device = cairo_pdf)

out_dir <- here::here("analysis", "output", "gradient")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "fig_gradient_grid_detailed.pdf"),
       plot_grid(detailed$shares, label_min = 0.12, label_size = 1.6, row_text = 5.6),
       width = 6.5, height = 8.35, device = cairo_pdf)

vote_n <- table(df$vote_3)
caption <- paste0(
  r"(Preferred regulatory model per substance by age, education, urbanisation and vote. Each bar shows the )",
  r"(weighted shares of a group choosing each of the nine options, from ban (left) to free sale (right); )",
  r"(colours as in Figure~\ref{fig:policy_preferences}, labels from 8\%. )",
  r"(Numbers in brackets are respondents (unweighted). Urban = very high or high address density, )",
  sprintf(r"(less urban = moderate, low or none (%d respondents without urbanisation are not in those rows). )",
          sum(is.na(df$urb))),
  r"(Vote (TK 2025) by the party's general left-right position in the Chapel Hill Expert Survey 2024 )",
  r"(\parencite{rovny2025}, split into equal thirds of the 0--10 scale: left = SP, PvdD, GroenLinks-PvdA; )",
  r"(middle = DENK, Volt, CU, D66, 50PLUS (2019 score), CDA; right = VVD, BBB, SGP, PVV, FvD and JA21 (not )",
  sprintf(r"(rated; placed between VVD and PVV). Other parties, blank and non-voters ($n = %d$) are not in the vote rows.)",
          sum(is.na(df$vote_3)))
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \includegraphics[width=\linewidth]{out/fig_gradient_grid.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:gradient})",
  r"(\end{figure})"
), here::here("out", "fig_gradient_grid.tex"))
